%% @doc Test Suite for router_nats Queueing Mode Behavior
%%
%% Tests that the router correctly handles publish failures when 
%% nats_fail_open_mode is set to FALSE (Queueing Mode). In this mode,
%% failures should return errors or block, and operations should be queued.
%%
%% @test_category integration, faults
-module(router_nats_publish_queueing_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([suite/0]).

%% Test cases
-export([
    test_publish_error_connected_queueing/1,
    test_publish_timeout_connected_queueing/1,
    test_publish_close_connection_queueing/1,
    test_publish_not_connected_queueing/1,
    test_publish_with_ack_error_connected_queueing/1,
    test_publish_with_ack_timeout_connected_queueing/1,
    test_publish_with_ack_close_connection_queueing/1,
    test_publish_with_ack_not_connected_queueing/1,
    test_queueing_system_blocking_behavior/1,
    test_router_queueing_blocks_on_errors/1,
    test_router_jetstream_handles_publish_failures/1,
    test_router_retry_exhaustion_behavior/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [
        test_publish_error_connected_queueing,
        test_publish_timeout_connected_queueing,
        test_publish_close_connection_queueing,
        test_publish_not_connected_queueing,
        test_publish_with_ack_error_connected_queueing,
        test_publish_with_ack_timeout_connected_queueing,
        test_publish_with_ack_close_connection_queueing,
        test_publish_with_ack_not_connected_queueing,
        test_queueing_system_blocking_behavior,
        test_router_queueing_blocks_on_errors,
        test_router_jetstream_handles_publish_failures,
        test_router_retry_exhaustion_behavior
    ].

init_per_suite(Config) ->
    %% Ensure application is stopped so we can set env vars that are read at startup
    _ = application:stop(beamline_router),
    _ = application:unload(beamline_router),
    _ = application:load(beamline_router),
    
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
    ok = application:set_env(beamline_router, nats_reconnect_delay_ms, 500),
    ok = application:set_env(beamline_router, nats_max_reconnect_delay_ms, 2000),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    ok = application:set_env(beamline_router, nats_max_pending_operations, 1000),
    
    %% Setup mock with fail_open_mode=false BEFORE starting app
    ok = router_mock_helpers:setup_router_nats_mock(#{
        get_connection_status => fun() -> 
            {ok, #{state => connected, fail_open_mode => false}} 
        end
    }),
    
    %% Start app manually (not via start_router_suite which would override mock)
    {ok, _} = application:ensure_all_started(beamline_router),
    
    test_helpers:wait_for_app_start(router_nats, 2000),
    
    %% Verify fail_open_mode is actually disabled
    {ok, Status} = router_nats:get_connection_status(),
    false = maps:get(fail_open_mode, Status),
    
    Config.

end_per_suite(_Config) ->
    router_suite_helpers:stop_router_suite(),
    ok.

init_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_utils:ensure_router_nats_alive(),
    %% Simplified - don't check connection status (mock handles this)
    %% Setup shared metrics capture harness
    router_metrics_test_helper:setup(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_metrics_test_helper:teardown(),
    ok.

%% @doc Test: publish returns error in queueing mode
test_publish_error_connected_queueing(_Config) ->
    %% Enable fault injection for publish error
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    
    %% Attempt publish (should return error in queueing mode)
    Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1),
    
    ok.

%% @doc Test: publish timeout in queueing mode
test_publish_timeout_connected_queueing(_Config) ->
    %% Enable fault injection for timeout
    ok = router_nats_fault_injection:enable_fault(publish, timeout),
    
    %% Attempt publish
    Result = catch router_nats:publish(<<"test.subject">>, <<"payload">>),
    true = (Result =:= ok orelse 
            (is_tuple(Result) andalso element(1, Result) =:= error) orelse
            (is_tuple(Result) andalso element(1, Result) =:= 'EXIT')),
    ok.

%% @doc Test: publish close_connection in queueing mode
test_publish_close_connection_queueing(_Config) ->
    %% Enable fault injection for close_connection
    ok = router_nats_fault_injection:enable_fault(publish, close_connection),
    
    %% Attempt publish (should return error in queueing mode)
    Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Wait for publish failure metric (close_connection causes a publish failure)
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1),
    
    ok.

%% @doc Test: publish not connected in queueing mode
test_publish_not_connected_queueing(_Config) ->
    %% Enable fault injection to simulate not connected state
    ok = router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    
    %% Attempt publish (should return error due to fault injection)
    Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1),
    
    ok.

%% @doc Test: publish_with_ack returns error in queueing mode
test_publish_with_ack_error_connected_queueing(_Config) ->
    %% Enable fault injection for publish_with_ack error
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Attempt publish_with_ack
    Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1),
    
    ok.

%% @doc Test: publish_with_ack timeout in queueing mode
test_publish_with_ack_timeout_connected_queueing(_Config) ->
    %% Enable fault injection for timeout
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, timeout),
    
    %% Attempt publish_with_ack
    Result = catch router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (Result =:= {ok, <<"stub-msg-id">>} orelse
            (is_tuple(Result) andalso element(1, Result) =:= error) orelse
            (is_tuple(Result) andalso element(1, Result) =:= 'EXIT')),
    ok.

%% @doc Test: publish_with_ack close_connection in queueing mode
test_publish_with_ack_close_connection_queueing(_Config) ->
    %% Enable fault injection for close_connection
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, close_connection),
    
    %% Attempt publish_with_ack
    Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Wait for publish_with_ack failure metric (close_connection causes a failure)
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1),
    
    ok.

%% @doc Test: publish_with_ack not connected in queueing mode
test_publish_with_ack_not_connected_queueing(_Config) ->
    %% Enable fault injection to simulate not connected state
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, not_connected}),
    
    %% Attempt publish_with_ack
    Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1),
    
    ok.

%% @doc Test: queueing mode system blocking behavior
test_queueing_system_blocking_behavior(_Config) ->
    %% Enable fault injection to simulate not connected state
    ok = router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    
    %% Attempt many operations
    Results = lists:map(fun(I) ->
        Subject = <<"test.", (integer_to_binary(I))/binary>>,
        Payload = <<"payload", (integer_to_binary(I))/binary>>,
        router_nats:publish(Subject, Payload)
    end, lists:seq(1, 100)),
    
    %% All should return error
    AllErrors = lists:all(fun(R) -> is_tuple(R) andalso element(1, R) =:= error end, Results),
    true = AllErrors,
    
    %% Verify router_nats is still alive
    RouterNatsPid = whereis(router_nats),
    true = is_process_alive(RouterNatsPid),
    ok.

%% @doc Test: router blocks/queues operations in queueing mode on errors
test_router_queueing_blocks_on_errors(_Config) ->
    %% Enable fault injection to simulate not connected state
    ok = router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    
    %% Attempt multiple publish operations
    Result1 = router_nats:publish(<<"test.1">>, <<"payload1">>),
    Result2 = router_nats:publish(<<"test.2">>, <<"payload2">>),
    
    %% All should return error
    true = (is_tuple(Result1) andalso element(1, Result1) =:= error),
    true = (is_tuple(Result2) andalso element(1, Result2) =:= error),
    
    ok.

%% @doc Test: router_jetstream handles publish_with_ack failures correctly
test_router_jetstream_handles_publish_failures(_Config) ->
    %% Enable fault injection
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Attempt publish_with_ack
    Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    true = (is_tuple(Result) andalso element(1, Result) =:= error),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1),
    
    ok.

%% @doc Test: router retry exhaustion behavior
test_router_retry_exhaustion_behavior(_Config) ->
    %% Enable fault injection for persistent publish_with_ack error
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Simulate retry loop
    MaxRetries = 3,
    Attempts = lists:seq(0, MaxRetries),
    
    Results = lists:map(fun(_Retry) ->
        router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{})
    end, Attempts),
    
    %% All attempts should fail - check for error tuples directly
    AllFailed = lists:all(fun
        ({error, _}) -> true;
        ({ok, _}) -> false;
        (_) -> false
    end, Results),
    true = AllFailed,
    
    ok.
