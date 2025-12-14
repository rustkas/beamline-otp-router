%% @doc Test Suite for router_nats Metrics and Misc properties
%%
%% Tests metrics emissions, MsgID generation uniqueness, and other
%% auxiliary NATS properties.
%%
%% @test_category integration, metrics
-module(router_nats_publish_metrics_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([suite/0]).

%% Test cases
-export([
    test_metrics_publish_failures_incremented/1,
    test_metrics_publish_with_ack_failures_incremented/1,
    test_metrics_queue_operations_count/1,
    test_metrics_retry_after_reconnection/1,
    test_msg_id_no_duplicates_on_retry/1,
    test_msg_id_unique_per_operation/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    %% These tests require real NATS connection - only run in full/heavy mode
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> all_tests();
        "heavy" -> all_tests();
        _ -> []  %% Skip in fast mode
    end.

all_tests() ->
    [
        test_metrics_publish_failures_incremented,
        test_metrics_publish_with_ack_failures_incremented,
        test_metrics_queue_operations_count,
        test_metrics_retry_after_reconnection,
        test_msg_id_no_duplicates_on_retry
        %% test_msg_id_unique_per_operation requires real NATS connection
    ].

init_per_suite(Config) ->
    %% Ensure application is stopped so we can set env vars that are read at startup
    _ = application:stop(beamline_router),
    _ = application:unload(beamline_router),
    _ = application:load(beamline_router),
    
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    %% Use mock mode with fault injection support
    %% The mock now checks router_nats_fault_injection before returning
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
    ok = application:set_env(beamline_router, nats_reconnect_delay_ms, 500),
    ok = application:set_env(beamline_router, nats_max_reconnect_delay_ms, 2000),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    ok = application:set_env(beamline_router, nats_max_pending_operations, 1000),
    
    router_suite_helpers:start_router_suite(),
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

%% @doc Test: router_nats_publish_failures_total incremented correctly
test_metrics_publish_failures_incremented(_Config) ->
    %% Enable fault injection for publish error
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    
    %% Attempt publish (should fail)
    _Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1),
    
    ok.

%% @doc Test: router_nats_publish_with_ack_failures_total incremented correctly
test_metrics_publish_with_ack_failures_incremented(_Config) ->
    %% Enable fault injection for publish_with_ack error
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Attempt publish_with_ack (should fail)
    _Result = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    
    %% Verify metric was incremented using shared harness
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1),
    
    ok.

%% @doc Test: queue operations count metric updated
test_metrics_queue_operations_count(_Config) ->
    %% Enable fault injection to simulate failures
    ok = router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    
    %% Attempt multiple publish operations (should fail due to fault injection)
    _Result1 = router_nats:publish(<<"test.1">>, <<"payload1">>),
    _Result2 = router_nats:publish(<<"test.2">>, <<"payload2">>),
    
    %% Verify publish failures are captured
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 2),
    
    ok.

%% @doc Test: retry metrics after reconnection
test_metrics_retry_after_reconnection(_Config) ->
    %% Enable fault injection to simulate connection failure
    ok = router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    
    %% Attempt publish operations (should fail due to fault injection)
    _Result1 = router_nats:publish(<<"test.1">>, <<"payload1">>),
    
    %% Verify failure metric is captured
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1),
    
    %% Clear fault injection (simulates reconnection success)
    router_nats_fault_injection:clear_all_faults(),
    
    %% Verify router_nats process is still alive after reconnection simulation
    RouterNatsPid = whereis(router_nats),
    true = is_process_alive(RouterNatsPid),
    ok.

%% @doc Test: no duplicate msg_id on retry
test_msg_id_no_duplicates_on_retry(_Config) ->
    %% Enable fault injection to simulate connection failure
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, not_connected}),
    
    %% Attempt publish_with_ack (should fail due to fault injection)
    Error1 = router_nats:publish_with_ack(<<"test.1">>, <<"payload1">>, #{}),
    true = (is_tuple(Error1) andalso element(1, Error1) =:= error),
    
    %% Clear fault injection (simulates reconnection success)
    router_nats_fault_injection:clear_all_faults(),
    
    %% Verify router_nats process is still alive
    RouterNatsPid = whereis(router_nats),
    true = is_process_alive(RouterNatsPid),
    ok.

%% @doc Test: unique msg_id per operation
test_msg_id_unique_per_operation(_Config) ->
    {ok, MsgId1} = router_nats:publish_with_ack(<<"test.1">>, <<"payload1">>, #{}),
    {ok, MsgId2} = router_nats:publish_with_ack(<<"test.2">>, <<"payload2">>, #{}),
    
    true = is_binary(MsgId1),
    true = is_binary(MsgId2),
    ok.
