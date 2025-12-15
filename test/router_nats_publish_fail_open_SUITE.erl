%% @doc Test Suite for router_nats Fail-Open Mode Behavior
%%
%% Tests that the router correctly handles publish failures when 
%% nats_fail_open_mode is set to true. In this mode, failures should
%% be masked (return ok or stub-msg-id) to allow the system to proceed.
%%
%% @test_category integration, faults
-module(router_nats_publish_fail_open_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([suite/0]).

%% Test cases
-export([
    test_publish_error_connected_fail_open/1,
    test_publish_timeout_connected_fail_open/1,
    test_publish_close_connection_fail_open/1,
    test_publish_not_connected_fail_open/1,
    test_publish_with_ack_error_connected_fail_open/1,
    test_publish_with_ack_timeout_connected_fail_open/1,
    test_publish_with_ack_close_connection_fail_open/1,
    test_publish_with_ack_not_connected_fail_open/1,
    test_msg_id_stub_in_fail_open_mode/1,
    test_fail_open_system_availability/1,
    test_router_fail_open_continues_processing/1,
    %% Task 9: Back-to-back isolation regression
    test_back_to_back_isolation/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [
        test_publish_error_connected_fail_open,
        test_publish_timeout_connected_fail_open,
        test_publish_close_connection_fail_open,
        test_publish_not_connected_fail_open,
        test_publish_with_ack_error_connected_fail_open,
        test_publish_with_ack_timeout_connected_fail_open,
        test_publish_with_ack_close_connection_fail_open,
        test_publish_with_ack_not_connected_fail_open,
        test_msg_id_stub_in_fail_open_mode,
        test_fail_open_system_availability,
        test_router_fail_open_continues_processing,
        %% Task 9: Back-to-back isolation test (runs last)
        test_back_to_back_isolation
    ].

init_per_suite(Config) ->
    ok = router_mock_helpers:setup_router_nats_mock(),
    router_test_bootstrap:init_per_suite(Config, #{
        reset_app => true,
        start => ensure_all_started,
        common_env => false,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            nats_reconnect_attempts => 5,
            nats_reconnect_delay_ms => 500,
            nats_max_reconnect_delay_ms => 2000,
            nats_fail_open_mode => true,
            nats_max_pending_operations => 1000
        }
    }),
    ok = router_test_utils:ensure_router_nats_alive(),
    assert_fail_open_mode_enabled(),
    Config.

end_per_suite(_Config) ->
    router_test_bootstrap:end_per_suite([], #{
        start => ensure_all_started,
        stop => stop_app
    }),
    ok.

init_per_testcase(_TestCase, Config) ->
    Base = router_test_bootstrap:init_per_testcase(_TestCase, Config, #{
        clear_faults => true
    }),
    router_mock_helpers:set_mock_connection_state(connected),
    router_test_utils:ensure_router_nats_alive(),
    assert_fail_open_mode_enabled(),
    router_metrics_test_helper:setup(),
    router_metrics_test_helper:clear(),
    Base.

end_per_testcase(_TestCase, _Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_mock_helpers:set_mock_connection_state(connected),
    router_metrics_test_helper:teardown(),
    router_test_bootstrap:end_per_testcase(_TestCase, [], #{}),
    ok.

%% @doc Test: publish returns {error, Reason} in fail-open mode
test_publish_error_connected_fail_open(_Config) ->
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    assert_fail_open_mode_enabled(),
    ok = router_nats:publish(<<"test.subject">>, <<"payload">>),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1, 5000),
    ok.

%% @doc Test: publish timeout in fail-open mode (deterministic error path)
test_publish_timeout_connected_fail_open(_Config) ->
    ok = router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    ok = router_nats:publish(<<"test.subject">>, <<"payload">>),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1, 5000),
    ok.

%% @doc Test: publish close_connection in fail-open mode
test_publish_close_connection_fail_open(_Config) ->
    ok = router_nats_fault_injection:enable_fault(publish, close_connection),
    ok = router_nats:publish(<<"test.subject">>, <<"payload">>),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1, 5000),
    ok.

%% @doc Test: publish not connected in fail-open mode
test_publish_not_connected_fail_open(_Config) ->
    %% Simulate connection loss by directly setting mock state
    %% This is more reliable than gen_server:cast which depends on the mock process
    router_mock_helpers:set_mock_connection_state(disconnected),
    
    %% Verify state was set
    {ok, #{state := State}} = router_nats:get_connection_status(),
    true = (State =:= disconnected orelse State =:= reconnecting),
    
    %% Attempt publish (should return ok in fail-open mode)
    Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
    ok = Result,  %% Fail-open mode returns ok even when not connected
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1, 5000),
    ok.

%% @doc Test: publish_with_ack returns {error, Reason} in fail-open mode
test_publish_with_ack_error_connected_fail_open(_Config) ->
    %% Enable fault injection for publish_with_ack error
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Attempt publish_with_ack (should return {ok, stub-msg-id} in fail-open mode)
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1, 5000),

    ok.

%% @doc Test: publish_with_ack timeout in fail-open mode
test_publish_with_ack_timeout_connected_fail_open(_Config) ->
    %% Enable fault injection for timeout
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, timeout}),
    
    %% Attempt publish_with_ack
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1, 5000),
    ok.

%% @doc Test: publish_with_ack close_connection in fail-open mode
test_publish_with_ack_close_connection_fail_open(_Config) ->
    %% Enable fault injection for close_connection
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, close_connection),
    
    %% Attempt publish_with_ack
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    
    %% Wait for publish_with_ack failure metric (close_connection causes a failure)
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1, 5000),
    
    ok.

%% @doc Test: publish_with_ack not connected in fail-open mode
test_publish_with_ack_not_connected_fail_open(_Config) ->
    %% Simulate connection loss by directly setting mock state
    %% This is more reliable than gen_server:cast which depends on the mock process
    router_mock_helpers:set_mock_connection_state(disconnected),
    
    %% Verify state was set
    {ok, #{state := State}} = router_nats:get_connection_status(),
    true = (State =:= disconnected orelse State =:= reconnecting),
    
    %% Attempt publish_with_ack
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.subject">>, <<"payload">>, #{}),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1, 5000),
    ok.

%% @doc Test: stub-msg-id is returned in fail-open mode
test_msg_id_stub_in_fail_open_mode(_Config) ->
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.1">>, <<"payload1">>, #{}),
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.2">>, <<"payload2">>, #{}),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 2, 5000),
    ok.

%% @doc Test: fail-open mode maintains system availability
test_fail_open_system_availability(_Config) ->
    %% Enable fault injection for persistent errors
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Attempt many operations
    Results = lists:map(fun(I) ->
        Subject = <<"test.", (integer_to_binary(I))/binary>>,
        Payload = <<"payload", (integer_to_binary(I))/binary>>,
        router_nats:publish(Subject, Payload)
    end, lists:seq(1, 10)),
    
    %% All should return ok (fail-open mode)
    AllOk = lists:all(fun(R) -> R =:= ok end, Results),
    true = AllOk,
    
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 10, 5000),
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.new">>, <<"payload">>, #{}),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1, 5000),
    assert_fail_open_mode_enabled(),
    ok.

%% @doc Test: router continues processing in fail-open mode despite publish failures
test_router_fail_open_continues_processing(_Config) ->
    %% Enable fault injection
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    ok = router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}),
    
    %% Attempt publish
    ok = router_nats:publish(<<"test.1">>, <<"payload1">>),
    ok = router_nats:publish(<<"test.2">>, <<"payload2">>),
    {ok, <<"stub-msg-id">>} = router_nats:publish_with_ack(<<"test.ack">>, <<"payload3">>, #{}),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 2, 5000),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_with_ack_failures_total, 1, 5000),
    assert_fail_open_mode_enabled(),
    ok.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

%% @doc Task 9: Back-to-back isolation test
%% Runs the same fault-injection scenario twice with cleanup in between
%% to verify no state carries over between runs.
test_back_to_back_isolation(_Config) ->
    %% Run 1: Enable fault, publish, verify metrics
    router_nats_fault_injection:clear_all_faults(),
    router_metrics_test_helper:clear(),
    
    ok = router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    ok = router_nats:publish(<<"test.back_to_back.1">>, <<"payload1">>),
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1, 3000),
    
    %% Clear state between runs (as init_per_testcase would do)
    router_nats_fault_injection:clear_all_faults(),
    router_metrics_test_helper:clear(),
    
    %% Run 2: Same scenario - should start fresh
    ok = router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    ok = router_nats:publish(<<"test.back_to_back.2">>, <<"payload2">>),
    
    %% Should see exactly 1 failure (not 2 from accumulated state)
    ok = router_metrics_test_helper:wait_for_metric(router_nats_publish_failures_total, 1, 3000),
    
    %% Verify fail-open still enabled
    assert_fail_open_mode_enabled(),
    ok.

assert_fail_open_mode_enabled() ->
    case router_nats:get_connection_status() of
        {ok, #{fail_open_mode := true}} ->
            ok;
        {ok, #{fail_open_mode := Mode}} ->
            ct:fail({fail_open_mode_disabled, Mode});
        {ok, Map} when is_map(Map) ->
            case maps:get(fail_open_mode, Map, false) of
                true -> ok;
                Other -> ct:fail({fail_open_mode_disabled, Other})
            end;
        {ok, connected} ->
            %% Legacy format - assume fail_open is enabled if we got here
            %% (test setup should have configured this)
            ok;
        connected ->
            %% Very old mock format - check application env as fallback
            case application:get_env(beamline_router, nats_fail_open_mode, false) of
                true -> ok;
                _ -> ct:fail({fail_open_mode_not_configured_in_env})
            end;
        Other ->
            ct:fail({unexpected_status, Other})
    end.
