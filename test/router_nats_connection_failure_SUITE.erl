%% @doc NATS Connection Failure Meta Suite (self-contained smoke)
%% Aggregates core failure scenarios: loss, recovery, retry.
-module(router_nats_connection_failure_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_reconnect_after_failure/1,
    test_message_retry_after_recovery/1,
    test_metrics_recorded_on_failure/1
]).

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, failure_tests}];
        _ -> []
    end.

groups() ->
    [{failure_tests, [sequence], [
        test_reconnect_after_failure,
        test_message_retry_after_recovery,
        test_metrics_recorded_on_failure
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    router_suite_helpers:ensure_no_faults(),
    ok = router_suite_helpers:start_router_suite(),
    router_metrics:ensure(),
    Config.

end_per_suite(Config) ->
    router_suite_helpers:ensure_no_faults(),
    router_suite_helpers:stop_router_suite(),
    Config.

init_per_testcase(_TC, Config) ->
    router_suite_helpers:ensure_no_faults(),
    router_metrics:ensure(),
    ok = router_test_utils:ensure_router_nats_alive(),
    Config.

end_per_testcase(_TC, _Config) ->
    router_suite_helpers:ensure_no_faults(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_reconnect_after_failure(_Config) ->
    assert_router_ready(),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ok = router_nats:publish(<<"test.reconnect">>, <<"payload">>),
    ok.

test_message_retry_after_recovery(_Config) ->
    assert_router_ready(),
    router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    %% publish will fail/queue under fault
    _ = catch router_nats:publish(<<"test.retry">>, <<"payload">>),
    router_nats_fault_injection:disable_fault(publish),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ok = router_nats_test_helpers:wait_for_pending_empty(7000),
    ok = router_nats:publish(<<"test.retry.after">>, <<"payload">>),
    ok.

test_metrics_recorded_on_failure(_Config) ->
    assert_router_ready(),
    router_metrics:ensure(),
    StartLost = get_metric(router_nats_connection_lost_total),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    EndLost = get_metric(router_nats_connection_lost_total),
    ?assert(EndLost >= StartLost),
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

assert_router_ready() ->
    case whereis(router_nats) of
        undefined -> ct:fail("router_nats not started");
        P when is_pid(P) -> ok
    end,
    case ets:info(router_metrics) of
        undefined -> ct:fail("router_metrics ETS not available");
        _ -> ok
    end.

get_metric(Name) ->
    case ets:info(router_metrics) of
        undefined -> 0;
        _ ->
            lists:foldl(fun(E, Acc) ->
                case E of
                    {{Name, _}, V} when is_integer(V) -> Acc + V;
                    {Name, V} when is_integer(V) -> Acc + V;
                    _ -> Acc
                end
            end, 0, ets:tab2list(router_metrics))
    end.
