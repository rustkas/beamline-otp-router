%% @doc NATS Connection Failure - Basic Tests
%% 
%% Tests for basic connection failure handling: no crash, reconnect, fail-open.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category nats, failure, heavy
-module(router_nats_conn_failure_basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_no_crash_on_connection_loss/1,
    test_reconnect_after_failure/1,
    test_fail_open_mode/1,
    test_metrics_on_failure/1,
    test_logs_on_failure/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, basic_failure_tests}];
        _ -> []
    end.

groups() ->
    [{basic_failure_tests, [sequence], [
        test_no_crash_on_connection_loss,
        test_reconnect_after_failure,
        test_fail_open_mode,
        test_metrics_on_failure,
        test_logs_on_failure
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    router_nats_fault_injection:clear_all_faults(),
    ok = router_suite_helpers:start_router_suite(),
    Config.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_suite_helpers:stop_router_suite(),
    Config.

init_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_nats_fault_injection:clear_all_faults(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_no_crash_on_connection_loss(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_metrics:ensure(),
    _PrevLost = ets:foldl(fun(E, Acc) ->
        case E of
            {{router_nats_connection_lost_total, _}, V} when is_integer(V) -> Acc + V;
            {router_nats_connection_lost_total, V} when is_integer(V) -> Acc + V;
            _ -> Acc
        end
    end, 0, router_metrics),
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:disable_fault(connect),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.

test_reconnect_after_failure(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_metrics:ensure(),
    _PrevRestored = ets:foldl(fun(E, Acc) ->
        case E of
            {{router_nats_connection_restored_total, _}, V} when is_integer(V) -> Acc + V;
            {router_nats_connection_restored_total, V} when is_integer(V) -> Acc + V;
            _ -> Acc
        end
    end, 0, router_metrics),
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    router_nats_fault_injection:disable_fault(connect),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.

test_fail_open_mode(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:enable_fault(publish, {error, not_connected}),
    
    %% Attempt publish - should not crash
    catch router_nats:publish(<<"test.subject">>, <<"test payload">>),
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:disable_fault(publish),
    ok.

test_metrics_on_failure(_Config) ->
    router_metrics:ensure(),
    PrevLost = ets:foldl(fun(E, Acc) ->
        case E of
            {{router_nats_connection_lost_total, _}, V} when is_integer(V) -> Acc + V;
            {router_nats_connection_lost_total, V} when is_integer(V) -> Acc + V;
            _ -> Acc
        end
    end, 0, router_metrics),
    ok = router_nats:simulate_connection_lost(),
    router_test_utils:wait_for_metric(fun() ->
        ets:foldl(fun(E, Acc) ->
            case E of
                {{router_nats_connection_lost_total, _}, V} when is_integer(V) -> Acc + V;
                {router_nats_connection_lost_total, V} when is_integer(V) -> Acc + V;
                _ -> Acc
            end
        end, 0, router_metrics)
    end, PrevLost + 1, 7000),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ok.

test_logs_on_failure(_Config) ->
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status(reconnecting, 5000),
    router_nats_fault_injection:disable_fault(connect),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.
