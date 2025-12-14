%% @doc NATS Connection Failure - Recovery Tests
%% 
%% Tests for recovery behavior after connection failures.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category nats, recovery, heavy
-module(router_nats_conn_failure_recovery_SUITE).
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
    test_recovery_after_short_drop/1,
    test_recovery_after_long_drop/1,
    test_message_retry_after_recovery/1,
    test_message_redelivery_after_reconnect/1,
    test_connection_flapping/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, recovery_tests}];
        _ -> []
    end.

groups() ->
    [{recovery_tests, [sequence], [
        test_recovery_after_short_drop,
        test_recovery_after_long_drop,
        test_message_retry_after_recovery,
        test_message_redelivery_after_reconnect,
        test_connection_flapping
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

test_recovery_after_short_drop(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    %% Short drop: force lost then recover
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.

test_recovery_after_long_drop(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 12000),
    ok = router_nats_test_helpers:wait_for_status(connected, 15000),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.

test_message_retry_after_recovery(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:enable_fault(publish, {error, not_connected}),

    %% Attempt publish during failure (will be queued)
    catch router_nats:publish(<<"test.subject">>, <<"test payload">>),
    router_nats_fault_injection:disable_fault(publish),
    %% Force connection lost to trigger retry logic on restoration
    ok = router_nats:simulate_connection_lost(),
    ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 5000),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    ok = router_nats_test_helpers:wait_for_pending_empty(7000),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.

test_message_redelivery_after_reconnect(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    router_nats_fault_injection:enable_fault(ack, {error, not_connected}),
    
    %% Attempt ACK during failure - should trigger redelivery
    catch router_nats:ack_message(<<"msg-redelivery">>),
    ok = router_nats_test_helpers:wait_for_status(connected, 7000),
    router_nats_fault_injection:disable_fault(ack),
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.

test_connection_flapping(_Config) ->
    ?assert(router_nats_test_helpers:is_service_available()),
    %% Flapping: rapid on/off cycles
    lists:foreach(fun(_) ->
        ok = router_nats:simulate_connection_lost(),
        ok = router_nats_test_helpers:wait_for_status_any([reconnecting, disconnected], 3000),
        ok = router_nats_test_helpers:wait_for_status(connected, 5000)
    end, lists:seq(1, 5)),
    
    ?assert(router_nats_test_helpers:is_service_available()),
    ok.
