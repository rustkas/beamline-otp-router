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
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
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
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, recovery_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, recovery_tests}];
        _ -> []
    end;
groups_for_level(_) ->
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
    router_nats_fault_injection:clear_all_faults(),
    Base = router_test_bootstrap:init_per_suite(Config, #{
        start => router_suite,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            tracing_enabled => false
        }
    }),
    Base.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:end_per_suite(Config, #{
        start => router_suite,
        stop => router_suite
    }).

init_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:init_per_testcase(_TestCase, Config, #{}).

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    router_test_bootstrap:end_per_testcase(_TestCase, Config, #{}).

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
