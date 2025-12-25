%% @doc Flapping Network Partition Tests
%% 
%% Tests for intermittent connectivity and flapping scenarios.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category network_partition, heavy, slow
-module(router_network_partition_flapping_SUITE).
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
    test_flapping_network_stability/1,
    test_flapping_network_no_resource_leaks/1,
    test_flapping_network_recovery/1,
    test_intermittent_connectivity/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, flapping_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, flapping_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, flapping_tests}];
        _ -> []
    end.
groups() ->
    [{flapping_tests, [sequence], [
        test_flapping_network_stability,
        test_flapping_network_no_resource_leaks,
        test_flapping_network_recovery,
        test_intermittent_connectivity
    ]}].

init_per_suite(Config) ->
    router_network_partition_helper:init_common_suite(Config).

end_per_suite(Config) ->
    router_network_partition_helper:end_common_suite(Config).

init_per_testcase(_TestCase, Config) ->
    router_network_partition_helper:init_common_testcase(Config).

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_flapping_network_stability(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    %% Flapping: rapid on/off cycles
    lists:foreach(fun(_) ->
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        ok = test_helpers:wait_for_condition(fun() ->
            %% Connection failure metric should increase or state becomes disconnected
            StatusOk = case router_nats:get_connection_status() of
                {ok, #{state := State}} -> (State =:= disconnected orelse State =:= reconnecting);
                {ok, State} when is_atom(State) -> (State =:= disconnected orelse State =:= reconnecting);
                _ -> false
            end,
            Metrics = router_network_partition_helper:get_metrics_snapshot(),
            StatusOk orelse maps:get(router_nats_connection_failures_total, Metrics, 0) > 0
        end, 1000),
        router_nats_fault_injection:disable_fault(connect),
        ok = test_helpers:wait_for_condition(fun() ->
            case router_nats:get_connection_status() of
                {ok, #{state := State}} -> (State =:= connected);
                {ok, State} when is_atom(State) -> (State =:= connected);
                _ -> false
            end
        end, 2000)
    end, lists:seq(1, 10)),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    ok = test_helpers:wait_for_condition(fun() ->
        %% Ensure system reached a steady observable state post flapping
        Metrics = router_network_partition_helper:get_metrics_snapshot(),
        maps:get(router_nats_connection_lost_total, Metrics, 0) >= 1
    end, 2000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 100
    }),
    
    application:stop(beamline_router),
    ok.

test_flapping_network_no_resource_leaks(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    
    InitialMemory = erlang:memory(total),
    InitialProcessCount = length(erlang:processes()),
    
    %% Many flapping cycles
    lists:foreach(fun(_) ->
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        ok = test_helpers:wait_for_condition(fun() ->
            case router_nats:get_connection_status() of
                {ok, #{state := State}} -> (State =:= disconnected);
                {ok, State} when is_atom(State) -> (State =:= disconnected);
                _ -> false
            end
        end, 1000),
        router_nats_fault_injection:disable_fault(connect),
        ok = test_helpers:wait_for_condition(fun() ->
            case router_nats:get_connection_status() of
                {ok, #{state := State}} -> (State =:= connected);
                {ok, State} when is_atom(State) -> (State =:= connected);
                _ -> false
            end
        end, 2000)
    end, lists:seq(1, 20)),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    FinalMemory = erlang:memory(total),
    FinalProcessCount = length(erlang:processes()),
    
    MemoryGrowth = FinalMemory - InitialMemory,
    ProcessGrowth = FinalProcessCount - InitialProcessCount,
    
    ct:log("Memory growth: ~p bytes, Process growth: ~p", [MemoryGrowth, ProcessGrowth]),
    
    %% Verify no unbounded growth
    ?assert(MemoryGrowth < 50 * 1024 * 1024),  %% Less than 50MB
    ?assert(ProcessGrowth < 100),
    
    application:stop(beamline_router),
    ok.

test_flapping_network_recovery(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    %% Flapping then recovery
    lists:foreach(fun(_) ->
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        ok = test_helpers:wait_for_condition(fun() ->
            case router_nats:get_connection_status() of
                {ok, #{state := State}} -> (State =:= disconnected);
                {ok, State} when is_atom(State) -> (State =:= disconnected);
                _ -> false
            end
        end, 1500),
        router_nats_fault_injection:disable_fault(connect),
        ok = test_helpers:wait_for_condition(fun() ->
            case router_nats:get_connection_status() of
                {ok, #{state := State}} -> (State =:= connected);
                {ok, State} when is_atom(State) -> (State =:= connected);
                _ -> false
            end
        end, 2500)
    end, lists:seq(1, 5)),
    
    %% Stable period
    ok = test_helpers:wait_for_condition(fun() ->
        case router_nats:get_connection_status() of
            {ok, #{state := State}} -> (State =:= connected);
            {ok, State} when is_atom(State) -> (State =:= connected);
            _ -> false
        end
    end, 5000),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 100
    }),
    
    application:stop(beamline_router),
    ok.

test_intermittent_connectivity(_Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    InitialMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    %% Intermittent: random-ish pattern
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.5}),
    ok = test_helpers:wait_for_condition(fun() ->
        Metrics = router_network_partition_helper:get_metrics_snapshot(),
        maps:get(router_nats_publish_failures_total, Metrics, 0) > 0
    end, 5000),
    router_nats_fault_injection:disable_fault(publish),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    ok = test_helpers:wait_for_condition(fun() ->
        case router_nats:get_connection_status() of
            {ok, #{state := State}} -> (State =:= connected);
            {ok, State} when is_atom(State) -> (State =:= connected);
            _ -> false
        end
    end, 3000),
    
    FinalMetrics = router_network_partition_helper:get_metrics_snapshot(),
    
    router_network_partition_helper:verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 100
    }),
    
    application:stop(beamline_router),
    ok.
