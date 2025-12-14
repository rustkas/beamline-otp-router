%% @doc JetStream Soak: Combined Scenarios
%%
%% - Sequential fault chains
%% - Repeating fault cycles
%%
%% @test_category soak, nightly, heavy
-module(router_jetstream_soak_combined_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_sequential_fault_chain/1,
    test_repeating_fault_cycles/1
]).

suite() -> [{timetrap, {hours, 4}}].

soak_enabled() -> os:getenv("RUN_JETSTREAM_SOAK") =:= "1".

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" ->
            case soak_enabled() of
                true -> [{group, combined_scenarios}];
                false -> []
            end;
        _ -> []
    end.

groups() ->
    [{combined_scenarios, [sequence], [
        test_sequential_fault_chain,
        test_repeating_fault_cycles
    ]}].

init_per_suite(Config) -> router_jetstream_ct_helpers:init_suite(Config).
end_per_suite(Config) -> router_jetstream_ct_helpers:end_suite(Config).
init_per_testcase(TC, Config) -> router_jetstream_ct_helpers:init_case(TC, Config).
end_per_testcase(TC, Config) -> router_jetstream_ct_helpers:end_case(TC, Config).

scale_duration(BaseMs) -> router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_sequential_fault_chain(_Config) ->
    ct:comment("=== Sequential Fault Chain (110+ minutes) ==="),
    
    FaultSequence = [
        {jetstream_restart, scale_duration(20 * 60 * 1000)},
        {router_restart, scale_duration(20 * 60 * 1000)},
        {network_partition, scale_duration(20 * 60 * 1000)},
        {connection_errors, scale_duration(20 * 60 * 1000)},
        {recovery, scale_duration(30 * 60 * 1000)}
    ],
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    lists:foreach(fun({FaultType, Duration}) ->
        ct:comment("Fault phase: ~p for ~p ms", [FaultType, Duration]),
        apply_fault(FaultType),
        timer:sleep(Duration),
        clear_fault(FaultType)
    end, FaultSequence),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_repeating_fault_cycles(_Config) ->
    ct:comment("=== Repeating Fault Cycles ==="),
    
    CycleDurationMs = scale_duration(10 * 60 * 1000),
    NumCycles = 6,
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    lists:foreach(fun(Cycle) ->
        ct:comment("Cycle ~p/~p", [Cycle, NumCycles]),
        FaultType = case Cycle rem 3 of
            0 -> jetstream_restart;
            1 -> network_partition;
            2 -> connection_errors
        end,
        apply_fault(FaultType),
        timer:sleep(CycleDurationMs div 2),
        clear_fault(FaultType),
        timer:sleep(CycleDurationMs div 2)
    end, lists:seq(1, NumCycles)),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

apply_fault(jetstream_restart) ->
    router_nats_fault_injection:enable_fault(subscribe, {error, connection_lost});
apply_fault(router_restart) ->
    router_jetstream_recovery_helpers:simulate_router_restart();
apply_fault(network_partition) ->
    router_jetstream_recovery_helpers:simulate_network_partition();
apply_fault(connection_errors) ->
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused});
apply_fault(recovery) ->
    ok.

clear_fault(jetstream_restart) ->
    router_nats_fault_injection:disable_fault(subscribe);
clear_fault(router_restart) ->
    ok;
clear_fault(network_partition) ->
    router_jetstream_recovery_helpers:heal_network_partition();
clear_fault(connection_errors) ->
    router_nats_fault_injection:disable_fault(connect);
clear_fault(recovery) ->
    ok.
