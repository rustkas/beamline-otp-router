%% @doc JetStream Soak: Production Scale Scenarios
%%
%% - Multi-node JetStream cluster failures
%% - Cross-region network partitions
%% - Rolling restart zero downtime
%%
%% @test_category soak, nightly, heavy, production
-module(router_jetstream_soak_prod_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    smoke_test/1,
    test_multi_node_cluster_failures/1,
    test_cross_region_partitions/1,
    test_rolling_restart_zero_downtime/1
]).

suite() -> [{timetrap, {hours, 4}}].

soak_enabled() -> os:getenv("RUN_JETSTREAM_SOAK") =:= "1".

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" ->
            case soak_enabled() of
                true -> [{group, smoke_tests}, {group, production_scale_scenarios}];
                false -> [{group, smoke_tests}]
            end;
        "full" -> [{group, smoke_tests}];
        _ -> []
    end.

groups() ->
    [
        {smoke_tests, [sequence], [smoke_test]},
        {production_scale_scenarios, [sequence], [
            test_multi_node_cluster_failures,
            test_cross_region_partitions,
            test_rolling_restart_zero_downtime
        ]}
    ].

init_per_suite(Config) -> router_jetstream_ct_helpers:init_suite(Config).
end_per_suite(Config) -> router_jetstream_ct_helpers:end_suite(Config).
init_per_testcase(TC, Config) -> router_jetstream_ct_helpers:init_case(TC, Config).
end_per_testcase(TC, Config) -> router_jetstream_ct_helpers:end_case(TC, Config).

scale_duration(BaseMs) -> router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ============================================================================
%% SMOKE TEST
%% ============================================================================

smoke_test(_Config) ->
    ct:comment("=== JetStream Soak Production SUITE Smoke Test ==="),
    ct:comment("Soak enabled: ~p", [soak_enabled()]),
    
    case router_jetstream_recovery_store:ensure() of
        ok -> ok;
        {error, Reason} -> ct:fail("Store ensure failed: ~p", [Reason])
    end,
    
    _ = router_jetstream_recovery_helpers:speedup_factor(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_multi_node_cluster_failures(_Config) ->
    ct:comment("=== Multi-Node Cluster Failures (90+ minutes) ==="),
    
    NumNodes = 3,
    FailureDurationMs = scale_duration(20 * 60 * 1000),
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Simulate node failures one by one
    lists:foreach(fun(Node) ->
        ct:comment("Node ~p failure simulation", [Node]),
        router_nats_fault_injection:enable_fault(subscribe, {error, node_down}),
        timer:sleep(FailureDurationMs),
        router_nats_fault_injection:disable_fault(subscribe),
        timer:sleep(RecoveryDurationMs)
    end, lists:seq(1, NumNodes)),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_cross_region_partitions(_Config) ->
    ct:comment("=== Cross-Region Network Partitions (90+ minutes) ==="),
    
    Regions = [us_east, us_west, eu_west],
    PartitionDurationMs = scale_duration(20 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    lists:foreach(fun(Region) ->
        ct:comment("Region ~p partition simulation", [Region]),
        router_jetstream_recovery_helpers:simulate_network_partition(),
        timer:sleep(PartitionDurationMs),
        router_jetstream_recovery_helpers:heal_network_partition(),
        timer:sleep(scale_duration(5 * 60 * 1000))
    end, Regions),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_rolling_restart_zero_downtime(_Config) ->
    ct:comment("=== Rolling Restart Zero Downtime (60+ minutes) ==="),
    
    NumInstances = 5,
    RestartIntervalMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(30000)),
    
    lists:foreach(fun(Instance) ->
        ct:comment("Instance ~p rolling restart", [Instance]),
        
        %% Simulate instance restart (brief disruption)
        router_jetstream_recovery_helpers:simulate_router_restart(),
        
        %% Verify throughput maintained (zero downtime)
        timer:sleep(scale_duration(5000)),
        CurrentThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)),
        ?assert(CurrentThroughput >= BaselineThroughput * 0.9),
        
        timer:sleep(RestartIntervalMs)
    end, lists:seq(1, NumInstances)),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.
