%% @doc JetStream Soak: Restart Scenarios
%%
%% - Repeated JetStream restarts
%% - Repeated router restarts  
%% - Network partition recovery
%%
%% @test_category soak, nightly, heavy
-module(router_jetstream_soak_restart_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_repeated_jetstream_restarts/1,
    test_repeated_router_restarts/1,
    test_network_partition_recovery/1
]).

suite() -> [{timetrap, {hours, 4}}].

soak_enabled() ->
    os:getenv("RUN_JETSTREAM_SOAK") =:= "1".

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    case soak_enabled() of
        true -> [{group, restart_scenarios}];
        false -> []
    end;
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{restart_scenarios, [sequence], [
        test_repeated_jetstream_restarts,
        test_repeated_router_restarts,
        test_network_partition_recovery
    ]}].

init_per_suite(Config) -> router_jetstream_ct_helpers:init_suite(Config).
end_per_suite(Config) -> router_jetstream_ct_helpers:end_suite(Config).
init_per_testcase(TC, Config) -> router_jetstream_ct_helpers:init_case(TC, Config).
end_per_testcase(TC, Config) -> router_jetstream_ct_helpers:end_case(TC, Config).

scale_duration(BaseMs) -> router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_repeated_jetstream_restarts(_Config) ->
    BaselineDurationMs = scale_duration(5 * 60 * 1000),
    RestartCyclesDurationMs = scale_duration(60 * 60 * 1000),
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    ct:comment("Phase 2: Restart cycles (60 minutes)"),
    _ = router_jetstream_recovery_store:init_connection_state(),
    
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        case router_jetstream_recovery_store:get_connection_state() of
            {ok, false} -> {error, connection_lost};
            _ -> {ok, <<"consumer-restart">>}
        end
    end),
    
    RestartCycleDurationMs = scale_duration(5 * 60 * 1000),
    Cycles = RestartCyclesDurationMs div RestartCycleDurationMs,
    [begin
        _ = router_jetstream_recovery_store:set_connection_state(false),
        timer:sleep(scale_duration(10000)),
        _ = router_jetstream_recovery_store:set_connection_state(true),
        timer:sleep(scale_duration(290000))
    end || _ <- lists:seq(1, Cycles)],
    
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_connection_state(true),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput, RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, _} -> ok;
        {timeout, ElapsedMs} -> ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    _ = router_jetstream_recovery_store:delete_connection_state(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_repeated_router_restarts(_Config) ->
    BaselineDurationMs = scale_duration(5 * 60 * 1000),
    RestartCyclesDurationMs = scale_duration(60 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    ct:comment("Phase 2: Router restart cycles"),
    RestartCycleDurationMs = scale_duration(5 * 60 * 1000),
    Cycles = RestartCyclesDurationMs div RestartCycleDurationMs,
    
    [begin
        ct:comment("Cycle ~p: Router restart", [Cycle]),
        router_jetstream_recovery_helpers:simulate_router_restart(),
        timer:sleep(scale_duration(290000))
    end || Cycle <- lists:seq(1, Cycles)],
    
    ct:comment("Phase 3: Verify recovery"),
    FinalThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(60000)),
    ?assert(FinalThroughput >= BaselineThroughput * 0.8),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.

test_network_partition_recovery(_Config) ->
    BaselineDurationMs = scale_duration(5 * 60 * 1000),
    PartitionDurationMs = scale_duration(30 * 60 * 1000),
    
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    ct:comment("Phase 1: Baseline"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    ct:comment("Phase 2: Network partition"),
    router_jetstream_recovery_helpers:simulate_network_partition(),
    timer:sleep(PartitionDurationMs),
    
    ct:comment("Phase 3: Heal partition"),
    router_jetstream_recovery_helpers:heal_network_partition(),
    timer:sleep(scale_duration(60000)),
    
    FinalThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(60000)),
    ?assert(FinalThroughput >= BaselineThroughput * 0.8),
    
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    ok.
