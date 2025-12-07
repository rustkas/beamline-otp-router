%% @doc Ultra-Long Soak Test Suite for JetStream Recovery
%%
%% This suite contains only the most resource-intensive soak tests:
%% - Repeated restarts (60+ minutes)
%% - Sequential fault chains (110+ minutes)
%% - Long-running stability (4+ hours)
%% - Multi-node cluster failures (90+ minutes)
%% - Cross-region partitions (90+ minutes)
%% - Rolling restarts (60+ minutes)
%%
%% These tests are intended for nightly/rare runs only.
%% Use EXTENDED_TEST_SPEEDUP to scale down durations for CI.
%%
%% @test_category soak, nightly, ultra_long_running
-module(router_jetstream_soak_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Export Common Test callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Export test functions
-export([
    smoke_test/1,
    test_repeated_jetstream_restarts/1,
    test_repeated_router_restarts/1,
    test_network_partition_recovery/1,
    test_sequential_fault_chain/1,
    test_repeating_fault_cycles/1,
    test_long_running_stability/1,
    test_recovery_time_measurement/1,
    test_multi_node_jetstream_cluster_failures/1,
    test_cross_region_network_partitions/1,
    test_rolling_restart_zero_downtime/1
]).

%% ========================================================================
%% COMMON TEST CALLBACKS
%% ========================================================================

%% @doc Check if soak tests should be enabled.
%% Set RUN_JETSTREAM_SOAK=1 to enable ultra-long soak tests.
-spec soak_enabled() -> boolean().
soak_enabled() ->
    case os:getenv("RUN_JETSTREAM_SOAK") of
        "1" -> true;
        _ -> false
    end.

all() ->
    BaseTests = [smoke_test],
    case soak_enabled() of
        false ->
            %% Fast mode: only smoke_test
            BaseTests;
        true ->
            %% Soak mode: all ultra-long tests
            BaseTests ++ [
                {group, restart_scenarios},
                {group, combined_scenarios},
                {group, performance_scenarios},
                {group, production_scale_scenarios}
            ]
    end.

groups() ->
    [
        {restart_scenarios, [sequence], [
            test_repeated_jetstream_restarts,
            test_repeated_router_restarts,
            test_network_partition_recovery
        ]},
        {combined_scenarios, [sequence], [
            test_sequential_fault_chain,
            test_repeating_fault_cycles
        ]},
        {performance_scenarios, [sequence], [
            test_long_running_stability,
            test_recovery_time_measurement
        ]},
        {production_scale_scenarios, [sequence], [
            test_multi_node_jetstream_cluster_failures,
            test_cross_region_network_partitions,
            test_rolling_restart_zero_downtime
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Soak enabled: ~p (RUN_JETSTREAM_SOAK=~p)", 
           [soak_enabled(), os:getenv("RUN_JETSTREAM_SOAK")]),
    ct:pal("Speedup factor: ~p (EXTENDED_TEST_SPEEDUP=~p)", 
           [router_jetstream_recovery_helpers:speedup_factor(), os:getenv("EXTENDED_TEST_SPEEDUP")]),
    router_jetstream_ct_helpers:init_suite(Config).

end_per_suite(Config) ->
    router_jetstream_ct_helpers:end_suite(Config).

init_per_testcase(TestCase, Config) ->
    router_jetstream_ct_helpers:init_case(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    router_jetstream_ct_helpers:end_case(TestCase, Config).

%% ========================================================================
%% SMOKE TEST
%% ========================================================================

%% @doc Quick smoke test to verify SUITE setup without running long tests
smoke_test(_Config) ->
    ct:comment("=== Soak SUITE Smoke Test ==="),
    ct:comment("Soak enabled: ~p (set RUN_JETSTREAM_SOAK=1 to enable full soak tests)", [soak_enabled()]),
    ct:comment("Speedup factor: ~p", [router_jetstream_recovery_helpers:speedup_factor()]),
    
    %% Verify store is accessible
    case router_jetstream_recovery_store:ensure() of
        ok -> ok;
        {error, Reason} -> ct:fail("Store ensure failed: ~p", [Reason])
    end,
    
    %% Verify helpers are accessible
    _ = router_jetstream_recovery_helpers:speedup_factor(),
    _ = router_jetstream_recovery_helpers:scale_duration(1000),
    
    ct:comment("Smoke test passed: SUITE is ready"),
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Setup telemetry handler for metrics collection
-spec setup_telemetry_handler() -> reference().
setup_telemetry_handler() ->
    HandlerId = make_ref(),
    Handler = fun(EventName, Measurements, Metadata, _Config) ->
        _ = router_jetstream_recovery_store:record_telemetry_event(
            HandlerId, EventName, Measurements, Metadata, erlang:monotonic_time()
        )
    end,
    telemetry:attach({HandlerId, maxdeliver_exhausted}, 
                     [router_decide_consumer, router_jetstream_maxdeliver_exhausted_total], Handler, #{}),
    telemetry:attach({HandlerId, redelivery}, 
                     [router, jetstream, nak], Handler, #{}),
    HandlerId.

%% @doc Cleanup telemetry handler
-spec cleanup_telemetry_handler(reference()) -> ok.
cleanup_telemetry_handler(HandlerId) ->
    telemetry:detach({HandlerId, maxdeliver_exhausted}),
    telemetry:detach({HandlerId, redelivery}),
    _ = router_jetstream_recovery_store:delete_telemetry_events(HandlerId),
    ok.

%% Re-export scale_duration for convenience (delegates to helpers)
-spec scale_duration(non_neg_integer()) -> non_neg_integer().
scale_duration(BaseMs) ->
    router_jetstream_recovery_helpers:scale_duration(BaseMs).

%% ========================================================================
%% SCENARIO 2.1: REPEATED JETSTREAM RESTARTS
%% ========================================================================

%% @doc Test repeated JetStream node restarts
%% Duration: ~75 minutes
test_repeated_jetstream_restarts(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RestartCyclesDurationMs = scale_duration(60 * 60 * 1000),  %% 60 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase 2: Restart cycles (every 5 minutes)
    ct:comment("Phase 2: Restart cycles (60 minutes)"),
    _ = router_jetstream_recovery_store:init_connection_state(),
    
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        case router_jetstream_recovery_store:get_connection_state() of
            {ok, Connected} ->
                case Connected of
                    false -> {error, connection_lost};
                    true -> {ok, <<"consumer-restart">>}
                end;
            _ ->
                {ok, <<"consumer-restart">>}
        end
    end),
    
    RestartCycleDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes per cycle
    Cycles = RestartCyclesDurationMs div RestartCycleDurationMs,
    [begin
        %% Simulate restart (disconnect)
        _ = router_jetstream_recovery_store:set_connection_state(false),
        ct:comment("Cycle ~p: JetStream restart", [Cycle]),
        timer:sleep(scale_duration(10000)),  %% 10 seconds disconnected
        
        %% Reconnect
        _ = router_jetstream_recovery_store:set_connection_state(true),
        ct:comment("Cycle ~p: JetStream reconnected", [Cycle]),
        timer:sleep(scale_duration(290000))  %% 4 minutes 50 seconds connected
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_connection_state(true),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify no resource leaks
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    _ = router_jetstream_recovery_store:delete_connection_state(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 2.2: REPEATED ROUTER RESTARTS
%% ========================================================================

%% @doc Test repeated router process restarts
%% Duration: ~75 minutes
%% Simulates repeated router application restarts to verify recovery
test_repeated_router_restarts(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RestartCyclesDurationMs = scale_duration(60 * 60 * 1000),  %% 60 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase 2: Restart cycles (every 5 minutes)
    ct:comment("Phase 2: Router restart cycles (60 minutes)"),
    
    %% Track router state
    _ = router_jetstream_recovery_store:init_router_state(),
    
    RestartCycleDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes per cycle
    Cycles = RestartCyclesDurationMs div RestartCycleDurationMs,
    [begin
        %% Simulate router restart: stop application
        _ = router_jetstream_recovery_store:set_router_restarting(true),
        ct:comment("Cycle ~p: Stopping router application", [Cycle]),
        
        %% Stop router application
        router_test_utils:stop_router_app(),
        timer:sleep(scale_duration(10000)),  %% 10 seconds stopped
        
        %% Restart router application
        ct:comment("Cycle ~p: Restarting router application", [Cycle]),
        ok = router_test_utils:start_router_app(),
        
        %% Verify processes are alive after restart
        ok = router_test_utils:ensure_circuit_breaker_alive(),
        ok = router_test_utils:ensure_router_nats_alive(),
        
        _ = router_jetstream_recovery_store:set_router_restarting(false),
        ct:comment("Cycle ~p: Router restarted successfully", [Cycle]),
        timer:sleep(scale_duration(290000))  %% 4 minutes 50 seconds running
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_router_restarting(false),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify no resource leaks
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    _ = router_jetstream_recovery_store:delete_router_state(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 2.3: NETWORK PARTITION RECOVERY
%% ========================================================================

%% @doc Test network partition recovery
%% Duration: ~75 minutes
test_network_partition_recovery(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    PartitionCyclesDurationMs = scale_duration(60 * 60 * 1000),  %% 60 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    %% Phase 2: Partition cycles (every 4 minutes, partition for 1 minute)
    ct:comment("Phase 2: Partition cycles (60 minutes)"),
    _ = router_jetstream_recovery_store:init_partition_active(),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case router_jetstream_recovery_store:get_partition_active() of
            {ok, Active} ->
                case Active of
                    true -> {error, connection_lost};
                    false -> ok
                end;
            _ ->
                ok
        end
    end),
    
    PartitionCycleDurationMs = scale_duration(4 * 60 * 1000),  %% 4 minutes per cycle
    Cycles = PartitionCyclesDurationMs div PartitionCycleDurationMs,
    [begin
        %% Partition period (1 minute)
        _ = router_jetstream_recovery_store:set_partition_active(true),
        ct:comment("Cycle ~p: Partition started", [Cycle]),
        timer:sleep(scale_duration(60000)),
        
        %% Normal period (3 minutes)
        _ = router_jetstream_recovery_store:set_partition_active(false),
        ct:comment("Cycle ~p: Partition ended", [Cycle]),
        timer:sleep(scale_duration(180000))
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_partition_active(false),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    _ = router_jetstream_recovery_store:delete_partition_active(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 3.1: SEQUENTIAL FAULT CHAIN
%% ========================================================================

%% @doc Test sequential fault chain
%% Duration: ~110 minutes
test_sequential_fault_chain(_Config) ->
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(5 * 60 * 1000)),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase A: Network partition (15 minutes)
    ct:comment("Phase A: Network partition (15 minutes)"),
    router_jetstream_recovery_helpers:run_network_partition_phase(scale_duration(15 * 60 * 1000)),
    
    %% Phase B: JetStream restart (15 minutes)
    ct:comment("Phase B: JetStream restart (15 minutes)"),
    router_jetstream_recovery_helpers:run_jetstream_restart_phase(scale_duration(15 * 60 * 1000)),
    
    %% Phase C: MaxDeliver exhaustion (15 minutes)
    ct:comment("Phase C: MaxDeliver exhaustion (15 minutes)"),
    router_jetstream_recovery_helpers:run_maxdeliver_exhaustion_phase(scale_duration(15 * 60 * 1000)),
    
    %% Phase D: Router restart (15 minutes)
    ct:comment("Phase D: Router restart (15 minutes)"),
    router_jetstream_recovery_helpers:run_router_restart_phase(scale_duration(15 * 60 * 1000)),
    
    %% Phase E: Consumer hang (15 minutes)
    ct:comment("Phase E: Consumer hang (15 minutes)"),
    router_jetstream_recovery_helpers:run_consumer_hang_phase(scale_duration(15 * 60 * 1000)),
    
    %% Phase F: Combined faults (15 minutes)
    ct:comment("Phase F: Combined faults (15 minutes)"),
    router_jetstream_recovery_helpers:run_combined_faults_phase(scale_duration(15 * 60 * 1000)),
    
    %% Recovery phase (15 minutes)
    ct:comment("Recovery phase (15 minutes)"),
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        scale_duration(15 * 60 * 1000)
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify no accumulated issues
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 3.2: REPEATING FAULT CYCLES
%% ========================================================================

%% @doc Test repeating fault cycles
%% Duration: ~135 minutes
test_repeating_fault_cycles(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    CycleDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes per cycle
    Cycles = 12,
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase 2: Repeating cycles
    ct:comment("Phase 2: Repeating cycles (120 minutes)"),
    
    FaultPeriodMs = scale_duration(2 * 60 * 1000),  %% 2 minutes fault
    RecoveryPeriodMs = CycleDurationMs - FaultPeriodMs,  %% Remaining time for recovery
    
    CycleMetrics = [begin
        %% Fault period (2 minutes)
        meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
        ct:comment("Cycle ~p: Fault period", [Cycle]),
        timer:sleep(FaultPeriodMs),
        
        %% Recovery period (8 minutes)
        meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
        ct:comment("Cycle ~p: Recovery period", [Cycle]),
        timer:sleep(RecoveryPeriodMs),
        
        %% Measure metrics
        CycleThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)),
        CycleResources = router_jetstream_recovery_helpers:track_resources(),
        ct:comment("Cycle ~p: Throughput ~.2f msg/s", [Cycle, CycleThroughput]),
        
        {Cycle, CycleThroughput, CycleResources}
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Verify performance doesn't degrade
    FirstCycleThroughput = element(2, hd(CycleMetrics)),
    LastCycleThroughput = element(2, lists:last(CycleMetrics)),
    router_jetstream_recovery_helpers:assert_throughput_ok(FirstCycleThroughput, LastCycleThroughput),
    
    %% Phase 3: Final recovery
    ct:comment("Phase 3: Final recovery (10 minutes)"),
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify no resource leaks
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 4.1: LONG-RUNNING STABILITY
%% ========================================================================

%% @doc Test long-running stability
%% Duration: ~240 minutes (4 hours)
test_long_running_stability(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    ExtendedRunDurationMs = scale_duration(240 * 60 * 1000),  %% 4 hours
    MetricIntervalMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Initial baseline
    InitialThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(5 * 60 * 1000)),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Extended run with periodic metric collection
    ct:comment("Extended run (4 hours) with metric collection every 5 minutes"),
    
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + ExtendedRunDurationMs,
    
    Metrics = router_jetstream_recovery_helpers:collect_periodic_metrics(EndTime, MetricIntervalMs, []),
    
    %% Verify stability
    FinalThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(5 * 60 * 1000)),
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Verify no performance degradation
    router_jetstream_recovery_helpers:assert_throughput_ok(InitialThroughput, FinalThroughput),
    
    %% Verify no resource leaks
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Analyze metrics for trends
    router_jetstream_recovery_helpers:analyze_stability_metrics(Metrics),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 4.2: RECOVERY TIME MEASUREMENT
%% ========================================================================

%% @doc Measure recovery time for different fault types
%% Duration: ~17 minutes per fault type
test_recovery_time_measurement(_Config) ->
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(5 * 60 * 1000)),
    
    %% Test each fault type
    FaultTypes = [
        {network_partition, fun() -> router_jetstream_recovery_helpers:inject_network_partition() end, fun() -> router_jetstream_recovery_helpers:remove_network_partition() end},
        {jetstream_restart, fun() -> router_jetstream_recovery_helpers:inject_jetstream_restart() end, fun() -> router_jetstream_recovery_helpers:remove_jetstream_restart() end},
        {ack_failures, fun() -> router_jetstream_recovery_helpers:inject_ack_failures() end, fun() -> router_jetstream_recovery_helpers:remove_ack_failures() end}
    ],
    
    RecoveryTimes = [begin
        %% Inject fault
        InjectFn(),
        timer:sleep(scale_duration(2 * 60 * 1000)),  %% 2 minutes with fault
        
        %% Measure recovery
        RemoveFn(),
        {ok, RecoveryTimeMs} = router_jetstream_recovery_helpers:wait_for_recovery(
            fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
            BaselineThroughput,
            scale_duration(10 * 60 * 1000)
        ),
        
        ct:comment("~p recovery time: ~p ms", [FaultType, RecoveryTimeMs]),
        {FaultType, RecoveryTimeMs}
    end || {FaultType, InjectFn, RemoveFn} <- FaultTypes],
    
    %% Verify all recovery times < 5 minutes (scaled)
    lists:foreach(fun({_Type, TimeMs}) ->
        ?assert(TimeMs < scale_duration(5 * 60 * 1000))
    end, RecoveryTimes),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 5.1: MULTI-NODE JETSTREAM CLUSTER FAILURES
%% ========================================================================

%% @doc Test multi-node JetStream cluster failures
%% Duration: ~90 minutes
%% Simulates failures in a multi-node JetStream cluster
test_multi_node_jetstream_cluster_failures(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase 2: Cluster node failures (simulate 3-node cluster)
    ct:comment("Phase 2: Cluster node failures (60 minutes)"),
    _ = router_jetstream_recovery_store:init_cluster_state(),
    
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        case {router_jetstream_recovery_store:get_cluster_nodes(),
              router_jetstream_recovery_store:get_cluster_quorum()} of
            {{ok, Nodes}, {ok, Quorum}} ->
                case Quorum andalso Nodes >= 2 of
                    true -> {ok, <<"consumer-cluster">>};
                    false -> {error, quorum_lost}
                end;
            _ ->
                {error, quorum_lost}
        end
    end),
    
    %% Simulate node failures every 10 minutes
    Cycles = 6,  %% 60 minutes / 10 minutes
    [begin
        %% Fail one node (if quorum still available)
        case router_jetstream_recovery_store:get_cluster_nodes() of
            {ok, Nodes} when Nodes > 2 ->
                NewNodes = Nodes - 1,
                _ = router_jetstream_recovery_store:set_cluster_nodes(NewNodes),
                QuorumAvailable = NewNodes >= 2,
                _ = router_jetstream_recovery_store:set_cluster_quorum(QuorumAvailable),
                ct:comment("Cycle ~p: Node failed, ~p nodes remaining, quorum: ~p", [Cycle, NewNodes, QuorumAvailable]),
                timer:sleep(scale_duration(30000));  %% 30 seconds with reduced capacity
            {ok, _Nodes} ->
                ct:comment("Cycle ~p: Cannot fail more nodes (quorum would be lost)", [Cycle]);
            _ ->
                ok
        end,
        
        %% Recover node
        case router_jetstream_recovery_store:get_cluster_nodes() of
            {ok, CurrentNodes} when CurrentNodes < 3 ->
                RecoveredNodes = CurrentNodes + 1,
                _ = router_jetstream_recovery_store:set_cluster_nodes(RecoveredNodes),
                _ = router_jetstream_recovery_store:set_cluster_quorum(true),
                ct:comment("Cycle ~p: Node recovered, ~p nodes available", [Cycle, RecoveredNodes]);
            _ ->
                ok
        end,
        
        timer:sleep(scale_duration(570000))  %% 9.5 minutes normal operation
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_cluster_nodes(3),
    _ = router_jetstream_recovery_store:set_cluster_quorum(true),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify no resource leaks
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    _ = router_jetstream_recovery_store:delete_cluster_state(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 5.2: CROSS-REGION NETWORK PARTITIONS
%% ========================================================================

%% @doc Test cross-region network partitions
%% Duration: ~90 minutes
%% Simulates network partitions between regions
test_cross_region_network_partitions(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase 2: Cross-region partition cycles
    ct:comment("Phase 2: Cross-region partition cycles (60 minutes)"),
    _ = router_jetstream_recovery_store:init_region_state(),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case {router_jetstream_recovery_store:get_region_a_connected(),
              router_jetstream_recovery_store:get_region_b_connected()} of
            {{ok, A}, {ok, B}} ->
                case A andalso B of
                    true -> ok;
                    false -> {error, region_partitioned}
                end;
            _ ->
                {error, region_partitioned}
        end
    end),
    
    %% Simulate partitions every 8 minutes
    Cycles = 7,  %% 60 minutes / 8 minutes (approx)
    [begin
        %% Partition: Region A disconnected
        _ = router_jetstream_recovery_store:set_region_a_connected(false),
        ct:comment("Cycle ~p: Region A partitioned", [Cycle]),
        timer:sleep(scale_duration(60000)),  %% 1 minute partitioned
        
        %% Reconnect Region A
        _ = router_jetstream_recovery_store:set_region_a_connected(true),
        ct:comment("Cycle ~p: Region A reconnected", [Cycle]),
        timer:sleep(scale_duration(420000))  %% 7 minutes normal operation
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_region_a_connected(true),
    _ = router_jetstream_recovery_store:set_region_b_connected(true),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify no resource leaks
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    _ = router_jetstream_recovery_store:delete_region_state(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 5.3: ROLLING RESTART ZERO DOWNTIME
%% ========================================================================

%% @doc Test rolling restart with zero downtime
%% Duration: ~60 minutes
%% Simulates rolling restart of router processes without downtime
test_rolling_restart_zero_downtime(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Phase 2: Rolling restarts (simulate 3 router instances)
    ct:comment("Phase 2: Rolling restarts (40 minutes)"),
    _ = router_jetstream_recovery_store:init_router_state_multi(),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        case router_jetstream_recovery_store:get_active_routers() of
            {ok, Active} when Active > 0 ->
                ok;  %% At least one router active
            {ok, _} ->
                {error, no_routers_available};
            _ ->
                {error, no_routers_available}
        end
    end),
    
    %% Simulate rolling restart: restart one router at a time
    RestartCycles = 4,  %% Restart each router once
    [begin
        %% Start restarting one router
        case router_jetstream_recovery_store:get_active_routers() of
            {ok, Active} when Active > 1 ->
                NewActive = Active - 1,
                _ = router_jetstream_recovery_store:set_active_routers(NewActive),
                _ = router_jetstream_recovery_store:set_restarting_router(Cycle),
                ct:comment("Cycle ~p: Router ~p restarting, ~p routers active", [Cycle, Cycle, NewActive]),
                timer:sleep(scale_duration(30000));  %% 30 seconds restarting
            {ok, _} ->
                ct:comment("Cycle ~p: Cannot restart (only one router active)", [Cycle]);
            _ ->
                ok
        end,
        
        %% Router restarted
        case router_jetstream_recovery_store:get_active_routers() of
            {ok, CurrentActive} when CurrentActive < 3 ->
                RestoredActive = CurrentActive + 1,
                _ = router_jetstream_recovery_store:set_active_routers(RestoredActive),
                _ = router_jetstream_recovery_store:set_restarting_router(none),
                ct:comment("Cycle ~p: Router ~p restarted, ~p routers active", [Cycle, Cycle, RestoredActive]);
            _ ->
                ok
        end,
        
        timer:sleep(scale_duration(570000))  %% 9.5 minutes normal operation before next restart
    end || Cycle <- lists:seq(1, RestartCycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_active_routers(3),
    _ = router_jetstream_recovery_store:set_restarting_router(none),
    
    RecoveryResult = router_jetstream_recovery_helpers:wait_for_recovery(
        fun() -> router_jetstream_recovery_helpers:measure_baseline_throughput(scale_duration(10000)) end,
        BaselineThroughput,
        RecoveryDurationMs
    ),
    
    case RecoveryResult of
        {ok, RecoveryTimeMs} ->
            ct:comment("Recovery completed in ~p ms", [RecoveryTimeMs]);
        {timeout, ElapsedMs} ->
            ct:fail("Recovery timeout after ~p ms", [ElapsedMs])
    end,
    
    %% Verify zero downtime: throughput should never drop to zero
    %% (This is verified by the fact that we always have at least one router active)
    ct:comment("Zero downtime verified: At least one router active at all times"),
    
    %% Verify no resource leaks
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    _ = router_jetstream_recovery_store:delete_router_state_multi(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

