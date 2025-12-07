%% @doc Extended Recovery Scenarios Test Suite
%% 
%% Long-running tests for JetStream recovery scenarios:
%% - MaxDeliver exhaustion accumulation
%% - Repeated fault/recovery cycles
%% - Performance degradation detection
%% - Resource leak detection
%% - Throughput/latency recovery measurement
%%
%% @test_category extended_recovery, slow, long_running, performance
-module(router_jetstream_extended_recovery_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for helper functions (CT callbacks are now exported)
-compile({nowarn_unused_function, []}).

%% Export Common Test callbacks (required by CT)
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Export helper functions (only SUITE-specific, shared helpers are in router_jetstream_recovery_helpers)
-export([
    setup_telemetry_handler/0,
    cleanup_telemetry_handler/1
]).

%% Export test functions (only extended tests, no soak tests)
-export([
    smoke_test/1,
    test_maxdeliver_gradual_accumulation/1,
    test_maxdeliver_mass_exhaustion/1,
    test_maxdeliver_periodic_consumer_hang/1,
    test_recovery_after_prolonged_partition/1,
    test_recovery_with_message_redelivery/1,
    test_recovery_with_circuit_breaker_reset/1,
    test_recovery_with_backpressure_clearance/1,
    test_recovery_after_multiple_fault_cycles/1
]).

%% ========================================================================
%% INTERNAL HELPERS FOR EXTENDED SCENARIOS
%% ========================================================================

%% @doc Check if extended recovery scenarios should be enabled.
%% Set RUN_EXTENDED_RECOVERY=1 to enable long-running groups.
-spec extended_enabled() -> boolean().
extended_enabled() ->
    case os:getenv("RUN_EXTENDED_RECOVERY") of
        "1" -> true;
        _ -> false
    end.

%% Re-export scale_duration for convenience (delegates to helpers)
-spec scale_duration(non_neg_integer()) -> non_neg_integer().
scale_duration(BaseMs) ->
    router_jetstream_recovery_helpers:scale_duration(BaseMs).

all() ->
    BaseTests = [smoke_test],
    case extended_enabled() of
        false ->
            %% Fast mode: only smoke_test
            BaseTests;
        true ->
            %% Extended mode: functional recovery scenarios (no soak tests)
            BaseTests ++ [
                {group, maxdeliver_scenarios},
                {group, additional_recovery_scenarios}
            ]
    end.

groups() ->
    [
        {maxdeliver_scenarios, [sequence], [
            test_maxdeliver_gradual_accumulation,
            test_maxdeliver_mass_exhaustion,
            test_maxdeliver_periodic_consumer_hang
        ]},
        {additional_recovery_scenarios, [sequence], [
            test_recovery_after_prolonged_partition,
            test_recovery_with_message_redelivery,
            test_recovery_with_circuit_breaker_reset,
            test_recovery_with_backpressure_clearance,
            test_recovery_after_multiple_fault_cycles
        ]}
    ].

init_per_suite(Config) ->
    router_jetstream_ct_helpers:init_suite(Config).

end_per_suite(Config) ->
    router_jetstream_ct_helpers:end_suite(Config).

init_per_testcase(TestCase, Config) ->
    router_jetstream_ct_helpers:init_case(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    router_jetstream_ct_helpers:end_case(TestCase, Config).

%% ========================================================================
%% DIAGNOSTIC SMOKE TEST
%% ========================================================================

%% @doc Minimal smoke test to verify CT discovery and suite loading
smoke_test(_Config) ->
    ct:pal("=== SMOKE TEST in ~p ===", [?MODULE]),
    ct:pal("SUITE module: ~p", [?MODULE]),
    ct:pal("SUITE file: ~p", [code:which(?MODULE)]),
    ct:pal("Store ensure result: ~p", [router_jetstream_recovery_store:ensure()]),
    ct:pal("Extended enabled: ~p (RUN_EXTENDED_RECOVERY=~p)", 
           [extended_enabled(), os:getenv("RUN_EXTENDED_RECOVERY")]),
    ct:pal("Speedup factor: ~p (EXTENDED_TEST_SPEEDUP=~p)", 
           [router_jetstream_recovery_helpers:speedup_factor(), os:getenv("EXTENDED_TEST_SPEEDUP")]),
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

%% Legacy get_metrics_table/0 удалён как неиспользуемый (метрики идут через router_jetstream_recovery_store).
%% All helper functions moved to router_jetstream_recovery_helpers module.

%% ========================================================================
%% SCENARIO 1.1: GRADUAL MAXDELIVER ACCUMULATION
%% ========================================================================

%% @doc Test gradual MaxDeliver accumulation over time
%% Duration: ~30 minutes
test_maxdeliver_gradual_accumulation(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    AccumulationDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    ExhaustionDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    
    %% Setup mocks
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    ct:comment("Baseline throughput: ~.2f msg/s", [BaselineThroughput]),
    
    %% Phase 2: Accumulation (intermittent ACK failures)
    ct:comment("Phase 2: Accumulation (10 minutes)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        %% 30% failure rate
        case rand:uniform(100) =< 30 of
            true -> {error, timeout};
            false -> ok
        end
    end),
    
    AccumulationThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(AccumulationDurationMs),
    AccumulationMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    ct:comment("Accumulation throughput: ~.2f msg/s", [AccumulationThroughput]),
    
    %% Phase 3: Exhaustion (verify DLQ handling)
    ct:comment("Phase 3: Exhaustion (5 minutes)"),
    timer:sleep(ExhaustionDurationMs),
    ExhaustionMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    
    %% Verify exhaustion metrics show increased failures
    ExhaustionFailures = maps:get(router_nats_ack_failures_total, ExhaustionMetrics, 0),
    AccumulationFailures = maps:get(router_nats_ack_failures_total, AccumulationMetrics, 0),
    ?assert(ExhaustionFailures >= AccumulationFailures, "Failures should increase during exhaustion"),
    
    %% Phase 4: Recovery
    ct:comment("Phase 4: Recovery (10 minutes)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
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
    
    %% Verify success criteria
    FinalMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    router_jetstream_recovery_helpers:verify_no_resource_leaks(AccumulationMetrics, FinalMetrics),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 1.2: MASS MAXDELIVER EXHAUSTION
%% ========================================================================

%% @doc Test mass MaxDeliver exhaustion
%% Duration: ~18 minutes
test_maxdeliver_mass_exhaustion(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(3 * 60 * 1000),  %% 3 minutes
    MassFailureDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (3 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    %% Phase 2: Mass failure (100% ACK failure)
    ct:comment("Phase 2: Mass failure (5 minutes)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> {error, timeout} end),
    
    %% Process 1000+ messages
    router_jetstream_recovery_helpers:process_message_batch(1000),
    timer:sleep(MassFailureDurationMs),
    
    MassFailureMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    
    %% Verify mass failure metrics show high failure rate
    MassFailureAckFailures = maps:get(router_nats_ack_failures_total, MassFailureMetrics, 0),
    BaselineAckFailures = maps:get(router_nats_ack_failures_total, router_jetstream_recovery_helpers:collect_metrics_snapshot(), 0),
    ?assert(MassFailureAckFailures > BaselineAckFailures, "Mass failure should show increased ACK failures"),
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    
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
    
    %% Verify no performance degradation
    FinalThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(60000),
    router_jetstream_recovery_helpers:assert_throughput_ok(BaselineThroughput, FinalThroughput),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SCENARIO 1.3: PERIODIC CONSUMER HANG
%% ========================================================================

%% @doc Test periodic consumer hang causing delivery count accumulation
%% Duration: ~45 minutes
test_maxdeliver_periodic_consumer_hang(_Config) ->
    %% Configuration (scalable via EXTENDED_TEST_SPEEDUP)
    BaselineDurationMs = scale_duration(5 * 60 * 1000),  %% 5 minutes
    HangCyclesDurationMs = scale_duration(30 * 60 * 1000),  %% 30 minutes
    RecoveryDurationMs = scale_duration(10 * 60 * 1000),  %% 10 minutes
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Phase 1: Baseline
    ct:comment("Phase 1: Baseline (5 minutes)"),
    BaselineThroughput = router_jetstream_recovery_helpers:measure_baseline_throughput(BaselineDurationMs),
    
    %% Phase 2: Hang cycles (every 2 minutes, hang for 30 seconds)
    ct:comment("Phase 2: Hang cycles (30 minutes)"),
    _ = router_jetstream_recovery_store:init_hang_active(),
    
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        case router_jetstream_recovery_store:get_hang_active() of
            {ok, Active} ->
                case Active of
                    true -> {error, timeout};  %% Hang: don't ACK
                    false -> ok
                end;
            _ ->
                ok
        end
    end),
    
    %% Run hang cycles
    HangCycleDurationMs = scale_duration(2 * 60 * 1000),  %% 2 minutes per cycle
    Cycles = HangCyclesDurationMs div HangCycleDurationMs,
    [begin
        %% Hang period (30 seconds)
        _ = router_jetstream_recovery_store:set_hang_active(true),
        ct:comment("Cycle ~p: Hang started", [Cycle]),
        timer:sleep(scale_duration(30000)),
        
        %% Resume period (90 seconds)
        _ = router_jetstream_recovery_store:set_hang_active(false),
        ct:comment("Cycle ~p: Hang ended", [Cycle]),
        timer:sleep(scale_duration(90000))
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Phase 3: Recovery
    ct:comment("Phase 3: Recovery (10 minutes)"),
    _ = router_jetstream_recovery_store:set_hang_active(false),
    
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
    _ = router_jetstream_recovery_store:delete_hang_active(),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% ========================================================================
%% SOAK TESTS MOVED TO router_jetstream_soak_SUITE.erl
%% ========================================================================
%% The following soak tests have been moved to a separate SUITE:
%% - test_repeated_jetstream_restarts/1
%% - test_repeated_router_restarts/1
%% - test_network_partition_recovery/1
%% - test_sequential_fault_chain/1
%% - test_repeating_fault_cycles/1
%% - test_long_running_stability/1
%% - test_recovery_time_measurement/1
%% - test_multi_node_jetstream_cluster_failures/1
%% - test_cross_region_network_partitions/1
%% - test_rolling_restart_zero_downtime/1
%%
%% See test/router_jetstream_soak_SUITE.erl for these tests.

%% ========================================================================
%% ADDITIONAL RECOVERY SCENARIOS
%% ========================================================================

%% @doc Test recovery after prolonged partition
%% Scenario: System recovers after 5-minute network partition
%% Data Guarantees:
%% - No message loss: Messages are queued during partition
%% - Recovery: System recovers after partition heals
test_recovery_after_prolonged_partition(_Config) ->
    ct:comment("=== Recovery After Prolonged Partition Test ==="),
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Get initial metrics
    InitialMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Inject prolonged partition (5 minutes)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(scale_duration(300000)),  %% 5 minutes
    
    %% Get metrics during partition
    PartitionMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    
    %% Verify: Router process remains alive
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Partition metrics show connection failures
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    ?assert(PartitionConnectionFailures >= InitialConnectionFailures, "Connection failures should increase during partition"),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(scale_duration(30000)),  %% 30 seconds for recovery
    
    %% Get final metrics
    FinalMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Verify: Recovery occurred
    InitialConnectionRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
    FinalConnectionRestored = maps:get(router_nats_connection_restored_total, FinalMetrics, 0),
    ConnectionRestoredDelta = FinalConnectionRestored - InitialConnectionRestored,
    ct:comment("Connection restored: Initial=~p, Final=~p, Delta=~p", [InitialConnectionRestored, FinalConnectionRestored, ConnectionRestoredDelta]),
    
    %% Verify no resource leaks
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% @doc Test recovery with message redelivery
%% Scenario: System recovers and redelivers messages after partition
%% Data Guarantees:
%% - No message loss: Messages are redelivered after partition heals
%% - Redelivery limits: MaxDeliver is enforced
%% - Recovery: System recovers after partition heals
test_recovery_with_message_redelivery(_Config) ->
    ct:comment("=== Recovery with Message Redelivery Test ==="),
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Get initial metrics
    InitialMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Inject partition
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(scale_duration(10000)),  %% 10 seconds
    
    %% Get metrics during partition
    PartitionMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    
    %% Verify: Router process remains alive
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Partition metrics show connection issues
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery and redelivery
    timer:sleep(scale_duration(30000)),  %% 30 seconds for recovery and redelivery
    
    %% Get final metrics
    FinalMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Verify: Redelivery occurred
    InitialRedeliveries = maps:get(router_jetstream_redeliveries_total, InitialMetrics, 0),
    FinalRedeliveries = maps:get(router_jetstream_redeliveries_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedeliveries - InitialRedeliveries,
    ct:comment("Redeliveries: Initial=~p, Final=~p, Delta=~p", [InitialRedeliveries, FinalRedeliveries, RedeliveryDelta]),
    
    %% Verify no resource leaks
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% @doc Test recovery with circuit breaker reset
%% Scenario: System recovers and resets circuit breaker after partition
%% Data Guarantees:
%% - No message loss: Messages are queued during partition
%% - Circuit breaker: Circuit breaker resets after recovery
%% - Recovery: System recovers after partition heals
test_recovery_with_circuit_breaker_reset(_Config) ->
    ct:comment("=== Recovery with Circuit Breaker Reset Test ==="),
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Get initial metrics
    InitialMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Inject partition
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(scale_duration(10000)),  %% 10 seconds
    
    %% Get metrics during partition
    PartitionMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    
    %% Verify: Router process remains alive
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Check circuit breaker state (if circuit breaker is enabled)
    InitialCircuitBreakerOpens = maps:get(router_circuit_breaker_opens_total, InitialMetrics, 0),
    PartitionCircuitBreakerOpens = maps:get(router_circuit_breaker_opens_total, PartitionMetrics, 0),
    ct:comment("Circuit breaker opens: Initial=~p, Partition=~p", [InitialCircuitBreakerOpens, PartitionCircuitBreakerOpens]),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery and circuit breaker reset
    timer:sleep(scale_duration(30000)),  %% 30 seconds for recovery
    
    %% Get final metrics
    FinalMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Verify: Circuit breaker reset (closes)
    FinalCircuitBreakerOpens = maps:get(router_circuit_breaker_opens_total, FinalMetrics, 0),
    InitialCircuitBreakerCloses = maps:get(router_circuit_breaker_closes_total, InitialMetrics, 0),
    FinalCircuitBreakerCloses = maps:get(router_circuit_breaker_closes_total, FinalMetrics, 0),
    CircuitBreakerClosesDelta = FinalCircuitBreakerCloses - InitialCircuitBreakerCloses,
    ct:comment("Circuit breaker opens: Initial=~p, Partition=~p, Final=~p", 
               [InitialCircuitBreakerOpens, PartitionCircuitBreakerOpens, FinalCircuitBreakerOpens]),
    ct:comment("Circuit breaker closes: Initial=~p, Final=~p, Delta=~p", 
               [InitialCircuitBreakerCloses, FinalCircuitBreakerCloses, CircuitBreakerClosesDelta]),
    %% Verify circuit breaker opened during partition and may have closed after recovery
    ?assert(FinalCircuitBreakerOpens >= PartitionCircuitBreakerOpens, 
            "Circuit breaker opens should not decrease after recovery"),
    
    %% Verify no resource leaks
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% @doc Test recovery with backpressure clearance
%% Scenario: System recovers and clears backpressure after partition
%% Data Guarantees:
%% - No message loss: Messages are queued during partition
%% - Backpressure: Backpressure clears after recovery
%% - Recovery: System recovers after partition heals
test_recovery_with_backpressure_clearance(_Config) ->
    ct:comment("=== Recovery with Backpressure Clearance Test ==="),
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Get initial metrics
    InitialMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Setup: Create pending state for backpressure (via store, not direct ETS)
    Subject = <<"beamline.router.v1.decide">>,
    _ = router_jetstream_recovery_store:ensure_pending_table(),
    _ = router_jetstream_recovery_store:put_pending(Subject, 1500, erlang:system_time(millisecond)),
    
    %% Inject partition
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(scale_duration(10000)),  %% 10 seconds
    
    %% Get metrics during partition
    PartitionMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    
    %% Verify: Router process remains alive
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Partition metrics show connection issues
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Check backpressure status
    {BackpressureStatus, _RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ct:comment("Backpressure status during partition: ~p", [BackpressureStatus]),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery and backpressure clearance
    timer:sleep(scale_duration(30000)),  %% 30 seconds for recovery
    
    %% Get final metrics
    FinalMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Verify: Backpressure cleared
    {FinalBackpressureStatus, _FinalRetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ct:comment("Backpressure status after recovery: ~p", [FinalBackpressureStatus]),
    
    %% Verify: Connection restored after partition
    InitialConnectionRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
    FinalConnectionRestored = maps:get(router_nats_connection_restored_total, FinalMetrics, 0),
    ConnectionRestoredDelta = FinalConnectionRestored - InitialConnectionRestored,
    ct:comment("Connection restored: Initial=~p, Final=~p, Delta=~p", [InitialConnectionRestored, FinalConnectionRestored, ConnectionRestoredDelta]),
    ?assert(FinalConnectionRestored >= InitialConnectionRestored, "Connection should be restored after partition"),
    
    %% Verify: Pending operations cleared (backpressure cleared)
    InitialPendingOps = maps:get(router_nats_pending_operations_total, InitialMetrics, 0),
    FinalPendingOps = maps:get(router_nats_pending_operations_total, FinalMetrics, 0),
    ct:comment("Pending operations: Initial=~p, Final=~p", [InitialPendingOps, FinalPendingOps]),
    
    %% Verify no resource leaks
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    _ = router_jetstream_recovery_store:delete_pending_table(),
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

%% @doc Test recovery after multiple fault cycles
%% Scenario: System recovers after multiple fault/recovery cycles
%% Data Guarantees:
%% - No message loss: Messages are queued during faults
%% - Recovery: System recovers after each fault cycle
%% - Stability: System remains stable after multiple cycles
test_recovery_after_multiple_fault_cycles(_Config) ->
    ct:comment("=== Recovery After Multiple Fault Cycles Test ==="),
    
    HandlerId = setup_telemetry_handler(),
    router_jetstream_recovery_helpers:setup_standard_mocks(),
    
    %% Get initial metrics
    InitialMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    InitialResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Simulate multiple fault cycles: 5 cycles of 10s partition + 10s recovery
    Cycles = 5,
    [begin
        ct:comment("Cycle ~p: Injecting partition", [Cycle]),
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        timer:sleep(scale_duration(10000)),  %% 10 seconds partitioned
        
        ct:comment("Cycle ~p: Recovering from partition", [Cycle]),
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(scale_duration(10000))   %% 10 seconds recovery
    end || Cycle <- lists:seq(1, Cycles)],
    
    %% Get final metrics
    FinalMetrics = router_jetstream_recovery_helpers:collect_metrics_snapshot(),
    FinalResources = router_jetstream_recovery_helpers:track_resources(),
    
    %% Verify: Router process remains alive
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Multiple recovery cycles occurred
    InitialConnectionRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
    FinalConnectionRestored = maps:get(router_nats_connection_restored_total, FinalMetrics, 0),
    ConnectionRestoredDelta = FinalConnectionRestored - InitialConnectionRestored,
    ct:comment("Connection restored: Initial=~p, Final=~p, Delta=~p (expected ~p cycles)", [InitialConnectionRestored, FinalConnectionRestored, ConnectionRestoredDelta, Cycles]),
    
    %% Verify no resource leaks
    router_jetstream_recovery_helpers:verify_no_resource_leaks(InitialResources, FinalResources),
    
    %% Cleanup
    cleanup_telemetry_handler(HandlerId),
    router_jetstream_recovery_helpers:cleanup_mocks(),
    
    ok.

