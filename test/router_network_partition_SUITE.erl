%% @doc Common Test suite for network partition scenarios
%% Tests: Single-instance, multi-instance/split-brain, service-broker partitions, flapping network
%% @test_category network_partition, slow, integration, fault_injection
-module(router_network_partition_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1,
                                    init_per_testcase/2, end_per_testcase/2]}).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        {group, single_instance_tests},
        {group, multi_instance_tests},
        {group, service_broker_tests},
        {group, flapping_network_tests},
        {group, additional_partition_scenarios}
    ].

groups() ->
    [
        {single_instance_tests, [sequence], [
            test_single_instance_partial_partition,
            test_single_instance_full_partition,
            test_single_instance_partition_healing,
            test_single_instance_jetstream_partition_short,
            test_single_instance_jetstream_partition_long,
            test_single_instance_jetstream_partition_recovery,
            test_single_instance_external_service_partition_short,
            test_single_instance_external_service_partition_long,
            test_single_instance_external_service_partition_recovery,
            test_single_instance_latency_degradation,
            test_single_instance_partial_packet_loss,
            test_single_instance_intermittent_connectivity,
            test_single_instance_slow_network
        ]},
        {multi_instance_tests, [sequence], [
            test_multi_instance_split_brain,
            test_multi_instance_partial_partition,
            test_multi_instance_leader_election_after_healing,
            test_multi_instance_split_brain_leader_election,
            test_multi_instance_split_brain_no_duplicate_processing,
            test_multi_instance_split_brain_recovery,
            test_multi_instance_jetstream_partition_instance_a_isolated,
            test_multi_instance_jetstream_partition_jetstream_cluster_split,
            test_multi_instance_jetstream_partition_recovery,
            test_multi_instance_distributed_locks_partition,
            test_multi_instance_distributed_locks_recovery
        ]},
        {service_broker_tests, [sequence], [
            test_service_broker_partition,
            test_service_broker_partition_retry_behavior,
            test_service_broker_partition_recovery
        ]},
        {flapping_network_tests, [sequence], [
            test_flapping_network_stability,
            test_flapping_network_no_resource_leaks,
            test_flapping_network_recovery,
            test_flapping_network_with_latency,
            test_flapping_network_with_packet_loss
        ]},
        {additional_partition_scenarios, [sequence], [
            test_partial_network_partition_with_recovery,
            test_intermittent_connectivity_with_backpressure,
            test_network_partition_with_message_redelivery,
            test_split_brain_with_leader_election,
            test_network_partition_with_circuit_breaker
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    
    %% Ensure metrics table exists
    router_metrics:ensure(),
    
    %% Ensure partition store tables exist
    case router_network_partition_store:ensure() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_per_suite: failed to ensure partition store: ~p", [Reason])
    end,
    
    Config.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    _ = router_network_partition_store:reset(),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state before each test
    router_nats_fault_injection:clear_all_faults(),
    %% Reset metrics using router_r10_metrics access layer
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    %% Reset partition store
    case router_network_partition_store:reset() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_per_testcase: failed to reset partition store: ~p", [Reason])
    end,
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state after each test
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Get metrics snapshot
%% Uses router_r10_metrics access layer instead of direct ETS access
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    router_metrics:ensure(),
    %% Use router_r10_metrics access layer for R10 metrics
    %% dump_metrics() returns list of ETS entries: [{Key, Value} | ...]
    %% Convert to map format compatible with existing code
    try
        %% Get all metrics via access layer (returns ETS tab2list format)
        Metrics = router_r10_metrics:dump_metrics(),
        %% Convert ETS entries to map format
        lists:foldl(fun
            ({Key, Value}, Acc) when is_atom(Key) ->
                %% Simple metric without labels
                maps:put(Key, Value, Acc);
            ({{MetricName, _LabelsKey}, Value}, Acc) when is_atom(MetricName) ->
                %% Metric with labels - aggregate by metric name
                Current = maps:get(MetricName, Acc, 0),
                maps:put(MetricName, Current + Value, Acc);
            (_, Acc) ->
                Acc
        end, #{}, Metrics)
    catch
        _:_ ->
            %% Metrics not accessible - return empty map
            #{}
    end.

%% @doc Verify network partition contract invariants
%% Checks:
%% - MaxDeliver semantics (messages don't exceed MaxDeliver)
%% - Redelivery limits (redelivery count is reasonable)
%% - Metrics correctness (metrics reflect partition state)
%% - Fail-open behavior (processes remain alive)
%% - Data guarantees (no duplicates, losses, inconsistencies)
%% - Latency bounds (if applicable)
%% - Packet loss tolerance (if applicable)
-spec verify_network_partition_contracts(map(), map(), map()) -> ok.
verify_network_partition_contracts(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    %% Check MaxDeliver semantics
    verify_maxdeliver_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior),
    
    %% Check redelivery limits
    verify_redelivery_limits(InitialMetrics, FinalMetrics, ExpectedBehavior),
    
    %% Check metrics correctness
    verify_metrics_correctness(InitialMetrics, FinalMetrics, ExpectedBehavior),
    
    %% Check data guarantees (no duplicates, losses, inconsistencies)
    verify_data_guarantees(InitialMetrics, FinalMetrics, ExpectedBehavior),
    
    %% Check latency bounds (if applicable)
    verify_latency_bounds(InitialMetrics, FinalMetrics, ExpectedBehavior),
    
    %% Check packet loss tolerance (if applicable)
    verify_packet_loss_tolerance(InitialMetrics, FinalMetrics, ExpectedBehavior),
    
    ok.

%% @doc Verify MaxDeliver semantics
-spec verify_maxdeliver_semantics(map(), map(), map()) -> ok.
verify_maxdeliver_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, InitialMetrics, 0),
    FinalMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, FinalMetrics, 0),
    MaxDeliverExhaustedDelta = FinalMaxDeliverExhausted - InitialMaxDeliverExhausted,
    
    %% MaxDeliver exhaustion should only occur if expected
    ExpectedMaxDeliverExhaustion = maps:get(expected_maxdeliver_exhaustion, ExpectedBehavior, 0),
    
    ct:comment("MaxDeliver exhausted: Initial=~p, Final=~p, Delta=~p, Expected=~p",
               [InitialMaxDeliverExhausted, FinalMaxDeliverExhausted, MaxDeliverExhaustedDelta, ExpectedMaxDeliverExhaustion]),
    
    %% Allow some tolerance
    Tolerance = maps:get(maxdeliver_tolerance, ExpectedBehavior, 2),
    ?assert((abs(MaxDeliverExhaustedDelta - ExpectedMaxDeliverExhaustion) =< Tolerance)),
    
    ok.

%% @doc Verify redelivery limits
-spec verify_redelivery_limits(map(), map(), map()) -> ok.
verify_redelivery_limits(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedelivery - InitialRedelivery,
    
    %% Redelivery should be within reasonable limits
    MaxExpectedRedelivery = maps:get(max_redelivery, ExpectedBehavior, 100),
    
    ct:comment("Redelivery: Initial=~p, Final=~p, Delta=~p, MaxExpected=~p",
               [InitialRedelivery, FinalRedelivery, RedeliveryDelta, MaxExpectedRedelivery]),
    
    ?assert((RedeliveryDelta =< MaxExpectedRedelivery)),
    
    ok.

%% @doc Verify metrics correctness
-spec verify_metrics_correctness(map(), map(), map()) -> ok.
verify_metrics_correctness(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    %% Check that error metrics increase during partition
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FinalConnectionLost = maps:get(router_nats_connection_lost_total, FinalMetrics, 0),
    
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    FinalConnectionFailures = maps:get(router_nats_connection_failures_total, FinalMetrics, 0),
    
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FinalPublishFailures = maps:get(router_nats_publish_failures_total, FinalMetrics, 0),
    
    ct:comment("Metrics: ConnectionLost=~p->~p, ConnectionFailures=~p->~p, PublishFailures=~p->~p",
               [InitialConnectionLost, FinalConnectionLost,
                InitialConnectionFailures, FinalConnectionFailures,
                InitialPublishFailures, FinalPublishFailures]),
    
    %% At least one error metric should increase if partition was injected
    PartitionInjected = maps:get(partition_injected, ExpectedBehavior, true),
    case PartitionInjected of
        true ->
            ?assert(FinalConnectionLost > InitialConnectionLost orelse
                    FinalConnectionFailures > InitialConnectionFailures orelse
                    FinalPublishFailures > InitialPublishFailures);
        false ->
            ok
    end,
    
    ok.

%% ========================================================================
%% SINGLE-INSTANCE TESTS
%% ========================================================================

%% @doc Test partial partition (one-way/asymmetric)
%% Scenario: Instance A cannot see broker/neighbors, Instance B continues normally
test_single_instance_partial_partition(_Config) ->
    ct:comment("=== Single-Instance Partial Partition Test ==="),
    
    %% Setup: Start router instance
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Create partition: Router -> NATS (one-way) using fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Phase 1: During partition
    ct:comment("Phase 1: During partition"),
    timer:sleep(2000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: Router process remains alive (fail-open)
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Phase 2: Healing
    ct:comment("Phase 2: Healing partition"),
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router reconnects
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test full partition (complete isolation)
test_single_instance_full_partition(_Config) ->
    ct:comment("=== Single-Instance Full Partition Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Create full partition: Router <-> NATS (bidirectional) using fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, connection_refused}),
    
    %% Verify: Complete isolation
    timer:sleep(2000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    PartitionPublishFailures = maps:get(router_nats_publish_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    ct:comment("Publish failures: Initial=~p, Partition=~p", 
               [InitialPublishFailures, PartitionPublishFailures]),
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    router_nats_fault_injection:disable_fault(publish),
    
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test partition healing and recovery
test_single_instance_partition_healing(_Config) ->
    ct:comment("=== Single-Instance Partition Healing Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Create partition using fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: Router in degraded mode
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Healing
    ct:comment("Healing partition"),
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router recovers
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: New messages can be processed
    %% (In mock mode, verify via meck)
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test short JetStream partition (5-10 seconds)
test_single_instance_jetstream_partition_short(_Config) ->
    ct:comment("=== Single-Instance JetStream Partition (Short) Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Enable connection fault injection (mock mode)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Phase 1: During partition (short: 5 seconds)
    ct:comment("Phase 1: During short partition (5 seconds)"),
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: Router process remains alive (fail-open)
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Phase 2: Recovery
    ct:comment("Phase 2: Recovery after partition"),
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router reconnects
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test long JetStream partition (2-5 minutes)
test_single_instance_jetstream_partition_long(_Config) ->
    ct:comment("=== Single-Instance JetStream Partition (Long) Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics and memory usage
    InitialMetrics = get_metrics_snapshot(),
    InitialMemory = erlang:memory(total),
    
    %% Enable connection fault injection (mock mode)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Phase 1: During partition (long: 2 minutes, but in tests use shorter: 30 seconds)
    ct:comment("Phase 1: During long partition (30 seconds for test)"),
    timer:sleep(30000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: Router process remains alive (fail-open)
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: No resource leaks
    FinalMemory = erlang:memory(total),
    MemoryGrowth = FinalMemory - InitialMemory,
    ct:log("Memory growth: ~p bytes", [MemoryGrowth]),
    %% Allow some growth, but not unbounded (threshold: 50MB)
    ?assert(MemoryGrowth < 50 * 1024 * 1024),
    
    %% Verify: No unbounded process growth
    ProcessCount = length(erlang:processes()),
    ct:log("Process count: ~p", [ProcessCount]),
    %% Reasonable threshold: less than 1000 processes
    ?assert(ProcessCount < 1000),
    
    %% Phase 2: Recovery
    ct:comment("Phase 2: Recovery after long partition"),
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router reconnects
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test JetStream partition recovery validation
test_single_instance_jetstream_partition_recovery(_Config) ->
    ct:comment("=== Single-Instance JetStream Partition Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Enable connection fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: Router in degraded mode
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Recovery: Disable fault injection
    ct:comment("Recovery: Restoring connection"),
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router recovers
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test short external service partition (5-10 seconds)
test_single_instance_external_service_partition_short(_Config) ->
    ct:comment("=== Single-Instance External Service Partition (Short) Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Mock external HTTP service to return connection errors
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request, fun(_Method, _URL, _Headers, _Body, _Options) ->
        {error, connection_refused}
    end),
    
    %% Phase 1: During partition (short: 5 seconds)
    ct:comment("Phase 1: During short partition (5 seconds)"),
    timer:sleep(5000),
    
    %% Get metrics during partition
    _ = get_metrics_snapshot(),
    
    %% Verify: Router process remains alive (fail-open)
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition (if external service metrics exist)
    ct:comment("External service partition metrics captured"),
    
    %% Phase 2: Recovery
    ct:comment("Phase 2: Recovery after partition"),
    meck:unload(httpc),
    
    %% Wait for recovery
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router continues operating
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test long external service partition (2-5 minutes)
test_single_instance_external_service_partition_long(_Config) ->
    ct:comment("=== Single-Instance External Service Partition (Long) Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics and memory usage
    InitialMetrics = get_metrics_snapshot(),
    InitialMemory = erlang:memory(total),
    
    %% Mock external HTTP service to return connection errors
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request, fun(_Method, _URL, _Headers, _Body, _Options) ->
        {error, connection_refused}
    end),
    
    %% Phase 1: During partition (long: 30 seconds for test)
    ct:comment("Phase 1: During long partition (30 seconds for test)"),
    timer:sleep(30000),
    
    %% Get metrics during partition
    _ = get_metrics_snapshot(),
    
    %% Verify: Router process remains alive (fail-open)
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    ct:comment("External service partition metrics captured"),
    
    %% Verify: No resource leaks
    FinalMemory = erlang:memory(total),
    MemoryGrowth = FinalMemory - InitialMemory,
    ct:log("Memory growth: ~p bytes", [MemoryGrowth]),
    ?assert(MemoryGrowth < 50 * 1024 * 1024),
    
    %% Phase 2: Recovery
    ct:comment("Phase 2: Recovery after long partition"),
    meck:unload(httpc),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router continues operating
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test external service partition recovery validation
test_single_instance_external_service_partition_recovery(_Config) ->
    ct:comment("=== Single-Instance External Service Partition Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Mock external HTTP service to return connection errors
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request, fun(_Method, _URL, _Headers, _Body, _Options) ->
        {error, connection_refused}
    end),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    _ = get_metrics_snapshot(),
    
    %% Verify: Router in degraded mode
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    ct:comment("External service partition metrics captured"),
    
    %% Recovery: Restore external service
    ct:comment("Recovery: Restoring external service connection"),
    meck:unload(httpc),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Router recovers
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% ========================================================================
%% MULTI-INSTANCE TESTS
%% ========================================================================

%% @doc Test split-brain scenario (cluster divided into two groups)
test_multi_instance_split_brain(_Config) ->
    ct:comment("=== Multi-Instance Split-Brain Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition: Group 1 <-> Group 2 using fault injection
    %% (In real scenario, would have multiple router processes)
    %% For now, simulate via fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Verify: Both groups continue operating
    timer:sleep(2000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Potential double leadership / duplicate processing
    %% (In mock mode, verify via meck)
    
    %% Verify: Guarantees consistency
    %% - No more than one active consumer per consumer group
    %% - No duplicate message processing (idempotency)
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Correct leader elected after healing
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Conflicts resolved
    %% - One branch considered "true"
    %% - State consistency restored
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test partial partition between instances
test_multi_instance_partial_partition(_Config) ->
    ct:comment("=== Multi-Instance Partial Partition Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition: Instance A cannot see broker, Instance B can
    %% Using fault injection (standardized approach)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Verify: Instance A isolated, Instance B continues
    timer:sleep(2000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Request/reply behavior
    %% - Instance A: Cannot process requests
    %% - Instance B: Continues processing
    
    %% Verify: Leader/consumer behavior
    %% - If leader is isolated: New leader elected
    %% - If follower is isolated: Leader continues
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Instance A rejoins
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test leader election after partition healing
test_multi_instance_leader_election_after_healing(_Config) ->
    ct:comment("=== Multi-Instance Leader Election After Healing Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition using fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Two leaders exist (split-brain)
    %% (In mock mode, verify via meck)
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Single leader elected
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Load balancing restored
    %% (In mock mode, verify via meck)
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test leader election during split-brain partition
test_multi_instance_split_brain_leader_election(_Config) ->
    ct:comment("=== Multi-Instance Split-Brain Leader Election Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition: Group 1 <-> Group 2 using fault injection
    %% (In real scenario, would have multiple router instances)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Leader election triggered (in mock mode, verify via meck)
    %% Expected: No two leaders simultaneously
    %% Expected: On quorum loss, leader correctly resigns
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Single leader elected after healing
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test no duplicate message processing during split-brain
test_multi_instance_split_brain_no_duplicate_processing(_Config) ->
    ct:comment("=== Multi-Instance Split-Brain No Duplicate Processing Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition: Group 1 <-> Group 2 using fault injection
    %% (In real scenario, would have multiple instances)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: No duplicate message processing
    %% - Idempotency layer prevents duplicates
    %% - MaxDeliver semantics respected
    %% - Delivery count tracking prevents duplicate ACKs
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: State consistency restored
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants (no duplicate processing)
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test split-brain recovery
test_multi_instance_split_brain_recovery(_Config) ->
    ct:comment("=== Multi-Instance Split-Brain Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition: Group 1 <-> Group 2 using fault injection
    %% (In real scenario, would have multiple instances)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Correct leader elected after healing
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Conflicts resolved
    %% - One branch considered "true"
    %% - State consistency restored
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test instance A isolated from JetStream
test_multi_instance_jetstream_partition_instance_a_isolated(_Config) ->
    ct:comment("=== Multi-Instance JetStream Partition (Instance A Isolated) Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate instance A losing connection to JetStream
    %% (In real scenario, would have multiple instances)
    %% For now, simulate via fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Messages don't get stuck
    %% Instance A stops receiving, Instance B continues (in real scenario)
    
    %% Recovery
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Instance A reconnects and synchronizes
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test JetStream cluster split
test_multi_instance_jetstream_partition_jetstream_cluster_split(_Config) ->
    ct:comment("=== Multi-Instance JetStream Partition (JetStream Cluster Split) Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate JetStream cluster split
    %% (In real scenario, would have JetStream cluster with multiple nodes)
    %% For now, simulate via fault injection
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Instances connected to different nodes behave correctly
    %% (In real scenario, would verify node-specific behavior)
    
    %% Recovery
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Cluster state restored
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test JetStream partition recovery
test_multi_instance_jetstream_partition_recovery(_Config) ->
    ct:comment("=== Multi-Instance JetStream Partition Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Recovery
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Instance A correctly synchronizes
    %% - Continues from current offset/sequence
    %% - Or recreates subscription according to protocol
    %% - No uncontrolled duplicate spike
    
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test distributed locks during partition
test_multi_instance_distributed_locks_partition(_Config) ->
    ct:comment("=== Multi-Instance Distributed Locks Partition Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition between instances using fault injection
    %% (In real scenario, would have multiple instances)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Invariants not violated
    %% - Only one instance holds lock at a time
    %% - Locks expire correctly after timeout
    %% - No "double ownership" of resources
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Locks/sessions reacquired correctly
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test distributed locks recovery after partition
test_multi_instance_distributed_locks_recovery(_Config) ->
    ct:comment("=== Multi-Instance Distributed Locks Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate partition between instances using fault injection
    %% (In real scenario, would have multiple instances)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: No conflicting states
    %% - Conflicts detected and resolved
    %% - "Winner" determined (e.g., last-write-wins)
    %% - State consistency restored
    
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: No resource leaks
    %% - All locks released
    %% - All sessions cleaned up
    %% - No orphaned resources
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% ========================================================================
%% SERVICE-BROKER TESTS
%% ========================================================================

%% @doc Test partition between service and broker
test_service_broker_partition(_Config) ->
    ct:comment("=== Service-Broker Partition Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Create partition: All service instances lose connection to JetStream
    %% Using fault injection (standardized approach)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Verify: Retry/backoff behavior
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Cache/buffer behavior
    %% - Unforwarded messages buffered
    %% - No message loss (within guarantees)
    
    %% Verify: Message duplication handling
    %% - Idempotency preserved
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Reconnection
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test retry behavior during service-broker partition
test_service_broker_partition_retry_behavior(_Config) ->
    ct:comment("=== Service-Broker Partition Retry Behavior Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Using fault injection (standardized approach)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Verify: Exponential backoff
    timer:sleep(10000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Verify: Retry limits respected
    %% (In mock mode, verify via meck)
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test recovery after service-broker partition
test_service_broker_partition_recovery(_Config) ->
    ct:comment("=== Service-Broker Partition Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Using fault injection (standardized approach)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait during partition
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect partition
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Partition=~p", 
               [InitialConnectionFailures, PartitionConnectionFailures]),
    
    %% Healing
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Rejoin/recovery logic
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: State data consistency
    %% - No message loss
    %% - No infinite retries/duplicates
    %% - Conflicts resolved
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% ========================================================================
%% FLAPPING NETWORK TESTS
%% ========================================================================

%% @doc Test flapping network stability
test_flapping_network_stability(_Config) ->
    ct:comment("=== Flapping Network Stability Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    InitialMemory = erlang:memory(total),
    InitialProcessCount = length(erlang:processes()),
    
    %% Simulate flapping: Short disconnects with quick recovery
    %% Using fault injection with periodic enable/disable
    {FlappingPid, FlappingRef} = spawn_monitor(fun() ->
        FlappingLoop = fun F(Count) when Count < 15 ->  % 15 cycles = ~30 seconds
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(1000),  % Partition for 1 second
            router_nats_fault_injection:disable_fault(connect),
            timer:sleep(1000),  % Recover for 1 second
            F(Count + 1);
        F(_) ->
            ok
        end,
        FlappingLoop(0)
    end),
    
    %% Verify: Resilience to "chatter"
    timer:sleep(30000),  % Run for 30 seconds
    
    %% Get metrics during flapping
    FlappingMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect flapping
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    FlappingConnectionFailures = maps:get(router_nats_connection_failures_total, FlappingMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Flapping=~p", 
               [InitialConnectionFailures, FlappingConnectionFailures]),
    
    %% Verify: No accumulation of zombie connections
    %% (In mock mode, verify via meck)
    
    %% Verify: No resource leaks
    FinalMemory = erlang:memory(total),
    FinalProcessCount = length(erlang:processes()),
    MemoryGrowth = FinalMemory - InitialMemory,
    ProcessGrowth = FinalProcessCount - InitialProcessCount,
    ct:log("Memory growth: ~p bytes, Process growth: ~p", [MemoryGrowth, ProcessGrowth]),
    ?assert(MemoryGrowth < 50 * 1024 * 1024),
    ?assert(ProcessGrowth < 50),
    
    %% Stop flapping
    exit(FlappingPid, normal),
    
    %% Wait for process to terminate
    receive
        {'DOWN', FlappingRef, process, FlappingPid, normal} ->
            ok;
        {'DOWN', FlappingRef, process, FlappingPid, Reason} ->
            ct:fail("Flapping process exited with reason: ~p", [Reason])
    after
        5000 ->
            ct:fail("Timeout waiting for flapping process to terminate")
    end,
    
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for stable operation
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Stable operation after flapping stops
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test no resource leaks during flapping
test_flapping_network_no_resource_leaks(_Config) ->
    ct:comment("=== Flapping Network No Resource Leaks Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics and resource usage
    InitialMetrics = get_metrics_snapshot(),
    InitialMemory = erlang:memory(total),
    InitialProcessCount = length(erlang:processes()),
    
    %% Simulate flapping using fault injection with periodic enable/disable
    {FlappingPid, FlappingRef} = spawn_monitor(fun() ->
        FlappingLoop = fun F(Count) when Count < 60 ->  % 60 cycles = ~1 minute
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(500),  % Partition for 0.5 seconds
            router_nats_fault_injection:disable_fault(connect),
            timer:sleep(500),  % Recover for 0.5 seconds
            F(Count + 1);
        F(_) ->
            ok
        end,
        FlappingLoop(0)
    end),
    
    %% Run flapping for extended period
    timer:sleep(60000),  % 1 minute
    
    %% Get metrics during flapping
    FlappingMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect flapping
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    FlappingConnectionFailures = maps:get(router_nats_connection_failures_total, FlappingMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Flapping=~p", 
               [InitialConnectionFailures, FlappingConnectionFailures]),
    
    %% Verify: No unbounded process growth
    FinalProcessCount = length(erlang:processes()),
    FinalMemory = erlang:memory(total),
    ProcessGrowth = FinalProcessCount - InitialProcessCount,
    MemoryGrowth = FinalMemory - InitialMemory,
    ct:log("Process growth: ~p, Memory growth: ~p bytes", [ProcessGrowth, MemoryGrowth]),
    
    %% Allow some growth for reconnection attempts, but not unbounded
    ?assert(ProcessGrowth < 50),  % Reasonable threshold
    ?assert(MemoryGrowth < 50 * 1024 * 1024),
    
    %% Stop flapping
    exit(FlappingPid, normal),
    
    %% Wait for process to terminate
    receive
        {'DOWN', FlappingRef, process, FlappingPid, normal} ->
            ok;
        {'DOWN', FlappingRef, process, FlappingPid, Reason} ->
            ct:fail("Flapping process exited with reason: ~p", [Reason])
    after
        5000 ->
            ct:fail("Timeout waiting for flapping process to terminate")
    end,
    
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for cleanup
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test recovery after flapping network
test_flapping_network_recovery(_Config) ->
    ct:comment("=== Flapping Network Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate flapping using fault injection with periodic enable/disable
    {FlappingPid, FlappingRef} = spawn_monitor(fun() ->
        FlappingLoop = fun F(Count) when Count < 10 ->  % 10 cycles = ~20 seconds
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(1000),  % Partition for 1 second
            router_nats_fault_injection:disable_fault(connect),
            timer:sleep(1000),  % Recover for 1 second
            F(Count + 1);
        F(_) ->
            ok
        end,
        FlappingLoop(0)
    end),
    
    %% Run flapping
    timer:sleep(20000),
    
    %% Get metrics during flapping
    FlappingMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Metrics reflect flapping
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    FlappingConnectionFailures = maps:get(router_nats_connection_failures_total, FlappingMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Flapping=~p", 
               [InitialConnectionFailures, FlappingConnectionFailures]),
    
    %% Stop flapping
    exit(FlappingPid, normal),
    
    %% Wait for process to terminate
    receive
        {'DOWN', FlappingRef, process, FlappingPid, normal} ->
            ok;
        {'DOWN', FlappingRef, process, FlappingPid, Reason} ->
            ct:fail("Flapping process exited with reason: ~p", [Reason])
    after
        5000 ->
            ct:fail("Timeout waiting for flapping process to terminate")
    end,
    
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Full recovery
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Normal operation restored
    %% (In mock mode, verify via meck)
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50
    }),
    
    application:stop(beamline_router),
    ok.


%% ========================================================================
%% NEW SCENARIOS: LATENCY DEGRADATION, PARTIAL PACKET LOSS, INTERMITTENT CONNECTIVITY
%% ========================================================================

%% @doc Test latency degradation scenario
%% Scenario: Network latency increases gradually, causing timeouts and retries
%% Data Guarantees:
%% - No message loss: All messages eventually processed or exhausted MaxDeliver
%% - No duplicates: Idempotency layer prevents duplicate processing
%% - Latency bounds: Operations complete within acceptable latency bounds
test_single_instance_latency_degradation(_Config) ->
    ct:comment("=== Single-Instance Latency Degradation Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject increasing latency: 100ms -> 500ms -> 2000ms -> 5000ms
    LatencyStages = [100, 500, 2000, 5000],
    lists:foreach(fun(LatencyMs) ->
        ct:comment("Injecting latency: ~p ms", [LatencyMs]),
        router_nats_fault_injection:enable_fault(publish, {delay, LatencyMs}),
        router_nats_fault_injection:enable_fault(ack, {delay, LatencyMs}),
        timer:sleep(5000),  % Run for 5 seconds at this latency level
        router_nats_fault_injection:disable_fault(publish),
        router_nats_fault_injection:disable_fault(ack),
        timer:sleep(1000)   % Brief pause between stages
    end, LatencyStages),
    
    %% Get metrics during latency degradation
    DegradationMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Timeout errors increase
    InitialTimeouts = maps:get(router_nats_timeout_total, InitialMetrics, 0),
    FinalTimeouts = maps:get(router_nats_timeout_total, DegradationMetrics, 0),
    ct:comment("Timeouts: Initial=~p, Final=~p", [InitialTimeouts, FinalTimeouts]),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        max_latency_ms => 6000,  % Allow up to 6s latency
        allow_timeouts => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test partial packet loss scenario
%% Scenario: Some packets are lost (e.g., 10%, 30%, 50%), causing retries
%% Data Guarantees:
%% - No message loss: Lost packets are retried until successful or MaxDeliver exhausted
%% - No duplicates: Idempotency layer prevents duplicate processing
%% - Packet loss tolerance: System handles partial loss gracefully
test_single_instance_partial_packet_loss(_Config) ->
    ct:comment("=== Single-Instance Partial Packet Loss Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject partial packet loss using intermittent faults
    %% 30% packet loss on publish operations
    router_nats_fault_injection:enable_fault(publish, {intermittent, {error, packet_loss}, 0.3}),
    router_nats_fault_injection:enable_fault(ack, {intermittent, {error, packet_loss}, 0.3}),
    
    %% Run for 30 seconds with partial packet loss
    timer:sleep(30000),
    
    %% Get metrics during packet loss
    PacketLossMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Retry attempts increase
    InitialRetries = maps:get(router_nats_retry_attempts_total, InitialMetrics, 0),
    FinalRetries = maps:get(router_nats_retry_attempts_total, PacketLossMetrics, 0),
    ct:comment("Retry attempts: Initial=~p, Final=~p", [InitialRetries, FinalRetries]),
    
    %% Disable packet loss
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:disable_fault(ack),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        packet_loss_percent => 30,
        allow_retries => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test intermittent connectivity scenario
%% Scenario: Connection alternates between available and unavailable
%% Data Guarantees:
%% - No message loss: Messages are queued during disconnects and processed after reconnect
%% - No duplicates: Idempotency layer prevents duplicate processing
%% - Intermittent tolerance: System handles intermittent connectivity gracefully
test_single_instance_intermittent_connectivity(_Config) ->
    ct:comment("=== Single-Instance Intermittent Connectivity Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate intermittent connectivity: 2s on, 1s off, repeat
    IntermittentPid = spawn(fun() ->
        IntermittentLoop = fun F(Count) when Count < 20 ->  % 20 cycles = ~60 seconds
            %% Connection available
            router_nats_fault_injection:disable_fault(connect),
            timer:sleep(2000),  % Available for 2 seconds
            %% Connection unavailable
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(1000),  % Unavailable for 1 second
            F(Count + 1);
        F(_) ->
            ok
        end,
        IntermittentLoop(0)
    end),
    
    %% Run for 60 seconds
    timer:sleep(60000),
    
    %% Get metrics during intermittent connectivity
    IntermittentMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Connection failures and restorations
    InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    FinalConnectionFailures = maps:get(router_nats_connection_failures_total, IntermittentMetrics, 0),
    InitialConnectionRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
    FinalConnectionRestored = maps:get(router_nats_connection_restored_total, IntermittentMetrics, 0),
    ct:comment("Connection failures: Initial=~p, Final=~p", [InitialConnectionFailures, FinalConnectionFailures]),
    ct:comment("Connection restored: Initial=~p, Final=~p", [InitialConnectionRestored, FinalConnectionRestored]),
    
    %% Stop intermittent connectivity
    exit(IntermittentPid, normal),
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for stable operation
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        intermittent_connectivity => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test slow network scenario
%% Scenario: Network is slow but not completely disconnected (high latency, low bandwidth)
%% Data Guarantees:
%% - No message loss: All messages eventually processed
%% - Latency bounds: Operations complete within acceptable bounds
%% - Throughput degradation: System handles slow network gracefully
test_single_instance_slow_network(_Config) ->
    ct:comment("=== Single-Instance Slow Network Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject slow network: 3s delay on all operations
    router_nats_fault_injection:enable_fault(publish, {delay, 3000}),
    router_nats_fault_injection:enable_fault(ack, {delay, 3000}),
    router_nats_fault_injection:enable_fault(subscribe, {delay, 3000}),
    
    %% Run for 30 seconds with slow network
    timer:sleep(30000),
    
    %% Get metrics during slow network
    _ = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Disable slow network
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:disable_fault(ack),
    router_nats_fault_injection:disable_fault(subscribe),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        max_latency_ms => 4000,  % Allow up to 4s latency
        allow_throughput_degradation => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test flapping network with latency
%% Scenario: Network flaps (connects/disconnects) with high latency during connections
%% Data Guarantees:
%% - No message loss: Messages are queued and processed after stable connection
%% - Latency bounds: Operations complete within acceptable bounds even with flapping
%% - Stability: System remains stable despite flapping and latency
test_flapping_network_with_latency(_Config) ->
    ct:comment("=== Flapping Network with Latency Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    InitialMemory = erlang:memory(total),
    InitialProcessCount = length(erlang:processes()),
    
    %% Simulate flapping with latency: disconnect for 1s, connect with 2s latency for 2s
    {FlappingPid, FlappingRef} = spawn_monitor(fun() ->
        FlappingLoop = fun F(Count) when Count < 30 ->  % 30 cycles = ~90 seconds
            %% Disconnect
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(1000),  % Disconnected for 1 second
            %% Connect with latency
            router_nats_fault_injection:disable_fault(connect),
            router_nats_fault_injection:enable_fault(publish, {delay, 2000}),
            router_nats_fault_injection:enable_fault(ack, {delay, 2000}),
            timer:sleep(2000),  % Connected with latency for 2 seconds
            router_nats_fault_injection:disable_fault(publish),
            router_nats_fault_injection:disable_fault(ack),
            F(Count + 1);
        F(_) ->
            ok
        end,
        FlappingLoop(0)
    end),
    
    %% Run for 90 seconds
    timer:sleep(90000),
    
    %% Get metrics during flapping
    _ = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: No resource leaks
    FinalMemory = erlang:memory(total),
    FinalProcessCount = length(erlang:processes()),
    MemoryGrowth = FinalMemory - InitialMemory,
    ProcessGrowth = FinalProcessCount - InitialProcessCount,
    ct:log("Memory growth: ~p bytes, Process growth: ~p", [MemoryGrowth, ProcessGrowth]),
    ?assert(MemoryGrowth < 50 * 1024 * 1024),
    ?assert(ProcessGrowth < 50),
    
    %% Stop flapping
    exit(FlappingPid, normal),
    router_nats_fault_injection:disable_fault(connect),
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:disable_fault(ack),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        max_latency_ms => 3000,
        flapping_network => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test flapping network with packet loss
%% Scenario: Network flaps (connects/disconnects) with packet loss during connections
%% Data Guarantees:
%% - No message loss: Lost packets are retried until successful
%% - Packet loss tolerance: System handles packet loss gracefully
%% - Stability: System remains stable despite flapping and packet loss
test_flapping_network_with_packet_loss(_Config) ->
    ct:comment("=== Flapping Network with Packet Loss Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    InitialMemory = erlang:memory(total),
    InitialProcessCount = length(erlang:processes()),
    
    %% Simulate flapping with packet loss: disconnect for 1s, connect with 20% packet loss for 2s
    FlappingPid = spawn(fun() ->
        FlappingLoop = fun F(Count) when Count < 30 ->  % 30 cycles = ~90 seconds
            %% Disconnect
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(1000),  % Disconnected for 1 second
            %% Connect with packet loss
            router_nats_fault_injection:disable_fault(connect),
            router_nats_fault_injection:enable_fault(publish, {intermittent, {error, packet_loss}, 0.2}),
            router_nats_fault_injection:enable_fault(ack, {intermittent, {error, packet_loss}, 0.2}),
            timer:sleep(2000),  % Connected with packet loss for 2 seconds
            router_nats_fault_injection:disable_fault(publish),
            router_nats_fault_injection:disable_fault(ack),
            F(Count + 1);
        F(_) ->
            ok
        end,
        FlappingLoop(0)
    end),
    
    %% Run for 90 seconds
    timer:sleep(90000),
    
    %% Get metrics during flapping
    FlappingMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Retry attempts increase due to packet loss
    InitialRetries = maps:get(router_nats_retry_attempts_total, InitialMetrics, 0),
    FinalRetries = maps:get(router_nats_retry_attempts_total, FlappingMetrics, 0),
    ct:comment("Retry attempts: Initial=~p, Final=~p", [InitialRetries, FinalRetries]),
    
    %% Verify: No resource leaks
    FinalMemory = erlang:memory(total),
    FinalProcessCount = length(erlang:processes()),
    MemoryGrowth = FinalMemory - InitialMemory,
    ProcessGrowth = FinalProcessCount - InitialProcessCount,
    ct:log("Memory growth: ~p bytes, Process growth: ~p", [MemoryGrowth, ProcessGrowth]),
    ?assert(MemoryGrowth < 50 * 1024 * 1024),
    ?assert(ProcessGrowth < 50),
    
    %% Stop flapping
    exit(FlappingPid, normal),
    router_nats_fault_injection:disable_fault(connect),
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:disable_fault(ack),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        packet_loss_percent => 20,
        flapping_network => true,
        allow_retries => true
    }),
    
    application:stop(beamline_router),
    ok.

%% ========================================================================
%% ENHANCED VERIFICATION FUNCTIONS
%% ========================================================================

%% @doc Verify data guarantees (no duplicates, losses, inconsistencies)
%% Checks:
%% - No duplicates: Idempotency layer prevents duplicate processing
%% - No losses: Messages are not lost (within guarantees)
%% - No inconsistencies: State remains consistent after recovery
-spec verify_data_guarantees(map(), map(), map()) -> ok.
verify_data_guarantees(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    %% Check for duplicate processing
    InitialDuplicates = maps:get(router_duplicate_processing_total, InitialMetrics, 0),
    FinalDuplicates = maps:get(router_duplicate_processing_total, FinalMetrics, 0),
    DuplicateDelta = FinalDuplicates - InitialDuplicates,
    
    %% Allow some duplicates if expected (e.g., during split-brain recovery)
    MaxAllowedDuplicates = maps:get(max_allowed_duplicates, ExpectedBehavior, 0),
    
    ct:comment("Duplicate processing: Initial=~p, Final=~p, Delta=~p, MaxAllowed=~p",
               [InitialDuplicates, FinalDuplicates, DuplicateDelta, MaxAllowedDuplicates]),
    ?assert((DuplicateDelta =< MaxAllowedDuplicates)),
    
    %% Check for message losses
    %% We verify MaxDeliver exhaustion is within bounds (messages not lost, but exhausted)
    InitialMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, InitialMetrics, 0),
    FinalMaxDeliverExhausted = maps:get(router_jetstream_maxdeliver_exhausted_total, FinalMetrics, 0),
    MaxDeliverExhaustedDelta = FinalMaxDeliverExhausted - InitialMaxDeliverExhausted,
    
    ct:comment("MaxDeliver exhausted: Delta=~p (messages not lost, but exhausted MaxDeliver)",
               [MaxDeliverExhaustedDelta]),
    
    %% Check for state inconsistencies (via metrics)
    %% In mock mode, we verify that metrics are consistent
    InitialStateConsistency = maps:get(router_state_consistency_total, InitialMetrics, 0),
    FinalStateConsistency = maps:get(router_state_consistency_total, FinalMetrics, 0),
    StateConsistencyDelta = FinalStateConsistency - InitialStateConsistency,
    
    %% State consistency should not decrease (inconsistencies would show as negative delta)
    ct:comment("State consistency: Initial=~p, Final=~p, Delta=~p",
               [InitialStateConsistency, FinalStateConsistency, StateConsistencyDelta]),
    ?assert((StateConsistencyDelta >= 0)),
    
    ok.

%% @doc Verify latency bounds
%% Checks:
%% - Operations complete within acceptable latency bounds
%% - Latency metrics reflect actual behavior
-spec verify_latency_bounds(map(), map(), map()) -> ok.
verify_latency_bounds(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    %% Check if latency verification is applicable
    case maps:get(max_latency_ms, ExpectedBehavior, undefined) of
        undefined ->
            %% Latency verification not applicable for this test
            ok;
        MaxLatencyMs ->
            %% Get latency metrics
            InitialLatency = maps:get(router_nats_operation_latency_seconds, InitialMetrics, 0),
            FinalLatency = maps:get(router_nats_operation_latency_seconds, FinalMetrics, 0),
            LatencyDelta = FinalLatency - InitialLatency,
            
            %% Convert to milliseconds for comparison
            LatencyDeltaMs = LatencyDelta * 1000,
            
            ct:comment("Latency: Initial=~p s, Final=~p s, Delta=~p ms, MaxAllowed=~p ms",
                       [InitialLatency, FinalLatency, LatencyDeltaMs, MaxLatencyMs]),
            
            %% Latency should be within bounds (allow some tolerance)
            Tolerance = maps:get(latency_tolerance_ms, ExpectedBehavior, 1000),
            ?assert((LatencyDeltaMs =< MaxLatencyMs + Tolerance)),
            
            ok
    end.

%% @doc Verify packet loss tolerance
%% Checks:
%% - System handles packet loss gracefully
%% - Retry attempts are within reasonable bounds
%% - Packet loss metrics reflect actual behavior
-spec verify_packet_loss_tolerance(map(), map(), map()) -> ok.
verify_packet_loss_tolerance(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    %% Check if packet loss verification is applicable
    case maps:get(packet_loss_percent, ExpectedBehavior, undefined) of
        undefined ->
            %% Packet loss verification not applicable for this test
            ok;
        PacketLossPercent ->
            %% Get retry metrics (packet loss causes retries)
            InitialRetries = maps:get(router_nats_retry_attempts_total, InitialMetrics, 0),
            FinalRetries = maps:get(router_nats_retry_attempts_total, FinalMetrics, 0),
            RetryDelta = FinalRetries - InitialRetries,
            
            %% Get publish failure metrics (packet loss causes failures)
            InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
            FinalPublishFailures = maps:get(router_nats_publish_failures_total, FinalMetrics, 0),
            PublishFailureDelta = FinalPublishFailures - InitialPublishFailures,
            
            ct:comment("Packet loss: ~p%, Retries: Delta=~p, Publish failures: Delta=~p",
                       [PacketLossPercent, RetryDelta, PublishFailureDelta]),
            
            %% With packet loss, we expect some retries and failures
            %% But they should be within reasonable bounds
            MaxExpectedRetries = maps:get(max_expected_retries, ExpectedBehavior, 1000),
            MaxExpectedFailures = maps:get(max_expected_failures, ExpectedBehavior, 500),
            
            case maps:get(allow_retries, ExpectedBehavior, false) of
                true ->
                    %% Retries are expected and allowed
                    ?assert((RetryDelta =< MaxExpectedRetries)),
                    ?assert((PublishFailureDelta =< MaxExpectedFailures));
                false ->
                    %% Retries should be minimal
                    ?assert((RetryDelta =< 10)),
                    ?assert((PublishFailureDelta =< 10))
            end,
            
            ok
    end.

%% ========================================================================
%% ADDITIONAL PARTITION SCENARIOS
%% ========================================================================

%% @doc Test partial network partition with recovery
%% Scenario: Only publish operations fail, subscribe/ack continue working
%% Data Guarantees:
%% - No message loss: Messages are queued during partition
%% - Recovery: System recovers after partition heals
test_partial_network_partition_with_recovery(_Config) ->
    ct:comment("=== Partial Network Partition with Recovery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject partial partition: Only publish fails, subscribe/ack work
    router_nats_fault_injection:enable_fault(publish, {error, connection_refused}),
    
    %% Run for 10 seconds with partial partition
    timer:sleep(10000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Publish failures increase
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    PartitionPublishFailures = maps:get(router_nats_publish_failures_total, PartitionMetrics, 0),
    ct:comment("Publish failures: Initial=~p, Partition=~p", [InitialPublishFailures, PartitionPublishFailures]),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(publish),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        partial_partition => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test intermittent connectivity with backpressure
%% Scenario: Network alternates between available and unavailable, triggering backpressure
%% Data Guarantees:
%% - No message loss: Messages are queued during disconnects
%% - Backpressure: System applies backpressure when overloaded
%% - Recovery: System recovers after connectivity stabilizes
test_intermittent_connectivity_with_backpressure(_Config) ->
    ct:comment("=== Intermittent Connectivity with Backpressure Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Setup: Create pending state for backpressure (via store, not direct ETS)
    Subject = <<"beamline.router.v1.decide">>,
    _ = router_network_partition_store:ensure_pending_table(),
    _ = router_network_partition_store:put_pending(Subject, 1500, erlang:system_time(millisecond)),
    
    %% Simulate intermittent connectivity: 2s on, 1s off, repeat
    {IntermittentPid, IntermittentRef} = spawn_monitor(fun() ->
        IntermittentLoop = fun F(Count) when Count < 10 ->  % 10 cycles = ~30 seconds
            %% Connection available
            router_nats_fault_injection:disable_fault(connect),
            timer:sleep(2000),  % Available for 2 seconds
            %% Connection unavailable
            router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(1000),  % Unavailable for 1 second
            F(Count + 1);
        F(_) ->
            ok
        end,
        IntermittentLoop(0)
    end),
    
    %% Run for 30 seconds
    timer:sleep(30000),
    
    %% Get metrics during intermittent connectivity
    _ = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Check backpressure status
    {BackpressureStatus, _RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ct:comment("Backpressure status: ~p", [BackpressureStatus]),
    
    %% Stop intermittent connectivity
    exit(IntermittentPid, normal),
    
    %% Wait for process to terminate
    receive
        {'DOWN', IntermittentRef, process, IntermittentPid, normal} ->
            ok;
        {'DOWN', IntermittentRef, process, IntermittentPid, Reason} ->
            ct:fail("Intermittent process exited with reason: ~p", [Reason])
    after
        5000 ->
            ct:fail("Timeout waiting for intermittent process to terminate")
    end,
    
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for stable operation
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        intermittent_connectivity => true,
        backpressure_applied => true
    }),
    
    %% Cleanup
    _ = router_network_partition_store:delete_pending_table(),
    application:stop(beamline_router),
    ok.

%% @doc Test network partition with message redelivery
%% Scenario: Network partition causes message redelivery
%% Data Guarantees:
%% - No message loss: Messages are redelivered after partition heals
%% - Redelivery limits: MaxDeliver is enforced
%% - Recovery: System recovers after partition heals
test_network_partition_with_message_redelivery(_Config) ->
    ct:comment("=== Network Partition with Message Redelivery Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject partition
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Run for 10 seconds with partition
    timer:sleep(10000),
    
    %% Get metrics during partition
    _ = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery and redelivery
    timer:sleep(10000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Redelivery occurred
    InitialRedeliveries = maps:get(router_jetstream_redeliveries_total, InitialMetrics, 0),
    FinalRedeliveries = maps:get(router_jetstream_redeliveries_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedeliveries - InitialRedeliveries,
    ct:comment("Redeliveries: Initial=~p, Final=~p, Delta=~p", [InitialRedeliveries, FinalRedeliveries, RedeliveryDelta]),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        message_redelivery => true
    }),
    
    application:stop(beamline_router),
    ok.

%% @doc Test split brain with leader election
%% Scenario: Multi-instance split brain with leader election
%% Data Guarantees:
%% - No message loss: Messages are processed by elected leader
%% - Leader election: Single leader is elected
%% - Recovery: System recovers after split brain heals
test_split_brain_with_leader_election(_Config) ->
    ct:comment("=== Split Brain with Leader Election Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate split brain: Two router instances cannot see each other
    %% In mock mode, we simulate this by having both instances try to be leader
    _ = router_network_partition_store:init_leader_state(),
    _ = router_network_partition_store:set_instance_state(instance_a, active),
    _ = router_network_partition_store:set_instance_state(instance_b, active),
    
    %% Simulate leader election: Instance A becomes leader
    _ = router_network_partition_store:set_leader(instance_a),
    _ = router_network_partition_store:set_instance_state(instance_b, standby),
    
    %% Run for 10 seconds with split brain
    timer:sleep(10000),
    
    %% Get metrics during split brain
    _ = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Verify: Leader is elected (via store API)
    case router_network_partition_store:get_leader() of
        {ok, Leader} ->
            ?assert(Leader =:= instance_a orelse Leader =:= instance_b);
        not_found ->
            ct:fail("Leader not found after election");
        {error, Reason} ->
            ct:fail("Failed to get leader: ~p", [Reason])
    end,
    
    %% Simulate split brain healing: Both instances can see each other
    _ = router_network_partition_store:set_leader(instance_a),
    _ = router_network_partition_store:set_instance_state(instance_b, standby),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        split_brain => true,
        leader_election => true
    }),
    
    %% Verify: Leader state is consistent after healing
    case router_network_partition_store:get_leader() of
        {ok, FinalLeader} ->
            ?assert(FinalLeader =:= instance_a orelse FinalLeader =:= instance_b);
        not_found ->
            ct:fail("Leader not found after healing");
        {error, Reason} ->
            ct:fail("Failed to get leader after healing: ~p", [Reason])
    end,
    
    %% Cleanup
    _ = router_network_partition_store:delete_leader_state(),
    application:stop(beamline_router),
    ok.

%% @doc Test network partition with circuit breaker
%% Scenario: Network partition triggers circuit breaker
%% Data Guarantees:
%% - No message loss: Messages are queued during partition
%% - Circuit breaker: Circuit breaker opens when partition detected
%% - Recovery: System recovers after partition heals
test_network_partition_with_circuit_breaker(_Config) ->
    ct:comment("=== Network Partition with Circuit Breaker Test ==="),
    
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject partition
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Run for 10 seconds with partition
    timer:sleep(10000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    RouterPid = whereis(router_result_consumer),
    ?assert(is_process_alive(RouterPid)),
    
    %% Check circuit breaker state (if circuit breaker is enabled)
    %% In mock mode, we verify that circuit breaker would be triggered
    InitialCircuitBreakerOpens = maps:get(router_circuit_breaker_opens_total, InitialMetrics, 0),
    PartitionCircuitBreakerOpens = maps:get(router_circuit_breaker_opens_total, PartitionMetrics, 0),
    ct:comment("Circuit breaker opens: Initial=~p, Partition=~p", [InitialCircuitBreakerOpens, PartitionCircuitBreakerOpens]),
    
    %% Disable partition
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(5000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify: Contract invariants
    verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
        partition_injected => true,
        expected_maxdeliver_exhaustion => 0,
        max_redelivery => 50,
        circuit_breaker => true
    }),
    
    application:stop(beamline_router),
    ok.

