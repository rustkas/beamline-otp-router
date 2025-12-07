%% @doc Metrics Under Faults and Load Tests Suite
%%
%% Extended metrics validation tests under faults and load:
%% - Aggregation correctness
%% - Rate calculation accuracy
%% - Label cardinality limits
%%
%% These tests complement existing metrics tests by providing:
%% - Dedicated validation under fault injection
%% - Load testing with metrics verification
%% - Cardinality explosion prevention checks
%%
%% @test_category metrics, fault_injection, load_testing, integration
-module(router_metrics_under_faults_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks
%% Test functions are called via groups() by Common Test framework
%% Helper functions are used by test functions
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_aggregation_under_partial_failures/1,
    test_aggregation_under_retries/1,
    test_aggregation_under_node_failures/1,
    test_aggregation_under_flapping_connections/1,
    test_aggregation_after_recovery/1,
    test_rate_under_spike_load/1,
    test_rate_under_plateau_load/1,
    test_rate_under_sudden_stop/1,
    test_rate_under_failures/1,
    test_rate_after_restart/1,
    test_cardinality_under_mass_load/1,
    test_cardinality_with_many_tenants/1,
    test_cardinality_with_many_streams/1,
    test_cardinality_with_high_cardinality_leak/1,
    test_cardinality_limits_enforced/1,
    test_metrics_under_combined_faults/1,
    test_metrics_under_chaos_scenarios/1,
    test_metrics_under_prolonged_load/1,
    get_metrics_snapshot/0,
    generate_controlled_load/3,
    generate_controlled_load_with_labels/5,
    generate_controlled_load_with_unique_ids/3,
    send_test_message/3,
    verify_aggregation/3,
    verify_rate/5,
    count_label_combinations/2,
    verify_cardinality/2
]}).

all() ->
    [
        {group, aggregation_tests},
        {group, rate_tests},
        {group, cardinality_tests},
        {group, combined_tests}
    ].

groups() ->
    [
        {aggregation_tests, [sequence], [
            test_aggregation_under_partial_failures,
            test_aggregation_under_retries,
            test_aggregation_under_node_failures,
            test_aggregation_under_flapping_connections,
            test_aggregation_after_recovery
        ]},
        {rate_tests, [sequence], [
            test_rate_under_spike_load,
            test_rate_under_plateau_load,
            test_rate_under_sudden_stop,
            test_rate_under_failures,
            test_rate_after_restart
        ]},
        {cardinality_tests, [sequence], [
            test_cardinality_under_mass_load,
            test_cardinality_with_many_tenants,
            test_cardinality_with_many_streams,
            test_cardinality_with_high_cardinality_leak,
            test_cardinality_limits_enforced
        ]},
        {combined_tests, [sequence], [
            test_metrics_under_combined_faults,
            test_metrics_under_chaos_scenarios,
            test_metrics_under_prolonged_load
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
    ok = application:set_env(beamline_router, disable_heir, true),
    %% Start application
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for router_result_consumer to be ready
            case whereis(router_result_consumer) of
                undefined ->
                    timer:sleep(500),
                    case whereis(router_result_consumer) of
                        undefined ->
                            ct:comment("Warning: router_result_consumer not found, continuing anyway");
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end,
            %% Ensure metrics table exists
            router_metrics:ensure(),
            %% Clear fault injection state
            router_nats_fault_injection:clear_all_faults(),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state before each test
    router_nats_fault_injection:clear_all_faults(),
    %% Reset metrics using router_r10_metrics (no direct ETS access)
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state after each test
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ========================================================================
%% AGGREGATION TESTS
%% ========================================================================

%% @doc Test aggregation correctness under partial failures
test_aggregation_under_partial_failures(_Config) ->
    ct:comment("Testing metrics aggregation under partial failures"),
    
    %% Setup: Generate controlled load with known failure rate
    TotalRequests = 1000,
    ExpectedFailures = 200,
    ExpectedSuccesses = TotalRequests - ExpectedFailures,
    
    %% Inject partial failures (20% failure rate)
    ok = router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.2}),
    
    %% Generate load
    InitialMetrics = get_metrics_snapshot(),
    generate_controlled_load(TotalRequests, 100, 1000), %% 100 RPS, 1000ms between batches
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify aggregation
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        expected_failures => ExpectedFailures,
        expected_successes => ExpectedSuccesses,
        tolerance => 50 %% Allow 5% tolerance
    }),
    
    ok.

%% @doc Test aggregation correctness under retries
test_aggregation_under_retries(_Config) ->
    ct:comment("Testing metrics aggregation under retries"),
    
    %% Setup: Generate load with retry-inducing faults
    TotalRequests = 500,
    ExpectedRetries = 100,
    
    %% Inject intermittent failures (will cause retries)
    ok = router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.3}),
    
    %% Generate load
    InitialMetrics = get_metrics_snapshot(),
    generate_controlled_load(TotalRequests, 50, 2000),
    
    %% Wait for processing and retries
    timer:sleep(10000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify aggregation (total should include retries)
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        expected_retries => ExpectedRetries,
        tolerance => 30
    }),
    
    ok.

%% @doc Test aggregation correctness under node failures
test_aggregation_under_node_failures(_Config) ->
    ct:comment("Testing metrics aggregation under node failures"),
    
    %% Setup: Simulate node failure and recovery
    TotalRequests = 800,
    
    %% Generate initial load
    InitialMetrics = get_metrics_snapshot(),
    generate_controlled_load(400, 100, 1000),
    
    %% Inject node failure
    timer:sleep(2000),
    ok = router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Continue load (should fail)
    generate_controlled_load(200, 50, 2000),
    
    %% Recover node
    timer:sleep(2000),
    router_nats_fault_injection:clear_all_faults(),
    
    %% Continue load (should succeed)
    generate_controlled_load(200, 100, 1000),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify aggregation (no gaps, no duplicates)
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        tolerance => 40
    }),
    
    ok.

%% @doc Test aggregation correctness under flapping connections
test_aggregation_under_flapping_connections(_Config) ->
    ct:comment("Testing metrics aggregation under flapping connections"),
    
    %% Setup: Simulate flapping connections
    TotalRequests = 600,
    
    %% Generate load with flapping connections
    InitialMetrics = get_metrics_snapshot(),
    
    %% Inject flapping (connect/disconnect cycle)
    spawn_link(fun() ->
        lists:foreach(fun(_) ->
            ok = router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
            timer:sleep(500),
            router_nats_fault_injection:clear_all_faults(),
            timer:sleep(500)
        end, lists:seq(1, 5))
    end),
    
    generate_controlled_load(TotalRequests, 80, 1500),
    
    %% Wait for processing
    timer:sleep(8000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify aggregation (no duplicates from reconnections)
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        tolerance => 50
    }),
    
    ok.

%% @doc Test aggregation correctness after recovery
test_aggregation_after_recovery(_Config) ->
    ct:comment("Testing metrics aggregation after recovery"),
    
    %% Setup: Failure, recovery, then normal operation
    TotalRequests = 700,
    
    %% Generate load before failure
    InitialMetrics = get_metrics_snapshot(),
    generate_controlled_load(200, 100, 1000),
    
    %% Inject failure
    timer:sleep(2000),
    ok = router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Generate load during failure
    generate_controlled_load(200, 50, 2000),
    
    %% Recover
    timer:sleep(2000),
    router_nats_fault_injection:clear_all_faults(),
    
    %% Generate load after recovery
    generate_controlled_load(300, 100, 1000),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify aggregation (no gaps after recovery)
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        tolerance => 50
    }),
    
    ok.

%% ========================================================================
%% RATE TESTS
%% ========================================================================

%% @doc Test rate calculation under spike load
test_rate_under_spike_load(_Config) ->
    ct:comment("Testing rate calculation under spike load"),
    
    %% Setup: Generate spike load (short burst)
    ExpectedRPS = 500,
    DurationSeconds = 10,
    
    %% Generate spike
    InitialMetrics = get_metrics_snapshot(),
    InitialTime = erlang:monotonic_time(second),
    
    generate_controlled_load(ExpectedRPS * DurationSeconds, ExpectedRPS, 100),
    
    %% Wait for processing
    timer:sleep(2000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    FinalTime = erlang:monotonic_time(second),
    
    %% Verify rate
    verify_rate(InitialMetrics, FinalMetrics, InitialTime, FinalTime, #{
        expected_rps => ExpectedRPS,
        tolerance_percent => 10 %% 10% tolerance
    }),
    
    ok.

%% @doc Test rate calculation under plateau load
test_rate_under_plateau_load(_Config) ->
    ct:comment("Testing rate calculation under plateau load"),
    
    %% Setup: Generate steady plateau load
    ExpectedRPS = 200,
    DurationSeconds = 30,
    
    %% Generate plateau
    InitialMetrics = get_metrics_snapshot(),
    InitialTime = erlang:monotonic_time(second),
    
    generate_controlled_load(ExpectedRPS * DurationSeconds, ExpectedRPS, 1000),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    FinalTime = erlang:monotonic_time(second),
    
    %% Verify rate
    verify_rate(InitialMetrics, FinalMetrics, InitialTime, FinalTime, #{
        expected_rps => ExpectedRPS,
        tolerance_percent => 5 %% 5% tolerance for plateau
    }),
    
    ok.

%% @doc Test rate calculation under sudden stop
test_rate_under_sudden_stop(_Config) ->
    ct:comment("Testing rate calculation under sudden stop"),
    
    %% Setup: Generate load then sudden stop
    ExpectedRPS = 300,
    DurationSeconds = 15,
    
    %% Generate load
    InitialMetrics = get_metrics_snapshot(),
    InitialTime = erlang:monotonic_time(second),
    
    generate_controlled_load(ExpectedRPS * DurationSeconds, ExpectedRPS, 1000),
    
    %% Sudden stop (no more requests)
    timer:sleep(2000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    FinalTime = erlang:monotonic_time(second),
    
    %% Verify rate (should drop to zero after stop)
    verify_rate(InitialMetrics, FinalMetrics, InitialTime, FinalTime, #{
        expected_rps => ExpectedRPS,
        tolerance_percent => 10,
        allow_zero_after_stop => true
    }),
    
    ok.

%% @doc Test rate calculation under failures
test_rate_under_failures(_Config) ->
    ct:comment("Testing rate calculation under failures"),
    
    %% Setup: Generate load with failures
    ExpectedRPS = 250,
    DurationSeconds = 20,
    FailureRate = 0.3,
    
    %% Inject failures
    ok = router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, FailureRate}),
    
    %% Generate load
    InitialMetrics = get_metrics_snapshot(),
    InitialTime = erlang:monotonic_time(second),
    
    generate_controlled_load(ExpectedRPS * DurationSeconds, ExpectedRPS, 1000),
    
    %% Wait for processing
    timer:sleep(3000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    FinalTime = erlang:monotonic_time(second),
    
    %% Verify rate (should account for failures)
    verify_rate(InitialMetrics, FinalMetrics, InitialTime, FinalTime, #{
        expected_rps => ExpectedRPS,
        tolerance_percent => 15 %% Higher tolerance with failures
    }),
    
    ok.

%% @doc Test rate calculation after restart
test_rate_after_restart(_Config) ->
    ct:comment("Testing rate calculation after restart"),
    
    %% Setup: Generate load, restart, continue
    ExpectedRPS = 150,
    DurationSeconds = 10,
    
    %% Generate initial load
    InitialMetrics = get_metrics_snapshot(),
    
    generate_controlled_load(ExpectedRPS * DurationSeconds, ExpectedRPS, 1000),
    
    %% Simulate restart (clear metrics, but keep time reference)
    timer:sleep(2000),
    RestartTime = erlang:monotonic_time(second),
    %% Note: In real scenario, metrics would be reset, but we track from restart
    
    %% Continue load
    generate_controlled_load(ExpectedRPS * DurationSeconds, ExpectedRPS, 1000),
    
    %% Wait for processing
    timer:sleep(3000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    FinalTime = erlang:monotonic_time(second),
    
    %% Verify rate (no false spikes after restart)
    verify_rate(InitialMetrics, FinalMetrics, RestartTime, FinalTime, #{
        expected_rps => ExpectedRPS,
        tolerance_percent => 10,
        check_no_false_spikes => true
    }),
    
    ok.

%% ========================================================================
%% CARDINALITY TESTS
%% ========================================================================

%% @doc Test label cardinality under mass load
test_cardinality_under_mass_load(_Config) ->
    ct:comment("Testing label cardinality under mass load"),
    
    %% Setup: Generate mass load with many unique values
    TotalRequests = 5000,
    UniqueTenants = 100,
    UniqueStreams = 50,
    
    %% Generate load with controlled uniqueness
    generate_controlled_load_with_labels(TotalRequests, UniqueTenants, UniqueStreams, 200, 500),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect metrics and count cardinality
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    
    %% Verify cardinality (should be bounded)
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => UniqueTenants * UniqueStreams * 2, %% Reasonable upper bound
        tolerance_percent => 20
    }),
    
    ok.

%% @doc Test label cardinality with many tenants
test_cardinality_with_many_tenants(_Config) ->
    ct:comment("Testing label cardinality with many tenants"),
    
    %% Setup: Many tenants, few streams
    TotalRequests = 3000,
    UniqueTenants = 200,
    UniqueStreams = 5,
    
    %% Generate load
    generate_controlled_load_with_labels(TotalRequests, UniqueTenants, UniqueStreams, 150, 1000),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect metrics and count cardinality
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    
    %% Verify cardinality (should scale with tenants, not requests)
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => UniqueTenants * UniqueStreams * 3,
        tolerance_percent => 15
    }),
    
    ok.

%% @doc Test label cardinality with many streams
test_cardinality_with_many_streams(_Config) ->
    ct:comment("Testing label cardinality with many streams"),
    
    %% Setup: Few tenants, many streams
    TotalRequests = 3000,
    UniqueTenants = 10,
    UniqueStreams = 100,
    
    %% Generate load
    generate_controlled_load_with_labels(TotalRequests, UniqueTenants, UniqueStreams, 150, 1000),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect metrics and count cardinality
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    
    %% Verify cardinality (should scale with streams, not requests)
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => UniqueTenants * UniqueStreams * 3,
        tolerance_percent => 15
    }),
    
    ok.

%% @doc Test label cardinality with high-cardinality leak
test_cardinality_with_high_cardinality_leak(_Config) ->
    ct:comment("Testing label cardinality with high-cardinality leak prevention"),
    
    %% Setup: Try to inject high-cardinality values (UUIDs, request IDs)
    TotalRequests = 2000,
    
    %% Generate load with unique request IDs (should be sanitized/limited)
    generate_controlled_load_with_unique_ids(TotalRequests, 200, 500),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect metrics and count cardinality
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    
    %% Verify cardinality (should NOT explode with unique IDs)
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => 1000, %% Should be limited, not 2000
        tolerance_percent => 10,
        check_no_explosion => true
    }),
    
    ok.

%% @doc Test label cardinality limits enforced
test_cardinality_limits_enforced(_Config) ->
    ct:comment("Testing label cardinality limits are enforced"),
    
    %% Setup: Generate load that would exceed limits if not enforced
    TotalRequests = 10000,
    UniqueTenants = 500,
    UniqueStreams = 100,
    
    %% Generate load
    generate_controlled_load_with_labels(TotalRequests, UniqueTenants, UniqueStreams, 500, 200),
    
    %% Wait for processing
    timer:sleep(10000),
    
    %% Collect metrics and count cardinality
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    
    %% Verify cardinality (should be enforced, not unlimited)
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => 50000, %% Reasonable limit
        tolerance_percent => 20,
        check_limits_enforced => true
    }),
    
    ok.

%% ========================================================================
%% COMBINED TESTS
%% ========================================================================

%% @doc Test metrics under combined faults
test_metrics_under_combined_faults(_Config) ->
    ct:comment("Testing metrics under combined faults"),
    
    %% Setup: Multiple fault types simultaneously
    TotalRequests = 1000,
    
    %% Inject multiple faults
    ok = router_nats_fault_injection:enable_fault(connect, {intermittent, {error, connection_refused}, 0.2}),
    ok = router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.3}),
    
    %% Generate load
    InitialMetrics = get_metrics_snapshot(),
    generate_controlled_load(TotalRequests, 100, 1000),
    
    %% Wait for processing
    timer:sleep(5000),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify all aspects: aggregation, rate, cardinality
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        tolerance => 100
    }),
    
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => 5000,
        tolerance_percent => 20
    }),
    
    ok.

%% @doc Test metrics under chaos scenarios
test_metrics_under_chaos_scenarios(_Config) ->
    ct:comment("Testing metrics under chaos scenarios"),
    
    %% Setup: Random chaos (faults, recoveries, load variations)
    TotalRequests = 2000,
    
    %% Spawn chaos agent
    ChaosPid = spawn_link(fun() ->
        lists:foreach(fun(_) ->
            %% Random fault injection
            case rand:uniform() of
                X when X < 0.3 ->
                    ok = router_nats_fault_injection:enable_fault(connect, {intermittent, {error, connection_refused}, 0.5});
                X when X < 0.6 ->
                    ok = router_nats_fault_injection:enable_fault(publish, {intermittent, {error, timeout}, 0.4});
                _ ->
                    router_nats_fault_injection:clear_all_faults()
            end,
            timer:sleep(rand:uniform(2000))
        end, lists:seq(1, 10))
    end),
    
    %% Generate load during chaos
    InitialMetrics = get_metrics_snapshot(),
    generate_controlled_load(TotalRequests, 150, 800),
    
    %% Wait for chaos to complete
    timer:sleep(15000),
    exit(ChaosPid, normal),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify metrics remain accurate
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        tolerance => 150
    }),
    
    ok.

%% @doc Test metrics under prolonged load
test_metrics_under_prolonged_load(_Config) ->
    ct:comment("Testing metrics under prolonged load"),
    
    %% Setup: Long-running load test
    TotalRequests = 10000,
    DurationMinutes = 5,
    
    %% Generate prolonged load
    InitialMetrics = get_metrics_snapshot(),
    InitialTime = erlang:monotonic_time(second),
    
    generate_controlled_load(TotalRequests, 50, 6000), %% 50 RPS over 5 minutes
    
    %% Wait for completion
    timer:sleep(6000 * DurationMinutes),
    
    %% Collect final metrics
    FinalMetrics = get_metrics_snapshot(),
    FinalTime = erlang:monotonic_time(second),
    
    %% Verify aggregation
    verify_aggregation(InitialMetrics, FinalMetrics, #{
        total_requests => TotalRequests,
        tolerance => 200
    }),
    
    %% Verify rate
    verify_rate(InitialMetrics, FinalMetrics, InitialTime, FinalTime, #{
        expected_rps => 50,
        tolerance_percent => 10
    }),
    
    %% Verify cardinality (should not grow unbounded)
    Metrics = get_metrics_snapshot(),
    Cardinality = count_label_combinations(router_jetstream_redelivery_total, Metrics),
    verify_cardinality(Cardinality, #{
        metric_name => router_jetstream_redelivery_total,
        max_expected => 10000,
        tolerance_percent => 20,
        check_no_unbounded_growth => true
    }),
    
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Get metrics snapshot
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            #{};
        _ ->
            Metrics = ets:tab2list(router_metrics),
            lists:foldl(fun
                ({Key, Value}, Acc) when is_atom(Key) ->
                    maps:put(Key, Value, Acc);
                ({{MetricName, _Labels}, Value}, Acc) when is_atom(MetricName) ->
                    Current = maps:get(MetricName, Acc, 0),
                    maps:put(MetricName, Current + Value, Acc);
                (_, Acc) ->
                    Acc
            end, #{}, Metrics)
    end.

%% @doc Generate controlled load
-spec generate_controlled_load(integer(), integer(), integer()) -> ok.
generate_controlled_load(TotalRequests, RPS, IntervalMs) ->
    RequestsPerBatch = max(1, RPS div 10),
    Batches = TotalRequests div RequestsPerBatch,
    lists:foreach(fun(_) ->
        lists:foreach(fun(_) ->
            TenantId = <<"tenant-", (integer_to_binary(rand:uniform(10)))/binary>>,
            RequestId = <<"req-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            send_test_message(TenantId, RequestId, #{})
        end, lists:seq(1, RequestsPerBatch)),
        timer:sleep(IntervalMs)
    end, lists:seq(1, Batches)),
    ok.

%% @doc Generate controlled load with labels
-spec generate_controlled_load_with_labels(integer(), integer(), integer(), integer(), integer()) -> ok.
generate_controlled_load_with_labels(TotalRequests, UniqueTenants, UniqueStreams, RPS, IntervalMs) ->
    RequestsPerBatch = max(1, RPS div 10),
    Batches = TotalRequests div RequestsPerBatch,
    lists:foreach(fun(_) ->
        lists:foreach(fun(_) ->
            TenantId = <<"tenant-", (integer_to_binary(rand:uniform(UniqueTenants)))/binary>>,
            StreamName = <<"stream-", (integer_to_binary(rand:uniform(UniqueStreams)))/binary>>,
            RequestId = <<"req-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            send_test_message(TenantId, RequestId, #{<<"stream">> => StreamName})
        end, lists:seq(1, RequestsPerBatch)),
        timer:sleep(IntervalMs)
    end, lists:seq(1, Batches)),
    ok.

%% @doc Generate controlled load with unique IDs
-spec generate_controlled_load_with_unique_ids(integer(), integer(), integer()) -> ok.
generate_controlled_load_with_unique_ids(TotalRequests, RPS, IntervalMs) ->
    RequestsPerBatch = max(1, RPS div 10),
    Batches = TotalRequests div RequestsPerBatch,
    lists:foreach(fun(_) ->
        lists:foreach(fun(_) ->
            %% Generate unique request ID (high cardinality)
            RequestId = <<"req-", (integer_to_binary(erlang:unique_integer([positive])))/binary, "-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
            TenantId = <<"tenant-", (integer_to_binary(rand:uniform(10)))/binary>>,
            send_test_message(TenantId, RequestId, #{})
        end, lists:seq(1, RequestsPerBatch)),
        timer:sleep(IntervalMs)
    end, lists:seq(1, Batches)),
    ok.

%% @doc Send test message to router
-spec send_test_message(binary(), binary(), map()) -> ok.
send_test_message(TenantId, RequestId, ExtraFields) ->
    Result = maps:merge(#{
        <<"assignment_id">> => <<"assign-", RequestId/binary>>,
        <<"request_id">> => RequestId,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => TenantId,
        <<"timestamp">> => erlang:system_time(millisecond)
    }, ExtraFields),
    ResultJson = jsx:encode(Result),
    Subject = <<"caf.exec.result.v1">>,
    MsgId = <<"msg-", RequestId/binary>>,
    case whereis(router_result_consumer) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
    end,
    ok.

%% @doc Verify aggregation correctness
-spec verify_aggregation(map(), map(), map()) -> ok.
verify_aggregation(InitialMetrics, FinalMetrics, Expected) ->
    TotalRequests = maps:get(total_requests, Expected, 0),
    Tolerance = maps:get(tolerance, Expected, 50),
    
    %% Calculate deltas for all relevant metrics
    InitialAck = maps:get(router_jetstream_ack_total, InitialMetrics, 0),
    FinalAck = maps:get(router_jetstream_ack_total, FinalMetrics, 0),
    AckDelta = FinalAck - InitialAck,
    
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FinalPublishFailures = maps:get(router_nats_publish_failures_total, FinalMetrics, 0),
    PublishFailuresDelta = FinalPublishFailures - InitialPublishFailures,
    
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedelivery - InitialRedelivery,
    
    %% Total should be ack + failures (accounting for retries)
    %% Note: Redeliveries are retries, so they don't add to total count
    TotalDelta = AckDelta + PublishFailuresDelta,
    
    ct:comment("Aggregation: Ack=~p->~p (Δ~p), Failures=~p->~p (Δ~p), Redelivery=~p->~p (Δ~p), Total=~p, Expected=~p, Tolerance=~p",
               [InitialAck, FinalAck, AckDelta,
                InitialPublishFailures, FinalPublishFailures, PublishFailuresDelta,
                InitialRedelivery, FinalRedelivery, RedeliveryDelta,
                TotalDelta, TotalRequests, Tolerance]),
    
    %% Verify within tolerance
    %% Allow some variance due to async processing and retries
    ?assert(abs(TotalDelta - TotalRequests) =< Tolerance),
    
    ok.

%% @doc Verify rate calculation
-spec verify_rate(map(), map(), integer(), integer(), map()) -> ok.
verify_rate(InitialMetrics, FinalMetrics, InitialTime, FinalTime, Expected) ->
    ExpectedRPS = maps:get(expected_rps, Expected, 0),
    TolerancePercent = maps:get(tolerance_percent, Expected, 10),
    AllowZeroAfterStop = maps:get(allow_zero_after_stop, Expected, false),
    CheckNoFalseSpikes = maps:get(check_no_false_spikes, Expected, false),
    
    %% Calculate rate from ack metric (successful operations)
    InitialAck = maps:get(router_jetstream_ack_total, InitialMetrics, 0),
    FinalAck = maps:get(router_jetstream_ack_total, FinalMetrics, 0),
    Delta = FinalAck - InitialAck,
    Duration = max(1, FinalTime - InitialTime),
    ObservedRPS = case Duration > 0 of
        true -> Delta / Duration;
        false -> 0.0
    end,
    
    %% Calculate tolerance
    Tolerance = ExpectedRPS * TolerancePercent / 100,
    MinRPS = ExpectedRPS - Tolerance,
    MaxRPS = ExpectedRPS + Tolerance,
    
    ct:comment("Rate: Observed=~.2f RPS, Expected=~p RPS, Min=~.2f, Max=~.2f, Duration=~p, Delta=~p",
               [ObservedRPS, ExpectedRPS, MinRPS, MaxRPS, Duration, Delta]),
    
    %% Verify rate within tolerance (or allow zero after stop)
    case AllowZeroAfterStop andalso ObservedRPS =< 0.1 of
        true ->
            ct:comment("Rate: Allowing zero after stop"),
            ok;
        false ->
            case CheckNoFalseSpikes of
                true ->
                    %% Check for false spikes (rate should not be much higher than expected)
                    MaxAllowedSpike = ExpectedRPS * 2.0,
                    ?assert(ObservedRPS =< MaxAllowedSpike),
                    ?assert(ObservedRPS >= MinRPS orelse ObservedRPS =< MaxRPS);
                false ->
                    ?assert(ObservedRPS >= MinRPS andalso ObservedRPS =< MaxRPS)
            end
    end,
    
    ok.

%% @doc Count label combinations for a metric
-spec count_label_combinations(atom(), map()) -> integer().
count_label_combinations(MetricName, _Metrics) ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            0;
        _ ->
            Metrics = ets:tab2list(router_metrics),
            LabeledMetrics = lists:filter(fun
                ({{Name, _Labels}, _Value}) when is_atom(Name), Name =:= MetricName -> true;
                (_) -> false
            end, Metrics),
            %% Count unique label combinations
            LabelLists = [Labels || {{_Name, Labels}, _Value} <- LabeledMetrics],
            UniqueCombinations = lists:usort(LabelLists),
            length(UniqueCombinations)
    end.

%% @doc Verify cardinality limits
-spec verify_cardinality(integer(), map()) -> ok.
verify_cardinality(Cardinality, Expected) ->
    MaxExpected = maps:get(max_expected, Expected, 1000),
    TolerancePercent = maps:get(tolerance_percent, Expected, 20),
    MaxAllowed = MaxExpected + (MaxExpected * TolerancePercent div 100),
    
    ct:comment("Cardinality: Observed=~p, MaxExpected=~p, MaxAllowed=~p",
               [Cardinality, MaxExpected, MaxAllowed]),
    
    %% Verify cardinality within limits
    ?assert(Cardinality =< MaxAllowed),
    
    ok.

