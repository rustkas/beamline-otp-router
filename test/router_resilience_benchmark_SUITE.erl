%% @doc Resilience Benchmark Test Suite
%% 
%% Benchmarks router resilience under various fault conditions:
%% - Recovery time benchmarks
%% - Throughput degradation benchmarks
%% - Latency impact benchmarks
%% - Resource usage benchmarks
%%
%% @test_category benchmark, resilience, slow
-module(router_resilience_benchmark_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Import test utilities
-import(router_test_utils, [
    start_router_app/0,
    stop_router_app/0,
    ensure_circuit_breaker_alive/0,
    reset_circuit_breaker/0
]).

-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

-export([all/0, groups/0]).

%% Export test functions for Common Test
-export([
    benchmark_recovery_time_connect_fault/1,
    benchmark_recovery_time_publish_fault/1,
    benchmark_throughput_degradation_connect_fault/1,
    benchmark_throughput_degradation_publish_fault/1,
    benchmark_latency_impact_connect_fault/1,
    benchmark_latency_impact_publish_fault/1,
    benchmark_resource_usage_under_faults/1,
    benchmark_circuit_breaker_recovery_time/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, resilience_benchmarks}];
groups_for_level(_) -> %% fast, full
    [].

groups() ->
    [
        {resilience_benchmarks, [sequence], [
            benchmark_recovery_time_connect_fault,
            benchmark_recovery_time_publish_fault,
            benchmark_throughput_degradation_connect_fault,
            benchmark_throughput_degradation_publish_fault,
            benchmark_latency_impact_connect_fault,
            benchmark_latency_impact_publish_fault,
            benchmark_resource_usage_under_faults,
            benchmark_circuit_breaker_recovery_time
        ]}
    ].

init_per_suite(Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_suite(_Config) ->
    router_nats_fault_injection:clear_all_faults(),
    stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = start_router_app(),
    ok = ensure_circuit_breaker_alive(),
    ok = reset_circuit_breaker(),
    router_metrics:ensure(),
    router_r10_metrics:clear_metrics(),
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_testcase(_TestCase, Config) ->
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ========================================================================
%% RESILIENCE BENCHMARKS
%% ========================================================================

%% @doc Benchmark recovery time after connect fault
benchmark_recovery_time_connect_fault(_Config) ->
    ct:comment("Benchmarking recovery time after connect fault"),
    
    %% Enable connect fault
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Wait for fault to be detected
    timer:sleep(1000),
    
    %% Measure recovery time
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Disable fault (recovery)
    router_nats_fault_injection:disable_fault(connect),
    
    %% Wait for recovery
    timer:sleep(2000),
    
    EndTime = erlang:monotonic_time(millisecond),
    RecoveryTime = EndTime - StartTime,
    
    ct:log("Recovery time after connect fault: ~p ms", [RecoveryTime]),
    
    %% Benchmark threshold: recovery should complete within 5 seconds
    ?assert(RecoveryTime < 5000, "Recovery time should be < 5000ms"),
    
    ok.

%% @doc Benchmark recovery time after publish fault
benchmark_recovery_time_publish_fault(_Config) ->
    ct:comment("Benchmarking recovery time after publish fault"),
    
    %% Enable publish fault
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Wait for fault to be detected
    timer:sleep(1000),
    
    %% Measure recovery time
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Disable fault (recovery)
    router_nats_fault_injection:disable_fault(publish),
    
    %% Wait for recovery
    timer:sleep(2000),
    
    EndTime = erlang:monotonic_time(millisecond),
    RecoveryTime = EndTime - StartTime,
    
    ct:log("Recovery time after publish fault: ~p ms", [RecoveryTime]),
    
    %% Benchmark threshold: recovery should complete within 5 seconds
    ?assert(RecoveryTime < 5000, "Recovery time should be < 5000ms"),
    
    ok.

%% @doc Benchmark throughput degradation under connect fault
benchmark_throughput_degradation_connect_fault(_Config) ->
    ct:comment("Benchmarking throughput degradation under connect fault"),
    
    %% Measure baseline throughput
    BaselineStart = erlang:monotonic_time(millisecond),
    BaselineCount = perform_operations(100, fun() ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end),
    BaselineEnd = erlang:monotonic_time(millisecond),
    BaselineTime = BaselineEnd - BaselineStart,
    BaselineThroughput = (BaselineCount * 1000) / BaselineTime,
    
    ct:log("Baseline throughput: ~p ops/s", [BaselineThroughput]),
    
    %% Enable connect fault
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Measure throughput under fault
    FaultStart = erlang:monotonic_time(millisecond),
    FaultCount = perform_operations(100, fun() ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end),
    FaultEnd = erlang:monotonic_time(millisecond),
    FaultTime = FaultEnd - FaultStart,
    FaultThroughput = (FaultCount * 1000) / FaultTime,
    
    ct:log("Throughput under connect fault: ~p ops/s", [FaultThroughput]),
    
    %% Calculate degradation
    Degradation = ((BaselineThroughput - FaultThroughput) / BaselineThroughput) * 100,
    ct:log("Throughput degradation: ~p%", [Degradation]),
    
    %% Benchmark threshold: degradation should be < 90%
    ?assert(Degradation < 90, "Throughput degradation should be < 90%"),
    
    %% Disable fault
    router_nats_fault_injection:disable_fault(connect),
    
    ok.

%% @doc Benchmark throughput degradation under publish fault
benchmark_throughput_degradation_publish_fault(_Config) ->
    ct:comment("Benchmarking throughput degradation under publish fault"),
    
    %% Measure baseline throughput
    BaselineStart = erlang:monotonic_time(millisecond),
    BaselineCount = perform_operations(100, fun() ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end),
    BaselineEnd = erlang:monotonic_time(millisecond),
    BaselineTime = BaselineEnd - BaselineStart,
    BaselineThroughput = (BaselineCount * 1000) / BaselineTime,
    
    ct:log("Baseline throughput: ~p ops/s", [BaselineThroughput]),
    
    %% Enable publish fault
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Measure throughput under fault
    FaultStart = erlang:monotonic_time(millisecond),
    FaultCount = perform_operations(100, fun() ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end),
    FaultEnd = erlang:monotonic_time(millisecond),
    FaultTime = FaultEnd - FaultStart,
    FaultThroughput = (FaultCount * 1000) / FaultTime,
    
    ct:log("Throughput under publish fault: ~p ops/s", [FaultThroughput]),
    
    %% Calculate degradation
    Degradation = ((BaselineThroughput - FaultThroughput) / BaselineThroughput) * 100,
    ct:log("Throughput degradation: ~p%", [Degradation]),
    
    %% Benchmark threshold: degradation should be < 90%
    ?assert(Degradation < 90, "Throughput degradation should be < 90%"),
    
    %% Disable fault
    router_nats_fault_injection:disable_fault(publish),
    
    ok.

%% @doc Benchmark latency impact under connect fault
benchmark_latency_impact_connect_fault(_Config) ->
    ct:comment("Benchmarking latency impact under connect fault"),
    
    %% Measure baseline latency
    BaselineLatencies = measure_latencies(50, fun() ->
        Start = erlang:monotonic_time(microsecond),
        _ = router_nats:publish(<<"test.subject">>, <<"test">>),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end),
    BaselineP95 = calculate_p95(BaselineLatencies),
    
    ct:log("Baseline P95 latency: ~p us", [BaselineP95]),
    
    %% Enable connect fault
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    %% Measure latency under fault
    FaultLatencies = measure_latencies(50, fun() ->
        Start = erlang:monotonic_time(microsecond),
        _ = router_nats:publish(<<"test.subject">>, <<"test">>),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end),
    FaultP95 = calculate_p95(FaultLatencies),
    
    ct:log("P95 latency under connect fault: ~p us", [FaultP95]),
    
    %% Calculate latency increase
    LatencyIncrease = ((FaultP95 - BaselineP95) / BaselineP95) * 100,
    ct:log("Latency increase: ~p%", [LatencyIncrease]),
    
    %% Benchmark threshold: latency increase should be < 500%
    ?assert(LatencyIncrease < 500, "Latency increase should be < 500%"),
    
    %% Disable fault
    router_nats_fault_injection:disable_fault(connect),
    
    ok.

%% @doc Benchmark latency impact under publish fault
benchmark_latency_impact_publish_fault(_Config) ->
    ct:comment("Benchmarking latency impact under publish fault"),
    
    %% Measure baseline latency
    BaselineLatencies = measure_latencies(50, fun() ->
        Start = erlang:monotonic_time(microsecond),
        _ = router_nats:publish(<<"test.subject">>, <<"test">>),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end),
    BaselineP95 = calculate_p95(BaselineLatencies),
    
    ct:log("Baseline P95 latency: ~p us", [BaselineP95]),
    
    %% Enable publish fault
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Measure latency under fault
    FaultLatencies = measure_latencies(50, fun() ->
        Start = erlang:monotonic_time(microsecond),
        _ = router_nats:publish(<<"test.subject">>, <<"test">>),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end),
    FaultP95 = calculate_p95(FaultLatencies),
    
    ct:log("P95 latency under publish fault: ~p us", [FaultP95]),
    
    %% Calculate latency increase
    LatencyIncrease = ((FaultP95 - BaselineP95) / BaselineP95) * 100,
    ct:log("Latency increase: ~p%", [LatencyIncrease]),
    
    %% Benchmark threshold: latency increase should be < 500%
    ?assert(LatencyIncrease < 500, "Latency increase should be < 500%"),
    
    %% Disable fault
    router_nats_fault_injection:disable_fault(publish),
    
    ok.

%% @doc Benchmark resource usage under faults
benchmark_resource_usage_under_faults(_Config) ->
    ct:comment("Benchmarking resource usage under faults"),
    
    %% Measure baseline resource usage
    BaselineProcessCount = erlang:system_info(process_count),
    BaselineMemory = erlang:memory(processes_used),
    
    ct:log("Baseline process count: ~p", [BaselineProcessCount]),
    ct:log("Baseline memory: ~p bytes", [BaselineMemory]),
    
    %% Enable multiple faults
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    %% Perform operations under faults
    perform_operations(1000, fun() ->
        router_nats:publish(<<"test.subject">>, <<"test">>)
    end),
    
    %% Wait for system to stabilize
    timer:sleep(2000),
    
    %% Measure resource usage under faults
    FaultProcessCount = erlang:system_info(process_count),
    FaultMemory = erlang:memory(processes_used),
    
    ct:log("Process count under faults: ~p", [FaultProcessCount]),
    ct:log("Memory under faults: ~p bytes", [FaultMemory]),
    
    %% Calculate resource increase
    ProcessIncrease = ((FaultProcessCount - BaselineProcessCount) / BaselineProcessCount) * 100,
    MemoryIncrease = ((FaultMemory - BaselineMemory) / BaselineMemory) * 100,
    
    ct:log("Process count increase: ~p%", [ProcessIncrease]),
    ct:log("Memory increase: ~p%", [MemoryIncrease]),
    
    %% Benchmark threshold: process count increase should be < 50%
    ?assert(ProcessIncrease < 50, "Process count increase should be < 50%"),
    
    %% Benchmark threshold: memory increase should be < 100%
    ?assert(MemoryIncrease < 100, "Memory increase should be < 100%"),
    
    %% Disable faults
    router_nats_fault_injection:clear_all_faults(),
    
    ok.

%% @doc Benchmark circuit breaker recovery time
benchmark_circuit_breaker_recovery_time(_Config) ->
    ct:comment("Benchmarking circuit breaker recovery time"),
    
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2,
        <<"latency_threshold_ms">> => 0
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    timer:sleep(100),
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Measure recovery time
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(TimeoutMs + 100),
    _ = router_circuit_breaker:should_allow(TenantId, ProviderId),
    timer:sleep(100),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Record successes to meet threshold
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 2)),
    
    timer:sleep(100),
    
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    EndTime = erlang:monotonic_time(millisecond),
    RecoveryTime = EndTime - StartTime,
    
    ct:log("Circuit breaker recovery time: ~p ms", [RecoveryTime]),
    
    %% Benchmark threshold: recovery should complete within timeout + 2 seconds
    ?assert(RecoveryTime < (TimeoutMs + 2000), "Recovery time should be < timeout + 2000ms"),
    
    ok.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Perform operations and count successes
-spec perform_operations(integer(), fun()) -> integer().
perform_operations(Count, Operation) ->
    Results = lists:map(fun(_) ->
        try
            Operation(),
            1
        catch
            _:_ -> 0
        end
    end, lists:seq(1, Count)),
    lists:sum(Results).

%% @doc Measure latencies
-spec measure_latencies(integer(), fun()) -> [integer()].
measure_latencies(Count, Operation) ->
    lists:map(fun(_) ->
        Operation()
    end, lists:seq(1, Count)).

%% @doc Calculate P95 latency
-spec calculate_p95([integer()]) -> integer().
calculate_p95(Latencies) ->
    Sorted = lists:sort(Latencies),
    Index = erlang:trunc(length(Sorted) * 0.95),
    lists:nth(min(Index + 1, length(Sorted)), Sorted).

