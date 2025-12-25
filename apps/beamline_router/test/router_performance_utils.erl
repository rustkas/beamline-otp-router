%% @doc Performance Testing Utilities
%%
%% Provides helper functions for performance testing, profiling, and optimization.
%% Used by performance test suites for consistent measurement and analysis.
%%
%% @test_category performance, utils
-module(router_performance_utils).

-export([
    profile_ets_operations/1,
    profile_policy_lookup/2,
    profile_metrics_collection/1,
    profile_logging/1,
    calculate_percentiles/2,
    measure_function_performance/2
]).

%% @doc Profile ETS operations for bottlenecks
%% @param OperationCount Number of operations to perform
%% @returns Map with timing statistics
-spec profile_ets_operations(integer()) -> map().
profile_ets_operations(OperationCount) ->
    router_metrics:ensure(),
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Perform ETS operations
    lists:foreach(fun(N) ->
        _Key = {test_metric, N},
        router_metrics:emit_metric(test_metric, #{count => 1}, #{index => N})
    end, lists:seq(1, OperationCount)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    Throughput = (OperationCount / Duration) * 1000,
    
    #{
        operation_count => OperationCount,
        duration_ms => Duration,
        throughput_ops_per_sec => Throughput,
        avg_latency_ms => Duration / OperationCount
    }.

%% @doc Profile policy lookup performance
%% @param TenantId Tenant identifier
%% @param PolicyId Policy identifier
%% @returns Map with timing statistics
-spec profile_policy_lookup(binary(), binary()) -> map().
profile_policy_lookup(TenantId, PolicyId) ->
    Iterations = 1000,
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Perform policy lookups
    lists:foreach(fun(_) ->
        router_policy_store:get_policy(TenantId, PolicyId, undefined)
    end, lists:seq(1, Iterations)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    Throughput = (Iterations / Duration) * 1000,
    
    #{
        iterations => Iterations,
        duration_ms => Duration,
        throughput_lookups_per_sec => Throughput,
        avg_latency_ms => Duration / Iterations
    }.

%% @doc Profile metrics collection performance
%% @param MetricCount Number of metrics to emit
%% @returns Map with timing statistics
-spec profile_metrics_collection(integer()) -> map().
profile_metrics_collection(MetricCount) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Emit metrics
    lists:foreach(fun(N) ->
        router_metrics:emit_metric(test_perf_metric, #{count => 1}, #{
            tenant_id => <<"t1">>,
            index => N
        })
    end, lists:seq(1, MetricCount)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    Throughput = (MetricCount / Duration) * 1000,
    
    #{
        metric_count => MetricCount,
        duration_ms => Duration,
        throughput_metrics_per_sec => Throughput,
        avg_latency_ms => Duration / MetricCount
    }.

%% @doc Profile logging performance
%% @param LogCount Number of log entries to emit
%% @returns Map with timing statistics
-spec profile_logging(integer()) -> map().
profile_logging(LogCount) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Emit logs
    lists:foreach(fun(N) ->
        router_logger:info(<<"Performance test log">>, #{
            <<"index">> => N,
            <<"tenant_id">> => <<"t1">>
        })
    end, lists:seq(1, LogCount)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    Throughput = (LogCount / Duration) * 1000,
    
    #{
        log_count => LogCount,
        duration_ms => Duration,
        throughput_logs_per_sec => Throughput,
        avg_latency_ms => Duration / LogCount
    }.

%% @doc Calculate percentiles from a list of values
%% @param Values List of numeric values
%% @param Percentiles List of percentile values (e.g., [50, 95, 99])
%% @returns Map with percentile -> value
-spec calculate_percentiles([number()], [number()]) -> map().
calculate_percentiles(Values, Percentiles) ->
    SortedValues = lists:sort(Values),
    Length = length(SortedValues),
    
    maps:from_list(lists:map(fun(P) ->
        Index = trunc(Length * (P / 100)),
        ActualIndex = case Index > 0 of
            true -> min(Index, Length);
            false -> 1
        end,
        Value = lists:nth(ActualIndex, SortedValues),
        {P, Value}
    end, Percentiles)).

%% @doc Measure function performance
%% @param Fun Function to measure
%% @param Iterations Number of iterations
%% @returns Map with timing statistics
-spec measure_function_performance(fun(() -> term()), integer()) -> map().
measure_function_performance(Fun, Iterations) ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Execute function multiple times
    Results = lists:map(fun(_) ->
        IterStart = erlang:monotonic_time(microsecond),
        _ = Fun(),
        IterEnd = erlang:monotonic_time(microsecond),
        IterEnd - IterStart
    end, lists:seq(1, Iterations)),
    
    EndTime = erlang:monotonic_time(millisecond),
    TotalDuration = EndTime - StartTime,
    
    %% Calculate statistics
    Percentiles = calculate_percentiles(Results, [50, 95, 99]),
    AvgLatency = lists:sum(Results) / Iterations / 1000,  %% Convert to ms
    MinLatency = lists:min(Results) / 1000,
    MaxLatency = lists:max(Results) / 1000,
    
    #{
        iterations => Iterations,
        total_duration_ms => TotalDuration,
        avg_latency_ms => AvgLatency,
        min_latency_ms => MinLatency,
        max_latency_ms => MaxLatency,
        p50_latency_ms => maps:get(50, Percentiles) / 1000,
        p95_latency_ms => maps:get(95, Percentiles) / 1000,
        p99_latency_ms => maps:get(99, Percentiles) / 1000
    }.

