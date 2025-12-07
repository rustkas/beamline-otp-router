%% @doc Performance Monitor for Stress/Soak Tests
%%
%% Provides detailed performance monitoring:
%% - Latency tracking (P50, P95, P99) for key operations
%% - Throughput tracking (operations per second)
%% - Queue size tracking
%% - Performance degradation detection
%%
%% Usage in stress/soak tests:
%%   PerfMonitor = router_stress_perf_monitor:start(),
%%   router_stress_perf_monitor:record_latency(PerfMonitor, operation, LatencyMs),
%%   router_stress_perf_monitor:record_throughput(PerfMonitor, operation, Count),
%%   Stats = router_stress_perf_monitor:get_stats(PerfMonitor),
%%   router_stress_perf_monitor:stop(PerfMonitor)
-module(router_stress_perf_monitor).
-behaviour(gen_server).

-export([
    start/0,
    start/1,
    stop/1,
    record_latency/3,
    record_throughput/3,
    record_queue_size/3,
    get_stats/1,
    get_latency_stats/2,
    get_throughput_stats/2,
    get_queue_stats/2,
    check_performance_degradation/3,
    reset/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    latency_samples = #{} :: map(),  %% Operation -> list of latency values
    throughput_samples = #{} :: map(),  %% Operation -> list of throughput values
    queue_sizes = #{} :: map(),  %% Operation -> list of queue sizes
    start_time :: integer(),
    collection_window_ms = 60000 :: integer()  %% 1 minute default
}).

%% ========================================================================
%% PUBLIC API
%% ========================================================================

%% @doc Start performance monitor with default settings
-spec start() -> {ok, pid()}.
start() ->
    start(#{}).

%% @doc Start performance monitor with custom settings
%% Options:
%%   - collection_window_ms: Collection window in milliseconds (default: 60000 = 1 min)
-spec start(map()) -> {ok, pid()}.
start(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Stop performance monitor
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Record latency for an operation
%% Operation: atom() - operation name (e.g., message_processing, publish, ack)
%% LatencyMs: float() - latency in milliseconds
-spec record_latency(pid(), atom(), float()) -> ok.
record_latency(Pid, Operation, LatencyMs) when is_atom(Operation), is_float(LatencyMs) ->
    gen_server:cast(Pid, {record_latency, Operation, LatencyMs}).

%% @doc Record throughput for an operation
%% Operation: atom() - operation name
%% Count: integer() - number of operations
-spec record_throughput(pid(), atom(), integer()) -> ok.
record_throughput(Pid, Operation, Count) when is_atom(Operation), is_integer(Count) ->
    gen_server:cast(Pid, {record_throughput, Operation, Count}).

%% @doc Record queue size for an operation
%% Operation: atom() - operation name
%% QueueSize: integer() - queue size
-spec record_queue_size(pid(), atom(), integer()) -> ok.
record_queue_size(Pid, Operation, QueueSize) when is_atom(Operation), is_integer(QueueSize) ->
    gen_server:cast(Pid, {record_queue_size, Operation, QueueSize}).

%% @doc Get all performance statistics
-spec get_stats(pid()) -> map().
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%% @doc Get latency statistics for an operation
-spec get_latency_stats(pid(), atom()) -> map() | {error, no_data}.
get_latency_stats(Pid, Operation) ->
    gen_server:call(Pid, {get_latency_stats, Operation}).

%% @doc Get throughput statistics for an operation
-spec get_throughput_stats(pid(), atom()) -> map() | {error, no_data}.
get_throughput_stats(Pid, Operation) ->
    gen_server:call(Pid, {get_throughput_stats, Operation}).

%% @doc Get queue size statistics for an operation
-spec get_queue_stats(pid(), atom()) -> map() | {error, no_data}.
get_queue_stats(Pid, Operation) ->
    gen_server:call(Pid, {get_queue_stats, Operation}).

%% @doc Check for performance degradation
%% Baseline: baseline statistics map
%% Returns: {ok, no_degradation} | {fail, Reason, Details}
-spec check_performance_degradation(pid(), map(), map()) -> {ok, no_degradation} | {fail, atom(), map()}.
check_performance_degradation(Pid, Baseline, Thresholds) ->
    gen_server:call(Pid, {check_performance_degradation, Baseline, Thresholds}).

%% @doc Reset all statistics
-spec reset(pid()) -> ok.
reset(Pid) ->
    gen_server:call(Pid, reset).

%% ========================================================================
%% GEN_SERVER CALLBACKS
%% ========================================================================

init(Options) ->
    CollectionWindow = maps:get(collection_window_ms, Options, 60000),
    StartTime = erlang:monotonic_time(millisecond),
    
    {ok, #state{
        latency_samples = #{},
        throughput_samples = #{},
        queue_sizes = #{},
        start_time = StartTime,
        collection_window_ms = CollectionWindow
    }}.

handle_call(get_stats, _From, State) ->
    Stats = #{
        latency => calculate_all_latency_stats(State#state.latency_samples),
        throughput => calculate_all_throughput_stats(State#state.throughput_samples),
        queue_sizes => calculate_all_queue_stats(State#state.queue_sizes),
        start_time => State#state.start_time,
        current_time => erlang:monotonic_time(millisecond)
    },
    {reply, Stats, State};

handle_call({get_latency_stats, Operation}, _From, State) ->
    Samples = maps:get(Operation, State#state.latency_samples, []),
    case Samples of
        [] ->
            {reply, {error, no_data}, State};
        _ ->
            Stats = calculate_latency_stats(Samples),
            {reply, Stats, State}
    end;

handle_call({get_throughput_stats, Operation}, _From, State) ->
    Samples = maps:get(Operation, State#state.throughput_samples, []),
    case Samples of
        [] ->
            {reply, {error, no_data}, State};
        _ ->
            Stats = calculate_throughput_stats(Samples),
            {reply, Stats, State}
    end;

handle_call({get_queue_stats, Operation}, _From, State) ->
    Samples = maps:get(Operation, State#state.queue_sizes, []),
    case Samples of
        [] ->
            {reply, {error, no_data}, State};
        _ ->
            Stats = calculate_queue_stats(Samples),
            {reply, Stats, State}
    end;

handle_call({check_performance_degradation, Baseline, Thresholds}, _From, State) ->
    Result = check_degradation_internal(State, Baseline, Thresholds),
    {reply, Result, State};

handle_call(reset, _From, _State) ->
    NewState = #state{
        latency_samples = #{},
        throughput_samples = #{},
        queue_sizes = #{},
        start_time = erlang:monotonic_time(millisecond),
        collection_window_ms = _State#state.collection_window_ms
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_latency, Operation, LatencyMs}, State) ->
    CurrentSamples = maps:get(Operation, State#state.latency_samples, []),
    NewSamples = [LatencyMs | CurrentSamples],
    %% Keep only last 10000 samples per operation to prevent memory growth
    LimitedSamples = lists:sublist(NewSamples, 10000),
    NewLatencySamples = maps:put(Operation, LimitedSamples, State#state.latency_samples),
    {noreply, State#state{latency_samples = NewLatencySamples}};

handle_cast({record_throughput, Operation, Count}, State) ->
    CurrentSamples = maps:get(Operation, State#state.throughput_samples, []),
    Timestamp = erlang:monotonic_time(millisecond),
    NewSamples = [{Timestamp, Count} | CurrentSamples],
    %% Keep only last 1000 samples per operation
    LimitedSamples = lists:sublist(NewSamples, 1000),
    NewThroughputSamples = maps:put(Operation, LimitedSamples, State#state.throughput_samples),
    {noreply, State#state{throughput_samples = NewThroughputSamples}};

handle_cast({record_queue_size, Operation, QueueSize}, State) ->
    CurrentSamples = maps:get(Operation, State#state.queue_sizes, []),
    Timestamp = erlang:monotonic_time(millisecond),
    NewSamples = [{Timestamp, QueueSize} | CurrentSamples],
    %% Keep only last 1000 samples per operation
    LimitedSamples = lists:sublist(NewSamples, 1000),
    NewQueueSizes = maps:put(Operation, LimitedSamples, State#state.queue_sizes),
    {noreply, State#state{queue_sizes = NewQueueSizes}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ========================================================================
%% INTERNAL FUNCTIONS
%% ========================================================================

%% @doc Calculate latency statistics (P50, P95, P99, mean, min, max)
-spec calculate_latency_stats(list(float())) -> map().
calculate_latency_stats(Samples) ->
    case Samples of
        [] ->
            #{};
        _ ->
            Sorted = lists:sort(Samples),
            Count = length(Sorted),
            P50 = percentile(Sorted, 0.50),
            P95 = percentile(Sorted, 0.95),
            P99 = percentile(Sorted, 0.99),
            Mean = lists:sum(Sorted) / Count,
            Min = hd(Sorted),
            Max = lists:last(Sorted),
            #{
                count => Count,
                p50_ms => P50,
                p95_ms => P95,
                p99_ms => P99,
                mean_ms => Mean,
                min_ms => Min,
                max_ms => Max
            }
    end.

%% @doc Calculate percentile from sorted list
-spec percentile(list(float()), float()) -> float().
percentile(Sorted, Percentile) ->
    Count = length(Sorted),
    Index = max(1, round(Count * Percentile)),
    lists:nth(Index, Sorted).

%% @doc Calculate throughput statistics
-spec calculate_throughput_stats(list({integer(), integer()})) -> map().
calculate_throughput_stats(Samples) ->
    case Samples of
        [] ->
            #{};
        [{FirstTime, _} | _] ->
            LastSample = lists:last(Samples),
            {LastTime, LastCount} = LastSample,
            _ = LastCount,  %% Last count not used in throughput calculation
            DurationMs = LastTime - FirstTime,
            TotalCount = lists:sum([Count || {_, Count} <- Samples]),
            Throughput = case DurationMs > 0 of
                true -> (TotalCount * 1000) / DurationMs;
                false -> 0.0
            end,
            #{
                count => length(Samples),
                total_operations => TotalCount,
                duration_ms => DurationMs,
                throughput_per_sec => Throughput
            }
    end.

%% @doc Calculate queue size statistics
-spec calculate_queue_stats(list({integer(), integer()})) -> map().
calculate_queue_stats(Samples) ->
    case Samples of
        [] ->
            #{};
        _ ->
            QueueSizes = [Size || {_, Size} <- Samples],
            Sorted = lists:sort(QueueSizes),
            Count = length(Sorted),
            Mean = lists:sum(QueueSizes) / Count,
            Min = hd(Sorted),
            Max = lists:last(Sorted),
            P95 = percentile(Sorted, 0.95),
            P99 = percentile(Sorted, 0.99),
            #{
                count => Count,
                mean => Mean,
                min => Min,
                max => Max,
                p95 => P95,
                p99 => P99
            }
    end.

%% @doc Calculate all latency statistics
-spec calculate_all_latency_stats(map()) -> map().
calculate_all_latency_stats(LatencySamples) ->
    maps:fold(fun
        (Operation, Samples, Acc) ->
            Stats = calculate_latency_stats(Samples),
            maps:put(Operation, Stats, Acc)
    end, #{}, LatencySamples).

%% @doc Calculate all throughput statistics
-spec calculate_all_throughput_stats(map()) -> map().
calculate_all_throughput_stats(ThroughputSamples) ->
    maps:fold(fun
        (Operation, Samples, Acc) ->
            Stats = calculate_throughput_stats(Samples),
            maps:put(Operation, Stats, Acc)
    end, #{}, ThroughputSamples).

%% @doc Calculate all queue statistics
-spec calculate_all_queue_stats(map()) -> map().
calculate_all_queue_stats(QueueSizes) ->
    maps:fold(fun
        (Operation, Samples, Acc) ->
            Stats = calculate_queue_stats(Samples),
            maps:put(Operation, Stats, Acc)
    end, #{}, QueueSizes).

%% @doc Check for performance degradation
-spec check_degradation_internal(#state{}, map(), map()) -> {ok, no_degradation} | {fail, atom(), map()}.
check_degradation_internal(State, Baseline, Thresholds) ->
    %% Get current stats
    CurrentStats = #{
        latency => calculate_all_latency_stats(State#state.latency_samples),
        throughput => calculate_all_throughput_stats(State#state.throughput_samples),
        queue_sizes => calculate_all_queue_stats(State#state.queue_sizes)
    },
    
    %% Check latency degradation
    BaselineLatency = maps:get(latency, Baseline, #{}),
    CurrentLatency = maps:get(latency, CurrentStats, #{}),
    LatencyThreshold = maps:get(p95_latency_multiplier, Thresholds, 2.0),
    
    LatencyResult = check_latency_degradation(BaselineLatency, CurrentLatency, LatencyThreshold),
    case LatencyResult of
        {fail, _, _} = Fail ->
            Fail;
        _ ->
            %% Check throughput degradation
            BaselineThroughput = maps:get(throughput, Baseline, #{}),
            CurrentThroughput = maps:get(throughput, CurrentStats, #{}),
            ThroughputThreshold = maps:get(throughput_drop_percent, Thresholds, 30.0),
            
            ThroughputResult = check_throughput_degradation(BaselineThroughput, CurrentThroughput, ThroughputThreshold),
            case ThroughputResult of
                {fail, _, _} = Fail ->
                    Fail;
                _ ->
                    {ok, no_degradation}
            end
    end.

%% @doc Check latency degradation
-spec check_latency_degradation(map(), map(), float()) -> {ok, no_degradation} | {fail, atom(), map()}.
check_latency_degradation(BaselineLatency, CurrentLatency, Threshold) ->
    maps:fold(fun
        (Operation, BaselineStats, Acc) ->
            case Acc of
                {fail, _, _} = Fail ->
                    Fail;
                _ ->
                    CurrentStats = maps:get(Operation, CurrentLatency, #{}),
                    BaselineP95 = maps:get(p95_ms, BaselineStats, undefined),
                    CurrentP95 = maps:get(p95_ms, CurrentStats, undefined),
                    case {BaselineP95, CurrentP95} of
                        {B, C} when is_float(B), is_float(C) ->
                            if
                                C > B * Threshold ->
                                    {fail, latency_degradation, #{
                                        operation => Operation,
                                        baseline_p95_ms => B,
                                        current_p95_ms => C,
                                        multiplier => C / B,
                                        threshold => Threshold
                                    }};
                                true ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end
            end
    end, {ok, no_degradation}, BaselineLatency).

%% @doc Check throughput degradation
-spec check_throughput_degradation(map(), map(), float()) -> {ok, no_degradation} | {fail, atom(), map()}.
check_throughput_degradation(BaselineThroughput, CurrentThroughput, Threshold) ->
    maps:fold(fun
        (Operation, BaselineStats, Acc) ->
            case Acc of
                {fail, _, _} = Fail ->
                    Fail;
                _ ->
                    CurrentStats = maps:get(Operation, CurrentThroughput, #{}),
                    BaselineTps = maps:get(throughput_per_sec, BaselineStats, undefined),
                    CurrentTps = maps:get(throughput_per_sec, CurrentStats, undefined),
                    case {BaselineTps, CurrentTps} of
                        {B, C} when is_float(B), is_float(C), B > 0 ->
                            DropPercent = ((B - C) / B) * 100.0,
                            if
                                DropPercent > Threshold ->
                                    {fail, throughput_degradation, #{
                                        operation => Operation,
                                        baseline_tps => B,
                                        current_tps => C,
                                        drop_percent => DropPercent,
                                        threshold => Threshold
                                    }};
                                true ->
                                    Acc
                            end;
                        _ ->
                            Acc
                    end
            end
    end, {ok, no_degradation}, BaselineThroughput).
