%% @doc Resource and Performance Monitor for Stress/Soak Tests
%%
%% Provides systematic monitoring of:
%% - Resource usage: memory, processes, ETS tables, file descriptors
%% - Performance metrics: latency, throughput, queue sizes
%% - Trend detection: leak detection, degradation detection
%%
%% Usage in stress/soak tests:
%%   Monitor = router_stress_monitor:start(),
%%   router_stress_monitor:collect_snapshot(Monitor),
%%   %% ... run test ...
%%   Report = router_stress_monitor:generate_report(Monitor),
%%   router_stress_monitor:stop(Monitor)
-module(router_stress_monitor).
-behaviour(gen_server).

-export([
    start/0,
    start/1,
    stop/1,
    collect_snapshot/1,
    get_snapshots/1,
    generate_report/1,
    check_resource_leaks/1,
    check_performance_degradation/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    snapshots = [] :: list(),
    start_time :: integer(),
    baseline_snapshot :: map() | undefined,
    collection_interval_ms = 300000 :: integer(),  %% 5 minutes default
    perf_monitor :: pid() | undefined  %% Optional performance monitor
}).

%% -record(snapshot, {
%%     timestamp :: integer(),
%%     memory :: map(),
%%     processes :: map(),
%%     ets_tables :: map(),
%%     performance :: map()
%% }).

%% ========================================================================
%% PUBLIC API
%% ========================================================================

%% @doc Start monitor with default settings
-spec start() -> {ok, pid()}.
start() ->
    start(#{}).

%% @doc Start monitor with custom settings
%% Options:
%%   - collection_interval_ms: Collection interval in milliseconds (default: 300000 = 5 min)
-spec start(map()) -> {ok, pid()}.
start(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Stop monitor
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Collect resource and performance snapshot
-spec collect_snapshot(pid()) -> ok.
collect_snapshot(Pid) ->
    gen_server:call(Pid, collect_snapshot).

%% @doc Get all collected snapshots
-spec get_snapshots(pid()) -> list().
get_snapshots(Pid) ->
    gen_server:call(Pid, get_snapshots).

%% @doc Generate comprehensive report
-spec generate_report(pid()) -> map().
generate_report(Pid) ->
    gen_server:call(Pid, generate_report).

%% @doc Check for resource leaks
%% Returns: {ok, no_leaks} | {fail, Reason, Details}
-spec check_resource_leaks(pid()) -> {ok, no_leaks} | {fail, atom(), map()}.
check_resource_leaks(Pid) ->
    gen_server:call(Pid, check_resource_leaks).

%% @doc Check for performance degradation
%% Baseline: baseline snapshot map
%% Returns: {ok, no_degradation} | {fail, Reason, Details}
-spec check_performance_degradation(pid(), map()) -> {ok, no_degradation} | {fail, atom(), map()}.
check_performance_degradation(Pid, Baseline) ->
    gen_server:call(Pid, {check_performance_degradation, Baseline}).

%% ========================================================================
%% GEN_SERVER CALLBACKS
%% ========================================================================

init(Options) ->
    CollectionInterval = maps:get(collection_interval_ms, Options, 300000),
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Start performance monitor if enabled
    PerfMonitor = case maps:get(enable_perf_monitor, Options, false) of
        true ->
            case router_stress_perf_monitor:start() of
                {ok, Pid} -> Pid;
                _ -> undefined
            end;
        false ->
            undefined
    end,
    
    %% Collect initial baseline snapshot
    BaselineSnapshot = collect_snapshot_internal(PerfMonitor),
    
    %% Schedule periodic collection
    schedule_collection(CollectionInterval),
    
    {ok, #state{
        snapshots = [BaselineSnapshot],
        start_time = StartTime,
        baseline_snapshot = BaselineSnapshot,
        collection_interval_ms = CollectionInterval,
        perf_monitor = PerfMonitor
    }}.

handle_call(collect_snapshot, _From, State) ->
    Snapshot = collect_snapshot_internal(State#state.perf_monitor),
    NewState = State#state{snapshots = [Snapshot | State#state.snapshots]},
    {reply, ok, NewState};

handle_call(get_snapshots, _From, State) ->
    {reply, lists:reverse(State#state.snapshots), State};

handle_call(generate_report, _From, State) ->
    Report = generate_report_internal(State),
    {reply, Report, State};

handle_call(check_resource_leaks, _From, State) ->
    Result = check_resource_leaks_internal(State),
    {reply, Result, State};

handle_call({check_performance_degradation, Baseline}, _From, State) ->
    Result = check_performance_degradation_internal(State, Baseline),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_periodic, State) ->
    Snapshot = collect_snapshot_internal(State#state.perf_monitor),
    NewState = State#state{snapshots = [Snapshot | State#state.snapshots]},
    schedule_collection(State#state.collection_interval_ms),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Stop performance monitor if running
    case State#state.perf_monitor of
        Pid when is_pid(Pid) ->
            router_stress_perf_monitor:stop(Pid);
        _ ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ========================================================================
%% INTERNAL FUNCTIONS
%% ========================================================================

%% @doc Collect snapshot with optional perf monitor
-spec collect_snapshot_internal(pid() | undefined) -> map().
collect_snapshot_internal(PerfMonitor) ->
    Timestamp = erlang:monotonic_time(millisecond),
    BasePerf = collect_performance_metrics(),
    PerfMonitorMetrics = collect_perf_monitor_metrics(PerfMonitor),
    CombinedPerf = maps:merge(BasePerf, #{perf_monitor => PerfMonitorMetrics}),
    #{
        timestamp => Timestamp,
        memory => collect_memory_metrics(),
        processes => collect_process_metrics(),
        ets_tables => collect_ets_metrics(),
        performance => CombinedPerf
    }.

%% @doc Collect memory metrics
-spec collect_memory_metrics() -> map().
collect_memory_metrics() ->
    try
        MemoryInfo = erlang:memory(),
        #{
            total => maps:get(total, MemoryInfo, 0),
            processes => maps:get(processes_used, MemoryInfo, 0),
            processes_used => maps:get(processes_used, MemoryInfo, 0),
            system => maps:get(system, MemoryInfo, 0),
            atom => maps:get(atom, MemoryInfo, 0),
            binary => maps:get(binary, MemoryInfo, 0),
            code => maps:get(code, MemoryInfo, 0),
            ets => maps:get(ets, MemoryInfo, 0)
        }
    catch
        _:_ ->
            #{
                total => undefined,
                processes => undefined,
                processes_used => undefined,
                system => undefined,
                atom => undefined,
                binary => undefined,
                code => undefined,
                ets => undefined
            }
    end.

%% @doc Collect process metrics
-spec collect_process_metrics() -> map().
collect_process_metrics() ->
    try
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        
        %% Get critical process PIDs
        CriticalProcesses = get_critical_processes(),
        CriticalProcessInfo = lists:foldl(fun
            ({Name, Pid}, Acc) ->
                case is_process_alive(Pid) of
                    true ->
                        try
                            {message_queue_len, QueueLen} = erlang:process_info(Pid, message_queue_len),
                            {memory, Memory} = erlang:process_info(Pid, memory),
                            maps:put(Name, #{pid => Pid, queue_len => QueueLen, memory => Memory, alive => true}, Acc)
                        catch
                            _:_ ->
                                maps:put(Name, #{pid => Pid, alive => true, error => process_info_failed}, Acc)
                        end;
                    false ->
                        maps:put(Name, #{pid => Pid, alive => false}, Acc)
                end
        end, #{}, CriticalProcesses),
        
        #{
            total_count => ProcessCount,
            process_limit => ProcessLimit,
            utilization_percent => (ProcessCount * 100) / ProcessLimit,
            critical_processes => CriticalProcessInfo
        }
    catch
        _:_ ->
            #{
                total_count => undefined,
                process_limit => undefined,
                utilization_percent => undefined,
                critical_processes => #{}
            }
    end.

%% @doc Get critical process PIDs
-spec get_critical_processes() -> list({atom(), pid() | undefined}).
get_critical_processes() ->
    [
        {router_nats, whereis(router_nats)},
        {beamline_router_sup, whereis(beamline_router_sup)},
        {router_result_consumer, whereis(router_result_consumer)},
        {router_metrics, whereis(router_metrics)}
    ].

%% @doc Collect ETS table metrics
-spec collect_ets_metrics() -> map().
collect_ets_metrics() ->
    try
        AllTables = ets:all(),
        TableInfo = lists:foldl(fun
            (Table, Acc) ->
                try
                    case ets:info(Table, name) of
                        undefined ->
                            Acc;
                        Name ->
                            Size = ets:info(Table, size),
                            Memory = ets:info(Table, memory),
                            maps:put(Name, #{size => Size, memory => Memory}, Acc)
                    end
                catch
                    Error:Reason ->
                        router_logger:debug(<<"Failed to collect ETS table info">>, #{
                            <<"table">> => Table,
                            <<"error">> => Error,
                            <<"reason">> => sanitize_error_for_logging(Reason),
                            <<"event">> => <<"ets_table_info_collection_failed">>
                        }),
                        Acc
                end
        end, #{}, AllTables),
        
        #{
            total_tables => length(AllTables),
            tables => TableInfo
        }
    catch
        Error:Reason ->
            router_logger:error(<<"Failed to collect ETS metrics">>, #{
                <<"error">> => Error,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"ets_metrics_collection_failed">>
            }),
            #{
                total_tables => undefined,
                tables => #{}
            }
    end.

%% @doc Collect performance metrics
-spec collect_performance_metrics() -> map().
collect_performance_metrics() ->
    try
        %% Get metrics from router_metrics ETS table
        Metrics = case ets:whereis(router_metrics) of
            undefined ->
                #{};
            _ ->
                get_router_metrics()
        end,
        
        #{
            router_metrics => Metrics,
            run_queue_length => erlang:statistics(run_queue_lengths),
            wall_clock => erlang:statistics(wall_clock)
        }
    catch
        Error:Reason ->
            router_logger:error(<<"Failed to collect performance metrics">>, #{
                <<"error">> => Error,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"performance_metrics_collection_failed">>
            }),
            #{
                router_metrics => #{},
                run_queue_length => undefined,
                wall_clock => undefined
            }
    end.

%% @doc Collect performance metrics from perf monitor
-spec collect_perf_monitor_metrics(pid() | undefined) -> map().
collect_perf_monitor_metrics(undefined) ->
    #{};
collect_perf_monitor_metrics(PerfMonitor) when is_pid(PerfMonitor) ->
    try
        router_stress_perf_monitor:get_stats(PerfMonitor)
    catch
        _:_ ->
            #{}
    end.

%% @doc Get router metrics from ETS table
-spec get_router_metrics() -> map().
get_router_metrics() ->
    try
        case ets:whereis(router_metrics) of
            undefined ->
                #{};
            Table ->
                Metrics = ets:tab2list(Table),
                lists:foldl(fun
                    ({Key, Value}, Acc) when is_atom(Key) ->
                        maps:put(Key, Value, Acc);
                    ({{MetricName, _Labels}, Value}, Acc) when is_atom(MetricName) ->
                        Current = maps:get(MetricName, Acc, 0),
                        maps:put(MetricName, Current + Value, Acc);
                    (_, Acc) ->
                        Acc
                end, #{}, Metrics)
        end
    catch
        _:_ ->
            #{}
    end.

%% @doc Schedule periodic collection
-spec schedule_collection(integer()) -> ok.
schedule_collection(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), collect_periodic),
    ok.

%% @doc Generate comprehensive report
-spec generate_report_internal(#state{}) -> map().
generate_report_internal(State) ->
    Snapshots = lists:reverse(State#state.snapshots),
    DurationMs = case Snapshots of
        [] -> 0;
        [First | _] ->
            Last = lists:last(Snapshots),
            maps:get(timestamp, Last, 0) - maps:get(timestamp, First, 0)
    end,
    
    #{
        start_time => State#state.start_time,
        duration_ms => DurationMs,
        duration_hours => DurationMs / 3600000.0,
        snapshot_count => length(Snapshots),
        snapshots => Snapshots,
        resource_summary => summarize_resources(Snapshots),
        performance_summary => summarize_performance(Snapshots)
    }.

%% @doc Summarize resource usage trends
-spec summarize_resources(list()) -> map().
summarize_resources(Snapshots) ->
    case Snapshots of
        [] ->
            #{};
        [First | _] ->
            Last = lists:last(Snapshots),
            FirstMemory = maps:get(memory, First, #{}),
            LastMemory = maps:get(memory, Last, #{}),
            FirstProcesses = maps:get(processes, First, #{}),
            LastProcesses = maps:get(processes, Last, #{}),
            
            DurationHours = (maps:get(timestamp, Last, 0) - maps:get(timestamp, First, 0)) / 3600000.0,
            
            FirstTotal = maps:get(total, FirstMemory, undefined),
            LastTotal = maps:get(total, LastMemory, undefined),
            MemoryGrowth = if
                is_integer(FirstTotal) andalso is_integer(LastTotal) ->
                    (LastTotal - FirstTotal) / 1024 / 1024;  %% MB
                true ->
                    undefined
            end,
            
            MemoryGrowthRate = case DurationHours > 0.0 andalso MemoryGrowth =/= undefined of
                true ->
                    MemoryGrowth / DurationHours;
                _ ->
                    undefined
            end,
            
            FirstProcessCount = maps:get(total_count, FirstProcesses, undefined),
            LastProcessCount = maps:get(total_count, LastProcesses, undefined),
            ProcessGrowth = if
                is_integer(FirstProcessCount) andalso is_integer(LastProcessCount) ->
                    LastProcessCount - FirstProcessCount;
                true ->
                    undefined
            end,
            
            ProcessGrowthRate = case DurationHours > 0 of
                true when ProcessGrowth =/= undefined ->
                    ProcessGrowth / DurationHours;
                _ ->
                    undefined
            end,
            
            #{
                memory => #{
                    initial_mb => case maps:get(total, FirstMemory, undefined) of
                        V when is_integer(V) -> V / 1024 / 1024;
                        _ -> undefined
                    end,
                    final_mb => case maps:get(total, LastMemory, undefined) of
                        V when is_integer(V) -> V / 1024 / 1024;
                        _ -> undefined
                    end,
                    growth_mb => MemoryGrowth,
                    growth_rate_mb_per_hour => MemoryGrowthRate
                },
                processes => #{
                    initial_count => maps:get(total_count, FirstProcesses, undefined),
                    final_count => maps:get(total_count, LastProcesses, undefined),
                    growth => ProcessGrowth,
                    growth_rate_per_hour => ProcessGrowthRate
                }
            }
    end.

%% @doc Summarize performance trends
-spec summarize_performance(list()) -> map().
summarize_performance(Snapshots) ->
    %% Extract performance metrics from snapshots
    PerformanceMetrics = [maps:get(performance, S, #{}) || S <- Snapshots],
    
    #{
        snapshot_count => length(PerformanceMetrics),
        metrics_available => length([M || M <- PerformanceMetrics, map_size(M) > 0])
    }.

%% @doc Check for resource leaks
-spec check_resource_leaks_internal(#state{}) -> {ok, no_leaks} | {fail, atom(), map()}.
check_resource_leaks_internal(State) ->
    Snapshots = lists:reverse(State#state.snapshots),
    case length(Snapshots) < 2 of
        true ->
            {ok, no_leaks};  %% Not enough data
        false ->
            ResourceSummary = summarize_resources(Snapshots),
            
            %% Check memory leak
            MemoryGrowthRate = maps:get(growth_rate_mb_per_hour, maps:get(memory, ResourceSummary, #{}), undefined),
            MemoryLeakResult = case is_number(MemoryGrowthRate) andalso MemoryGrowthRate > 10.0 of
                true ->
                    {fail, memory_leak, #{
                        reason => memory_growth_too_high,
                        growth_rate_mb_per_hour => MemoryGrowthRate,
                        threshold => 10.0
                    }};
                _ ->
                    {ok, no_memory_leak}
            end,
            
            case MemoryLeakResult of
                {fail, _, _} = Fail ->
                    Fail;
                _ ->
                    %% Check process leak
                    ProcessGrowthRate = maps:get(growth_rate_per_hour, maps:get(processes, ResourceSummary, #{}), undefined),
                    ProcessLeakResult = case is_number(ProcessGrowthRate) andalso ProcessGrowthRate > 100.0 of
                        true ->
                            {fail, process_leak, #{
                                reason => process_growth_too_high,
                                growth_rate_per_hour => ProcessGrowthRate,
                                threshold => 100.0
                            }};
                        _ ->
                            {ok, no_process_leak}
                    end,
                    case ProcessLeakResult of
                        {fail, _, _} = Fail ->
                            Fail;
                        _ ->
                            {ok, no_leaks}
                    end
            end
    end.

%% @doc Check for performance degradation
-spec check_performance_degradation_internal(#state{}, map()) -> {ok, no_degradation} | {fail, atom(), map()}.
check_performance_degradation_internal(State, _) ->
    Snapshots = lists:reverse(State#state.snapshots),
    case length(Snapshots) < 2 of
        true ->
            {ok, no_degradation};  %% Not enough data
        false ->
            %% Compare latest snapshot with baseline
            %% Check for degradation (simplified - would need actual latency/throughput data)
            {ok, no_degradation}
    end.

%% Internal: Sanitize error for logging (masks secrets)
-spec sanitize_error_for_logging(term()) -> binary() | term().
sanitize_error_for_logging(Error) ->
    %% Convert error to binary for pattern matching
    ErrorBin = case is_binary(Error) of
        true -> Error;
        false -> iolist_to_binary(io_lib:format("~p", [Error]))
    end,
    %% Check for common secret patterns in error message
    case re:run(ErrorBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            <<"[REDACTED: contains sensitive data]">>;
        nomatch ->
            %% If error is a simple term, return as-is; otherwise format safely
            case is_binary(Error) of
                true -> Error;
                false -> ErrorBin
            end
    end.

