-module(router_cpu_profiler).

-doc "CPU Profiling Helper Module".
%% Provides structure and helpers for CPU profiling and performance monitoring
%%
%% Features:
%% - CPU usage tracking
%% - Profiling state management
%% - Hot path identification helpers
%% - Process CPU monitoring
%%
%% ⚠️ NOTE: Actual profiling requires runtime tools (fprof, eprof, recon)
%% This module provides structure and helpers for when profiling is enabled
%%
%% @see PERFORMANCE_GUIDE.md#profiling For profiling guidelines

-export([
    get_cpu_usage/0,
    get_process_cpu_usage/1,
    get_scheduler_utilization/0,
    start_profiling/1,
    stop_profiling/1,
    get_profiling_state/1,
    track_hot_path/3,
    get_hot_paths/0
]).

-include("beamline_router.hrl").

%% Returns: {ok, CpuUsagePercent} | {error, Reason}
%% NOTE: Actual CPU usage requires runtime monitoring tools
-spec get_cpu_usage() -> {ok, float()} | {error, term()}.
get_cpu_usage() ->
    try
        %% Get scheduler utilization (approximation of CPU usage)
        case get_scheduler_utilization() of
            {ok, SchedulerUtil} ->
                %% Convert scheduler utilization to CPU usage percentage
                CpuUsage = SchedulerUtil * 100.0,
                {ok, CpuUsage};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:CatchReason ->
            {error, CatchReason}
    end.

%% Returns: {ok, CpuUsagePercent} | {error, Reason}
-spec get_process_cpu_usage(pid()) -> {ok, float()} | {error, term()}.
get_process_cpu_usage(Pid) when is_pid(Pid) ->
    try
        case erlang:process_info(Pid, [reductions, current_function]) of
            undefined ->
                {error, process_not_found};
            ProcessInfo ->
                %% Get reduction count (approximation of CPU usage)
                Reductions = proplists:get_value(reductions, ProcessInfo, 0),
                %% Get process message queue length
                MessageQueueLen = case erlang:process_info(Pid, message_queue_len) of
                    {message_queue_len, Len} -> Len;
                    _ -> 0
                end,
                %% Calculate CPU usage approximation
                %% Higher reductions and message queue = higher CPU usage
                CpuUsage = calculate_process_cpu_usage(Reductions, MessageQueueLen),
                {ok, CpuUsage}
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec calculate_process_cpu_usage(integer(), integer()) -> float().
calculate_process_cpu_usage(Reductions, MessageQueueLen) ->
    %% Normalize reductions (typical process has 0-1000 reductions)
    NormalizedReductions = min(Reductions / 1000.0, 1.0),
    %% Normalize message queue (typical process has 0-100 messages)
    NormalizedQueue = min(MessageQueueLen / 100.0, 1.0),
    %% Combine factors (reductions are more indicative of CPU usage)
    CpuUsage = (NormalizedReductions * 0.7) + (NormalizedQueue * 0.3),
    min(CpuUsage * 100.0, 100.0).

%% Returns: {ok, Utilization} | {error, Reason}
%% Utilization is a float between 0.0 and 1.0
-spec get_scheduler_utilization() -> {ok, float()} | {error, term()}.
get_scheduler_utilization() ->
    try
        %% Get scheduler information
        SchedulerCount = erlang:system_info(schedulers),
        OnlineSchedulers = erlang:system_info(schedulers_online),
        
        %% Calculate utilization (approximation)
        %% More online schedulers = higher potential utilization
        Utilization = case OnlineSchedulers > 0 of
            true ->
                %% Approximation: assume schedulers are busy if online
                %% Actual utilization requires runtime profiling
                min(OnlineSchedulers / SchedulerCount, 1.0);
            false ->
                0.0
        end,
        {ok, Utilization}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% ProfilingType: fprof | eprof | recon
%% Returns: {ok, ProfilingState} | {error, Reason}
%% NOTE: Actual profiling requires runtime tools
-spec start_profiling(atom()) -> {ok, map()} | {error, term()}.
start_profiling(ProfilingType) when ProfilingType =:= fprof; ProfilingType =:= eprof; ProfilingType =:= recon ->
    try
        %% Create profiling state
        ProfilingState = #{
            type => ProfilingType,
            started_at => erlang:system_time(millisecond),
            status => active,
            trace_file => generate_trace_filename(ProfilingType)
        },
        
        %% Store profiling state in ETS
        store_profiling_state(ProfilingType, ProfilingState),
        
        %% Emit profiling started metric
        router_metrics:emit_metric(router_cpu_profiling_started_total, #{count => 1}, #{
            type => atom_to_binary(ProfilingType, utf8)
        }),
        
        {ok, ProfilingState}
    catch
        _:Reason ->
            {error, Reason}
    end;
start_profiling(_) ->
    {error, invalid_profiling_type}.

%% Returns: {ok, Results} | {error, Reason}
-spec stop_profiling(atom()) -> {ok, map()} | {error, term()}.
stop_profiling(ProfilingType) when ProfilingType =:= fprof; ProfilingType =:= eprof; ProfilingType =:= recon ->
    try
        %% Get profiling state
        case get_profiling_state(ProfilingType) of
            {ok, State} ->
                %% Calculate duration
                StartedAt = maps:get(started_at, State, erlang:system_time(millisecond)),
                Duration = erlang:system_time(millisecond) - StartedAt,
                
                %% Create results
                Results = #{
                    type => ProfilingType,
                    duration_ms => Duration,
                    stopped_at => erlang:system_time(millisecond),
                    trace_file => maps:get(trace_file, State, undefined)
                },
                
                %% Update state
                UpdatedState = maps:merge(State, #{
                    status => stopped,
                    results => Results
                }),
                store_profiling_state(ProfilingType, UpdatedState),
                
                %% Emit profiling stopped metric
                router_metrics:emit_metric(router_cpu_profiling_stopped_total, #{count => 1}, #{
                    type => atom_to_binary(ProfilingType, utf8),
                    duration_ms => integer_to_binary(Duration)
                }),
                
                {ok, Results};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:CatchReason ->
            {error, CatchReason}
    end;
stop_profiling(_) ->
    {error, invalid_profiling_type}.

-spec get_profiling_state(atom()) -> {ok, map()} | {error, term()}.
get_profiling_state(ProfilingType) ->
    try
        Table = router_cpu_profiling_state,
        case ets:whereis(Table) of
            undefined ->
                {error, profiling_not_started};
            _Tid ->
                case ets:lookup(Table, ProfilingType) of
                    [{ProfilingType, State}] ->
                        {ok, State};
                    [] ->
                        {error, profiling_not_started}
                end
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

%% Module: Module name
%% Function: Function name
%% CpuTime: CPU time in microseconds
-spec track_hot_path(atom(), atom(), integer()) -> ok.
track_hot_path(Module, Function, CpuTime) when is_atom(Module), is_atom(Function), is_integer(CpuTime) ->
    try
        %% Initialize hot paths table if needed
        Table = router_cpu_hot_paths,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, ordered_set, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        %% Track hot path
        Key = {Module, Function},
        case ets:lookup(Table, Key) of
            [{Key, Count, TotalCpuTime, LastSeen}] ->
                %% Update existing entry
                _ = LastSeen,  %% Last seen timestamp not used in update calculation
                NewCount = Count + 1,
                NewTotalCpuTime = TotalCpuTime + CpuTime,
                ets:insert(Table, {Key, NewCount, NewTotalCpuTime, erlang:system_time(millisecond)});
            [] ->
                %% Create new entry
                ets:insert(Table, {Key, 1, CpuTime, erlang:system_time(millisecond)})
        end,
        
        ok
    catch
        _:_ ->
            ok
    end.

%% Returns: [{Module, Function, Count, TotalCpuTime, AvgCpuTime}, ...]
-spec get_hot_paths() -> [{atom(), atom(), integer(), integer(), float()}].
get_hot_paths() ->
    try
        Table = router_cpu_hot_paths,
        case ets:whereis(Table) of
            undefined ->
                [];
            _Tid ->
                %% Get all hot paths
                AllPaths = ets:tab2list(Table),
                %% Sort by total CPU time (descending)
                SortedPaths = lists:sort(fun({_, _, _, Total1, _}, {_, _, _, Total2, _}) ->
                    Total1 > Total2
                end, lists:map(fun({Key, Count, TotalCpuTime, _LastSeen}) ->
                    {Module, Function} = Key,
                    AvgCpuTime = case Count > 0 of
                        true -> TotalCpuTime / Count;
                        false -> 0.0
                    end,
                    {Module, Function, Count, TotalCpuTime, AvgCpuTime}
                end, AllPaths)),
                %% Return top 10 hot paths
                lists:sublist(SortedPaths, 10)
        end
    catch
        _:_ ->
            []
    end.

-spec store_profiling_state(atom(), map()) -> ok.
store_profiling_state(ProfilingType, State) ->
    Table = router_cpu_profiling_state,
    case ets:whereis(Table) of
        undefined ->
            _ = ets:new(Table, [named_table, public, {write_concurrency, true}]);
        _ ->
            ok
    end,
    ets:insert(Table, {ProfilingType, State}),
    ok.

-spec generate_trace_filename(atom()) -> binary().
generate_trace_filename(ProfilingType) ->
    Timestamp = erlang:system_time(millisecond),
    <<"router_", (atom_to_binary(ProfilingType, utf8))/binary, "_", (integer_to_binary(Timestamp))/binary, ".trace">>.

