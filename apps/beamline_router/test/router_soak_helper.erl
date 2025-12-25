%% @doc Soak Testing Helper Module
%%
%% Provides utilities for long-running stability tests:
%% - Memory monitoring and leak detection
%% - ETS table growth tracking
%% - JetStream consumer lag monitoring
%% - Process stability checks
%% - Load generation
%%
%% @test_category soak, helper
-module(router_soak_helper).

-include("router_soak_helper.hrl").

-export([
    start_memory_monitor/1,
    stop_memory_monitor/1,
    get_memory_snapshot/0,
    analyze_memory_trend/1,
    get_ets_snapshot/0,
    analyze_ets_growth/2,
    get_jetstream_lag/1,
    get_process_snapshot/0,
    generate_sustained_load/3,
    check_stability_criteria/2
]).

%% ============================================================================
%% Memory Monitoring
%% ============================================================================

%% @doc Start periodic memory monitoring
%% Interval: seconds between snapshots
-spec start_memory_monitor(pos_integer()) -> {ok, pid()}.
start_memory_monitor(IntervalSeconds) ->
    Pid = spawn_link(fun() -> memory_monitor_loop(IntervalSeconds, []) end),
    register(router_soak_memory_monitor, Pid),
    {ok, Pid}.

%% @doc Stop memory monitoring
-spec stop_memory_monitor(pid()) -> ok.
stop_memory_monitor(Pid) ->
    case is_process_alive(Pid) of
        true -> 
            Pid ! stop,
            ok;
        false -> 
            ok
    end.

memory_monitor_loop(IntervalSeconds, Snapshots) ->
    receive
        stop ->
            %% Simply terminate normally
            ok;
        {get_snapshots, From} ->
            From ! {snapshots, Snapshots},
            memory_monitor_loop(IntervalSeconds, Snapshots)
    after IntervalSeconds * 1000 ->
        Snapshot = get_memory_snapshot(),
        NewSnapshots = [Snapshot | Snapshots],
        memory_monitor_loop(IntervalSeconds, NewSnapshots)
    end.

%% @doc Get current memory snapshot
-spec get_memory_snapshot() -> #memory_snapshot{}.
get_memory_snapshot() ->
    Memory = erlang:memory(),
    #memory_snapshot{
        timestamp = erlang:system_time(millisecond),
        total = proplists:get_value(total, Memory, 0),
        processes = proplists:get_value(processes, Memory, 0),
        system = proplists:get_value(system, Memory, 0),
        atom = proplists:get_value(atom, Memory, 0),
        binary = proplists:get_value(binary, Memory, 0),
        code = proplists:get_value(code, Memory, 0),
        ets = proplists:get_value(ets, Memory, 0)
    }.

%% @doc Analyze memory trend to detect leaks
%% Returns: {ok, GrowthPercent, Trend} | {error, Reason}
%% Trend: stable | linear_growth | exponential_growth
-spec analyze_memory_trend([#memory_snapshot{}]) -> 
    {ok, float(), atom()} | {error, term()}.
analyze_memory_trend([]) ->
    {error, no_snapshots};
analyze_memory_trend([_]) ->
    {error, insufficient_snapshots};
analyze_memory_trend(Snapshots) ->
    %% Sort by timestamp
    Sorted = lists:sort(fun(A, B) -> 
        A#memory_snapshot.timestamp =< B#memory_snapshot.timestamp 
    end, Snapshots),
    
    First = hd(Sorted),
    Last = lists:last(Sorted),
    
    InitialMem = First#memory_snapshot.total,
    FinalMem = Last#memory_snapshot.total,
    
    GrowthPercent = ((FinalMem - InitialMem) / InitialMem) * 100,
    
    %% Analyze trend (simple linear regression)
    Trend = case GrowthPercent of
        G when G < 5.0 -> stable;
        G when G < 15.0 -> linear_growth;
        _ -> exponential_growth
    end,
    
    {ok, GrowthPercent, Trend}.

%% ============================================================================
%% ETS Table Monitoring
%% ============================================================================

%% @doc Get current ETS snapshot
-spec get_ets_snapshot() -> #ets_snapshot{}.
get_ets_snapshot() ->
    Tables = ets:all(),
    TableInfo = lists:map(fun(Table) ->
        try
            Size = ets:info(Table, size),
            Memory = ets:info(Table, memory),
            {Table, Size, Memory}
        catch
            _:_ -> {Table, 0, 0}
        end
    end, Tables),
    
    #ets_snapshot{
        timestamp = erlang:system_time(millisecond),
        tables = TableInfo
    }.

%% @doc Analyze ETS growth between two snapshots
-spec analyze_ets_growth(#ets_snapshot{}, #ets_snapshot{}) -> 
    {ok, [{atom(), integer(), float()}]} | {error, term()}.
analyze_ets_growth(#ets_snapshot{tables = InitialTables}, 
                   #ets_snapshot{tables = FinalTables}) ->
    Growth = lists:filtermap(fun({TableName, FinalSize, _FinalMem}) ->
        case lists:keyfind(TableName, 1, InitialTables) of
            {TableName, InitialSize, _} when InitialSize > 0 ->
                GrowthPercent = ((FinalSize - InitialSize) / InitialSize) * 100,
                {true, {TableName, FinalSize - InitialSize, GrowthPercent}};
            _ ->
                false
        end
    end, FinalTables),
    
    {ok, Growth}.

%% ============================================================================
%% JetStream Consumer Monitoring
%% ============================================================================

%% @doc Get JetStream consumer lag for a subject
-spec get_jetstream_lag(binary()) -> {ok, integer()} | {error, term()}.
get_jetstream_lag(Subject) ->
    %% Query pending messages from ETS cache
    try
        case ets:lookup(router_jetstream_pending_cache, Subject) of
            [{Subject, PendingCount, _Timestamp}] ->
                {ok, PendingCount};
            [] ->
                {ok, 0}
        end
    catch
        _:Reason ->
            {error, Reason}
    end.

%% ============================================================================
%% Process Monitoring
%% ============================================================================

%% @doc Get process snapshot
-spec get_process_snapshot() -> #process_snapshot{}.
get_process_snapshot() ->
    ProcessCount = erlang:system_info(process_count),
    
    %% Count supervisors
    AllProcesses = erlang:processes(),
    Supervisors = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, dictionary) of
            {dictionary, Dict} ->
                lists:keyfind('$initial_call', 1, Dict) =:= 
                    {'$initial_call', {supervisor, init, 1}};
            _ ->
                false
        end
    end, AllProcesses),
    
    %% Total message queue length
    MessageQueueTotal = lists:foldl(fun(Pid, Acc) ->
        case erlang:process_info(Pid, message_queue_len) of
            {message_queue_len, Len} -> Acc + Len;
            _ -> Acc
        end
    end, 0, AllProcesses),
    
    #process_snapshot{
        timestamp = erlang:system_time(millisecond),
        count = ProcessCount,
        supervisors = length(Supervisors),
        message_queue_total = MessageQueueTotal
    }.

%% ============================================================================
%% Load Generation
%% ============================================================================

%% @doc Generate sustained load for soak testing
%% RequestsPerSecond: Target request rate
%% DurationSeconds: How long to generate load
%% RequestFun: Function to execute for each request
-spec generate_sustained_load(pos_integer(), pos_integer(), fun(() -> any())) -> ok.
generate_sustained_load(RequestsPerSecond, DurationSeconds, RequestFun) ->
    IntervalMs = 1000 div RequestsPerSecond,
    EndTime = erlang:monotonic_time(millisecond) + (DurationSeconds * 1000),
    
    generate_load_loop(IntervalMs, EndTime, RequestFun).

generate_load_loop(IntervalMs, EndTime, RequestFun) ->
    Now = erlang:monotonic_time(millisecond),
    if
        Now >= EndTime ->
            ok;
        true ->
            %% Execute request
            spawn(fun() -> 
                try
                    RequestFun()
                catch
                    _:_ -> ok
                end
            end),
            
            %% Wait for next interval
            timer:sleep(IntervalMs),
            generate_load_loop(IntervalMs, EndTime, RequestFun)
    end.

%% ============================================================================
%% Stability Criteria Checking
%% ============================================================================

%% @doc Check if soak test meets stability criteria
-spec check_stability_criteria([#memory_snapshot{}], map()) -> 
    {pass, map()} | {fail, map()}.
check_stability_criteria(MemorySnapshots, Config) ->
    MaxMemoryGrowth = maps:get(max_memory_growth_percent, Config, 10.0),
    
    case analyze_memory_trend(MemorySnapshots) of
        {ok, GrowthPercent, Trend} ->
            Checks = #{
                memory_growth_ok => GrowthPercent =< MaxMemoryGrowth,
                trend => Trend,
                growth_percent => GrowthPercent
            },
            
            case GrowthPercent =< MaxMemoryGrowth of
                true -> {pass, Checks};
                false -> {fail, Checks}
            end;
        {error, Reason} ->
            {fail, #{error => Reason}}
    end.
