-module(router_resource_monitor).

-doc "Resource Monitor".
%% Monitors ETS table sizes and memory usage for resource management
%% Provides functions to check table sizes, memory usage, and enforce limits
%%
%% Features:
%% - ETS table size monitoring
%% - Memory usage tracking
%% - Size limit enforcement with eviction
%% - Resource usage reporting
%%
%% @see PERFORMANCE_GUIDE.md#resource-management For resource management guidelines
-export([
    get_table_size/1,
    get_table_memory/1,
    get_all_table_sizes/0,
    get_all_table_memory/0,
    check_table_size_limit/2,
    enforce_table_size_limit/3,
    get_resource_stats/0
]).

-include("beamline_router.hrl").

-spec get_table_size(atom() | ets:tid()) -> integer() | undefined.
get_table_size(Table) ->
    case catch ets:info(Table, size) of
        Size when is_integer(Size) -> Size;
        _ -> undefined
    end.

-spec get_table_memory(atom() | ets:tid()) -> integer() | undefined.
get_table_memory(Table) ->
    case catch ets:info(Table, memory) of
        Memory when is_integer(Memory) -> Memory * erlang:system_info(wordsize);
        _ -> undefined
    end.

-spec get_all_table_sizes() -> [{atom(), integer()}].
get_all_table_sizes() ->
    KnownTables = [
        router_metrics,
        router_idem,
        rate_limits,
        policy_store,
        policy_store_index,
        router_circuit_breaker,
        rbac_roles,
        rbac_user_roles,
        rbac_permissions,
        tenant_quotas,
        audit_logs,
        router_rate_limits,
        router_jetstream_state,
        extension_registry,
        router_jetstream_pending_cache,
        router_processing_latency_cache,
        sticky_sessions,
        router_acl
    ],
    lists:foldl(fun(TableName, Acc) ->
        case get_table_size(TableName) of
            undefined -> Acc;
            Size -> [{TableName, Size} | Acc]
        end
    end, [], KnownTables).

-spec get_all_table_memory() -> [{atom(), integer()}].
get_all_table_memory() ->
    KnownTables = [
        router_metrics,
        router_idem,
        rate_limits,
        policy_store,
        policy_store_index,
        router_circuit_breaker,
        rbac_roles,
        rbac_user_roles,
        rbac_permissions,
        tenant_quotas,
        audit_logs,
        router_rate_limits,
        router_jetstream_state,
        extension_registry,
        router_jetstream_pending_cache,
        router_processing_latency_cache,
        sticky_sessions,
        router_acl
    ],
    lists:foldl(fun(TableName, Acc) ->
        case get_table_memory(TableName) of
            undefined -> Acc;
            Memory -> [{TableName, Memory} | Acc]
        end
    end, [], KnownTables).

-spec check_table_size_limit(atom() | ets:tid(), integer()) -> {ok, integer()} | {error, exceeded, integer(), integer()}.
check_table_size_limit(Table, MaxSize) when is_integer(MaxSize), MaxSize > 0 ->
    case get_table_size(Table) of
        undefined -> {error, table_not_found};
        Size when Size =< MaxSize -> {ok, Size};
        Size -> {error, exceeded, Size, MaxSize}
    end.

%% EvictionFunction: fun((Table, CountToEvict) -> EvictedCount)
-spec enforce_table_size_limit(atom() | ets:tid(), integer(), fun((atom() | ets:tid(), integer()) -> integer())) -> {ok, integer(), integer()} | {error, term()}.
enforce_table_size_limit(Table, MaxSize, EvictionFunction) when is_integer(MaxSize), MaxSize > 0, is_function(EvictionFunction, 2) ->
    case get_table_size(Table) of
        undefined -> {error, table_not_found};
        Size when Size =< MaxSize -> {ok, Size, 0};
        Size ->
            CountToEvict = Size - MaxSize,
            EvictedCount = EvictionFunction(Table, CountToEvict),
            NewSize = get_table_size(Table),
            {ok, NewSize, EvictedCount}
    end.

-spec get_resource_stats() -> map().
get_resource_stats() ->
    TableSizes = get_all_table_sizes(),
    TableMemory = get_all_table_memory(),
    TotalEntries = lists:sum([Size || {_, Size} <- TableSizes]),
    TotalMemory = lists:sum([Mem || {_, Mem} <- TableMemory]),
    
    #{
        ~"ets_tables" => #{
            ~"count" => length(TableSizes),
            ~"total_entries" => TotalEntries,
            ~"total_memory_bytes" => TotalMemory,
            ~"table_sizes" => maps:from_list([{atom_to_binary(Name, utf8), Size} || {Name, Size} <- TableSizes]),
            ~"table_memory" => maps:from_list([{atom_to_binary(Name, utf8), Mem} || {Name, Mem} <- TableMemory])
        },
        ~"vm_memory" => get_vm_memory_stats()
    }.

-spec get_vm_memory_stats() -> map().
get_vm_memory_stats() ->
    try
        MemoryInfo = erlang:memory(),
        #{
            ~"total" => proplists:get_value(total, MemoryInfo, 0),
            ~"processes" => proplists:get_value(processes, MemoryInfo, 0),
            ~"processes_used" => proplists:get_value(processes_used, MemoryInfo, 0),
            ~"system" => proplists:get_value(system, MemoryInfo, 0),
            ~"atom" => proplists:get_value(atom, MemoryInfo, 0),
            ~"atom_used" => proplists:get_value(atom_used, MemoryInfo, 0),
            ~"binary" => proplists:get_value(binary, MemoryInfo, 0),
            ~"code" => proplists:get_value(code, MemoryInfo, 0),
            ~"ets" => proplists:get_value(ets, MemoryInfo, 0)
        }
    catch
        _:_ -> #{}
    end.

