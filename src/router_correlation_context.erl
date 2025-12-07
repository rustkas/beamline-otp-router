%% @doc Correlation Context Module
%% Provides storage and retrieval of correlation context mappings
%% 
%% Features:
%% - assignment_id/request_id mapping storage
%% - Correlation context storage with TTL
%% - Lookup by assignment_id or request_id
%% - Automatic cleanup of expired entries
%%
%% @see INTEGRATION_GUIDE.md#correlation-context For integration guidelines
-module(router_correlation_context).

-export([
    store/3,
    store/4,
    lookup_by_assignment_id/1,
    lookup_by_request_id/1,
    get_correlation_context/1,
    get_correlation_context/2,
    delete/1,
    delete_by_assignment_id/1,
    delete_by_request_id/1,
    cleanup_expired/0,
    get_table_size/0,
    get_table_memory/0
]).

-include("beamline_router.hrl").

-define(TABLE, router_correlation_context).
-define(DEFAULT_TTL_SECONDS, 3600).  % 1 hour default TTL

%% @doc Store correlation context with default TTL
-spec store(binary(), binary(), map()) -> ok.
store(AssignmentId, RequestId, Context) ->
    store(AssignmentId, RequestId, Context, ?DEFAULT_TTL_SECONDS).

%% @doc Store correlation context with custom TTL
-spec store(binary(), binary(), map(), integer()) -> ok.
store(AssignmentId, RequestId, Context, TTLSeconds) when is_binary(AssignmentId), is_binary(RequestId), is_map(Context), is_integer(TTLSeconds), TTLSeconds > 0 ->
    ensure_table(),
    
    ExpiresAt = erlang:system_time(second) + TTLSeconds,
    
    %% Store by assignment_id
    EntryByAssignment = {
        {assignment_id, AssignmentId},
        RequestId,
        Context,
        ExpiresAt,
        erlang:system_time(millisecond)
    },
    ets:insert(?TABLE, EntryByAssignment),
    
    %% Store by request_id
    EntryByRequest = {
        {request_id, RequestId},
        AssignmentId,
        Context,
        ExpiresAt,
        erlang:system_time(millisecond)
    },
    ets:insert(?TABLE, EntryByRequest),
    
    %% Store full correlation map
    CorrelationMap = {
        {correlation, AssignmentId, RequestId},
        Context,
        ExpiresAt,
        erlang:system_time(millisecond)
    },
    ets:insert(?TABLE, CorrelationMap),
    
    ok.

%% @doc Lookup request_id by assignment_id
-spec lookup_by_assignment_id(binary()) -> {ok, binary(), map()} | {error, not_found}.
lookup_by_assignment_id(AssignmentId) when is_binary(AssignmentId) ->
    ensure_table(),
    Key = {assignment_id, AssignmentId},
    case ets:lookup(?TABLE, Key) of
        [{Key, RequestId, Context, ExpiresAt, _}] ->
            Now = erlang:system_time(second),
            case ExpiresAt >= Now of
                true ->
                    {ok, RequestId, Context};
                false ->
                    %% Entry expired, delete it
                    ets:delete(?TABLE, Key),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Lookup assignment_id by request_id
-spec lookup_by_request_id(binary()) -> {ok, binary(), map()} | {error, not_found}.
lookup_by_request_id(RequestId) when is_binary(RequestId) ->
    ensure_table(),
    Key = {request_id, RequestId},
    case ets:lookup(?TABLE, Key) of
        [{Key, AssignmentId, Context, ExpiresAt, _}] ->
            Now = erlang:system_time(second),
            case ExpiresAt >= Now of
                true ->
                    {ok, AssignmentId, Context};
                false ->
                    %% Entry expired, delete it
                    ets:delete(?TABLE, Key),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Get correlation context by assignment_id
-spec get_correlation_context(binary()) -> {ok, map()} | {error, not_found}.
get_correlation_context(AssignmentId) when is_binary(AssignmentId) ->
    case lookup_by_assignment_id(AssignmentId) of
        {ok, _RequestId, Context} ->
            {ok, Context};
        {error, not_found} = Error ->
            Error
    end.

%% @doc Get correlation context by assignment_id and request_id
-spec get_correlation_context(binary(), binary()) -> {ok, map()} | {error, not_found}.
get_correlation_context(AssignmentId, RequestId) when is_binary(AssignmentId), is_binary(RequestId) ->
    ensure_table(),
    Key = {correlation, AssignmentId, RequestId},
    case ets:lookup(?TABLE, Key) of
        [{Key, Context, ExpiresAt, _}] ->
            Now = erlang:system_time(second),
            case ExpiresAt >= Now of
                true ->
                    {ok, Context};
                false ->
                    %% Entry expired, delete it
                    ets:delete(?TABLE, Key),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Delete correlation context by assignment_id
-spec delete_by_assignment_id(binary()) -> ok.
delete_by_assignment_id(AssignmentId) when is_binary(AssignmentId) ->
    ensure_table(),
    case lookup_by_assignment_id(AssignmentId) of
        {ok, RequestId, _Context} ->
            %% Delete all related entries
            ets:delete(?TABLE, {assignment_id, AssignmentId}),
            ets:delete(?TABLE, {request_id, RequestId}),
            ets:delete(?TABLE, {correlation, AssignmentId, RequestId}),
            ok;
        {error, not_found} ->
            ok
    end.

%% @doc Delete correlation context by request_id
-spec delete_by_request_id(binary()) -> ok.
delete_by_request_id(RequestId) when is_binary(RequestId) ->
    ensure_table(),
    case lookup_by_request_id(RequestId) of
        {ok, AssignmentId, _Context} ->
            %% Delete all related entries
            ets:delete(?TABLE, {assignment_id, AssignmentId}),
            ets:delete(?TABLE, {request_id, RequestId}),
            ets:delete(?TABLE, {correlation, AssignmentId, RequestId}),
            ok;
        {error, not_found} ->
            ok
    end.

%% @doc Delete correlation context by assignment_id and request_id
-spec delete(binary()) -> ok.
delete(AssignmentId) when is_binary(AssignmentId) ->
    delete_by_assignment_id(AssignmentId).

%% @doc Cleanup expired entries
-spec cleanup_expired() -> {ok, integer()}.
cleanup_expired() ->
    ensure_table(),
    Now = erlang:system_time(second),
    Deleted = ets:foldl(fun(Entry, Acc) ->
        case Entry of
            {_Key, _Value1, _Value2, ExpiresAt, _} when ExpiresAt < Now ->
                ets:delete(?TABLE, element(1, Entry)),
                Acc + 1;
            {_Key, _Value1, ExpiresAt, _} when ExpiresAt < Now ->
                ets:delete(?TABLE, element(1, Entry)),
                Acc + 1;
            _ ->
                Acc
        end
    end, 0, ?TABLE),
    {ok, Deleted}.

%% @doc Get table size
-spec get_table_size() -> integer().
get_table_size() ->
    ensure_table(),
    case ets:info(?TABLE, size) of
        undefined -> 0;
        Size -> Size
    end.

%% @doc Get table memory usage
-spec get_table_memory() -> integer().
get_table_memory() ->
    ensure_table(),
    case ets:info(?TABLE, memory) of
        undefined -> 0;
        Memory -> Memory * erlang:system_info(wordsize)
    end.

%% Internal: Ensure ETS table exists
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [
                named_table,
                set,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

