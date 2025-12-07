%% @doc Connection Pool Management Module
%% Provides structure and helpers for connection pooling (NATS, gRPC, etc.)
%%
%% Features:
%% - Connection pool state management
%% - Connection lifecycle tracking
%% - Pool metrics and monitoring
%% - Connection utilization tracking
%%
%% ⚠️ NOTE: Actual connection pooling requires external client libraries
%% This module provides structure and helpers for when pooling is implemented
%%
%% @see PERFORMANCE_GUIDE.md#connection-pooling For connection pooling guidelines
-module(router_connection_pool).

-export([
    create_pool/2,
    get_pool_config/1,
    get_pool_size/1,
    get_pool_utilization/1,
    acquire_connection/1,
    release_connection/2,
    get_pool_metrics/1,
    track_connection_usage/3
]).

-include("beamline_router.hrl").

%% @doc Create connection pool
%% PoolName: Name of the pool
%% Config: Pool configuration map
%% Returns: {ok, PoolState} | {error, Reason}
-spec create_pool(atom(), map()) -> {ok, map()} | {error, term()}.
create_pool(PoolName, Config) when is_atom(PoolName), is_map(Config) ->
    try
        %% Validate config
        case validate_pool_config(Config) of
            {ok, ValidConfig} ->
                %% Create pool state
                PoolState = #{
                    name => PoolName,
                    max_connections => maps:get(max_connections, ValidConfig, 10),
                    min_connections => maps:get(min_connections, ValidConfig, 2),
                    current_connections => 0,
                    active_connections => 0,
                    idle_connections => 0,
                    created_at => erlang:system_time(millisecond),
                    config => ValidConfig
                },
                
                %% Store pool state in ETS
                store_pool_state(PoolName, PoolState),
                
                %% Emit pool created metric
                router_metrics:emit_metric(router_connection_pool_created_total, #{count => 1}, #{
                    pool_name => atom_to_binary(PoolName, utf8),
                    max_connections => integer_to_binary(maps:get(max_connections, ValidConfig, 10))
                }),
                
                {ok, PoolState};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:CatchReason ->
            router_logger:error(<<"Failed to create connection pool">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(CatchReason),
                <<"event">> => <<"connection_pool_create_failed">>
            }),
            {error, CatchReason}
    end.

%% @doc Validate pool configuration
-spec validate_pool_config(map()) -> {ok, map()} | {error, term()}.
validate_pool_config(Config) ->
    MaxConnections = maps:get(max_connections, Config, 10),
    MinConnections = maps:get(min_connections, Config, 2),
    
    case MaxConnections > 0 andalso MinConnections >= 0 andalso MinConnections =< MaxConnections of
        true ->
            ValidConfig = maps:merge(Config, #{
                max_connections => MaxConnections,
                min_connections => MinConnections
            }),
            {ok, ValidConfig};
        false ->
            router_logger:error(<<"Invalid pool configuration">>, #{
                <<"max_connections">> => MaxConnections,
                <<"min_connections">> => MinConnections,
                <<"event">> => <<"pool_config_validation_failed">>
            }),
            {error, invalid_pool_config}
    end.

%% @doc Get pool configuration
-spec get_pool_config(atom()) -> {ok, map()} | {error, term()}.
get_pool_config(PoolName) ->
    try
        Table = router_connection_pools,
        case ets:whereis(Table) of
            undefined ->
                {error, pool_not_found};
            _Tid ->
                case ets:lookup(Table, PoolName) of
                    [{PoolName, PoolState}] ->
                        Config = maps:get(config, PoolState, #{}),
                        {ok, Config};
                    [] ->
                        {error, pool_not_found}
                end
        end
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to get pool configuration">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"pool_config_get_failed">>
            }),
            {error, Reason}
    end.

%% @doc Get pool size (current number of connections)
-spec get_pool_size(atom()) -> {ok, integer()} | {error, term()}.
get_pool_size(PoolName) ->
    try
        Table = router_connection_pools,
        case ets:whereis(Table) of
            undefined ->
                {error, pool_not_found};
            _Tid ->
                case ets:lookup(Table, PoolName) of
                    [{PoolName, PoolState}] ->
                        CurrentConnections = maps:get(current_connections, PoolState, 0),
                        {ok, CurrentConnections};
                    [] ->
                        {error, pool_not_found}
                end
        end
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to get pool size">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"pool_size_get_failed">>
            }),
            {error, Reason}
    end.

%% @doc Get pool utilization (active connections / max connections)
-spec get_pool_utilization(atom()) -> {ok, float()} | {error, term()}.
get_pool_utilization(PoolName) ->
    try
        Table = router_connection_pools,
        case ets:whereis(Table) of
            undefined ->
                {error, pool_not_found};
            _Tid ->
                case ets:lookup(Table, PoolName) of
                    [{PoolName, PoolState}] ->
                        ActiveConnections = maps:get(active_connections, PoolState, 0),
                        MaxConnections = maps:get(max_connections, maps:get(config, PoolState, #{}), 10),
                        Utilization = case MaxConnections > 0 of
                            true -> min(ActiveConnections / MaxConnections, 1.0);
                            false -> 0.0
                        end,
                        {ok, Utilization};
                    [] ->
                        {error, pool_not_found}
                end
        end
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to get pool utilization">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"pool_utilization_get_failed">>
            }),
            {error, Reason}
    end.

%% @doc Acquire connection from pool (stub - returns connection ID)
%% Returns: {ok, ConnectionId} | {error, Reason}
-spec acquire_connection(atom()) -> {ok, binary()} | {error, term()}.
acquire_connection(PoolName) when is_atom(PoolName) ->
    try
        Table = router_connection_pools,
        case ets:whereis(Table) of
            undefined ->
                {error, pool_not_found};
            _Tid ->
                case ets:lookup(Table, PoolName) of
                    [{PoolName, PoolState}] ->
                        ActiveConnections = maps:get(active_connections, PoolState, 0),
                        MaxConnections = maps:get(max_connections, maps:get(config, PoolState, #{}), 10),
                        case ActiveConnections < MaxConnections of
                            true ->
                                %% Generate connection ID
                                ConnectionId = generate_connection_id(PoolName),
                                %% Update pool state
                                UpdatedState = maps:update_with(active_connections, fun(C) -> C + 1 end, 1, PoolState),
                                UpdatedState2 = maps:update_with(current_connections, fun(C) -> max(C, ActiveConnections + 1) end, 1, UpdatedState),
                                store_pool_state(PoolName, UpdatedState2),
                                
                                %% Track connection usage
                                track_connection_usage(PoolName, ConnectionId, acquired),
                                
                                {ok, ConnectionId};
                            false ->
                                router_logger:warn(<<"Connection pool exhausted">>, #{
                                    <<"pool_name">> => atom_to_binary(PoolName, utf8),
                                    <<"active_connections">> => ActiveConnections,
                                    <<"max_connections">> => MaxConnections,
                                    <<"event">> => <<"pool_exhausted">>
                                }),
                                {error, pool_exhausted}
                        end;
                    [] ->
                        {error, pool_not_found}
                end
        end
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to acquire connection">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"connection_acquire_failed">>
            }),
            {error, Reason}
    end.

%% @doc Release connection back to pool
-spec release_connection(atom(), binary()) -> ok | {error, term()}.
release_connection(PoolName, ConnectionId) when is_atom(PoolName), is_binary(ConnectionId) ->
    %% Validate inputs
    case ConnectionId =/= <<>> of
        false ->
            router_logger:warn(<<"Invalid connection ID provided for release">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"event">> => <<"connection_release_invalid_id">>
            }),
            {error, invalid_connection_id};
        true ->
            release_connection_impl(PoolName, ConnectionId)
    end.

release_connection_impl(PoolName, ConnectionId) ->
    try
        Table = router_connection_pools,
        case ets:whereis(Table) of
            undefined ->
                {error, pool_not_found};
            _Tid ->
                case ets:lookup(Table, PoolName) of
                    [{PoolName, PoolState}] ->
                        ActiveConnections = maps:get(active_connections, PoolState, 0),
                        case ActiveConnections > 0 of
                            true ->
                                %% Update pool state
                                UpdatedState = maps:update_with(active_connections, fun(C) -> max(0, C - 1) end, 0, PoolState),
                                UpdatedState2 = maps:update_with(idle_connections, fun(C) -> C + 1 end, 1, UpdatedState),
                                store_pool_state(PoolName, UpdatedState2),
                                
                                %% Track connection usage
                                track_connection_usage(PoolName, ConnectionId, released),
                                
                                ok;
                            false ->
                                ok
                        end;
                    [] ->
                        {error, pool_not_found}
                end
        end
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to release connection">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"connection_id">> => ConnectionId,
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"connection_release_failed">>
            }),
            ok
    end.

%% @doc Get pool metrics
-spec get_pool_metrics(atom()) -> {ok, map()} | {error, term()}.
get_pool_metrics(PoolName) ->
    try
        Table = router_connection_pools,
        case ets:whereis(Table) of
            undefined ->
                {error, pool_not_found};
            _Tid ->
                case ets:lookup(Table, PoolName) of
                    [{PoolName, PoolState}] ->
                        Config = maps:get(config, PoolState, #{}),
                        Metrics = #{
                            pool_name => PoolName,
                            max_connections => maps:get(max_connections, Config, 10),
                            min_connections => maps:get(min_connections, Config, 2),
                            current_connections => maps:get(current_connections, PoolState, 0),
                            active_connections => maps:get(active_connections, PoolState, 0),
                            idle_connections => maps:get(idle_connections, PoolState, 0),
                            utilization => case maps:get(max_connections, Config, 10) > 0 of
                                true -> maps:get(active_connections, PoolState, 0) / maps:get(max_connections, Config, 10);
                                false -> 0.0
                            end,
                            created_at => maps:get(created_at, PoolState, 0)
                        },
                        {ok, Metrics};
                    [] ->
                        {error, pool_not_found}
                end
        end
    catch
        Class:Reason ->
            router_logger:error(<<"Failed to get pool metrics">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"error">> => Class,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"pool_metrics_get_failed">>
            }),
            {error, Reason}
    end.

%% @doc Track connection usage
-spec track_connection_usage(atom(), binary(), atom()) -> ok.
track_connection_usage(PoolName, ConnectionId, Action) when is_atom(PoolName), is_binary(ConnectionId), is_atom(Action) ->
    try
        %% Initialize connection usage table if needed
        Table = router_connection_usage,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, bag, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        %% Track usage
        Usage = {
            PoolName,
            ConnectionId,
            Action,
            erlang:system_time(millisecond)
        },
        ets:insert(Table, Usage),
        
        %% Emit metric
        router_metrics:emit_metric(router_connection_pool_usage_total, #{count => 1}, #{
            pool_name => atom_to_binary(PoolName, utf8),
            action => atom_to_binary(Action, utf8)
        }),
        
        ok
    catch
        Error:Reason ->
            router_logger:debug(<<"Failed to track connection usage">>, #{
                <<"pool_name">> => atom_to_binary(PoolName, utf8),
                <<"connection_id">> => ConnectionId,
                <<"action">> => atom_to_binary(Action, utf8),
                <<"error">> => Error,
                <<"reason">> => sanitize_error_for_logging(Reason),
                <<"event">> => <<"connection_usage_tracking_failed">>
            }),
            ok
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

%% @doc Store pool state in ETS
-spec store_pool_state(atom(), map()) -> ok.
store_pool_state(PoolName, PoolState) ->
    Table = router_connection_pools,
    case ets:whereis(Table) of
        undefined ->
            _ = ets:new(Table, [named_table, public, {write_concurrency, true}]);
        _ ->
            ok
    end,
    ets:insert(Table, {PoolName, PoolState}),
    ok.

%% @doc Generate connection ID
-spec generate_connection_id(atom()) -> binary().
generate_connection_id(PoolName) ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    <<(atom_to_binary(PoolName, utf8))/binary, "-", (integer_to_binary(Timestamp))/binary, "-", (integer_to_binary(Random))/binary>>.

