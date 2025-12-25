-module(router_grpc_connection).

-doc "gRPC Connection Handling Module".
%% Provides structure and helpers for gRPC connection management
%%
%% Features:
%% - Connection state tracking
%% - Connection lifecycle management
%% - Connection metrics
%% - Request/response tracking
%%
%% ⚠️ NOTE: Actual gRPC connection handling requires gRPC client changes
%% This module provides structure and helpers for when connection optimization is implemented
%%
%% @see PERFORMANCE_GUIDE.md#grpc-optimization For gRPC optimization guidelines

-export([
    track_connection/2,
    get_connection_state/1,
    track_request/2,
    track_response/3,
    get_connection_metrics/1,
    get_all_connections/0
]).

-include("beamline_router.hrl").

%% ConnectionId: Unique connection identifier
%% Config: Connection configuration map
%% Returns: {ok, ConnectionState} | {error, Reason}
-spec track_connection(binary(), map()) -> {ok, map()} | {error, term()}.
track_connection(ConnectionId, Config) when is_binary(ConnectionId), is_map(Config) ->
    try
        %% Create connection state
        ConnectionState = #{
            connection_id => ConnectionId,
            state => active,
            created_at => erlang:system_time(millisecond),
            last_activity => erlang:system_time(millisecond),
            request_count => 0,
            response_count => 0,
            error_count => 0,
            config => Config
        },
        
        %% Store connection state in ETS
        store_connection_state(ConnectionId, ConnectionState),
        
        %% Emit connection tracked metric
        router_metrics:emit_metric(router_grpc_connection_tracked_total, #{count => 1}, #{
            connection_id => ConnectionId
        }),
        
        {ok, ConnectionState}
    catch
        _:CatchReason ->
            {error, CatchReason}
    end.

-spec get_connection_state(binary()) -> {ok, map()} | {error, term()}.
get_connection_state(ConnectionId) ->
    try
        Table = router_grpc_connections,
        case ets:whereis(Table) of
            undefined ->
                {error, connection_not_found};
            _Tid ->
                case ets:lookup(Table, ConnectionId) of
                    [{ConnectionId, State}] ->
                        {ok, State};
                    [] ->
                        {error, connection_not_found}
                end
        end
    catch
        _:CatchReason ->
            {error, CatchReason}
    end.

-spec track_request(binary(), map()) -> ok.
track_request(ConnectionId, RequestInfo) when is_binary(ConnectionId), is_map(RequestInfo) ->
    try
        Table = router_grpc_connections,
        case ets:whereis(Table) of
            undefined ->
                ok;
            _Tid ->
                case ets:lookup(Table, ConnectionId) of
                    [{ConnectionId, State}] ->
                        %% Update connection state
                        UpdatedState = maps:update_with(request_count, fun(C) -> C + 1 end, 1, State),
                        UpdatedState2 = maps:put(last_activity, erlang:system_time(millisecond), UpdatedState),
                        store_connection_state(ConnectionId, UpdatedState2),
                        
                        %% Track request details
                        track_request_details(ConnectionId, RequestInfo),
                        
                        %% Emit metric
                        router_metrics:emit_metric(router_grpc_requests_total, #{count => 1}, #{
                            connection_id => ConnectionId,
                            method => maps:get(method, RequestInfo, ~"unknown")
                        });
                    [] ->
                        ok
                end
        end
    catch
        _:_ ->
            ok
    end.

-spec track_response(binary(), map(), atom()) -> ok.
track_response(ConnectionId, ResponseInfo, Status) when is_binary(ConnectionId), is_map(ResponseInfo), is_atom(Status) ->
    try
        Table = router_grpc_connections,
        case ets:whereis(Table) of
            undefined ->
                ok;
            _Tid ->
                case ets:lookup(Table, ConnectionId) of
                    [{ConnectionId, State}] ->
                        %% Update connection state
                        UpdatedState = maps:update_with(response_count, fun(C) -> C + 1 end, 1, State),
                        UpdatedState2 = case Status of
                            error -> maps:update_with(error_count, fun(C) -> C + 1 end, 1, UpdatedState);
                            _ -> UpdatedState
                        end,
                        UpdatedState3 = maps:put(last_activity, erlang:system_time(millisecond), UpdatedState2),
                        store_connection_state(ConnectionId, UpdatedState3),
                        
                        %% Track response details
                        track_response_details(ConnectionId, ResponseInfo, Status),
                        
                        %% Emit metric
                        router_metrics:emit_metric(router_grpc_responses_total, #{count => 1}, #{
                            connection_id => ConnectionId,
                            status => atom_to_binary(Status, utf8)
                        });
                    [] ->
                        ok
                end
        end
    catch
        _:_ ->
            ok
    end.

-spec get_connection_metrics(binary()) -> {ok, map()} | {error, term()}.
get_connection_metrics(ConnectionId) ->
    try
        case get_connection_state(ConnectionId) of
            {ok, State} ->
                Metrics = #{
                    connection_id => ConnectionId,
                    state => maps:get(state, State, unknown),
                    request_count => maps:get(request_count, State, 0),
                    response_count => maps:get(response_count, State, 0),
                    error_count => maps:get(error_count, State, 0),
                    created_at => maps:get(created_at, State, 0),
                    last_activity => maps:get(last_activity, State, 0)
                },
                {ok, Metrics};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:CatchReason ->
            {error, CatchReason}
    end.

-spec get_all_connections() -> [binary()].
get_all_connections() ->
    try
        Table = router_grpc_connections,
        case ets:whereis(Table) of
            undefined ->
                [];
            _Tid ->
                [ConnectionId || {ConnectionId, _} <- ets:tab2list(Table)]
        end
    catch
        _:_ ->
            []
    end.

-spec store_connection_state(binary(), map()) -> ok.
store_connection_state(ConnectionId, State) ->
    Table = router_grpc_connections,
    case ets:whereis(Table) of
        undefined ->
            _ = ets:new(Table, [named_table, public, {write_concurrency, true}]);
        _ ->
            ok
    end,
    ets:insert(Table, {ConnectionId, State}),
    ok.

-spec track_request_details(binary(), map()) -> ok.
track_request_details(ConnectionId, RequestInfo) ->
    try
        Table = router_grpc_request_tracking,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, bag, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        RequestTracking = {
            ConnectionId,
            erlang:system_time(millisecond),
            RequestInfo
        },
        ets:insert(Table, RequestTracking),
        ok
    catch
        _:_ ->
            ok
    end.

-spec track_response_details(binary(), map(), atom()) -> ok.
track_response_details(ConnectionId, ResponseInfo, Status) ->
    try
        Table = router_grpc_response_tracking,
        case ets:whereis(Table) of
            undefined ->
                _ = ets:new(Table, [named_table, public, bag, {write_concurrency, true}]);
            _ ->
                ok
        end,
        
        ResponseTracking = {
            ConnectionId,
            erlang:system_time(millisecond),
            Status,
            ResponseInfo
        },
        ets:insert(Table, ResponseTracking),
        ok
    catch
        _:_ ->
            ok
    end.

