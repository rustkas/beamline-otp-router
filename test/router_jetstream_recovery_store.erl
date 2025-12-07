%% @doc Test-only ETS store for router_jetstream_extended_recovery tests.
%% Encapsulates all ETS access for recovery state tracking, metrics, and test state.
-module(router_jetstream_recovery_store).

-export([
    ensure/0,
    reset/0,
    %% Metrics table
    record_telemetry_event/5,
    delete_telemetry_events/1,
    get_all_telemetry_events/0,
    %% Processed count
    init_processed_count/0,
    inc_processed_count/0,
    get_processed_count/0,
    delete_processed_count/0,
    %% Hang active flag
    init_hang_active/0,
    set_hang_active/1,
    get_hang_active/0,
    delete_hang_active/0,
    %% Connection state
    init_connection_state/0,
    set_connection_state/1,
    get_connection_state/0,
    delete_connection_state/0,
    %% Router state
    init_router_state/0,
    set_router_restarting/1,
    get_router_restarting/0,
    delete_router_state/0,
    %% Partition active flag
    init_partition_active/0,
    set_partition_active/1,
    get_partition_active/0,
    delete_partition_active/0,
    %% Cluster state
    init_cluster_state/0,
    set_cluster_nodes/1,
    set_cluster_quorum/1,
    get_cluster_nodes/0,
    get_cluster_quorum/0,
    delete_cluster_state/0,
    %% Region state
    init_region_state/0,
    set_region_a_connected/1,
    set_region_b_connected/1,
    get_region_a_connected/0,
    get_region_b_connected/0,
    delete_region_state/0,
    %% Router state (multi-router)
    init_router_state_multi/0,
    set_active_routers/1,
    set_restarting_router/1,
    get_active_routers/0,
    get_restarting_router/0,
    delete_router_state_multi/0,
    %% Pending table
    ensure_pending_table/0,
    put_pending/3,
    get_pending/1,
    delete_pending_table/0
]).

-define(METRICS_TABLE, router_jetstream_extended_recovery_metrics).
-define(PROCESSED_COUNT_TABLE, router_jetstream_processed_count).
-define(HANG_ACTIVE_TABLE, router_jetstream_hang_active).
-define(CONNECTION_STATE_TABLE, router_jetstream_connection_state).
-define(ROUTER_STATE_TABLE, router_jetstream_router_state).
-define(PARTITION_ACTIVE_TABLE, router_jetstream_partition_active).
-define(CLUSTER_STATE_TABLE, router_jetstream_cluster_state).
-define(REGION_STATE_TABLE, router_jetstream_region_state).
-define(ROUTER_STATE_MULTI_TABLE, router_jetstream_router_state_multi).
-define(PENDING_TABLE, router_jetstream_pending_cache).

%% @doc Ensure all ETS tables exist.
%% Returns:
%%   ok              - all tables exist or were created
%%   {error, Reason} - creation failed
ensure() ->
    Tables = [
        {?METRICS_TABLE, [set, named_table, public, {write_concurrency, true}]},
        {?PROCESSED_COUNT_TABLE, [set, named_table, public]},
        {?HANG_ACTIVE_TABLE, [set, named_table, public]},
        {?CONNECTION_STATE_TABLE, [set, named_table, public]},
        {?ROUTER_STATE_TABLE, [set, named_table, public]},
        {?PARTITION_ACTIVE_TABLE, [set, named_table, public]},
        {?CLUSTER_STATE_TABLE, [set, named_table, public]},
        {?REGION_STATE_TABLE, [set, named_table, public]},
        {?ROUTER_STATE_MULTI_TABLE, [set, named_table, public]},
        {?PENDING_TABLE, [set, named_table, public]}
    ],
    ensure_tables(Tables).

ensure_tables([]) ->
    ok;
ensure_tables([{Table, Options} | Rest]) ->
    case ensure_table(Table, Options) of
        ok ->
            ensure_tables(Rest);
        Error ->
            Error
    end.

ensure_table(Table, Options) ->
    case catch ets:whereis(Table) of
        undefined ->
            case catch ets:new(Table, Options) of
                {'EXIT', {badarg, _}} ->
                    %% Table already created in parallel.
                    ok;
                {'EXIT', Reason} ->
                    {error, Reason};
                _Tid ->
                    ok
            end;
        {'EXIT', Reason} ->
            {error, Reason};
        _Tid ->
            ok
    end.

%% @doc Clear all data but keep tables.
reset() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?METRICS_TABLE),
            ets:delete_all_objects(?PROCESSED_COUNT_TABLE),
            ets:delete_all_objects(?HANG_ACTIVE_TABLE),
            ets:delete_all_objects(?CONNECTION_STATE_TABLE),
            ets:delete_all_objects(?ROUTER_STATE_TABLE),
            ets:delete_all_objects(?PARTITION_ACTIVE_TABLE),
            ets:delete_all_objects(?CLUSTER_STATE_TABLE),
            ets:delete_all_objects(?REGION_STATE_TABLE),
            ets:delete_all_objects(?ROUTER_STATE_MULTI_TABLE),
            ets:delete_all_objects(?PENDING_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Metrics table
%% ========================================================================

record_telemetry_event(HandlerId, EventName, Measurements, Metadata, Timestamp) ->
    case ensure() of
        ok ->
            true = ets:insert(?METRICS_TABLE, {HandlerId, EventName, Measurements, Metadata, Timestamp}),
            ok;
        Error ->
            Error
    end.

delete_telemetry_events(HandlerId) ->
    case ensure() of
        ok ->
            ets:match_delete(?METRICS_TABLE, {HandlerId, '_', '_', '_', '_'}),
            ok;
        Error ->
            Error
    end.

get_all_telemetry_events() ->
    case ensure() of
        ok ->
            ets:tab2list(?METRICS_TABLE);
        Error ->
            {error, Error}
    end.

%% ========================================================================
%% Processed count
%% ========================================================================

init_processed_count() ->
    case ensure() of
        ok ->
            true = ets:insert(?PROCESSED_COUNT_TABLE, {count, 0}),
            ok;
        Error ->
            Error
    end.

inc_processed_count() ->
    case ensure() of
        ok ->
            Count = case ets:lookup(?PROCESSED_COUNT_TABLE, count) of
                [] -> 1;
                [{count, C}] -> C + 1
            end,
            true = ets:insert(?PROCESSED_COUNT_TABLE, {count, Count}),
            {ok, Count};
        Error ->
            Error
    end.

get_processed_count() ->
    case ensure() of
        ok ->
            case ets:lookup(?PROCESSED_COUNT_TABLE, count) of
                [{count, Count}] ->
                    {ok, Count};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_processed_count() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?PROCESSED_COUNT_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Hang active flag
%% ========================================================================

init_hang_active() ->
    case ensure() of
        ok ->
            true = ets:insert(?HANG_ACTIVE_TABLE, {active, false}),
            ok;
        Error ->
            Error
    end.

set_hang_active(Active) when is_boolean(Active) ->
    case ensure() of
        ok ->
            true = ets:insert(?HANG_ACTIVE_TABLE, {active, Active}),
            ok;
        Error ->
            Error
    end.

get_hang_active() ->
    case ensure() of
        ok ->
            case ets:lookup(?HANG_ACTIVE_TABLE, active) of
                [{active, Active}] ->
                    {ok, Active};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_hang_active() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?HANG_ACTIVE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Connection state
%% ========================================================================

init_connection_state() ->
    case ensure() of
        ok ->
            true = ets:insert(?CONNECTION_STATE_TABLE, {connected, true}),
            ok;
        Error ->
            Error
    end.

set_connection_state(Connected) when is_boolean(Connected) ->
    case ensure() of
        ok ->
            true = ets:insert(?CONNECTION_STATE_TABLE, {connected, Connected}),
            ok;
        Error ->
            Error
    end.

get_connection_state() ->
    case ensure() of
        ok ->
            case ets:lookup(?CONNECTION_STATE_TABLE, connected) of
                [{connected, Connected}] ->
                    {ok, Connected};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_connection_state() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?CONNECTION_STATE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Router state
%% ========================================================================

init_router_state() ->
    case ensure() of
        ok ->
            true = ets:insert(?ROUTER_STATE_TABLE, {restarting, false}),
            ok;
        Error ->
            Error
    end.

set_router_restarting(Restarting) when is_boolean(Restarting) ->
    case ensure() of
        ok ->
            true = ets:insert(?ROUTER_STATE_TABLE, {restarting, Restarting}),
            ok;
        Error ->
            Error
    end.

get_router_restarting() ->
    case ensure() of
        ok ->
            case ets:lookup(?ROUTER_STATE_TABLE, restarting) of
                [{restarting, Restarting}] ->
                    {ok, Restarting};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_router_state() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?ROUTER_STATE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Partition active flag
%% ========================================================================

init_partition_active() ->
    case ensure() of
        ok ->
            true = ets:insert(?PARTITION_ACTIVE_TABLE, {active, false}),
            ok;
        Error ->
            Error
    end.

set_partition_active(Active) when is_boolean(Active) ->
    case ensure() of
        ok ->
            true = ets:insert(?PARTITION_ACTIVE_TABLE, {active, Active}),
            ok;
        Error ->
            Error
    end.

get_partition_active() ->
    case ensure() of
        ok ->
            case ets:lookup(?PARTITION_ACTIVE_TABLE, active) of
                [{active, Active}] ->
                    {ok, Active};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_partition_active() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?PARTITION_ACTIVE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Cluster state
%% ========================================================================

init_cluster_state() ->
    case ensure() of
        ok ->
            true = ets:insert(?CLUSTER_STATE_TABLE, {nodes_available, 3}),
            true = ets:insert(?CLUSTER_STATE_TABLE, {quorum_available, true}),
            ok;
        Error ->
            Error
    end.

set_cluster_nodes(Nodes) when is_integer(Nodes), Nodes >= 0 ->
    case ensure() of
        ok ->
            true = ets:insert(?CLUSTER_STATE_TABLE, {nodes_available, Nodes}),
            ok;
        Error ->
            Error
    end.

set_cluster_quorum(Quorum) when is_boolean(Quorum) ->
    case ensure() of
        ok ->
            true = ets:insert(?CLUSTER_STATE_TABLE, {quorum_available, Quorum}),
            ok;
        Error ->
            Error
    end.

get_cluster_nodes() ->
    case ensure() of
        ok ->
            case ets:lookup(?CLUSTER_STATE_TABLE, nodes_available) of
                [{nodes_available, Nodes}] ->
                    {ok, Nodes};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

get_cluster_quorum() ->
    case ensure() of
        ok ->
            case ets:lookup(?CLUSTER_STATE_TABLE, quorum_available) of
                [{quorum_available, Quorum}] ->
                    {ok, Quorum};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_cluster_state() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?CLUSTER_STATE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Region state
%% ========================================================================

init_region_state() ->
    case ensure() of
        ok ->
            true = ets:insert(?REGION_STATE_TABLE, {region_a_connected, true}),
            true = ets:insert(?REGION_STATE_TABLE, {region_b_connected, true}),
            ok;
        Error ->
            Error
    end.

set_region_a_connected(Connected) when is_boolean(Connected) ->
    case ensure() of
        ok ->
            true = ets:insert(?REGION_STATE_TABLE, {region_a_connected, Connected}),
            ok;
        Error ->
            Error
    end.

set_region_b_connected(Connected) when is_boolean(Connected) ->
    case ensure() of
        ok ->
            true = ets:insert(?REGION_STATE_TABLE, {region_b_connected, Connected}),
            ok;
        Error ->
            Error
    end.

get_region_a_connected() ->
    case ensure() of
        ok ->
            case ets:lookup(?REGION_STATE_TABLE, region_a_connected) of
                [{region_a_connected, Connected}] ->
                    {ok, Connected};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

get_region_b_connected() ->
    case ensure() of
        ok ->
            case ets:lookup(?REGION_STATE_TABLE, region_b_connected) of
                [{region_b_connected, Connected}] ->
                    {ok, Connected};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_region_state() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?REGION_STATE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Router state (multi-router)
%% ========================================================================

init_router_state_multi() ->
    case ensure() of
        ok ->
            true = ets:insert(?ROUTER_STATE_MULTI_TABLE, {active_routers, 3}),
            true = ets:insert(?ROUTER_STATE_MULTI_TABLE, {restarting_router, none}),
            ok;
        Error ->
            Error
    end.

set_active_routers(Count) when is_integer(Count), Count >= 0 ->
    case ensure() of
        ok ->
            true = ets:insert(?ROUTER_STATE_MULTI_TABLE, {active_routers, Count}),
            ok;
        Error ->
            Error
    end.

set_restarting_router(Router) ->
    case ensure() of
        ok ->
            true = ets:insert(?ROUTER_STATE_MULTI_TABLE, {restarting_router, Router}),
            ok;
        Error ->
            Error
    end.

get_active_routers() ->
    case ensure() of
        ok ->
            case ets:lookup(?ROUTER_STATE_MULTI_TABLE, active_routers) of
                [{active_routers, Count}] ->
                    {ok, Count};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

get_restarting_router() ->
    case ensure() of
        ok ->
            case ets:lookup(?ROUTER_STATE_MULTI_TABLE, restarting_router) of
                [{restarting_router, Router}] ->
                    {ok, Router};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_router_state_multi() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?ROUTER_STATE_MULTI_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Pending table
%% ========================================================================

ensure_pending_table() ->
    ensure_table(?PENDING_TABLE, [set, named_table, public]).

put_pending(Subject, Value, Timestamp) ->
    case ensure_pending_table() of
        ok ->
            true = ets:insert(?PENDING_TABLE, {Subject, Value, Timestamp}),
            ok;
        Error ->
            Error
    end.

get_pending(Subject) ->
    case ensure_pending_table() of
        ok ->
            case ets:lookup(?PENDING_TABLE, Subject) of
                [{Subject, Value, Timestamp}] ->
                    {ok, Value, Timestamp};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

delete_pending_table() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?PENDING_TABLE),
            ok;
        Error ->
            Error
    end.

