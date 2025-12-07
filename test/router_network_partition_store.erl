%% @doc Test-only ETS store for router_network_partition tests.
%% Encapsulates all ETS access for leader state, pending routes, and partition metrics.
-module(router_network_partition_store).

-export([
    ensure/0,
    reset/0,
    %% Leader state
    init_leader_state/0,
    set_leader/1,
    get_leader/0,
    set_instance_state/2,
    get_instance_state/1,
    delete_leader_state/0,
    %% Pending table
    ensure_pending_table/0,
    put_pending/3,
    get_pending/1,
    delete_pending_table/0
]).

-define(LEADER_STATE_TABLE, router_network_partition_leader_state).
-define(PENDING_TABLE, router_jetstream_pending_cache).

%% @doc Ensure all ETS tables exist.
%% Returns:
%%   ok              - all tables exist or were created
%%   {error, Reason} - creation failed
ensure() ->
    Tables = [
        {?LEADER_STATE_TABLE, [set, named_table, public]},
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
            ets:delete_all_objects(?LEADER_STATE_TABLE),
            ets:delete_all_objects(?PENDING_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Leader state
%% ========================================================================

%% @doc Initialize leader state table.
-spec init_leader_state() -> ok | {error, term()}.
init_leader_state() ->
    case ensure() of
        ok ->
            true = ets:insert(?LEADER_STATE_TABLE, {leader, none}),
            ok;
        Error ->
            Error
    end.

%% @doc Set leader.
-spec set_leader(atom()) -> ok | {error, term()}.
set_leader(Leader) when is_atom(Leader) ->
    case ensure() of
        ok ->
            true = ets:insert(?LEADER_STATE_TABLE, {leader, Leader}),
            ok;
        Error ->
            Error
    end.

%% @doc Get leader.
-spec get_leader() -> {ok, atom()} | not_found | {error, term()}.
get_leader() ->
    case ensure() of
        ok ->
            case ets:lookup(?LEADER_STATE_TABLE, leader) of
                [{leader, Leader}] ->
                    {ok, Leader};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Set instance state.
-spec set_instance_state(atom(), atom()) -> ok | {error, term()}.
set_instance_state(Instance, State) when is_atom(Instance), is_atom(State) ->
    case ensure() of
        ok ->
            true = ets:insert(?LEADER_STATE_TABLE, {Instance, State}),
            ok;
        Error ->
            Error
    end.

%% @doc Get instance state.
-spec get_instance_state(atom()) -> {ok, atom()} | not_found | {error, term()}.
get_instance_state(Instance) when is_atom(Instance) ->
    case ensure() of
        ok ->
            case ets:lookup(?LEADER_STATE_TABLE, Instance) of
                [{Instance, State}] ->
                    {ok, State};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc Delete leader state table data.
-spec delete_leader_state() -> ok | {error, term()}.
delete_leader_state() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?LEADER_STATE_TABLE),
            ok;
        Error ->
            Error
    end.

%% ========================================================================
%% Pending table
%% ========================================================================

%% @doc Ensure pending table exists.
-spec ensure_pending_table() -> ok | {error, term()}.
ensure_pending_table() ->
    ensure_table(?PENDING_TABLE, [set, named_table, public]).

%% @doc Put pending entry.
-spec put_pending(binary(), integer(), integer()) -> ok | {error, term()}.
put_pending(Subject, Value, Timestamp) when is_binary(Subject), is_integer(Value), is_integer(Timestamp) ->
    case ensure_pending_table() of
        ok ->
            true = ets:insert(?PENDING_TABLE, {Subject, Value, Timestamp}),
            ok;
        Error ->
            Error
    end.

%% @doc Get pending entry.
-spec get_pending(binary()) -> {ok, integer(), integer()} | not_found | {error, term()}.
get_pending(Subject) when is_binary(Subject) ->
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

%% @doc Delete pending table data.
-spec delete_pending_table() -> ok | {error, term()}.
delete_pending_table() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?PENDING_TABLE),
            ok;
        Error ->
            Error
    end.

