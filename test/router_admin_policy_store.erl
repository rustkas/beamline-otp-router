%% @doc Test-only ETS store for mocked router_admin_grpc policies.
%% Encapsulates all ETS access to avoid races and test-suite noise.
-module(router_admin_policy_store).

-export([
    ensure/0,
    reset/0,
    put/2,
    get/1,
    list/1,
    delete/1
]).

-define(TABLE, router_admin_policies).

%% @doc Ensure ETS table exists.
%% Returns:
%%   ok              - table exists or was created
%%   {error, Reason} - creation failed
ensure() ->
    case catch ets:whereis(?TABLE) of
        undefined ->
            case catch ets:new(?TABLE, [named_table, public, set]) of
                {'EXIT', {badarg, _}} ->
                    %% Таблица уже создана параллельно.
                    ok;
                {'EXIT', Reason} ->
                    {error, Reason};
                _Tid ->
                    %% Любой валидный идентификатор таблицы.
                    ok
            end;
        {'EXIT', Reason} ->
            {error, Reason};
        _Tid ->
            %% Таблица уже существует (tid может печататься как router_admin_policies).
            ok
    end.

%% @doc Clear all data but keep table.
reset() ->
    case ensure() of
        ok ->
            ets:delete_all_objects(?TABLE),
            ok;
        Error ->
            Error
    end.

%% @doc Put {Key, Policy} into ETS.
%% Key is usually {TenantId, PolicyId}.
put(Key, Policy) ->
    case ensure() of
        ok ->
            true = ets:insert(?TABLE, {Key, Policy}),
            ok;
        Error ->
            Error
    end.

%% @doc Get policy by key.
%% Returns:
%%   {ok, Policy} | not_found | {error, Reason}
get(Key) ->
    case ensure() of
        ok ->
            case ets:lookup(?TABLE, Key) of
                [{Key, Policy}] ->
                    {ok, Policy};
                [] ->
                    not_found
            end;
        Error ->
            Error
    end.

%% @doc List all policies for tenant.
%% Returns list of Policy terms (no keys).
list(TenantId) ->
    case ensure() of
        ok ->
            %% Ключи: {TenantId, PolicyId}
            Pattern = {{TenantId, '_'}, '_'},
            Pairs = ets:match_object(?TABLE, Pattern),
            [Policy || {_Key, Policy} <- Pairs];
        {error, _Reason} ->
            []
    end.

%% @doc Delete policy by key.
%% Returns:
%%   ok | {error, Reason}
delete(Key) ->
    case ensure() of
        ok ->
            ets:delete(?TABLE, Key),
            ok;
        Error ->
            Error
    end.
