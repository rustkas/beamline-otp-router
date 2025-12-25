%% @doc ETS helpers for CT suites.
%% Provides safe named-table creation and lookup helpers to avoid badarg.
-module(router_ets_helpers).

-export([
    ensure_named_ets_table/2,
    ets_lookup/2,
    ets_delete_all/1
]).

-spec ensure_named_ets_table(atom(), [term()]) -> atom().
ensure_named_ets_table(TableName, Options) when is_atom(TableName), is_list(Options) ->
    NamedOptions = ensure_named_option(Options),
    ok = router_test_init:ensure_ets_table(TableName, NamedOptions),
    TableName.

-spec ets_lookup(atom(), term()) -> list().
ets_lookup(TableName, Key) when is_atom(TableName) ->
    case ets:info(TableName, name) of
        undefined -> [];
        _ -> ets:lookup(TableName, Key)
    end.

-spec ets_delete_all(atom()) -> ok.
ets_delete_all(TableName) when is_atom(TableName) ->
    case ets:info(TableName, name) of
        undefined -> ok;
        _ ->
            ets:delete_all_objects(TableName),
            ok
    end.

ensure_named_option(Options) ->
    case lists:member(named_table, Options) of
        true -> Options;
        false -> [named_table | Options]
    end.
