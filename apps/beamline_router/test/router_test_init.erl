%% # Test init helpers
%%
%% Common bootstrap logic for test suites:
%% - Create and track ETS tables used by tests
%% - Provide deterministic cleanup at the end of a suite
%%
%% The tracker table is `created_tables` (a named ETS set) containing:
%%   {TableName, true}
%%
-module(router_test_init).

-export([
    start/0,
    stop/0,
    ensure_ets_table/2,
    create_ets_table/2,
    delete_ets_table/1,
    get_created_tables/0,
    ets_exists/1,
    reset_ets_table/1,
    snapshot_ets/1,
    diff_ets_snapshot/2
]).

start() ->
    ensure_ets_table_exists(created_tables, [named_table, public, set]),
    ok.

stop() ->
    Tabs = get_created_tables(),
    lists:foreach(fun delete_ets_table/1, Tabs),
    delete_ets_table(created_tables),
    ok.

ensure_ets_table(TableName, Options) ->
    case ets:info(TableName, name) of
        undefined ->
            create_ets_table(TableName, Options),
            track_table(TableName),
            ok;
        _ ->
            ets:delete_all_objects(TableName),
            ok
    end.

create_ets_table(TableName, Options) ->
    _ = ets:new(TableName, Options),
    ok.

delete_ets_table(TableName) ->
    try ets:delete(TableName) of
        true -> ok
    catch
        _:_ -> ok
    end.

get_created_tables() ->
    case ets:info(created_tables, name) of
        undefined -> [];
        _ -> [Tab || {Tab, true} <- ets:tab2list(created_tables)]
    end.

ets_exists(TableName) ->
    case ets:info(TableName, name) of
        undefined -> false;
        _ -> true
    end.

reset_ets_table(TableName) ->
    case ets:info(TableName, name) of
        undefined -> ok;
        _ ->
            ets:delete_all_objects(TableName),
            ok
    end.

snapshot_ets(Prefixes) ->
    Tabs = ets:all(),
    [T || T <- Tabs, matches_prefixes(T, Prefixes)].

diff_ets_snapshot(Before, After) ->
    Added = After -- Before,
    Removed = Before -- After,
    {Added, Removed}.

%% Internal

ensure_ets_table_exists(TableName, Options) ->
    case ets:info(TableName, name) of
        undefined ->
            _ = ets:new(TableName, Options),
            ok;
        _ ->
            ok
    end.

track_table(TableName) ->
    try ets:insert(created_tables, {TableName, true}) of
        true -> ok
    catch
        _:_ -> ok
    end.

matches_prefixes(Tab, Prefixes) ->
    case table_name_to_list(Tab) of
        undefined ->
            false;
        TabStr ->
            lists:any(fun(P) ->
                lists:prefix(atom_to_list(P), TabStr)
            end, Prefixes)
    end.

table_name_to_list(Tab) when is_atom(Tab) ->
    atom_to_list(Tab);
table_name_to_list(Tab) when is_reference(Tab) ->
    case ets:info(Tab, name) of
        undefined -> undefined;
        Name when is_atom(Name) -> atom_to_list(Name);
        _ -> undefined
    end;
table_name_to_list(_Tab) ->
    undefined.
