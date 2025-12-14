-module(router_test_init).

-export([ensure_ets_table/2, reset_ets_table/1, delete_ets_table/1, ets_exists/1]).
-export([snapshot_ets/0, snapshot_ets/1, diff_ets_snapshot/2]).

%% @doc Create ETS table only if missing; otherwise wipe it
%% Returns table name/reference for consistency
ensure_ets_table(Name, Options) ->
    case ets:whereis(Name) of
        undefined -> ets:new(Name, Options);
        _Ref -> ets:delete_all_objects(Name), Name
    end.

%% @doc Reset (clear) an existing ETS table
%% Safe: does nothing if table doesn't exist
reset_ets_table(Name) ->
    case ets:whereis(Name) of
        undefined -> ok;
        _Ref -> ets:delete_all_objects(Name), ok
    end.

%% @doc Delete an ETS table if it exists
%% Safe: does nothing if table doesn't exist
delete_ets_table(Name) ->
    case ets:whereis(Name) of
        undefined -> ok;
        _Ref ->
            catch ets:delete(Name),
            ok
    end.

%% @doc Check if ETS table exists
ets_exists(Name) ->
    case ets:whereis(Name) of
        undefined -> false;
        _Ref -> true
    end.

%% @doc Snapshot all named ETS tables (for leak detection)
%% Returns list of table names
-spec snapshot_ets() -> [atom()].
snapshot_ets() ->
    snapshot_ets([]).

%% @doc Snapshot ETS tables matching prefix filter
%% Prefixes: list of atoms like [router_, beamline_, test_]
%% Empty list means all named tables
-spec snapshot_ets(Prefixes :: [atom()]) -> [atom()].
snapshot_ets([]) ->
    [Name || Tab <- ets:all(),
             is_atom(Name = ets:info(Tab, name)),
             Name =/= undefined];
snapshot_ets(Prefixes) ->
    [Name || Tab <- ets:all(),
             is_atom(Name = ets:info(Tab, name)),
             Name =/= undefined,
             matches_prefix(Name, Prefixes)].

matches_prefix(Name, Prefixes) ->
    NameStr = atom_to_list(Name),
    lists:any(fun(Prefix) ->
        lists:prefix(atom_to_list(Prefix), NameStr)
    end, Prefixes).

%% @doc Compute diff between two ETS snapshots
%% Returns {Added, Removed} where both are lists of table names
-spec diff_ets_snapshot(Before :: [atom()], After :: [atom()]) -> 
    {Added :: [atom()], Removed :: [atom()]}.
diff_ets_snapshot(Before, After) ->
    BeforeSet = sets:from_list(Before),
    AfterSet = sets:from_list(After),
    Added = sets:to_list(sets:subtract(AfterSet, BeforeSet)),
    Removed = sets:to_list(sets:subtract(BeforeSet, AfterSet)),
    {Added, Removed}.
