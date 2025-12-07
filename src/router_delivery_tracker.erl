%% @doc JetStream delivery count tracker using ETS
%% Tracks per-message delivery counts to approximate MaxDeliver exhaustion client-side
-module(router_delivery_tracker).
-export([
    ensure_table/1,
    increment/2,
    get_count/2,
    cleanup/2
]).

-define(DEFAULT_MAX_DELIVER, 5).

%% Ensure ETS table exists
-spec ensure_table(atom()) -> ok.
ensure_table(Table) ->
    case ets:info(Table) of
        undefined ->
            _ = ets:new(Table, [named_table, ordered_set, public, {read_concurrency, true}, {write_concurrency, true}]),
            ok;
        _ -> ok
    end.

%% Increment delivery count for key and return new count
-spec increment(atom(), binary()) -> non_neg_integer().
increment(Table, Key) ->
    ensure_table(Table),
    ets:update_counter(Table, Key, {2, 1}, {Key, 0}).

%% Get current count (0 if not found)
-spec get_count(atom(), binary()) -> {ok, non_neg_integer()}.
get_count(Table, Key) ->
    ensure_table(Table),
    case ets:lookup(Table, Key) of
        [{_K, Count}] -> {ok, Count};
        [] -> {ok, 0}
    end.

%% Cleanup tracking entry
-spec cleanup(atom(), binary()) -> ok.
cleanup(Table, Key) ->
    ensure_table(Table),
    ets:delete(Table, Key),
    ok.

%% Internal: compare against configured MaxDeliver
