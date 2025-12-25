-module(router_perf_observations).

%% Minimal observations writer for perf suites.
%% - Uses jsx (maps + binary keys).
%% - Stores observations as: #{<<"suites">> => [Entry...]}
%% - Merges by (suite, level, workload.scenario, workload.degraded_instance).
%% - Uses file lock + atomic write (temp + rename).

-export([
    default_path/0,
    record/1,
    record/2
]).

-type json_map() :: map().

-define(DEFAULT_FILE, "perf_observations.json").

-spec default_path() -> file:filename_all().
default_path() ->
    Dir =
        case os:getenv("CT_LOGDIR") of
            false -> "_build/test/logs";
            "" -> "_build/test/logs";
            V -> V
        end,
    filename:join(Dir, ?DEFAULT_FILE).

%% Record an entry into the default observations file (CT_LOGDIR/perf_observations.json).
-spec record(json_map()) -> ok | {error, term()}.
record(Entry) ->
    record(default_path(), Entry).

%% Record an entry into the given observations file.
-spec record(file:filename_all(), json_map()) -> ok | {error, term()}.
record(Path, Entry0) ->
    Entry = normalize_entry(Entry0),
    case validate_entry(Entry) of
        ok ->
            ok = ensure_dir(filename:dirname(Path)),
            global:trans({router_perf_observations, Path}, fun() -> upsert_entry(Path, Entry) end);
        {error, _} = Err ->
            Err
    end.

%% -------------------------
%% Core logic
%% -------------------------

-spec upsert_entry(file:filename_all(), json_map()) -> ok | {error, term()}.
upsert_entry(Path, Entry) ->
    Doc0 = read_doc(Path),
    Suites0 = maps:get(<<"suites">>, Doc0, []),
    Suites1 = merge_suites(Suites0, Entry),
    Doc1 = Doc0#{<<"suites">> => Suites1},
    atomic_write_json(Path, Doc1).

-spec read_doc(file:filename_all()) -> json_map().
read_doc(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try
                Decoded = jsx:decode(Bin, [return_maps]),
                case Decoded of
                    M when is_map(M) -> M;
                    _ -> #{<<"suites">> => []}
                end
            catch
                _:_ -> #{<<"suites">> => []}
            end;
        {error, enoent} ->
            #{<<"suites">> => []};
        {error, _Reason} ->
            %% If unreadable, do not crash suites; start fresh.
            #{<<"suites">> => []}
    end.

-spec merge_suites(list(), json_map()) -> list().
merge_suites(Suites0, Entry) ->
    Key = entry_key(Entry),
    {Replaced, Suites1} = lists:foldl(
        fun(E, {DidReplace, Acc}) ->
            case entry_key(normalize_entry(E)) of
                Key ->
                    {true, [Entry | Acc]};
                _ ->
                    {DidReplace, [E | Acc]}
            end
        end,
        {false, []},
        Suites0
    ),
    Suites2 = lists:reverse(Suites1),
    case Replaced of
        true -> Suites2;
        false -> Suites2 ++ [Entry]
    end.

-spec entry_key(json_map()) -> {binary(), binary(), binary(), boolean()}.
entry_key(Entry) ->
    Suite = get_bin(Entry, <<"suite">>, <<>>),
    Level = get_bin(Entry, <<"level">>, <<>>),
    Workload = get_map(Entry, <<"workload">>, #{}),
    Scenario = get_bin(Workload, <<"scenario">>, <<>>),
    Degraded = get_bool(Workload, <<"degraded_instance">>, false),
    {Suite, Level, Scenario, Degraded}.

-spec atomic_write_json(file:filename_all(), json_map()) -> ok | {error, term()}.
atomic_write_json(Path, Doc) ->
    Tmp = Path ++ ".tmp",
    Bin = jsx:encode(Doc),
    case file:write_file(Tmp, Bin) of
        ok ->
            %% rename is atomic on the same filesystem
            case file:rename(Tmp, Path) of
                ok -> ok;
                {error, Reason} -> {error, {rename_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {write_failed, Reason}}
    end.

%% -------------------------
%% Entry normalization + validation
%% -------------------------

-spec normalize_entry(json_map()) -> json_map().
normalize_entry(E) when is_map(E) ->
    %% Ensure expected keys exist (do not invent metrics).
    Suite = get_bin(E, <<"suite">>, <<>>),
    Level = get_bin(E, <<"level">>, <<>>),
    Workload = get_map(E, <<"workload">>, #{}),
    Metrics = get_map(E, <<"metrics">>, #{}),
    E#{
        <<"suite">> => Suite,
        <<"level">> => Level,
        <<"workload">> => normalize_workload(Workload),
        <<"metrics">> => Metrics
    }.

-spec normalize_workload(json_map()) -> json_map().
normalize_workload(W) ->
    Scenario = get_bin(W, <<"scenario">>, <<>>),
    Degraded = get_bool(W, <<"degraded_instance">>, false),
    W#{
        <<"scenario">> => Scenario,
        <<"degraded_instance">> => Degraded
    }.

-spec validate_entry(json_map()) -> ok | {error, term()}.
validate_entry(E) ->
    Suite = get_bin(E, <<"suite">>, <<>>),
    Level = get_bin(E, <<"level">>, <<>>),
    Workload = get_map(E, <<"workload">>, #{}),
    Scenario = get_bin(Workload, <<"scenario">>, <<>>),
    Metrics = get_map(E, <<"metrics">>, #{}),
    case {Suite, Level, Scenario, map_size(Metrics)} of
        {<<>>, _, _, _} -> {error, {invalid_entry, missing_suite}};
        {_, <<>>, _, _} -> {error, {invalid_entry, missing_level}};
        {_, _, <<>>, _} -> {error, {invalid_entry, missing_workload_scenario}};
        {_, _, _, 0} -> {error, {invalid_entry, missing_metrics}};
        _ -> ok
    end.

%% -------------------------
%% Small helpers
%% -------------------------

-spec ensure_dir(file:filename_all()) -> ok.
ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> _ = filelib:ensure_dir(filename:join(Dir, "dummy")), ok
    end.

-spec get_map(json_map(), binary(), json_map()) -> json_map().
get_map(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_map(V) -> V;
        _ -> Default
    end.

-spec get_bin(json_map(), binary(), binary()) -> binary().
get_bin(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_binary(V) -> V;
        V when is_atom(V) -> atom_to_binary(V, utf8);
        V when is_list(V) -> list_to_binary(V);
        _ -> Default
    end.

-spec get_bool(json_map(), binary(), boolean()) -> boolean().
get_bool(M, K, Default) ->
    case maps:get(K, M, undefined) of
        true -> true;
        false -> false;
        _ -> Default
    end.
