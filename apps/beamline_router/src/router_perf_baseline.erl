-module(router_perf_baseline).

-export([
    load_baseline/1,
    load_observations/1,
    validate_suite/3,
    validate/2
]).

-type json_map() :: map().
-type suite_name() :: binary() | atom().

-spec load_baseline(file:filename_all()) -> {ok, json_map()} | {error, term()}.
load_baseline(Path) ->
    load_json(Path).

-spec load_observations(file:filename_all()) -> {ok, json_map()} | {error, term()}.
load_observations(Path) ->
    load_json(Path).

-spec validate_suite(json_map(), suite_name(), json_map()) -> ok | {error, [term()]}.
validate_suite(Baseline, Suite0, ObservedMetrics) ->
    case find_suite_entry(Baseline, Suite0) of
        {ok, Entry} ->
            Tolerance = get_map(Entry, <<"tolerance">>, #{}),
            BaselineMetrics = get_map(Entry, <<"metrics">>, #{}),
            Errors =
                validate_latency(BaselineMetrics, Tolerance, ObservedMetrics) ++
                validate_throughput(BaselineMetrics, Tolerance, ObservedMetrics) ++
                validate_error_rate(Tolerance, ObservedMetrics),
            case Errors of
                [] -> ok;
                _ -> {error, Errors}
            end;
        {error, Reason} ->
            {error, [Reason]}
    end.

%% Observations can be either:
%% - #{<<"suites">> => [#{<<"suite">> => <<"name">>, <<"metrics">> => #{...}}...]}
%% - or a map keyed by suite name: #{<<"suite">> => #{...metrics...}, ...}
-spec validate(json_map(), json_map()) -> ok | {error, [term()]}.
validate(Baseline, Observations0) ->
    Observations = normalize_observations(Observations0),
    SuiteBins = maps:keys(Observations),
    Errors = lists:flatmap(
        fun(SuiteBin) ->
            Observed = maps:get(SuiteBin, Observations),
            case validate_suite(Baseline, SuiteBin, Observed) of
                ok -> [];
                {error, SuiteErrors} -> [{suite_failed, SuiteBin, SuiteErrors}]
            end
        end,
        SuiteBins
    ),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%% ------------------------
%% JSON load
%% ------------------------

-spec load_json(file:filename_all()) -> {ok, json_map()} | {error, term()}.
load_json(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try
                {ok, jsx:decode(Bin, [return_maps])}
            catch
                Class:Reason:Stack ->
                    {error, {json_decode_failed, Path, Class, Reason, Stack}}
            end;
        {error, Reason} ->
            {error, {read_failed, Path, Reason}}
    end.

%% ------------------------
%% Observations normalization
%% ------------------------

-spec normalize_observations(json_map()) -> json_map().
normalize_observations(Obs) ->
    case maps:get(<<"suites">>, Obs, undefined) of
        Suites when is_list(Suites) ->
            lists:foldl(
                fun(E, Acc) ->
                    Suite = get_bin(E, <<"suite">>, <<>>),
                    Metrics = get_map(E, <<"metrics">>, #{}),
                    case Suite of
                        <<>> -> Acc;
                        _ -> Acc#{Suite => Metrics}
                    end
                end,
                #{},
                Suites
            );
        _ ->
            Obs
    end.

%% ------------------------
%% Suite lookup
%% ------------------------

-spec find_suite_entry(json_map(), suite_name()) -> {ok, json_map()} | {error, term()}.
find_suite_entry(Baseline, Suite0) ->
    Suite = normalize_suite_name(Suite0),
    Suites = get_list(Baseline, <<"suites">>, []),
    case lists:filter(fun(E) -> get_bin(E, <<"suite">>, <<>>) =:= Suite end, Suites) of
        [Entry | _] -> {ok, Entry};
        [] -> {error, {suite_not_found, Suite}}
    end.

-spec normalize_suite_name(suite_name()) -> binary().
normalize_suite_name(Suite) when is_atom(Suite) ->
    atom_to_binary(Suite, utf8);
normalize_suite_name(Suite) when is_binary(Suite) ->
    Suite.

%% ------------------------
%% Validators
%% ------------------------

-spec validate_latency(json_map(), json_map(), json_map()) -> [term()].
validate_latency(BaselineMetrics, Tolerance, Observed) ->
    BLat = get_map(BaselineMetrics, <<"latency_ms">>, #{}),
    OLat = get_map(Observed, <<"latency_ms">>, #{}),
    TLat = get_map(Tolerance, <<"latency_ms">>, #{}),
    validate_percentiles(<<"latency_ms">>, BLat, TLat, OLat, [<<"p50">>, <<"p95">>, <<"p99">>]).

-spec validate_percentiles(binary(), json_map(), json_map(), json_map(), [binary()]) -> [term()].
validate_percentiles(Group, BGroup, TGroup, OGroup, Keys) ->
    lists:flatmap(
        fun(Key) ->
            B = get_number(BGroup, Key, undefined),
            O = get_number(OGroup, Key, undefined),
            Tol = get_map(TGroup, Key, #{}),
            case {B, O} of
                {undefined, _} -> [];
                {_, undefined} -> [{missing_observed_metric, Group, Key}];
                _ ->
                    case check_latency_tolerance(B, O, Tol) of
                        ok -> [];
                        {error, Reason} ->
                            [{threshold_failed, Group, Key, #{baseline => B, observed => O, tolerance => Tol, reason => Reason}}]
                    end
            end
        end,
        Keys
    ).

-spec validate_throughput(json_map(), json_map(), json_map()) -> [term()].
validate_throughput(BaselineMetrics, Tolerance, Observed) ->
    BThr = get_map(BaselineMetrics, <<"throughput_rps">>, #{}),
    OThr = get_map(Observed, <<"throughput_rps">>, #{}),
    TThr = get_map(Tolerance, <<"throughput_rps">>, #{}),
    B = get_number(BThr, <<"avg">>, undefined),
    O = get_number(OThr, <<"avg">>, undefined),
    Tol = get_map(TThr, <<"avg">>, #{}),
    case {B, O} of
        {undefined, _} -> [];
        {_, undefined} -> [{missing_observed_metric, <<"throughput_rps">>, <<"avg">>}];
        _ ->
            case check_throughput_tolerance(B, O, Tol) of
                ok -> [];
                {error, Reason} ->
                    [{threshold_failed, <<"throughput_rps">>, <<"avg">>, #{baseline => B, observed => O, tolerance => Tol, reason => Reason}}]
            end
    end.

%% Error-rate baseline value is optional; tolerance defines max_value.
-spec validate_error_rate(json_map(), json_map()) -> [term()].
validate_error_rate(Tolerance, Observed) ->
    OER = get_map(Observed, <<"error_rate">>, #{}),
    TER = get_map(Tolerance, <<"error_rate">>, #{}),
    O = get_number(OER, <<"fraction">>, undefined),
    Tol = get_map(TER, <<"fraction">>, #{}),
    case O of
        undefined -> [{missing_observed_metric, <<"error_rate">>, <<"fraction">>}];
        _ ->
            case check_error_rate_tolerance(O, Tol) of
                ok -> [];
                {error, Reason} ->
                    [{threshold_failed, <<"error_rate">>, <<"fraction">>, #{observed => O, tolerance => Tol, reason => Reason}}]
            end
    end.

%% latency tolerance:
%% - mode=relative: observed <= baseline * (1 + max_increase_fraction)
%% - mode=absolute: observed <= baseline + max_increase_ms
-spec check_latency_tolerance(number(), number(), json_map()) -> ok | {error, term()}.
check_latency_tolerance(Baseline, Observed, Tol) ->
    Mode = get_bin(Tol, <<"mode">>, <<"relative">>),
    case Mode of
        <<"relative">> ->
            MaxFrac = get_number(Tol, <<"max_increase_fraction">>, 0.0),
            Limit = Baseline * (1.0 + MaxFrac),
            if Observed =< Limit -> ok; true -> {error, {gt_limit, Limit}} end;
        <<"absolute">> ->
            MaxMs = get_number(Tol, <<"max_increase_ms">>, 0),
            Limit = Baseline + MaxMs,
            if Observed =< Limit -> ok; true -> {error, {gt_limit, Limit}} end;
        _ ->
            {error, {unknown_tolerance_mode, Mode}}
    end.

%% throughput tolerance:
%% - mode=relative: observed >= baseline * (1 - max_drop_fraction)
-spec check_throughput_tolerance(number(), number(), json_map()) -> ok | {error, term()}.
check_throughput_tolerance(Baseline, Observed, Tol) ->
    Mode = get_bin(Tol, <<"mode">>, <<"relative">>),
    case Mode of
        <<"relative">> ->
            MaxDrop = get_number(Tol, <<"max_drop_fraction">>, 0.0),
            Limit = Baseline * (1.0 - MaxDrop),
            if Observed >= Limit -> ok; true -> {error, {lt_limit, Limit}} end;
        _ ->
            {error, {unknown_throughput_tolerance_mode, Mode}}
    end.

%% error-rate tolerance:
%% - mode=absolute: observed <= max_value
-spec check_error_rate_tolerance(number(), json_map()) -> ok | {error, term()}.
check_error_rate_tolerance(Observed, Tol) ->
    Mode = get_bin(Tol, <<"mode">>, <<"absolute">>),
    case Mode of
        <<"absolute">> ->
            MaxValue = get_number(Tol, <<"max_value">>, 0.0),
            if Observed =< MaxValue -> ok; true -> {error, {gt_limit, MaxValue}} end;
        _ ->
            {error, {unknown_error_rate_tolerance_mode, Mode}}
    end.

%% ------------------------
%% Helpers
%% ------------------------

-spec get_map(json_map(), binary(), json_map()) -> json_map().
get_map(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_map(V) -> V;
        _ -> Default
    end.

-spec get_list(json_map(), binary(), list()) -> list().
get_list(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_list(V) -> V;
        _ -> Default
    end.

-spec get_bin(json_map(), binary(), binary()) -> binary().
get_bin(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_binary(V) -> V;
        _ -> Default
    end.

-spec get_number(json_map(), binary(), term()) -> term().
get_number(M, K, Default) ->
    case maps:get(K, M, undefined) of
        V when is_integer(V) -> V;
        V when is_float(V) -> V;
        _ -> Default
    end.
