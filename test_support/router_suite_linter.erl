-module(router_suite_linter).

-export([run/0]).

%% Run all lints and return ok | {error, Issues}
run() ->
    Suites = suite_files(),
    Baseline = load_baseline(),
    Strict = strict_mode(),
    Issues = lists:flatmap(fun(S) -> lint_suite(S, Baseline) end, Suites),
    case Issues of
        [] ->
            Mode = case Strict of true -> "strict"; false -> "baseline" end,
            io:format("router_suite_linter: ok (~p suites checked, mode=~s)~n", [length(Suites), Mode]),
            ok;
        _ ->
            lists:foreach(fun print_issue/1, Issues),
            {error, Issues}
    end.

%% ------------------------------------------------------------------
%% Internal helpers
%% ------------------------------------------------------------------

suite_files() ->
    case file:list_dir("test") of
        {ok, Entries} ->
            [filename:join("test", E) || E <- Entries, lists:suffix("_SUITE.erl", E)];
        {error, _} ->
            []
    end.

load_baseline() ->
    case file:consult("test_support/router_suite_linter_baseline.eterm") of
        {ok, [Map]} when is_map(Map) -> Map;
        _ -> #{}
    end.

strict_mode() ->
    case os:getenv("ROUTER_SUITE_LINTER_STRICT") of
        "1" -> true;
        "true" -> true;
        "strict" -> true;
        _ -> false
    end.

lint_suite(Path, Baseline) ->
    File = filename:basename(Path),
    Bin = case file:read_file(Path) of
        {ok, B} -> B;
        {error, Reason} ->
            [{File, read_error, Reason, 0, 0}]
    end,
    lint_suite(File, Bin, Baseline).

lint_suite(File, Bin, Baseline) when is_binary(Bin) ->
    Counts = count_rules(Bin),
    CallbackIssues = missing_callbacks(File, Bin),
    RuleIssues = compare_to_baseline(File, Counts, Baseline),
    CallbackIssues ++ RuleIssues.

missing_callbacks(File, Bin) ->
    Callbacks = [
        {init_per_suite, <<"\\binit_per_suite\\s*\\(">>},
        {end_per_suite, <<"\\bend_per_suite\\s*\\(">>},
        {init_per_testcase, <<"\\binit_per_testcase\\s*\\(">>},
        {end_per_testcase, <<"\\bend_per_testcase\\s*\\(">>}
    ],
    [ {File, missing_callback, Name, 0, 1}
      || {Name, Pattern} <- Callbacks,
         not has_match(Pattern, Bin)].

compare_to_baseline(File, Counts, Baseline) ->
    Rules = [meck_new, meck_new_in_init_per_testcase, ets_new, sleep],
    FileKey = list_to_binary(File),
    lists:flatmap(fun(Rule) ->
        Current = maps:get(Rule, Counts, 0),
        Allowed = allowed_for(FileKey, Rule, Baseline),
        case Current > Allowed of
            true -> [{File, Rule, Current, Allowed, Current - Allowed}];
            false -> []
        end
    end, Rules).

allowed_for(FileKey, Rule, Baseline) ->
    case strict_mode() of
        true when Rule =:= meck_new_in_init_per_testcase; Rule =:= ets_new ->
            0;
        _ ->
            BaselineCounts = maps:get(Rule, Baseline, #{}),
            maps:get(FileKey, BaselineCounts, 0)
    end.

count_rules(Bin) ->
    #{
        meck_new => count(<<"\\bmeck:new\\b">>, Bin),
        ets_new => count(<<"\\bets:new\\b">>, Bin),
        sleep => count(<<"(?:\\btimer:sleep\\b|\\bsleep\\b)">>, Bin),
        meck_new_in_init_per_testcase => count_meck_in_init_per_testcase(Bin)
    }.

count_meck_in_init_per_testcase(Bin) ->
    case re:run(Bin, <<"init_per_testcase\\s*\\([^)]*\\)\\s*->">>, [unicode]) of
        {match, [{Start, Len}]} ->
            StartPos = Start + Len,
            Tail = binary:part(Bin, StartPos, byte_size(Bin) - StartPos),
            EndPos = case re:run(Tail, <<"\\n[A-Za-z0-9_]+\\s*\\([^\\n]*\\)\\s*->">>, [unicode]) of
                {match, [{S, _}] } -> S;
                nomatch -> byte_size(Tail)
            end,
            Section = binary:part(Tail, 0, EndPos),
            count(<<"\\bmeck:new\\b">>, Section);
        nomatch ->
            0
    end.

count(Pattern, Bin) ->
    case re:run(Bin, Pattern, [global, unicode]) of
        {match, Matches} when is_list(Matches) -> length(Matches);
        {match, _} -> 1;
        nomatch -> 0
    end.

has_match(Pattern, Bin) ->
    case re:run(Bin, Pattern, [unicode]) of
        {match, _} -> true;
        nomatch -> false
    end.

print_issue({File, missing_callback, Name, _Current, _Allowed}) ->
    io:format("~s: missing callback ~p~n", [File, Name]);
print_issue({File, Rule, Current, Allowed, _Over}) ->
    io:format("~s: rule ~p exceeds baseline (current=~p, allowed=~p)~n",
              [File, Rule, Current, Allowed]).
