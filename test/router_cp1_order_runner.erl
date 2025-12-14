%% @doc CP1 Suite Stability Runner
%%
%% Validates CP1 test suite stability under:
%% - Repeat runs (same order, N times)
%% - Randomized suite ordering
%%
%% Usage:
%%   router_cp1_order_runner:run().           %% Random order, 3 iterations
%%   router_cp1_order_runner:run(5).          %% Random order, 5 iterations
%%   router_cp1_order_runner:repeat_run().    %% Same order, 3 iterations
%%   router_cp1_order_runner:repeat_run(5).   %% Same order, 5 iterations
%%
%% @test_category helper
-module(router_cp1_order_runner).

-export([run/0, run/1, repeat_run/0, repeat_run/1, main/1]).

%% @doc Run CP1 suites in random order 3 times (default)
-spec run() -> ok | {error, term()}.
run() ->
    run(3).

%% @doc Run CP1 suites in random order N times
-spec run(N :: pos_integer()) -> ok | {error, term()}.
run(N) when is_integer(N), N > 0 ->
    io:format("~n=== CP1 Random Order Runner (iterations=~p) ===~n", [N]),
    run_iterations(N, 0, [], random).

%% @doc Run CP1 suites in fixed order 3 times (default)
-spec repeat_run() -> ok | {error, term()}.
repeat_run() ->
    repeat_run(3).

%% @doc Run CP1 suites in fixed order N times (Task 1: stability check)
-spec repeat_run(N :: pos_integer()) -> ok | {error, term()}.
repeat_run(N) when is_integer(N), N > 0 ->
    io:format("~n=== CP1 Repeat Runner (iterations=~p) ===~n", [N]),
    run_iterations(N, 0, [], fixed).

%% @doc Entry point for escript / rebar3 escriptize
-spec main([string()]) -> no_return().
main(Args) ->
    {Mode, Iterations} = parse_args(Args),
    Result = case Mode of
        random -> run(Iterations);
        fixed -> repeat_run(Iterations)
    end,
    case Result of
        ok -> halt(0);
        {error, _} -> halt(1)
    end.

parse_args([]) ->
    {random, 3};
parse_args(["--repeat" | Rest]) ->
    {fixed, parse_iterations(Rest)};
parse_args(["--random" | Rest]) ->
    {random, parse_iterations(Rest)};
parse_args([N | _]) ->
    {random, list_to_integer(N)}.

parse_iterations([]) -> 3;
parse_iterations([N | _]) -> list_to_integer(N).

run_iterations(0, Passed, [], _Mode) ->
    io:format("~n=== All ~p iterations passed ===~n", [Passed]),
    ok;
run_iterations(0, Passed, Failures, _Mode) ->
    io:format("~n=== ~p passed, ~p failed ===~n", [Passed, length(Failures)]),
    io:format("Failures: ~p~n", [lists:reverse(Failures)]),
    {error, {failures, lists:reverse(Failures)}};
run_iterations(N, Passed, Failures, Mode) ->
    IterNum = Passed + 1,
    io:format("~n--- Iteration ~p ---~n", [IterNum]),
    Suites = case Mode of
        random -> shuffle(cp1_suites());
        fixed -> cp1_suites()
    end,
    io:format("Suite order: ~p~n", [Suites]),
    case run_suites(Suites) of
        ok ->
            run_iterations(N - 1, Passed + 1, Failures, Mode);
        {error, Reason} ->
            run_iterations(N - 1, Passed, [{IterNum, Reason} | Failures], Mode)
    end.

%% @doc Full CP1 suite list (all stability-critical suites)
cp1_suites() ->
    [
        router_cp1_smoke_SUITE,
        router_cp1_red_bar_SUITE,
        router_core_basic_SUITE,
        router_policy_SUITE,
        router_policy_store_SUITE,
        router_error_SUITE,
        router_mock_helpers_SUITE,
        router_test_init_SUITE,
        router_cp1_default_env_SUITE
    ].

%% @doc Run suites in order, stop on first failure
run_suites([]) ->
    ok;
run_suites([Suite | Rest]) ->
    io:format("  Running ~p...", [Suite]),
    case catch ct:run_test([{suite, Suite}, {logdir, "/tmp/ct_logs"}]) of
        {_Ok, 0, {0, 0}} ->
            io:format(" PASS~n"),
            run_suites(Rest);
        {_Ok, Failed, {UserSkip, AutoSkip}} when Failed > 0 ->
            io:format(" FAIL (failed=~p, user_skip=~p, auto_skip=~p)~n", 
                      [Failed, UserSkip, AutoSkip]),
            {error, {suite_failed, Suite, Failed}};
        {_Ok, 0, {_UserSkip, _AutoSkip}} ->
            %% All passed but some skipped - that's OK
            io:format(" PASS (some skipped)~n"),
            run_suites(Rest);
        {'EXIT', Reason} ->
            io:format(" CRASH: ~p~n", [Reason]),
            {error, {suite_crashed, Suite, Reason}};
        Other ->
            io:format(" result=~p~n", [Other]),
            %% ct:run_test may return various formats
            case Other of
                ok -> run_suites(Rest);
                {ok, _} -> run_suites(Rest);
                _ -> {error, {unexpected_result, Suite, Other}}
            end
    end.

%% @doc Fisher-Yates shuffle
shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), E} || E <- List])].
