%%%-------------------------------------------------------------------
%%% @doc
%%% Property-based tests for R10 Circuit Breaker state machine
%%% Light mode: 20-30 random sequences, basic invariants
%%% @end
%%%-------------------------------------------------------------------
-module(router_circuit_breaker_prop_SUITE).

+ compile ( { nowarn_unused_function , [ init_per_testcase / 2 , end_per_testcase / 2 ] } ) .

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([prop_random_sequences_basic_invariants/1]).

all() ->
    [{group, prop_tests}].

groups() ->
    [{prop_tests, [sequence], [prop_random_sequences_basic_invariants]}].

init_per_suite(Config) ->
    Config1 = router_test_bootstrap:init_per_suite(Config, #{}),
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    %% Prune old test metrics before property tests
    {ok, _PrunedCount} = router_r10_metrics:prune_old_test_metrics(5),
    Config1.

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{}).

init_per_testcase(TestCase, Config) ->
    router_test_bootstrap:init_per_testcase(TestCase, Config, #{}).

end_per_testcase(TestCase, Config) ->
    router_test_bootstrap:end_per_testcase(TestCase, Config, #{}).

%%--------------------------------------------------------------------
%% @doc
%% Property-like test: random sequences of success/failure
%% Verifies basic invariants of the state machine
%% @end
%%--------------------------------------------------------------------
prop_random_sequences_basic_invariants(_Config) ->
    ct:comment("R10 property: random sequences basic invariants"),

    TenantId = <<"tenant_prop">>,
    ProviderId = <<"provider_prop">>,

    %% Configure "aggressive" config for fast reaction
    Config =
        #{<<"failure_threshold">> => 3,
          <<"error_rate_threshold">> => 1.0,   %% Ignore error_rate here
          <<"error_rate_window_seconds">> => 30,
          %% <<"latency_threshold_ms">> => 0,     %% Disable latency trigger (undefined = disabled)
          <<"open_timeout_ms">> => 2000,
          <<"half_open_max_attempts">> => 2,
          <<"success_threshold">> => 2},

    ok = router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),

    %% Run several random scenarios
    NumScenarios = 20,
    lists:foreach(fun(N) ->
                     ct:comment("Running scenario ~p/~p", [N, NumScenarios]),
                     run_random_sequence(TenantId, ProviderId, N)
                  end,
                  lists:seq(1, NumScenarios)),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% Run one random sequence scenario
%% @end
%%--------------------------------------------------------------------
run_random_sequence(TenantId, ProviderId, ScenarioId) ->
    %% Reset state to ensure clean slate
    _ = router_circuit_breaker:reset_recovery_state(TenantId, ProviderId),

    %% Seed random number generator
    RandSeed = {erlang:monotonic_time(), erlang:phash2(self()), ScenarioId},
    _ = rand:seed(exsplus, RandSeed),

    %% Initial state should be closed or not_found -> treated as closed
    ensure_closed_or_not_found(TenantId, ProviderId),

    Steps = 30,
    do_steps(TenantId, ProviderId, Steps, 0).

ensure_closed_or_not_found(TenantId, ProviderId) ->
    case router_circuit_breaker:get_state(TenantId, ProviderId) of
        {ok, closed} ->
            ok;
        {error, not_found} ->
            ok;
        {ok, Other} ->
            ct:fail({unexpected_initial_state, Other})
    end.

do_steps(_TenantId, _ProviderId, 0, _ConsecutiveFailures) ->
    ok;
do_steps(TenantId, ProviderId, N, ConsecutiveFailures) when N > 0 ->
    %% Randomly choose event: success or failure
    R = rand:uniform(),
    {Event, NewConsecutiveFailures} =
        case R < 0.5 of
            true ->
                %% success
                ok = router_circuit_breaker:record_success(TenantId, ProviderId),
                {success, 0};
            false ->
                %% failure
                ok = router_circuit_breaker:record_failure(TenantId, ProviderId),
                {failure, ConsecutiveFailures + 1}
        end,

    %% Check invariants using public API
    InvariantsResult = check_invariants(TenantId, ProviderId, Event, NewConsecutiveFailures),

    case InvariantsResult of
        ok ->
            do_steps(TenantId, ProviderId, N - 1, NewConsecutiveFailures);
        {fail, Reason} ->
            ct:fail({property_violation, Reason})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check basic invariants of the state machine
%% @end
%%--------------------------------------------------------------------
check_invariants(TenantId, ProviderId, Event, ConsecutiveFailures) ->
    StateRes = router_circuit_breaker:get_state(TenantId, ProviderId),

    %% Failure threshold from config (hardcoded in test setup)
    FailureThreshold = 3,

    case {Event, StateRes} of
        %% Invariant 1: From pure successes, should not transition to open
        %% {success, {ok, open}} when ConsecutiveFailures =:= 0 ->
        %%    {fail, {open_after_only_successes, StateRes}};
        %% Invariant 2: If many consecutive failures, should be open (or at least not closed)
        {failure, {ok, closed}} when ConsecutiveFailures >= FailureThreshold ->
            {fail, {still_closed_after_many_failures, ConsecutiveFailures}};
        %% Additional check: if state=open, should_allow must return circuit_open
        {_, {ok, open}} ->
            case router_circuit_breaker:should_allow(TenantId, ProviderId) of
                {error, circuit_open} ->
                    ok;
                Other ->
                    {fail, {should_allow_mismatch_for_open, Other}}
            end;
        %% Additional check: if state=closed, should_allow should return ok
        {_, {ok, closed}} ->
            case router_circuit_breaker:should_allow(TenantId, ProviderId) of
                {ok, allow} ->
                    ok;
                Other ->
                    {fail, {should_allow_mismatch_for_closed, Other}}
            end;
        %% Everything else is acceptable
        _ ->
            ok
    end.
