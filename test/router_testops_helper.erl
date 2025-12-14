%% @doc TestOps helper module for test infrastructure enforcement
%%
%% This module provides:
%% - Chaos mode detection and enforcement
%% - Mock discipline checks
%% - Deterministic seed management for reproducible tests
%% - CI integration helpers
%%
%% Usage in SUITE:
%%
%% init_per_suite(Config) ->
%%   router_testops_helper:init_chaos_mode(Config).
%%
%% init_per_testcase(TestCase, Config) ->
%%   router_testops_helper:setup_deterministic_seed(TestCase, Config).
%%
%% end_per_testcase(TestCase, Config) ->
%%   router_testops_helper:check_mock_discipline(TestCase, Config).
%%
%% @test_category test_infrastructure, testops
-module(router_testops_helper).

-include_lib("common_test/include/ct.hrl").

-export([
    %% Chaos mode
    init_chaos_mode/1,
    get_chaos_mode/0,
    require_docker_mode/0,
    skip_if_mock_mode/0,
    
    %% Deterministic seeds
    setup_deterministic_seed/2,
    get_test_seed/1,
    log_seed_for_reproduction/1,
    
    %% Mock discipline
    check_mock_discipline/2,
    enforce_strict_mocks/0,
    is_strict_discipline/0,
    
    %% CI helpers
    ci_fail_on_mock_mode/0,
    ci_log_test_mode/1,
    get_ci_env/1
]).

%% ============================================================================
%% Chaos Mode Detection and Enforcement
%% ============================================================================

%% @doc Initialize chaos mode detection in init_per_suite
%% Returns updated Config with chaos_mode key
-spec init_chaos_mode(list()) -> list().
init_chaos_mode(Config) ->
    Mode = detect_chaos_mode(),
    ct:pal("~n"
           "╔════════════════════════════════════════════════════════════════╗~n"
           "║ CHAOS MODE: ~p~n"
           "╚════════════════════════════════════════════════════════════════╝~n", 
           [Mode]),
    [{chaos_mode, Mode} | Config].

%% @doc Get current chaos mode
%% Returns: docker | mock | unknown
-spec get_chaos_mode() -> docker | mock | unknown.
get_chaos_mode() ->
    detect_chaos_mode().

%% @doc Require Docker mode, skip test if mock mode
%% Use in test cases that MUST run with real Docker
-spec require_docker_mode() -> ok | {skip, string()}.
require_docker_mode() ->
    case detect_chaos_mode() of
        docker -> ok;
        mock -> {skip, "Test requires Docker mode (CHAOS_REQUIRE_DOCKER=true)"};
        unknown -> {skip, "Could not detect chaos mode"}
    end.

%% @doc Skip test if running in mock mode
-spec skip_if_mock_mode() -> ok | {skip, string()}.
skip_if_mock_mode() ->
    case detect_chaos_mode() of
        mock -> {skip, "Test skipped in mock mode"};
        _ -> ok
    end.

%% Internal: detect chaos mode
-spec detect_chaos_mode() -> docker | mock | unknown.
detect_chaos_mode() ->
    %% Check env var first
    case os:getenv("CHAOS_MODE") of
        "docker" -> docker;
        "mock" -> mock;
        _ ->
            %% Try to detect Docker availability
            case os:cmd("docker ps 2>/dev/null") of
                "" -> mock;
                Output when is_list(Output) ->
                    case string:find(Output, "CONTAINER") of
                        nomatch -> mock;
                        _ -> docker
                    end
            end
    end.

%% ============================================================================
%% Deterministic Seeds for Reproducible Tests
%% ============================================================================

%% @doc Setup deterministic seed for a test case
%% Reads from env var or uses default based on test name
-spec setup_deterministic_seed(atom(), list()) -> list().
setup_deterministic_seed(TestCase, Config) ->
    Seed = get_test_seed(TestCase),
    rand:seed(exsss, Seed),
    ct:pal("Using seed ~p for ~p", [Seed, TestCase]),
    [{test_seed, Seed} | Config].

%% @doc Get seed for a test (from env or deterministic default)
-spec get_test_seed(atom()) -> {integer(), integer(), integer()}.
get_test_seed(TestCase) ->
    case os:getenv("TEST_SEED") of
        false ->
            %% Generate deterministic seed from test name
            Hash = erlang:phash2(TestCase),
            {Hash rem 10000, (Hash div 10000) rem 10000, (Hash div 100000000) rem 10000};
        SeedStr ->
            parse_seed_string(SeedStr)
    end.

%% @doc Log seed for reproduction in case of failure
-spec log_seed_for_reproduction({integer(), integer(), integer()}) -> ok.
log_seed_for_reproduction(Seed) ->
    {S1, S2, S3} = Seed,
    ct:pal("~n"
           "╔════════════════════════════════════════════════════════════════╗~n"
           "║ TO REPRODUCE THIS TEST:                                        ║~n"
           "║ TEST_SEED=~p,~p,~p make test-...                   ║~n"
           "╚════════════════════════════════════════════════════════════════╝~n",
           [S1, S2, S3]),
    ok.

%% Internal: parse seed string "A,B,C" to tuple
-spec parse_seed_string(string()) -> {integer(), integer(), integer()}.
parse_seed_string(SeedStr) ->
    case string:tokens(SeedStr, ",") of
        [A, B, C] ->
            {list_to_integer(A), list_to_integer(B), list_to_integer(C)};
        _ ->
            %% Fallback to default seed
            {1234, 5678, 91011}
    end.

%% ============================================================================
%% Mock Discipline Enforcement
%% ============================================================================

%% @doc Check mock discipline at end of test case
%% Logs warnings or fails based on STRICT_MOCK_DISCIPLINE env
-spec check_mock_discipline(atom(), list()) -> ok.
check_mock_discipline(TestCase, _Config) ->
    case code:which(meck) of
        non_existing ->
            ok;  %% No meck, nothing to check
        _ ->
            check_mock_coverage(TestCase)
    end.

%% @doc Enforce strict mock discipline (for CI)
%% Fails test if only basic mocks were used
-spec enforce_strict_mocks() -> ok | no_return().
enforce_strict_mocks() ->
    case router_nats_test_helper:check_corner_case_coverage_strict() of
        ok -> ok;
        _ -> ct:fail("Strict mock discipline violation")
    end.

%% @doc Check if strict discipline mode is enabled
-spec is_strict_discipline() -> boolean().
is_strict_discipline() ->
    os:getenv("STRICT_MOCK_DISCIPLINE", "false") =:= "true".

%% Internal: check mock coverage
-spec check_mock_coverage(atom()) -> ok.
check_mock_coverage(TestCase) ->
    try
        case meck:history(router_nats) of
            [] ->
                ok;  %% No mocking happened
            History when is_list(History) ->
                analyze_and_report(TestCase, History)
        end
    catch
        _:_ -> ok  %% Mock not active
    end.

%% Internal: analyze mock history and report
-spec analyze_and_report(atom(), list()) -> ok.
analyze_and_report(TestCase, History) ->
    CallCount = length(History),
    UniqueSubjects = lists:usort([S || {_, {_, publish_with_ack, [S, _, _]}, _} <- History]),
    UniqueResults = lists:usort([R || {_, {_, publish_with_ack, _}, R} <- History]),
    
    IsBasicOnly = (length(UniqueSubjects) =< 1) andalso (length(UniqueResults) =< 1),
    
    case {IsBasicOnly, is_strict_discipline()} of
        {true, true} ->
            ct:pal("❌ STRICT_MOCK_DISCIPLINE: ~p used basic mocks only (~b calls)", 
                   [TestCase, CallCount]),
            ct:fail({strict_mock_discipline_violation, TestCase});
        {true, false} ->
            ct:pal("💡 Mock hint for ~p: ~b calls, consider corner-case tests",
                   [TestCase, CallCount]);
        {false, _} ->
            ct:pal("✓ Good mock coverage in ~p: ~b calls, ~b subjects, ~b results",
                   [TestCase, CallCount, length(UniqueSubjects), length(UniqueResults)])
    end,
    ok.

%% ============================================================================
%% CI Integration Helpers
%% ============================================================================

%% @doc Fail CI if running in mock mode (for chaos tests)
-spec ci_fail_on_mock_mode() -> ok | no_return().
ci_fail_on_mock_mode() ->
    RequireDocker = os:getenv("CHAOS_REQUIRE_DOCKER", "false") =:= "true",
    case {RequireDocker, detect_chaos_mode()} of
        {true, mock} ->
            ct:pal("~n"
                   "╔════════════════════════════════════════════════════════════════╗~n"
                   "║ ❌ CI FAILURE: CHAOS_REQUIRE_DOCKER=true but running in MOCK   ║~n"
                   "║                                                                ║~n"
                   "║ Either:                                                        ║~n"
                   "║   1. Start Docker/NATS for full chaos testing                  ║~n"
                   "║   2. Use CHAOS_MOCK_ALLOWED=true for degraded mode             ║~n"
                   "╚════════════════════════════════════════════════════════════════╝~n", []),
            ct:fail("CI requires Docker mode for chaos tests");
        _ ->
            ok
    end.

%% @doc Log test mode for CI reports
-spec ci_log_test_mode(list()) -> ok.
ci_log_test_mode(Config) ->
    ChaosMode = proplists:get_value(chaos_mode, Config, unknown),
    StrictMocks = is_strict_discipline(),
    Seed = proplists:get_value(test_seed, Config, undefined),
    
    ct:pal("~n"
           "╔════════════════════════════════════════════════════════════════╗~n"
           "║ TEST CONFIGURATION                                             ║~n"
           "║ Chaos Mode:        ~p~n"
           "║ Strict Mocks:      ~p~n"
           "║ Seed:              ~p~n"
           "╚════════════════════════════════════════════════════════════════╝~n",
           [ChaosMode, StrictMocks, Seed]),
    ok.

%% @doc Get CI environment variable with default
-spec get_ci_env(string()) -> string() | false.
get_ci_env(VarName) ->
    os:getenv(VarName).
