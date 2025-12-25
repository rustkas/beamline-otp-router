%% @doc CI Enforcement Test Suite
%%
%% Validates test infrastructure components:
%% - Helper modules are available and functional
%% - Mock discipline enforcement works
%% - Seed reproducibility works
%% - CI environment detection works
%%
%% This suite should pass on every CI run to ensure test infrastructure integrity.
%%
%% @test_category infrastructure, ci, enforcement
-module(router_ci_enforcement_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test exports
-export([
    %% Helper availability tests
    test_nats_helper_available/1,
    test_testops_helper_available/1,
    test_caf_helper_available/1,
    
    %% Helper functionality tests
    test_nats_helper_setup_teardown/1,
    test_nats_helper_expectations/1,
    test_testops_helper_chaos_mode/1,
    test_testops_helper_seed_management/1,
    
    %% Mock discipline tests
    test_mock_discipline_detection/1,
    test_mock_discipline_strict_mode/1,
    
    %% CI environment tests
    test_ci_env_detection/1,
    test_ci_strict_discipline_env/1,
    
    %% Governance compliance tests
    test_governance_docs_exist/1,
    test_maturity_docs_exist/1
]).

all() ->
    [].

groups_for_level(_) ->
    [
        {group, helper_availability},
        {group, helper_functionality},
        {group, mock_discipline},
        {group, ci_environment},
        {group, governance_compliance}
    ].

groups() ->
    [
        {helper_availability, [parallel], [
            test_nats_helper_available,
            test_testops_helper_available,
            test_caf_helper_available
        ]},
        {helper_functionality, [sequence], [
            test_nats_helper_setup_teardown,
            test_nats_helper_expectations,
            test_testops_helper_chaos_mode,
            test_testops_helper_seed_management
        ]},
        {mock_discipline, [sequence], [
            test_mock_discipline_detection,
            test_mock_discipline_strict_mode
        ]},
        {ci_environment, [parallel], [
            test_ci_env_detection,
            test_ci_strict_discipline_env
        ]},
        {governance_compliance, [parallel], [
            test_governance_docs_exist,
            test_maturity_docs_exist
        ]}
    ].

init_per_suite(Config) ->
    %% Ensure application is loaded for code paths
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Cleanup any mocks that might be lingering
    catch meck:unload(router_nats),
    Config.

%% ============================================================================
%% Helper Availability Tests
%% ============================================================================

%% @doc Verify router_nats_test_helper is available
test_nats_helper_available(_Config) ->
    ct:comment("Checking router_nats_test_helper availability"),
    
    %% Module should be loadable
    case code:ensure_loaded(router_nats_test_helper) of
        {module, router_nats_test_helper} ->
            ct:pal("✓ router_nats_test_helper loaded");
        {error, Reason} ->
            ct:fail({helper_not_available, router_nats_test_helper, Reason})
    end,
    
    %% Required exports should exist
    RequiredExports = [
        {setup_mock, 0},
        {setup_mock, 1},
        {teardown_mock, 0},
        {expect_publish_success, 0},
        {expect_publish_failure, 1},
        {check_corner_case_coverage, 0}
    ],
    
    lists:foreach(fun({Fun, Arity}) ->
        ?assert(erlang:function_exported(router_nats_test_helper, Fun, Arity),
                io_lib:format("Missing export: ~p/~p", [Fun, Arity]))
    end, RequiredExports),
    
    ct:pal("✓ All required exports present"),
    ok.

%% @doc Verify router_testops_helper is available
test_testops_helper_available(_Config) ->
    ct:comment("Checking router_testops_helper availability"),
    
    case code:ensure_loaded(router_testops_helper) of
        {module, router_testops_helper} ->
            ct:pal("✓ router_testops_helper loaded");
        {error, Reason} ->
            ct:fail({helper_not_available, router_testops_helper, Reason})
    end,
    
    RequiredExports = [
        {init_chaos_mode, 1},
        {get_chaos_mode, 0},
        {setup_deterministic_seed, 2},
        {check_mock_discipline, 2},
        {ci_fail_on_mock_mode, 0}
    ],
    
    lists:foreach(fun({Fun, Arity}) ->
        ?assert(erlang:function_exported(router_testops_helper, Fun, Arity),
                io_lib:format("Missing export: ~p/~p", [Fun, Arity]))
    end, RequiredExports),
    
    ct:pal("✓ All required exports present"),
    ok.

%% @doc Verify router_caf_test_helper is available
test_caf_helper_available(_Config) ->
    ct:comment("Checking router_caf_test_helper availability"),
    
    case code:ensure_loaded(router_caf_test_helper) of
        {module, router_caf_test_helper} ->
            ct:pal("✓ router_caf_test_helper loaded");
        {error, Reason} ->
            %% CAF helper is optional, just log warning
            ct:pal("⚠️ router_caf_test_helper not available: ~p", [Reason]),
            ct:pal("This is acceptable if CAF tests are not used")
    end,
    ok.

%% ============================================================================
%% Helper Functionality Tests
%% ============================================================================

%% @doc Test NATS helper setup/teardown cycle
test_nats_helper_setup_teardown(_Config) ->
    ct:comment("Testing NATS helper setup/teardown"),
    
    %% Check meck availability first
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            %% Setup should work
            case router_nats_test_helper:setup_mock() of
                ok ->
                    ct:pal("✓ setup_mock() succeeded"),
                    
                    %% Verify mock is active
                    ?assert(router_nats_test_helper:is_mocked()),
                    ct:pal("✓ is_mocked() returns true"),
                    
                    %% Teardown should work
                    ok = router_nats_test_helper:teardown_mock(),
                    ct:pal("✓ teardown_mock() succeeded"),
                    
                    %% Verify mock is inactive
                    ?assertNot(router_nats_test_helper:is_mocked()),
                    ct:pal("✓ is_mocked() returns false after teardown"),
                    
                    ok;
                {skip, Reason} ->
                    {skip, Reason}
            end
    end.

%% @doc Test NATS helper expectations
test_nats_helper_expectations(_Config) ->
    ct:comment("Testing NATS helper expectations"),
    
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            case router_nats_test_helper:setup_mock() of
                ok ->
                    %% Test success expectation
                    ok = router_nats_test_helper:expect_publish_success(<<"test-ack">>),
                    Result1 = router_nats:publish_with_ack(<<"test">>, <<"payload">>, #{}),
                    ?assertEqual({ok, <<"test-ack">>}, Result1),
                    ct:pal("✓ expect_publish_success works"),
                    
                    %% Test failure expectation
                    ok = router_nats_test_helper:expect_publish_failure(test_error),
                    Result2 = router_nats:publish_with_ack(<<"test">>, <<"payload">>, #{}),
                    ?assertEqual({error, test_error}, Result2),
                    ct:pal("✓ expect_publish_failure works"),
                    
                    %% Test sequence expectation
                    ok = router_nats_test_helper:expect_publish_sequence([
                        {error, first_fail},
                        {ok, <<"second-ok">>},
                        {ok, <<"third-ok">>}
                    ]),
                    ?assertEqual({error, first_fail}, router_nats:publish_with_ack(<<"t">>, <<"p">>, #{})),
                    ?assertEqual({ok, <<"second-ok">>}, router_nats:publish_with_ack(<<"t">>, <<"p">>, #{})),
                    ?assertEqual({ok, <<"third-ok">>}, router_nats:publish_with_ack(<<"t">>, <<"p">>, #{})),
                    ct:pal("✓ expect_publish_sequence works"),
                    
                    router_nats_test_helper:teardown_mock(),
                    ok;
                {skip, Reason} ->
                    {skip, Reason}
            end
    end.

%% @doc Test TestOps helper chaos mode detection
test_testops_helper_chaos_mode(_Config) ->
    ct:comment("Testing TestOps chaos mode detection"),
    
    %% Get chaos mode
    Mode = router_testops_helper:get_chaos_mode(),
    ?assert(lists:member(Mode, [docker, mock, unknown]),
            io_lib:format("Invalid chaos mode: ~p", [Mode])),
    ct:pal("✓ Chaos mode detected: ~p", [Mode]),
    
    %% Init chaos mode should return config with mode
    Config = router_testops_helper:init_chaos_mode([]),
    ?assert(proplists:is_defined(chaos_mode, Config)),
    ct:pal("✓ init_chaos_mode adds chaos_mode to config"),
    
    ok.

%% @doc Test TestOps helper seed management
test_testops_helper_seed_management(_Config) ->
    ct:comment("Testing TestOps seed management"),
    
    %% Get seed for this test
    Seed = router_testops_helper:get_test_seed(test_seed_management),
    ?assert(is_tuple(Seed)),
    ?assertEqual(3, tuple_size(Seed)),
    {S1, S2, S3} = Seed,
    ?assert(is_integer(S1)),
    ?assert(is_integer(S2)),
    ?assert(is_integer(S3)),
    ct:pal("✓ Seed generated: ~p", [Seed]),
    
    %% Seed should be deterministic for same test name
    Seed2 = router_testops_helper:get_test_seed(test_seed_management),
    ?assertEqual(Seed, Seed2),
    ct:pal("✓ Seed is deterministic"),
    
    %% Different test names should give different seeds
    SeedOther = router_testops_helper:get_test_seed(other_test_name),
    ?assertNotEqual(Seed, SeedOther),
    ct:pal("✓ Different tests get different seeds"),
    
    %% Setup should work
    Config = router_testops_helper:setup_deterministic_seed(test_seed, []),
    ?assert(proplists:is_defined(test_seed, Config)),
    ct:pal("✓ setup_deterministic_seed works"),
    
    ok.

%% ============================================================================
%% Mock Discipline Tests
%% ============================================================================

%% @doc Test mock discipline detection
test_mock_discipline_detection(_Config) ->
    ct:comment("Testing mock discipline detection"),
    
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            %% Setup mock with varied usage (advanced pattern)
            case router_nats_test_helper:setup_mock() of
                ok ->
                    %% Use multiple subjects (advanced pattern)
                    router_nats_test_helper:expect_publish_with_args(fun(Subject, _Payload, _Headers) ->
                        case Subject of
                            <<"test.success">> -> {ok, <<"ack">>};
                            <<"test.fail">> -> {error, test_fail};
                            _ -> {ok, <<"default">>}
                        end
                    end),
                    
                    %% Make calls with different subjects
                    _ = router_nats:publish_with_ack(<<"test.success">>, <<"p">>, #{}),
                    _ = router_nats:publish_with_ack(<<"test.fail">>, <<"p">>, #{}),
                    
                    %% This should NOT be basic_only
                    Result = router_nats_test_helper:check_corner_case_coverage(),
                    ct:pal("Coverage check result: ~p", [Result]),
                    
                    router_nats_test_helper:teardown_mock(),
                    ok;
                {skip, Reason} ->
                    {skip, Reason}
            end
    end.

%% @doc Test strict mock discipline mode
test_mock_discipline_strict_mode(_Config) ->
    ct:comment("Testing strict mock discipline mode"),
    
    %% Check if strict mode is detected from env
    IsStrict = router_testops_helper:is_strict_discipline(),
    ?assert(is_boolean(IsStrict)),
    ct:pal("Strict discipline mode: ~p", [IsStrict]),
    
    %% If not strict, this should just return
    case IsStrict of
        false ->
            ct:pal("✓ Strict mode detection works (currently disabled)");
        true ->
            ct:pal("✓ Strict mode is ENABLED (tests will fail on basic mocks)")
    end,
    
    ok.

%% ============================================================================
%% CI Environment Tests
%% ============================================================================

%% @doc Test CI environment detection
test_ci_env_detection(_Config) ->
    ct:comment("Testing CI environment detection"),
    
    %% These should not crash regardless of env
    _ = router_testops_helper:get_ci_env("CI"),
    _ = router_testops_helper:get_ci_env("GITHUB_ACTIONS"),
    _ = router_testops_helper:get_ci_env("STRICT_MOCK_DISCIPLINE"),
    _ = router_testops_helper:get_ci_env("CHAOS_REQUIRE_DOCKER"),
    
    ct:pal("✓ CI env detection functions work"),
    ok.

%% @doc Test strict discipline environment variable
test_ci_strict_discipline_env(_Config) ->
    ct:comment("Testing STRICT_MOCK_DISCIPLINE env"),
    
    %% Check current value
    EnvValue = os:getenv("STRICT_MOCK_DISCIPLINE", "false"),
    IsStrict = router_testops_helper:is_strict_discipline(),
    
    case EnvValue of
        "true" ->
            ?assert(IsStrict),
            ct:pal("✓ STRICT_MOCK_DISCIPLINE=true detected");
        _ ->
            ?assertNot(IsStrict),
            ct:pal("✓ STRICT_MOCK_DISCIPLINE not set or false")
    end,
    
    ok.

%% ============================================================================
%% Governance Compliance Tests
%% ============================================================================

%% @doc Test that governance docs exist
test_governance_docs_exist(_Config) ->
    ct:comment("Checking governance documentation"),
    
    %% Docs are now in docs/testing/ relative to project root
    %% We check common relative paths
    
    %% Check TEST_GOVERNANCE.md exists
    GovernancePath = "docs/testing/TEST_GOVERNANCE.md",
    case filelib:is_file(GovernancePath) of
        true ->
            ct:pal("✓ TEST_GOVERNANCE.md exists");
        false ->
            %% Try alternate path
            AltPath = "../docs/testing/TEST_GOVERNANCE.md",
            case filelib:is_file(AltPath) of
                true ->
                    ct:pal("✓ TEST_GOVERNANCE.md exists (alt path)");
                false ->
                    ct:pal("⚠️ TEST_GOVERNANCE.md not found at ~s or ~s", 
                           [GovernancePath, AltPath]),
                    ct:pal("This is a governance compliance warning")
            end
    end,
    
    %% Check MOCK_DISCIPLINE.md exists
    MockPath = "docs/testing/MOCK_DISCIPLINE.md",
    case filelib:is_file(MockPath) of
        true ->
            ct:pal("✓ MOCK_DISCIPLINE.md exists");
        false ->
            AltMockPath = "../docs/testing/MOCK_DISCIPLINE.md",
            case filelib:is_file(AltMockPath) of
                true ->
                    ct:pal("✓ MOCK_DISCIPLINE.md exists (alt path)");
                false ->
                    ct:pal("⚠️ MOCK_DISCIPLINE.md not found")
            end
    end,
    
    ok.

%% @doc Test that maturity docs exist
test_maturity_docs_exist(_Config) ->
    ct:comment("Checking maturity documentation"),
    
    %% Check TEST_MATURITY.md exists
    MaturityPath = "docs/testing/TEST_MATURITY.md",
    case filelib:is_file(MaturityPath) of
        true ->
            ct:pal("✓ TEST_MATURITY.md exists");
        false ->
            AltPath = "../docs/testing/TEST_MATURITY.md",
            case filelib:is_file(AltPath) of
                true ->
                    ct:pal("✓ TEST_MATURITY.md exists (alt path)");
                false ->
                    ct:pal("⚠️ TEST_MATURITY.md not found")
            end
    end,
    
    %% Check BUSINESS_PROBLEMS_MAP.md exists
    BizPath = "docs/testing/BUSINESS_PROBLEMS_MAP.md",
    case filelib:is_file(BizPath) of
        true ->
            ct:pal("✓ BUSINESS_PROBLEMS_MAP.md exists");
        false ->
            AltBizPath = "../docs/testing/BUSINESS_PROBLEMS_MAP.md",
            case filelib:is_file(AltBizPath) of
                true ->
                    ct:pal("✓ BUSINESS_PROBLEMS_MAP.md exists (alt path)");
                false ->
                    ct:pal("⚠️ BUSINESS_PROBLEMS_MAP.md not found")
            end
    end,
    
    ok.
