%% @doc Policy Integration - Edge Cases
%% 
%% Tests for edge cases in policy integration.
%% Runs with ROUTER_TEST_LEVEL=full or heavy.
%%
%% @test_category policy, integration, edge_cases
-module(router_policy_integration_edge_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_inconsistent_weights_sum_not_100/1,
    test_inconsistent_weights_sum_zero/1,
    test_inconsistent_weights_sum_over_100/1,
    test_invalid_ttl_duration_invalid_format/1,
    test_conflicting_fallback_rules/1,
    test_empty_extensions_arrays/1
]).

-define(TENANT_ID, <<"default_tenant">>).
-define(TEST_POLICY_ID, <<"test_policy">>).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, edge_cases_tests}];
        "heavy" -> [{group, edge_cases_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, edge_cases_tests}];
        "heavy" -> [{group, edge_cases_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, edge_cases_tests}];
        "heavy" -> [{group, edge_cases_tests}];
        _ -> []
    end.
groups() ->
    [{edge_cases_tests, [parallel], [
        test_inconsistent_weights_sum_not_100,
        test_inconsistent_weights_sum_zero,
        test_inconsistent_weights_sum_over_100,
        test_invalid_ttl_duration_invalid_format,
        test_conflicting_fallback_rules,
        test_empty_extensions_arrays
    ]}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_inconsistent_weights_sum_not_100(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 30},
            #{<<"id">> => <<"anthropic:claude-3">>, <<"weight">> => 30}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    %% Should still parse
    ?assert(is_map(Weights)),
    ok.

test_inconsistent_weights_sum_zero(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 0}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    ?assert(is_map(Weights)),
    ok.

test_inconsistent_weights_sum_over_100(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 60},
            #{<<"id">> => <<"anthropic:claude-3">>, <<"weight">> => 60}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    ?assert(is_map(Weights)),
    ok.

test_invalid_ttl_duration_invalid_format(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"key">> => <<"session_id">>,
            <<"ttl">> => <<"invalid">>
        },
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ]
    },
    
    try
        _Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
        ok
    catch
        _:_ -> ok
    end.

test_conflicting_fallback_rules(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ],
        <<"fallbacks">> => [
            #{<<"when">> => [<<"timeout">>], <<"to">> => <<"anthropic:claude-3">>},
            #{<<"when">> => [<<"timeout">>], <<"to">> => <<"google:gemini">>}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Fallbacks = Policy#policy.fallbacks,
    
    %% Both rules should be parsed
    ?assertEqual(2, length(Fallbacks)),
    ok.

test_empty_extensions_arrays(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ],
        <<"extensions">> => #{
            <<"pre">> => [],
            <<"validators">> => [],
            <<"post">> => []
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    
    ?assertEqual([], Policy#policy.pre),
    ?assertEqual([], Policy#policy.validators),
    ?assertEqual([], Policy#policy.post),
    ok.
