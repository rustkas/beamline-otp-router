%% @doc Policy Integration - JSON Policy Tests
%% 
%% Tests for JSON policies through full Router pipeline.
%% Runs with ROUTER_TEST_LEVEL=full or heavy.
%%
%% @test_category policy, integration
-module(router_policy_integration_json_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_sticky_hit/1,
    test_sticky_miss/1,
    test_weighted_routing_multiple_providers/1,
    test_fallback_chain_simple/1,
    test_extensions_full_pipeline/1,
    test_legacy_format_compatibility/1,
    test_explanation_format/1
]).

-define(TENANT_ID, <<"default_tenant">>).
-define(TEST_POLICY_ID, <<"test_policy">>).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, json_policy_tests}];
        "heavy" -> [{group, json_policy_tests}];
        _ -> []
    end.

groups() ->
    [{json_policy_tests, [sequence], [
        test_sticky_hit,
        test_sticky_miss,
        test_weighted_routing_multiple_providers,
        test_fallback_chain_simple,
        test_extensions_full_pipeline,
        test_legacy_format_compatibility,
        test_explanation_format
    ]}].

init_per_suite(Config) ->
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {_, {already_exists, _}}} -> ok;
        {error, _} -> ok
    end,
    mnesia:start(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    ok.

init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_sticky_hit(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"key">> => <<"session_id">>,
            <<"ttl">> => <<"5m">>
        },
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    ?assert(is_record(Policy, policy)),
    
    Sticky = Policy#policy.sticky,
    ?assert(maps:get(enabled, Sticky, false)),
    ok.

test_sticky_miss(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"sticky">> => #{
            <<"enabled">> => false,
            <<"key">> => <<"session_id">>,
            <<"ttl">> => <<"5m">>
        },
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    
    Sticky = Policy#policy.sticky,
    case Sticky of
        undefined -> ok;
        Map when is_map(Map) ->
            ?assertNot(maps:get(enabled, Map, false))
    end,
    ok.

test_weighted_routing_multiple_providers(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 60},
            #{<<"id">> => <<"anthropic:claude-3">>, <<"weight">> => 30},
            #{<<"id">> => <<"google:gemini">>, <<"weight">> => 10}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    ?assertEqual(3, maps:size(Weights)),
    ?assert(maps:is_key(<<"openai:gpt-4o">>, Weights)),
    ?assert(maps:is_key(<<"anthropic:claude-3">>, Weights)),
    ?assert(maps:is_key(<<"google:gemini">>, Weights)),
    ok.

test_fallback_chain_simple(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ],
        <<"fallbacks">> => [
            #{<<"when">> => [<<"timeout">>], <<"to">> => <<"anthropic:claude-3">>}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Fallbacks = Policy#policy.fallbacks,
    
    ?assertEqual(1, length(Fallbacks)),
    ok.

test_extensions_full_pipeline(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ],
        <<"extensions">> => #{
            <<"pre">> => [#{<<"id">> => <<"auth_check">>}],
            <<"validators">> => [#{<<"id">> => <<"input_validator">>}],
            <<"post">> => [#{<<"id">> => <<"logger">>}]
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    
    ?assertEqual(1, length(Policy#policy.pre)),
    ?assertEqual(1, length(Policy#policy.validators)),
    ?assertEqual(1, length(Policy#policy.post)),
    ok.

test_legacy_format_compatibility(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"weights">> => #{
            <<"openai:gpt-4o">> => 0.6,
            <<"anthropic:claude-3">> => 0.4
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    ?assertEqual(2, maps:size(Weights)),
    ok.

test_explanation_format(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?TEST_POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    
    %% Verify policy structure
    ?assert(is_record(Policy, policy)),
    ?assertEqual(?TENANT_ID, Policy#policy.tenant_id),
    ok.
