%% @doc Policy Applier DSL - Parsing Tests
%% 
%% Tests for DSL parsing: providers, fallbacks, sticky, extensions.
%% Runs on fast CI (CP1).
%%
%% @test_category policy, dsl, fast
-module(router_policy_dsl_parsing_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Use the #policy{} record from beamline_router.hrl

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).
-export([groups_for_level/1]).

-export([
    test_providers_array_parsing/1,
    test_providers_weight_conversion/1,
    test_providers_empty_array/1,
    test_fallbacks_array_parsing/1,
    test_fallbacks_when_condition_parsing/1,
    test_sticky_ttl_string_parsing/1,
    test_sticky_session_key_parsing/1,
    test_extensions_pre_parsing/1,
    test_extensions_validators_parsing/1,
    test_extensions_post_parsing/1
]).

-define(TENANT_ID, <<"test_tenant">>).
-define(POLICY_ID, <<"test_policy">>).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end.

meta_all() ->
    [].
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [{group, dsl_parsing_tests}];
groups_for_level(full) -> [{group, dsl_parsing_tests}];
groups_for_level(heavy) -> [{group, dsl_parsing_tests}].

%% groups_for_level/1 exported above

groups() ->
    [{dsl_parsing_tests, [parallel], [
        test_providers_array_parsing,
        test_providers_weight_conversion,
        test_providers_empty_array,
        test_fallbacks_array_parsing,
        test_fallbacks_when_condition_parsing,
        test_sticky_ttl_string_parsing,
        test_sticky_session_key_parsing,
        test_extensions_pre_parsing,
        test_extensions_validators_parsing,
        test_extensions_post_parsing
    ]}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_providers_array_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 60},
            #{<<"id">> => <<"anthropic:claude-3">>, <<"weight">> => 40}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    ?assert(maps:is_key(<<"openai:gpt-4o">>, Weights)),
    ?assert(maps:is_key(<<"anthropic:claude-3">>, Weights)),
    ok.

test_providers_weight_conversion(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    Weight = maps:get(<<"openai:gpt-4o">>, Weights),
    ?assertEqual(1.0, Weight),
    ok.

test_providers_empty_array(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => []
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    ?assertEqual(#{}, Weights),
    ok.

test_fallbacks_array_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"fallbacks">> => [
            #{<<"when">> => [<<"timeout">>], <<"to">> => <<"anthropic:claude-3">>}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Fallbacks = Policy#policy.fallbacks,
    
    ?assertEqual(1, length(Fallbacks)),
    ok.

test_fallbacks_when_condition_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"fallbacks">> => [
            #{<<"when">> => [<<"timeout">>, <<"rate_limited">>], <<"to">> => <<"backup:provider">>}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Fallbacks = Policy#policy.fallbacks,
    
    ?assertEqual(1, length(Fallbacks)),
    [Fallback | _] = Fallbacks,
    WhenConditions = maps:get(when_conditions, Fallback, []),
    ?assertEqual(2, length(WhenConditions)),
    ok.

test_sticky_ttl_string_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"key">> => <<"session_id">>,
            <<"ttl">> => <<"5m">>
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Sticky = Policy#policy.sticky,
    
    ?assert(maps:get(enabled, Sticky, false)),
    TtlMs = maps:get(ttl_ms, Sticky, 0),
    ?assertEqual(300000, TtlMs),  %% 5 minutes in ms
    ok.

test_sticky_session_key_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"key">> => <<"custom_session_key">>,
            <<"ttl">> => <<"1h">>
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Sticky = Policy#policy.sticky,
    
    Key = maps:get(key, Sticky, undefined),
    ?assertEqual(<<"custom_session_key">>, Key),
    ok.

test_extensions_pre_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"extensions">> => #{
            <<"pre">> => [
                #{<<"id">> => <<"auth_check">>, <<"config">> => #{}}
            ]
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Pre = Policy#policy.pre,
    
    ?assertEqual(1, length(Pre)),
    ok.

test_extensions_validators_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"extensions">> => #{
            <<"validators">> => [
                #{<<"id">> => <<"input_validator">>, <<"config">> => #{}}
            ]
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Validators = Policy#policy.validators,
    
    ?assertEqual(1, length(Validators)),
    ok.

test_extensions_post_parsing(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"extensions">> => #{
            <<"post">> => [
                #{<<"id">> => <<"logger">>, <<"config">> => #{}}
            ]
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Post = Policy#policy.post,
    
    ?assertEqual(1, length(Post)),
    ok.
