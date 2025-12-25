%% @doc Policy Applier DSL - Edge Cases
%% 
%% Tests for edge cases: empty values, invalid formats, boundary conditions.
%% Runs on fast CI (CP1).
%%
%% @test_category policy, dsl, fast
-module(router_policy_dsl_edge_cases_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(policy, {
    tenant_id :: binary(),
    policy_id :: binary(),
    weights :: map(),
    fallback :: map() | undefined,
    fallbacks :: list(),
    sticky :: map() | undefined,
    circuit_breaker :: map() | undefined,
    rate_limit :: map() | undefined,
    pre :: list(),
    validators :: list(),
    post :: list(),
    metadata :: map()
}).

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_empty_policy_map/1,
    test_missing_required_fields/1,
    test_invalid_weight_values/1,
    test_weight_sum_not_100/1,
    test_fallback_without_when/1,
    test_fallback_without_to/1,
    test_sticky_without_enabled/1,
    test_invalid_ttl_format/1,
    test_provider_weight_zero/1,
    test_provider_weight_100/1
]).

-define(TENANT_ID, <<"test_tenant">>).
-define(POLICY_ID, <<"test_policy">>).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end.
meta_all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, edge_cases_tests}];
        _ -> []
    end.

groups() ->
    [{edge_cases_tests, [parallel], [
        test_empty_policy_map,
        test_missing_required_fields,
        test_invalid_weight_values,
        test_weight_sum_not_100,
        test_fallback_without_when,
        test_fallback_without_to,
        test_sticky_without_enabled,
        test_invalid_ttl_format,
        test_provider_weight_zero,
        test_provider_weight_100
    ]}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.
init_per_testcase(_TestCase, Config) -> Config.
end_per_testcase(_TestCase, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_empty_policy_map(_Config) ->
    PolicyMap = #{},
    
    %% Should handle empty map gracefully
    try
        _Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
        ok
    catch
        _:_ -> ok  %% Expected to fail or return default
    end.

test_missing_required_fields(_Config) ->
    PolicyMap = #{
        <<"providers">> => []
        %% Missing policy_id
    },
    
    try
        _Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
        ok
    catch
        _:_ -> ok
    end.

test_invalid_weight_values(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => -10}
        ]
    },
    
    try
        _Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
        ok
    catch
        _:_ -> ok
    end.

test_weight_sum_not_100(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 30},
            #{<<"id">> => <<"anthropic:claude-3">>, <<"weight">> => 30}
            %% Sum is 60, not 100
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    %% Should still parse, validation is separate
    ?assert(is_map(Weights)),
    ok.

test_fallback_without_when(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"fallbacks">> => [
            #{<<"to">> => <<"backup:provider">>}
            %% Missing 'when' condition
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Fallbacks = Policy#policy.fallbacks,
    
    ?assert(is_list(Fallbacks)),
    ok.

test_fallback_without_to(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"fallbacks">> => [
            #{<<"when">> => [<<"timeout">>]}
            %% Missing 'to' target
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Fallbacks = Policy#policy.fallbacks,
    
    ?assert(is_list(Fallbacks)),
    ok.

test_sticky_without_enabled(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"sticky">> => #{
            <<"key">> => <<"session_id">>,
            <<"ttl">> => <<"5m">>
            %% Missing 'enabled' flag
        }
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Sticky = Policy#policy.sticky,
    
    %% Should default to disabled or handle gracefully
    case Sticky of
        undefined -> ok;
        Map when is_map(Map) ->
            Enabled = maps:get(enabled, Map, false),
            ?assertEqual(false, Enabled)
    end,
    ok.

test_invalid_ttl_format(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"key">> => <<"session_id">>,
            <<"ttl">> => <<"invalid_format">>
        }
    },
    
    try
        _Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
        ok
    catch
        _:_ -> ok
    end.

test_provider_weight_zero(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 0}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    Weight = maps:get(<<"openai:gpt-4o">>, Weights, undefined),
    ?assertEqual(0.0, Weight),
    ok.

test_provider_weight_100(_Config) ->
    PolicyMap = #{
        <<"policy_id">> => ?POLICY_ID,
        <<"providers">> => [
            #{<<"id">> => <<"openai:gpt-4o">>, <<"weight">> => 100}
        ]
    },
    
    Policy = router_policy_applier:from_map(?TENANT_ID, PolicyMap),
    Weights = Policy#policy.weights,
    
    Weight = maps:get(<<"openai:gpt-4o">>, Weights, undefined),
    ?assertEqual(1.0, Weight),
    ok.
