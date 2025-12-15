%% @doc Common Test Suite for Router Policy
%% Tests policy loading, parsing, and basic operations
%% @test_category cp1_smoke, fast
-module(router_policy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

-define(TENANT_ID, <<"test_tenant">>).
-define(POLICY_ID, <<"test_policy">>).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_load_policy_success/1,
    test_load_policy_not_found/1,
    test_parse_policy_json_valid/1,
    test_parse_policy_json_invalid/1,
    test_parse_policy_json_map/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_load_policy_not_found/1,
    test_load_policy_success/1,
    test_parse_policy_json_invalid/1,
    test_parse_policy_json_map/1,
    test_parse_policy_json_valid/1,
    %% Negative tests (Task 3)
    test_validate_missing_required_fields/1,
    test_validate_empty_weights/1,
    test_validate_invalid_weight_values/1,
    test_validate_negative_weight/1,
    test_validate_weight_over_one/1,
    test_validate_non_numeric_weight/1
]).


%% Test suite configuration
all() ->
    %% CP1 core tests run by default - no env gating (Task 1 fix)
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_load_policy_success,
            test_load_policy_not_found,
            test_parse_policy_json_valid,
            test_parse_policy_json_invalid,
            test_parse_policy_json_map,
            test_validate_missing_required_fields,
            test_validate_empty_weights,
            test_validate_invalid_weight_values,
            test_validate_negative_weight,
            test_validate_weight_over_one,
            test_validate_non_numeric_weight
        ]}
    ].

init_per_suite(Config) ->
    router_test_bootstrap:init_per_suite(Config, #{
        start => ensure_all_started,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock
        },
        wait_for_app_start => [{router_policy_store, 1000}]
    }).

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{
        start => ensure_all_started,
        stop => stop_app
    }).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Load policy successfully
test_load_policy_success(_Config) ->
    TenantId = ?TENANT_ID,
    PolicyId = ?POLICY_ID,
    
    %% Create and store a test policy
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    
    %% Store policy
    {ok, _StoredPolicy} = router_policy_store:upsert_policy(TenantId, Policy),
    
    %% Load policy using router_policy:load_policy/2
    {ok, LoadedPolicy} = router_policy:load_policy(TenantId, PolicyId),
    
    %% Verify loaded policy matches stored policy
    ?assertEqual(PolicyId, LoadedPolicy#policy.policy_id),
    ?assertEqual(TenantId, LoadedPolicy#policy.tenant_id),
    ?assertEqual(0.7, maps:get(<<"openai">>, LoadedPolicy#policy.weights)),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, LoadedPolicy#policy.weights)),
    
    ok.

%% Test: Load non-existent policy
test_load_policy_not_found(_Config) ->
    TenantId = ?TENANT_ID,
    NonExistentPolicyId = <<"non_existent_policy">>,
    
    %% Try to load non-existent policy
    {error, not_found} = router_policy:load_policy(TenantId, NonExistentPolicyId),
    
    ok.

%% Test: Parse valid JSON policy
test_parse_policy_json_valid(_Config) ->
    ValidJson = jsx:encode(#{
        <<"version">> => <<"1.0">>,
        <<"weights">> => #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        }
    }),
    
    {ok, ParsedPolicy} = router_policy:parse_policy_json(ValidJson),
    ?assertEqual(<<"1.0">>, maps:get(<<"version">>, ParsedPolicy)),
    ?assertEqual(0.7, maps:get(<<"openai">>, maps:get(<<"weights">>, ParsedPolicy))),
    ?assertEqual(0.3, maps:get(<<"anthropic">>, maps:get(<<"weights">>, ParsedPolicy))),
    
    ok.

%% Test: Parse invalid JSON policy
test_parse_policy_json_invalid(_Config) ->
    InvalidJson = <<"{invalid json}">>,
    
    %% Should return error for invalid JSON
    Result = router_policy:parse_policy_json(InvalidJson),
    ?assertMatch({error, _}, Result),
    
    %% Test with non-JSON binary - jsx:decode returns {error, _} which becomes {error, {invalid_json, _}}
    Result2 = router_policy:parse_policy_json(<<"not json">>),
    ?assertMatch({error, {invalid_json, _}}, Result2),
    
    ok.

%% Test: Parse policy JSON that is already a map
test_parse_policy_json_map(_Config) ->
    PolicyMap = #{
        <<"version">> => <<"1.0">>,
        <<"weights">> => #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        }
    },
    
    {ok, ParsedPolicy} = router_policy:parse_policy_json(PolicyMap),
    ?assertEqual(PolicyMap, ParsedPolicy),
    
    ok.

%% ============================================================================
%% NEGATIVE TESTS (Task 3: Policy validation edge cases)
%% ============================================================================

%% Test: Validate policy with missing required fields
test_validate_missing_required_fields(_Config) ->
    %% Missing tenant_id
    Policy1 = #{
        <<"policy_id">> => <<"test">>,
        <<"weights">> => #{<<"openai">> => 1.0}
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy1)),
    
    %% Missing policy_id
    Policy2 = #{
        <<"tenant_id">> => <<"test">>,
        <<"weights">> => #{<<"openai">> => 1.0}
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy2)),
    
    %% Missing weights
    Policy3 = #{
        <<"tenant_id">> => <<"test">>,
        <<"policy_id">> => <<"test">>
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy3)),
    
    ok.

%% Test: Validate policy with empty weights
test_validate_empty_weights(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test">>,
        <<"policy_id">> => <<"test">>,
        <<"weights">> => #{}
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy)),
    ok.

%% Test: Validate policy with invalid weight structure
test_validate_invalid_weight_values(_Config) ->
    %% weights must be a map
    Policy = #{
        <<"tenant_id">> => <<"test">>,
        <<"policy_id">> => <<"test">>,
        <<"weights">> => [0.5, 0.5]  %% List instead of map
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy)),
    ok.

%% Test: Validate policy with negative weight
test_validate_negative_weight(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test">>,
        <<"policy_id">> => <<"test">>,
        <<"weights">> => #{
            <<"openai">> => -0.5,
            <<"anthropic">> => 0.5
        }
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy)),
    ok.

%% Test: Validate policy with weight over 1.0
test_validate_weight_over_one(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test">>,
        <<"policy_id">> => <<"test">>,
        <<"weights">> => #{
            <<"openai">> => 1.5,
            <<"anthropic">> => 0.5
        }
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy)),
    ok.

%% Test: Validate policy with non-numeric weight
test_validate_non_numeric_weight(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test">>,
        <<"policy_id">> => <<"test">>,
        <<"weights">> => #{
            <<"openai">> => <<"half">>,  %% String instead of number
            <<"anthropic">> => 0.5
        }
    },
    ?assertMatch({error, {invalid_policy, _}}, router_policy_validator:validate(Policy)),
    ok.
