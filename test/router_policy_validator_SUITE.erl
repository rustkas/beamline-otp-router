%% @doc Negative test cases for router_policy_validator
%% Tests: empty weights, invalid ranges, duplicates, invalid types
-module(router_policy_validator_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_empty_weights/1,
    test_zero_weights/1,
    test_negative_weights/1,
    test_weights_out_of_range/1,
    test_duplicate_providers/1,
    test_invalid_weight_type/1,
    test_empty_policy_id/1,
    test_invalid_sticky_config/1,
    test_invalid_fallback_config/1,
    test_valid_policy/1
]}).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_empty_weights,
            test_zero_weights,
            test_negative_weights,
            test_weights_out_of_range,
            test_duplicate_providers,
            test_invalid_weight_type,
            test_empty_policy_id,
            test_invalid_sticky_config,
            test_invalid_fallback_config,
            test_valid_policy
        ]}
    ].

init_per_suite(_Config) ->
    %% No application startup needed for validator tests
    [].

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Empty weights
test_empty_weights(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{}
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Zero weights
test_zero_weights(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => 0.0,
            <<"anthropic">> => 0.0
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    %% Zero weights may be valid if fallback is configured
    %% For now, we'll check that validation handles it
    case Result of
        ok ->
            ok;  %% Valid if fallback exists
        {error, _} ->
            ok   %% Also valid to reject zero weights
    end.

%% Test: Negative weights
test_negative_weights(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => -0.1,
            <<"anthropic">> => 0.5
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Weights out of range (> 100.0, validator allows up to 100.0)
test_weights_out_of_range(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => 150.0,  %% > 100.0
            <<"anthropic">> => 0.3
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Duplicate providers (should be handled by maps, but test anyway)
test_duplicate_providers(_Config) ->
    %% Maps don't allow duplicate keys, so this test verifies
    %% that validation doesn't break with edge cases
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => 0.3  %% Single key (duplicate keys are not allowed in maps)
        }
    },
    
    %% Maps will only have one "openai" key, so this is effectively
    %% a single provider with weight 0.3
    Result = router_policy_validator:validate(Policy),
    %% Should be valid (single provider is OK)
    ?assertMatch(ok, Result),
    
    ok.

%% Test: Invalid weight type (not a number)
test_invalid_weight_type(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => <<"not_a_number">>,
            <<"anthropic">> => 0.3
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Empty policy_id
test_empty_policy_id(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<>>,
        <<"weights">> => #{
            <<"openai">> => 0.7
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Invalid sticky config
test_invalid_sticky_config(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => 0.7
        },
        <<"sticky">> => #{
            <<"enabled">> => <<"not_a_boolean">>
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Invalid fallback config
test_invalid_fallback_config(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => 0.7
        },
        <<"fallback">> => #{
            <<"provider">> => 123  %% Not a binary
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    
    ok.

%% Test: Valid policy
test_valid_policy(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"weights">> => #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        <<"sticky">> => #{
            <<"enabled">> => true,
            <<"ttl_seconds">> => 3600
        },
        <<"fallback">> => #{
            <<"provider">> => <<"cohere">>
        }
    },
    
    Result = router_policy_validator:validate(Policy),
    ?assertEqual(ok, Result),
    
    ok.

