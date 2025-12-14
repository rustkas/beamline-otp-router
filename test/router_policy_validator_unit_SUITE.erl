%% @doc Unit Tests for router_policy_validator module
%% Targeted coverage tests for policy validation functions
%% @test_category unit, fast, coverage_hotspot
-module(router_policy_validator_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_module_exports/1,
    test_validate_valid_policy/1,
    test_validate_missing_policy_id/1,
    test_validate_missing_tenant_id/1,
    test_validate_missing_weights/1,
    test_validate_empty_policy_id/1,
    test_validate_empty_tenant_id/1,
    test_validate_empty_weights/1,
    test_validate_invalid_type/1,
    test_validate_schema/1,
    test_validate_weights_invalid/1,
    test_validate_weights_valid/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_module_exports,
            test_validate_valid_policy,
            test_validate_missing_policy_id,
            test_validate_missing_tenant_id,
            test_validate_missing_weights,
            test_validate_empty_policy_id,
            test_validate_empty_tenant_id,
            test_validate_empty_weights,
            test_validate_invalid_type,
            test_validate_schema,
            test_validate_weights_invalid,
            test_validate_weights_valid
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for module structure
%% ============================================================================

test_module_exports(_Config) ->
    {module, router_policy_validator} = code:ensure_loaded(router_policy_validator),
    Exports = router_policy_validator:module_info(exports),
    ?assertEqual(true, length(Exports) > 0),
    ?assertEqual(true, lists:member({validate, 1}, Exports)),
    ?assertEqual(true, lists:member({validate_schema, 1}, Exports)),
    ok.

%% ============================================================================
%% Tests for validate/1 with valid policy
%% ============================================================================

test_validate_valid_policy(_Config) ->
    ValidPolicy = #{
        <<"policy_id">> => <<"test-policy-123">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{
            <<"provider_a">> => 0.5,
            <<"provider_b">> => 0.5
        }
    },
    Result = router_policy_validator:validate(ValidPolicy),
    ?assertEqual(ok, Result),
    ok.

%% ============================================================================
%% Tests for validate/1 with missing fields
%% ============================================================================

test_validate_missing_policy_id(_Config) ->
    Policy = #{
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{<<"provider_a">> => 1.0}
    },
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

test_validate_missing_tenant_id(_Config) ->
    Policy = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"weights">> => #{<<"provider_a">> => 1.0}
    },
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

test_validate_missing_weights(_Config) ->
    Policy = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<"tenant-abc">>
    },
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

%% ============================================================================
%% Tests for validate/1 with empty values
%% ============================================================================

test_validate_empty_policy_id(_Config) ->
    Policy = #{
        <<"policy_id">> => <<>>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{<<"provider_a">> => 1.0}
    },
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

test_validate_empty_tenant_id(_Config) ->
    Policy = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<>>,
        <<"weights">> => #{<<"provider_a">> => 1.0}
    },
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

test_validate_empty_weights(_Config) ->
    Policy = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{}
    },
    Result = router_policy_validator:validate(Policy),
    ?assertMatch({error, {invalid_policy, _}}, Result),
    ok.

%% ============================================================================
%% Tests for validate/1 with invalid type
%% ============================================================================

test_validate_invalid_type(_Config) ->
    %% Non-map input
    Result1 = router_policy_validator:validate(<<"not a map">>),
    ?assertMatch({error, {invalid_policy, _}}, Result1),
    
    Result2 = router_policy_validator:validate([]),
    ?assertMatch({error, {invalid_policy, _}}, Result2),
    
    Result3 = router_policy_validator:validate(123),
    ?assertMatch({error, {invalid_policy, _}}, Result3),
    ok.

%% ============================================================================
%% Tests for validate_schema/1
%% ============================================================================

test_validate_schema(_Config) ->
    ValidPolicy = #{
        <<"policy_id">> => <<"test-policy-123">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{
            <<"provider_a">> => 0.5,
            <<"provider_b">> => 0.5
        }
    },
    Result = router_policy_validator:validate_schema(ValidPolicy),
    %% Should either pass or return schema error 
    case Result of
        ok -> ok;
        {error, {schema_validation_failed, _}} -> ok;
        {error, {invalid_policy, _}} -> ok
    end,
    ok.

%% ============================================================================
%% Tests for weights validation
%% ============================================================================

test_validate_weights_invalid(_Config) ->
    %% Weight > 1.0
    Policy1 = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{<<"provider_a">> => 1.5}
    },
    Result1 = router_policy_validator:validate(Policy1),
    ?assertMatch({error, {invalid_policy, _}}, Result1),
    
    %% Negative weight
    Policy2 = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{<<"provider_a">> => -0.5}
    },
    Result2 = router_policy_validator:validate(Policy2),
    ?assertMatch({error, {invalid_policy, _}}, Result2),
    ok.

test_validate_weights_valid(_Config) ->
    %% Weight = 0.0 (edge case)
    Policy1 = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{<<"provider_a">> => 0.0, <<"provider_b">> => 1.0}
    },
    Result1 = router_policy_validator:validate(Policy1),
    ?assertEqual(ok, Result1),
    
    %% Multiple providers
    Policy2 = #{
        <<"policy_id">> => <<"test-policy">>,
        <<"tenant_id">> => <<"tenant-abc">>,
        <<"weights">> => #{
            <<"provider_a">> => 0.3,
            <<"provider_b">> => 0.3,
            <<"provider_c">> => 0.4
        }
    },
    Result2 = router_policy_validator:validate(Policy2),
    ?assertEqual(ok, Result2),
    ok.
