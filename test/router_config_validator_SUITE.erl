%% @doc Configuration Validator Tests
%%
%% Tests configuration validation, templates, and compatibility checks.
%% Verifies that configuration validation works correctly.
%%
%% @test_category configuration
-module(router_config_validator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_validate_config/1,
    test_validate_config_value/1,
    test_get_config_template/1,
    test_check_required_config/1,
    test_check_config_compatibility/1
]}).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_validate_config/1,
    test_validate_config_value/1,
    test_get_config_template/1,
    test_check_required_config/1,
    test_check_config_compatibility/1
]).

all() -> [
    test_validate_config,
    test_validate_config_value,
    test_get_config_template,
    test_check_required_config,
    test_check_config_compatibility
].

init_per_suite(Config) ->
    ok = router_test_utils:start_router_app(),
    Config.

end_per_suite(_Config) ->
    router_test_utils:stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: Validate configuration
test_validate_config(_Config) ->
    case router_config_validator:validate_config() of
        {ok, Report} ->
            ?assert(maps:is_key(all_valid, Report)),
            ?assert(maps:is_key(required_config, Report)),
            ?assert(maps:is_key(validation_results, Report)),
            ?assert(maps:is_key(compatibility, Report)),
            ok;
        {error, Reason} ->
            ct:fail({validation_failed, Reason})
    end.

%% @doc Test: Validate configuration value
test_validate_config_value(_Config) ->
    %% Test valid values
    ?assertMatch({ok, true}, router_config_validator:validate_config_value(grpc_enabled, true)),
    ?assertMatch({ok, 9000}, router_config_validator:validate_config_value(grpc_port, 9000)),
    ?assertMatch({ok, _}, router_config_validator:validate_config_value(admin_api_key, <<"test_key_123">>)),
    ?assertMatch({ok, mock}, router_config_validator:validate_config_value(nats_mode, mock)),
    ?assertMatch({ok, 90}, router_config_validator:validate_config_value(audit_retention_days, 90)),
    
    %% Test invalid values
    ?assertMatch({error, _}, router_config_validator:validate_config_value(grpc_enabled, <<"true">>)),
    ?assertMatch({error, _}, router_config_validator:validate_config_value(grpc_port, 70000)),
    ?assertMatch({error, _}, router_config_validator:validate_config_value(admin_api_key, <<"short">>)),
    ?assertMatch({error, _}, router_config_validator:validate_config_value(nats_mode, <<"mock">>)),
    ?assertMatch({error, _}, router_config_validator:validate_config_value(audit_retention_days, -1)),
    
    ok.

%% @doc Test: Get configuration template
test_get_config_template(_Config) ->
    Template = router_config_validator:get_config_template(),
    
    ?assert(maps:is_key(grpc_enabled, Template)),
    ?assert(maps:is_key(grpc_port, Template)),
    ?assert(maps:is_key(admin_api_key, Template)),
    ?assert(maps:is_key(nats_mode, Template)),
    ?assert(maps:is_key(audit_retention_days, Template)),
    
    ok.

%% @doc Test: Check required configuration
test_check_required_config(_Config) ->
    RequiredCheck = router_config_validator:check_required_config(),
    
    ?assert(maps:is_key(passed, RequiredCheck)),
    ?assert(maps:is_key(required_keys, RequiredCheck)),
    ?assert(maps:is_key(results, RequiredCheck)),
    
    ok.

%% @doc Test: Check configuration compatibility
test_check_config_compatibility(_Config) ->
    Config = #{
        grpc_enabled => true,
        admin_grpc_enabled => true,
        telemetry_enabled => true,
        metrics_export_enabled => true,
        cp2_plus_allowed => true,
        idempotency_enabled => true
    },
    
    Compatibility = router_config_validator:check_config_compatibility(Config),
    
    ?assert(maps:is_key(passed, Compatibility)),
    ?assert(maps:is_key(admin_grpc, Compatibility)),
    ?assert(maps:is_key(metrics, Compatibility)),
    ?assert(maps:is_key(cp2_features, Compatibility)),
    
    ok.

