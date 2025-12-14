%% @doc Unit tests for CP2 error code normalization in router_result_consumer
-module(router_errors_mapping_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_parse_error_code/1,
    test_missing_status_code/1,
    test_invalid_status_code/1,
    test_tenant_validation_failed_code/1,
    test_usage_emit_failed_code/1,
    test_unknown_error_code/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_invalid_status_code/1,
    test_missing_status_code/1,
    test_parse_error_code/1,
    test_tenant_validation_failed_code/1,
    test_unknown_error_code/1,
    test_usage_emit_failed_code/1
]).



-export([groups_for_level/1]).

all() ->
    [].

%% Unit tests run in all tiers
groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast, sanity
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_parse_error_code,
            test_missing_status_code,
            test_invalid_status_code,
            test_tenant_validation_failed_code,
            test_usage_emit_failed_code,
            test_unknown_error_code
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, false),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

test_parse_error_code(_Config) ->
    <<"PARSE_ERROR">> = router_result_consumer:error_code_reason(parse_error),
    ok.

test_missing_status_code(_Config) ->
    <<"MISSING_STATUS">> = router_result_consumer:error_code_reason(missing_status),
    ok.

test_invalid_status_code(_Config) ->
    <<"INVALID_STATUS">> = router_result_consumer:error_code_reason(invalid_status),
    ok.

test_tenant_validation_failed_code(_Config) ->
    <<"TENANT_VALIDATION_FAILED">> = router_result_consumer:error_code_reason(tenant_validation_failed),
    ok.

test_usage_emit_failed_code(_Config) ->
    <<"USAGE_EMIT_FAILED">> = router_result_consumer:error_code_reason(usage_emit_failed),
    ok.

test_unknown_error_code(_Config) ->
    <<"UNKNOWN_ERROR">> = router_result_consumer:error_code_reason(some_unexpected_atom),
    ok.
