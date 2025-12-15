%% @doc Unit Tests for router_intake_error_codes module
%% @test_category unit, fast, coverage_hotspot
-module(router_intake_error_codes_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test exports
-export([
    all/0, 
    groups/0, 
    init_per_suite/1, 
    end_per_suite/1, 
    init_per_group/2, 
    end_per_group/2,
    init_per_testcase/2, 
    end_per_testcase/2
]).

%% Test function exports
-export([
    test_error_code_to_string_schema_validation/1,
    test_error_code_to_string_version_unsupported/1,
    test_error_code_to_string_correlation_fields/1,
    test_error_code_to_string_tenant_forbidden/1,
    test_error_code_to_string_idempotency_violation/1,
    test_error_code_to_string_payload_too_small/1,
    test_error_code_to_string_internal_error/1,
    test_error_code_severity_error/1,
    test_error_code_severity_warn/1,
    test_error_code_message_schema/1,
    test_error_code_message_version/1,
    test_error_code_message_tenant/1,
    test_error_code_message_idempotency/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, 
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2,
    test_error_code_to_string_schema_validation/1,
    test_error_code_to_string_version_unsupported/1,
    test_error_code_to_string_correlation_fields/1,
    test_error_code_to_string_tenant_forbidden/1,
    test_error_code_to_string_idempotency_violation/1,
    test_error_code_to_string_payload_too_small/1,
    test_error_code_to_string_internal_error/1,
    test_error_code_severity_error/1,
    test_error_code_severity_warn/1,
    test_error_code_message_schema/1,
    test_error_code_message_version/1,
    test_error_code_message_tenant/1,
    test_error_code_message_idempotency/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_error_code_to_string_schema_validation,
            test_error_code_to_string_version_unsupported,
            test_error_code_to_string_correlation_fields,
            test_error_code_to_string_tenant_forbidden,
            test_error_code_to_string_idempotency_violation,
            test_error_code_to_string_payload_too_small,
            test_error_code_to_string_internal_error,
            test_error_code_severity_error,
            test_error_code_severity_warn,
            test_error_code_message_schema,
            test_error_code_message_version,
            test_error_code_message_tenant,
            test_error_code_message_idempotency
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for error_code_to_string/1
%% ============================================================================

test_error_code_to_string_schema_validation(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(schema_validation_failed),
    ?assertEqual(<<"SCHEMA_VALIDATION_FAILED">>, Result),
    ok.

test_error_code_to_string_version_unsupported(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(version_unsupported),
    ?assertEqual(<<"VERSION_UNSUPPORTED">>, Result),
    ok.

test_error_code_to_string_correlation_fields(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(correlation_fields_invalid),
    ?assertEqual(<<"CORRELATION_FIELDS_INVALID">>, Result),
    ok.

test_error_code_to_string_tenant_forbidden(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(tenant_forbidden),
    ?assertEqual(<<"TENANT_FORBIDDEN">>, Result),
    ok.

test_error_code_to_string_idempotency_violation(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(idempotency_violation),
    ?assertEqual(<<"IDEMPOTENCY_VIOLATION">>, Result),
    ok.

test_error_code_to_string_payload_too_small(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(payload_too_small),
    ?assertEqual(<<"PAYLOAD_TOO_SMALL">>, Result),
    ok.

test_error_code_to_string_internal_error(_Config) ->
    Result = router_intake_error_codes:error_code_to_string(internal_validation_error),
    ?assertEqual(<<"INTERNAL_VALIDATION_ERROR">>, Result),
    ok.

%% ============================================================================
%% Tests for error_code_severity/1
%% ============================================================================

test_error_code_severity_error(_Config) ->
    %% These should return 'error' severity
    ?assertEqual(error, router_intake_error_codes:error_code_severity(schema_validation_failed)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(version_unsupported)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(correlation_fields_invalid)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(tenant_forbidden)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(internal_validation_error)),
    ok.

test_error_code_severity_warn(_Config) ->
    %% These should return 'warn' severity
    ?assertEqual(warn, router_intake_error_codes:error_code_severity(idempotency_violation)),
    ?assertEqual(warn, router_intake_error_codes:error_code_severity(payload_too_small)),
    ok.

%% ============================================================================
%% Tests for error_code_message/2
%% ============================================================================

test_error_code_message_schema(_Config) ->
    Context = #{<<"reason">> => <<"missing required field">>},
    Result = router_intake_error_codes:error_code_message(schema_validation_failed, Context),
    
    ?assertEqual(true, is_binary(Result)),
    ?assertEqual(true, binary:match(Result, <<"Schema validation failed">>) =/= nomatch),
    
    ok.

test_error_code_message_version(_Config) ->
    Context = #{
        <<"version">> => <<"2">>,
        <<"supported_versions">> => [<<"1">>]
    },
    Result = router_intake_error_codes:error_code_message(version_unsupported, Context),
    
    ?assertEqual(true, is_binary(Result)),
    ?assertEqual(true, binary:match(Result, <<"Unsupported schema version">>) =/= nomatch),
    
    ok.

test_error_code_message_tenant(_Config) ->
    Context = #{<<"reason">> => <<"not in allowlist">>},
    Result = router_intake_error_codes:error_code_message(tenant_forbidden, Context),
    
    ?assertEqual(true, is_binary(Result)),
    ?assertEqual(true, binary:match(Result, <<"Tenant validation failed">>) =/= nomatch),
    
    ok.

test_error_code_message_idempotency(_Config) ->
    Context = #{<<"reason">> => <<"message already processed">>},
    Result = router_intake_error_codes:error_code_message(idempotency_violation, Context),
    
    ?assertEqual(true, is_binary(Result)),
    ?assertEqual(true, binary:match(Result, <<"Idempotency violation">>) =/= nomatch),
    
    ok.
