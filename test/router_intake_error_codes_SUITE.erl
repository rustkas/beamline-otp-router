%% @doc Common Test suite for router_intake_error_codes
%% Tests: Error code definitions, string conversion, severity, message generation
-module(router_intake_error_codes_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
%% Test functions are called via groups() by Common Test framework
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_error_code_to_string/1,
    test_error_code_severity/1,
    test_error_code_message_schema_validation_failed/1,
    test_error_code_message_version_unsupported/1,
    test_error_code_message_correlation_fields_invalid/1,
    test_error_code_message_tenant_forbidden/1,
    test_error_code_message_idempotency_violation/1,
    test_error_code_message_internal_validation_error/1,
    test_all_error_codes_defined/1
]}).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_error_code_to_string,
            test_error_code_severity,
            test_error_code_message_schema_validation_failed,
            test_error_code_message_version_unsupported,
            test_error_code_message_correlation_fields_invalid,
            test_error_code_message_tenant_forbidden,
            test_error_code_message_idempotency_violation,
            test_error_code_message_internal_validation_error,
            test_all_error_codes_defined
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% @doc Test error code to string conversion
test_error_code_to_string(_Config) ->
    ?assertEqual(<<"SCHEMA_VALIDATION_FAILED">>, router_intake_error_codes:error_code_to_string(schema_validation_failed)),
    ?assertEqual(<<"VERSION_UNSUPPORTED">>, router_intake_error_codes:error_code_to_string(version_unsupported)),
    ?assertEqual(<<"CORRELATION_FIELDS_INVALID">>, router_intake_error_codes:error_code_to_string(correlation_fields_invalid)),
    ?assertEqual(<<"TENANT_FORBIDDEN">>, router_intake_error_codes:error_code_to_string(tenant_forbidden)),
    ?assertEqual(<<"IDEMPOTENCY_VIOLATION">>, router_intake_error_codes:error_code_to_string(idempotency_violation)),
    ?assertEqual(<<"INTERNAL_VALIDATION_ERROR">>, router_intake_error_codes:error_code_to_string(internal_validation_error)),
    ok.

%% @doc Test error code severity
test_error_code_severity(_Config) ->
    %% Most errors are ERROR severity
    ?assertEqual(error, router_intake_error_codes:error_code_severity(schema_validation_failed)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(version_unsupported)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(correlation_fields_invalid)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(tenant_forbidden)),
    ?assertEqual(error, router_intake_error_codes:error_code_severity(internal_validation_error)),
    
    %% Idempotency violation is WARN (not critical)
    ?assertEqual(warn, router_intake_error_codes:error_code_severity(idempotency_violation)),
    ok.

%% @doc Test error code message for schema_validation_failed
test_error_code_message_schema_validation_failed(_Config) ->
    Context = #{<<"reason">> => <<"protobuf_decode_failed">>},
    Message = router_intake_error_codes:error_code_message(schema_validation_failed, Context),
    ?assert(is_binary(Message)),
    ?assert(binary:match(Message, <<"Schema validation failed">>) =/= nomatch),
    ?assert(binary:match(Message, <<"protobuf_decode_failed">>) =/= nomatch),
    ok.

%% @doc Test error code message for version_unsupported
test_error_code_message_version_unsupported(_Config) ->
    Context = #{
        <<"version">> => <<"2">>,
        <<"supported_versions">> => [<<"1">>]
    },
    Message = router_intake_error_codes:error_code_message(version_unsupported, Context),
    ?assert(is_binary(Message)),
    ?assert(binary:match(Message, <<"Unsupported schema version">>) =/= nomatch),
    ?assert(binary:match(Message, <<"2">>) =/= nomatch),
    ?assert(binary:match(Message, <<"1">>) =/= nomatch),
    ok.

%% @doc Test error code message for correlation_fields_invalid
test_error_code_message_correlation_fields_invalid(_Config) ->
    Context = #{<<"reason">> => <<"missing_run_id">>},
    Message = router_intake_error_codes:error_code_message(correlation_fields_invalid, Context),
    ?assert(is_binary(Message)),
    ?assert(binary:match(Message, <<"Correlation fields validation failed">>) =/= nomatch),
    ?assert(binary:match(Message, <<"missing_run_id">>) =/= nomatch),
    ok.

%% @doc Test error code message for tenant_forbidden
test_error_code_message_tenant_forbidden(_Config) ->
    Context = #{<<"reason">> => <<"tenant_not_in_allowlist">>},
    Message = router_intake_error_codes:error_code_message(tenant_forbidden, Context),
    ?assert(is_binary(Message)),
    ?assert(binary:match(Message, <<"Tenant validation failed">>) =/= nomatch),
    ?assert(binary:match(Message, <<"tenant_not_in_allowlist">>) =/= nomatch),
    ok.

%% @doc Test error code message for idempotency_violation
test_error_code_message_idempotency_violation(_Config) ->
    Context = #{<<"reason">> => <<"duplicate_request_with_conflicting_data">>},
    Message = router_intake_error_codes:error_code_message(idempotency_violation, Context),
    ?assert(is_binary(Message)),
    ?assert(binary:match(Message, <<"Idempotency violation">>) =/= nomatch),
    ?assert(binary:match(Message, <<"duplicate_request_with_conflicting_data">>) =/= nomatch),
    ok.

%% @doc Test error code message for internal_validation_error
test_error_code_message_internal_validation_error(_Config) ->
    Context = #{<<"reason">> => <<"ets_table_not_available">>},
    Message = router_intake_error_codes:error_code_message(internal_validation_error, Context),
    ?assert(is_binary(Message)),
    ?assert(binary:match(Message, <<"Internal validation error">>) =/= nomatch),
    ?assert(binary:match(Message, <<"ets_table_not_available">>) =/= nomatch),
    ok.

%% @doc Test that all error codes are properly defined
test_all_error_codes_defined(_Config) ->
    %% Test that all error codes can be converted to string
    AllCodes = [
        schema_validation_failed,
        version_unsupported,
        correlation_fields_invalid,
        tenant_forbidden,
        idempotency_violation,
        internal_validation_error
    ],
    
    lists:foreach(fun(Code) ->
        String = router_intake_error_codes:error_code_to_string(Code),
        ?assert(is_binary(String)),
        ?assert(byte_size(String) > 0),
        
        Severity = router_intake_error_codes:error_code_severity(Code),
        ?assert(Severity =:= warn orelse Severity =:= error),
        
        %% Test message generation with minimal context
        Context = #{<<"reason">> => <<"test">>},
        Message = router_intake_error_codes:error_code_message(Code, Context),
        ?assert(is_binary(Message)),
        ?assert(byte_size(Message) > 0)
    end, AllCodes),
    
    ok.

