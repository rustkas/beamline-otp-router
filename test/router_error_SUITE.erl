%% @doc Unit tests for router_error module
%% Tests error mapping table and gRPC status code conversion
%% @test_category cp1_smoke, fast

-module(router_error_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("grpcbox/include/grpcbox.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    %% Test functions called via groups
    test_error_mapping_table/1,
    test_to_grpc_basic/1,
    test_to_grpc_with_context/1,
    test_to_grpc_unknown/1,
    test_get/1,
    test_extended_mapping/1,
    test_reload_validation/1,
    test_reload/1,
    test_nats_unavailable/1,
    test_invalid_payload/1,
    test_internal_error/1,
    test_no_persistent_term_leak/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_error_mapping_table/1,
    test_extended_mapping/1,
    test_get/1,
    test_internal_error/1,
    test_invalid_payload/1,
    test_nats_unavailable/1,
    test_reload/1,
    test_reload_validation/1,
    test_to_grpc_basic/1,
    test_to_grpc_unknown/1,
    test_to_grpc_with_context/1,
    %% Additional deep coverage tests (Task 4)
    test_to_grpc_tuple_format/1,
    test_error_ensure_binary/1,
    %% Persistent term cleanup regression (Task 10)
    test_no_persistent_term_leak/1
]).



all() ->
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
        %% Sequential execution required - tests modify global persistent_term
        {unit_tests, [sequence], [
            test_error_mapping_table,
            test_to_grpc_basic,
            test_to_grpc_with_context,
            test_to_grpc_unknown,
            test_to_grpc_tuple_format,
            test_error_ensure_binary,
            test_get,
            test_nats_unavailable,
            test_invalid_payload,
            test_internal_error,
            %% Reload tests last - they modify global state
            test_reload_validation,
            test_reload,
            %% Persistent term cleanup check (Task 10)
            test_no_persistent_term_leak
        ]}
    ].

init_per_suite(Config) ->
    ok = application:set_env(beamline_router, disable_heir, true),
    ok = application:set_env(beamline_router, telemetry_enabled, false),
    Config.

end_per_suite(_Config) ->
    %% Clean up persistent_term to not leak state to other suites
    catch persistent_term:erase({router_error, error_mapping}),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test error mapping table structure
test_error_mapping_table(_Config) ->
    Mapping = router_error:error_mapping_table(),
    
    %% Verify mapping is a map
    ?assert(is_map(Mapping)),
    
    %% Verify expected error codes are present
    ?assert(maps:is_key(missing_tenant_id, Mapping)),
    ?assert(maps:is_key(policy_not_found, Mapping)),
    ?assert(maps:is_key(no_provider_available, Mapping)),
    ?assert(maps:is_key(invalid_policy, Mapping)),
    
    %% Verify status codes are correct (using numeric values)
    {3, _} = maps:get(missing_tenant_id, Mapping),    % INVALID_ARGUMENT
    {5, _} = maps:get(policy_not_found, Mapping),     % NOT_FOUND
    {13, _} = maps:get(no_provider_available, Mapping), % INTERNAL
    {3, _} = maps:get(invalid_policy, Mapping),       % INVALID_ARGUMENT
    
    ok.

%% @doc Test basic error mapping (data-driven)
test_to_grpc_basic(_Config) ->
    %% Data-driven test: list of {ErrorReason, ExpectedStatus, ExpectedMessagePattern}
    %% Note: Using numeric status codes directly to avoid include conflicts
    TestCases = [
        {missing_tenant_id, 3, <<"missing tenant_id">>},           % INVALID_ARGUMENT
        {policy_not_found, 5, <<"policy not found">>},              % NOT_FOUND
        {no_provider_available, 13, <<"no provider available">>},   % INTERNAL
        {invalid_policy, 3, <<"invalid policy">>},                  % INVALID_ARGUMENT
        {invalid_request, 3, <<"invalid request">>},                % INVALID_ARGUMENT
        {missing_message, 3, <<"missing message">>},                % INVALID_ARGUMENT
        {rate_limit_exceeded, 8, <<"rate limit exceeded">>},        % RESOURCE_EXHAUSTED
        {quota_exceeded, 8, <<"quota exceeded">>},                  % RESOURCE_EXHAUSTED
        {service_down, 14, <<"service temporarily unavailable">>},  % UNAVAILABLE
        {timeout, 14, <<"request timeout">>}                        % UNAVAILABLE
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus, ExpectedMessage}) ->
        {Status, Message} = router_error:to_grpc(ErrorReason),
        ?assertEqual(ExpectedStatus, Status, io_lib:format("Status mismatch for ~p", [ErrorReason])),
        ?assertEqual(ExpectedMessage, Message, io_lib:format("Message mismatch for ~p", [ErrorReason])),
        ok
    end, TestCases),
    
    ok.

%% @doc Test error mapping with context override
test_to_grpc_with_context(_Config) ->
    %% Test that context message overrides default
    {Status, Message} = router_error:to_grpc(missing_tenant_id, #{
        context => <<"Custom error message">>
    }),
    ?assertEqual(3, Status),  % INVALID_ARGUMENT
    ?assertEqual(<<"Custom error message">>, Message),
    
    %% Test that default message is used when context is missing
    {Status2, Message2} = router_error:to_grpc(policy_not_found, #{}),
    ?assertEqual(5, Status2),  % NOT_FOUND
    ?assertEqual(<<"policy not found">>, Message2),
    
    ok.

%% @doc Test unknown error mapping (should map to INTERNAL)
test_to_grpc_unknown(_Config) ->
    %% Unknown error should map to INTERNAL
    {Status, Message} = router_error:to_grpc(unknown_error_reason),
    ?assertEqual(13, Status),  % INTERNAL
    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0),
    
    ok.

%% @doc Test get functionality
test_get(_Config) ->
    Mapping = router_error:get(),
    ?assert(is_map(Mapping)),
    ?assert(maps:size(Mapping) > 0),
    
    %% Verify structure: all values are {Status, Message} tuples
    maps:fold(fun
        (_Key, {Status, Message}, Acc) when is_integer(Status), is_binary(Message) ->
            Acc;
        (Key, Value, _Acc) ->
            ct:fail("Invalid mapping entry: ~p => ~p", [Key, Value])
    end, ok, Mapping),
    
    ok.

%% @doc Test extended error mapping (new error types)
test_extended_mapping(_Config) ->
    %% Test new error types added (using numeric status codes)
    TestCases = [
        {nats_unavailable, 14, <<"NATS service unavailable">>},  % UNAVAILABLE
        {rate_limited, 8, <<"rate limited">>},                    % RESOURCE_EXHAUSTED
        {budget_exceeded, 8, <<"budget exceeded">>},              % RESOURCE_EXHAUSTED
        {unauthenticated, 16, <<"unauthenticated">>},             % UNAUTHENTICATED
        {permission_denied, 7, <<"permission denied">>}           % PERMISSION_DENIED
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus, ExpectedMessage}) ->
        {Status, Message} = router_error:to_grpc(ErrorReason),
        ?assertEqual(ExpectedStatus, Status, io_lib:format("Status mismatch for ~p", [ErrorReason])),
        ?assertEqual(ExpectedMessage, Message, io_lib:format("Message mismatch for ~p", [ErrorReason])),
        ok
    end, TestCases),
    
    ok.

%% @doc Test reload validation
test_reload_validation(_Config) ->
    %% Test invalid mapping (should fail)
    InvalidMapping = #{invalid_key => <<"not a tuple">>},
    {error, _} = router_error:reload(InvalidMapping),
    
    %% Test invalid status code (should fail)
    InvalidStatus = #{invalid_status => {999, <<"invalid">>}},
    {error, _} = router_error:reload(InvalidStatus),
    
    ok.

%% @doc Test reload functionality
test_reload(_Config) ->
    %% Get original mapping
    Original = router_error:get(),
    ?assert(is_map(Original)),
    ?assert(maps:is_key(missing_tenant_id, Original)),
    
    %% Create new mapping with additional entry
    NewMapping = maps:put(test_custom_error, {3, <<"test error">>}, Original),  % INVALID_ARGUMENT
    
    %% Reload mapping
    ok = router_error:reload(NewMapping),
    
    %% Verify new mapping is active
    {Status, Message} = router_error:to_grpc(test_custom_error),
    ?assertEqual(3, Status),  % INVALID_ARGUMENT
    ?assertEqual(<<"test error">>, Message),
    
    %% Restore original mapping
    ok = router_error:reload(Original),
    
    %% Verify original mapping restored
    {Status2, _} = router_error:to_grpc(missing_tenant_id),
    ?assertEqual(3, Status2),  % INVALID_ARGUMENT
    
    %% Test invalid mapping (should fail)
    InvalidMapping = #{invalid_key => <<"not a tuple">>},
    {error, _} = router_error:reload(InvalidMapping),
    
    %% Test invalid status code (should fail)
    InvalidStatus = #{invalid_status => {999, <<"invalid">>}},
    {error, _} = router_error:reload(InvalidStatus),
    
    ok.

%% @doc Test NATS unavailable error mapping
test_nats_unavailable(_Config) ->
    %% NATS unavailable should map to UNAVAILABLE
    {Status, Message} = router_error:to_grpc(nats_unavailable),
    ?assertEqual(14, Status),  % UNAVAILABLE
    ?assertEqual(<<"NATS service unavailable">>, Message),
    
    ok.

%% @doc Test invalid payload error mapping
test_invalid_payload(_Config) ->
    %% Invalid payload should map to INVALID_ARGUMENT
    {Status, Message} = router_error:to_grpc(invalid_request),
    ?assertEqual(3, Status),  % INVALID_ARGUMENT
    ?assertEqual(<<"invalid request">>, Message),
    
    %% Test with context
    {Status2, Message2} = router_error:to_grpc(invalid_request, #{
        context => <<"Payload size exceeds limit">>
    }),
    ?assertEqual(3, Status2),  % INVALID_ARGUMENT
    ?assertEqual(<<"Payload size exceeds limit">>, Message2),
    
    ok.

%% @doc Test internal error mapping
test_internal_error(_Config) ->
    %% Internal error should map to INTERNAL
    {Status, Message} = router_error:to_grpc(internal_error),
    ?assertEqual(13, Status),  % INTERNAL
    ?assertEqual(<<"internal server error">>, Message),

    %% Test with context
    {Status2, Message2} = router_error:to_grpc(internal_error, #{
        context => <<"Unexpected error in routing logic">>
    }),
    ?assertEqual(13, Status2),  % INTERNAL
    ?assertEqual(<<"Unexpected error in routing logic">>, Message2),

    ok.

%% @doc Test tuple format: {ErrorReason, ErrorContext}
test_to_grpc_tuple_format(_Config) ->
    %% Tuple format should work just like calling to_grpc/2
    {Status, Message} = router_error:to_grpc({missing_tenant_id, #{
        context => <<"Tenant header missing">>
    }}),
    ?assertEqual(3, Status),  % INVALID_ARGUMENT
    ?assertEqual(<<"Tenant header missing">>, Message),
    
    %% Tuple with empty context uses default message
    {Status2, Message2} = router_error:to_grpc({policy_not_found, #{}}),
    ?assertEqual(5, Status2),  % NOT_FOUND
    ?assertEqual(<<"policy not found">>, Message2),
    
    ok.

%% @doc Test ensure_binary converts various types
test_error_ensure_binary(_Config) ->
    %% Test with atom context (should be converted to binary)
    {Status, Message} = router_error:to_grpc(internal_error, #{
        context => test_atom_error
    }),
    ?assertEqual(13, Status),  % INTERNAL
    ?assert(is_binary(Message)),
    
    %% Test with list context (should be converted to binary)
    {Status2, Message2} = router_error:to_grpc(internal_error, #{
        context => "string error message"
    }),
    ?assertEqual(13, Status2),  % INTERNAL
    ?assert(is_binary(Message2)),
    ?assertEqual(<<"string error message">>, Message2),
    
    %% Test with nested tuple context (Task 9 - regression)
    %% Nested structures should be converted to binary representation
    {Status3, Message3} = router_error:to_grpc(internal_error, #{
        context => {nested, {error, some_reason}}
    }),
    ?assertEqual(13, Status3),  % INTERNAL
    ?assert(is_binary(Message3)),
    %% Should contain string representation of the tuple
    ?assert(byte_size(Message3) > 0),
    
    %% Test with integer context (edge case)
    {Status4, Message4} = router_error:to_grpc(internal_error, #{
        context => 12345
    }),
    ?assertEqual(13, Status4),  % INTERNAL
    ?assert(is_binary(Message4)),
    
    ok.

%% @doc Regression test: no unexpected persistent_term keys after operations (Task 10)
%% Verifies router_error only uses its designated key and doesn't leak others.
test_no_persistent_term_leak(_Config) ->
    %% Snapshot router_error's known key
    ExpectedKey = {router_error, error_mapping},
    
    %% Force the mapping to be initialized
    _ = router_error:error_mapping_table(),
    
    %% Get all persistent_term keys that belong to router
    AllKeys = persistent_term:get(),
    RouterErrorKeys = [K || {K, _V} <- AllKeys, 
                            is_tuple(K) andalso 
                            tuple_size(K) >= 1 andalso
                            element(1, K) =:= router_error],
    
    %% Should only have the expected key
    ?assertEqual([ExpectedKey], RouterErrorKeys, 
                 "router_error should only have error_mapping persistent_term key"),
    
    %% Cleanup the key
    persistent_term:erase(ExpectedKey),
    
    %% Verify cleanup
    RouterErrorKeysAfter = [K || {K, _V} <- persistent_term:get(), 
                                  is_tuple(K) andalso 
                                  tuple_size(K) >= 1 andalso
                                  element(1, K) =:= router_error],
    ?assertEqual([], RouterErrorKeysAfter, "router_error keys should be cleaned up"),
    
    ok.

