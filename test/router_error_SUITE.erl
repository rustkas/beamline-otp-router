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
    test_internal_error/1
]}).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_error_mapping_table,
            test_to_grpc_basic,
            test_to_grpc_with_context,
            test_to_grpc_unknown,
            test_reload,
            test_reload_validation,
            test_get,
            test_nats_unavailable,
            test_invalid_payload,
            test_internal_error
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
    
    %% Verify status codes are correct
    {?GRPC_STATUS_INVALID_ARGUMENT, _} = maps:get(missing_tenant_id, Mapping),
    {?GRPC_STATUS_NOT_FOUND, _} = maps:get(policy_not_found, Mapping),
    {?GRPC_STATUS_INTERNAL, _} = maps:get(no_provider_available, Mapping),
    {?GRPC_STATUS_INVALID_ARGUMENT, _} = maps:get(invalid_policy, Mapping),
    
    ok.

%% @doc Test basic error mapping (data-driven)
test_to_grpc_basic(_Config) ->
    %% Data-driven test: list of {ErrorReason, ExpectedStatus, ExpectedMessagePattern}
    TestCases = [
        {missing_tenant_id, ?GRPC_STATUS_INVALID_ARGUMENT, <<"missing tenant_id">>},
        {policy_not_found, ?GRPC_STATUS_NOT_FOUND, <<"policy not found">>},
        {no_provider_available, ?GRPC_STATUS_INTERNAL, <<"no provider available">>},
        {invalid_policy, ?GRPC_STATUS_INVALID_ARGUMENT, <<"invalid policy">>},
        {invalid_request, ?GRPC_STATUS_INVALID_ARGUMENT, <<"invalid request">>},
        {missing_message, ?GRPC_STATUS_INVALID_ARGUMENT, <<"missing message">>},
        {rate_limit_exceeded, ?GRPC_STATUS_RESOURCE_EXHAUSTED, <<"rate limit exceeded">>},
        {quota_exceeded, ?GRPC_STATUS_RESOURCE_EXHAUSTED, <<"quota exceeded">>},
        {service_down, ?GRPC_STATUS_UNAVAILABLE, <<"service temporarily unavailable">>},
        {timeout, ?GRPC_STATUS_UNAVAILABLE, <<"request timeout">>}
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus, ExpectedMessage}) ->
        {Status, Message} = router_error:to_grpc(ErrorReason),
        ExpectedStatus = Status,
        ExpectedMessage = Message,
        ok
    end, TestCases),
    
    ok.

%% @doc Test error mapping with context override
test_to_grpc_with_context(_Config) ->
    %% Test that context message overrides default
    {Status, Message} = router_error:to_grpc(missing_tenant_id, #{
        context => <<"Custom error message">>
    }),
    ?GRPC_STATUS_INVALID_ARGUMENT = Status,
    <<"Custom error message">> = Message,
    
    %% Test that default message is used when context is missing
    {Status2, Message2} = router_error:to_grpc(policy_not_found, #{}),
    ?GRPC_STATUS_NOT_FOUND = Status2,
    <<"policy not found">> = Message2,
    
    ok.

%% @doc Test unknown error mapping (should map to INTERNAL)
test_to_grpc_unknown(_Config) ->
    %% Unknown error should map to INTERNAL
    {Status, Message} = router_error:to_grpc(unknown_error_reason),
    ?GRPC_STATUS_INTERNAL = Status,
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
    %% Test new error types added
    TestCases = [
        {nats_unavailable, ?GRPC_STATUS_UNAVAILABLE, <<"NATS service unavailable">>},
        {rate_limited, ?GRPC_STATUS_RESOURCE_EXHAUSTED, <<"rate limited">>},
        {budget_exceeded, ?GRPC_STATUS_RESOURCE_EXHAUSTED, <<"budget exceeded">>},
        {unauthenticated, ?GRPC_STATUS_UNAUTHENTICATED, <<"unauthenticated">>},
        {permission_denied, ?GRPC_STATUS_PERMISSION_DENIED, <<"permission denied">>}
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus, ExpectedMessage}) ->
        {Status, Message} = router_error:to_grpc(ErrorReason),
        ExpectedStatus = Status,
        ExpectedMessage = Message,
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
    NewMapping = maps:put(test_custom_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"test error">>}, Original),
    
    %% Reload mapping
    ok = router_error:reload(NewMapping),
    
    %% Verify new mapping is active
    {Status, Message} = router_error:to_grpc(test_custom_error),
    ?GRPC_STATUS_INVALID_ARGUMENT = Status,
    <<"test error">> = Message,
    
    %% Restore original mapping
    ok = router_error:reload(Original),
    
    %% Verify original mapping restored
    {Status2, _} = router_error:to_grpc(missing_tenant_id),
    ?GRPC_STATUS_INVALID_ARGUMENT = Status2,
    
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
    ?GRPC_STATUS_UNAVAILABLE = Status,
    <<"NATS service unavailable">> = Message,
    
    ok.

%% @doc Test invalid payload error mapping
test_invalid_payload(_Config) ->
    %% Invalid payload should map to INVALID_ARGUMENT
    {Status, Message} = router_error:to_grpc(invalid_request),
    ?GRPC_STATUS_INVALID_ARGUMENT = Status,
    <<"invalid request">> = Message,
    
    %% Test with context
    {Status2, Message2} = router_error:to_grpc(invalid_request, #{
        context => <<"Payload size exceeds limit">>
    }),
    ?GRPC_STATUS_INVALID_ARGUMENT = Status2,
    <<"Payload size exceeds limit">> = Message2,
    
    ok.

%% @doc Test internal error mapping
test_internal_error(_Config) ->
    %% Internal error should map to INTERNAL
    {Status, Message} = router_error:to_grpc(internal_error),
    ?GRPC_STATUS_INTERNAL = Status,
    <<"internal server error">> = Message,
    
    %% Test with context
    {Status2, Message2} = router_error:to_grpc(internal_error, #{
        context => <<"Unexpected error in routing logic">>
    }),
    ?GRPC_STATUS_INTERNAL = Status2,
    <<"Unexpected error in routing logic">> = Message2,
    
    ok.

