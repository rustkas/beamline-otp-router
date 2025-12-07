%% @doc CT suite for error mapping to gRPC status codes
%% Tests error mapping decision matrix from OPERATIONAL_GUIDE.md

-module(router_error_status_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]}).

%% gRPC Status Codes (use integer constants directly)
%% Note: These are gRPC status codes as defined in the gRPC specification
-define(STATUS_INVALID_ARGUMENT, 3).
-define(STATUS_NOT_FOUND, 5).
-define(STATUS_PERMISSION_DENIED, 7).
-define(STATUS_RESOURCE_EXHAUSTED, 8).
-define(STATUS_INTERNAL, 13).
-define(STATUS_UNAVAILABLE, 14).
-define(STATUS_UNAUTHENTICATED, 16).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_invalid_argument_mapping,
            test_not_found_mapping,
            test_resource_exhausted_mapping,
            test_unavailable_mapping,
            test_unauthenticated_mapping,
            test_permission_denied_mapping,
            test_internal_mapping,
            test_unknown_error_mapping,
            test_error_mapping_table_completeness,
            test_context_override
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

%% @doc Test INVALID_ARGUMENT (3) mapping
test_invalid_argument_mapping(_Config) ->
    TestCases = [
        {missing_tenant_id, ?STATUS_INVALID_ARGUMENT},
        {invalid_policy, ?STATUS_INVALID_ARGUMENT},
        {invalid_request, ?STATUS_INVALID_ARGUMENT},
        {missing_message, ?STATUS_INVALID_ARGUMENT}
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus}) ->
        {Status, _Message} = router_error:to_grpc(ErrorReason),
        ExpectedStatus = Status,
        ok
    end, TestCases),
    
    ok.

%% @doc Test NOT_FOUND (5) mapping
test_not_found_mapping(_Config) ->
    {Status, Message} = router_error:to_grpc(policy_not_found),
    ?STATUS_NOT_FOUND = Status,
    <<"policy not found">> = Message,
    ok.

%% @doc Test RESOURCE_EXHAUSTED (8) mapping
test_resource_exhausted_mapping(_Config) ->
    TestCases = [
        {rate_limit_exceeded, ?STATUS_RESOURCE_EXHAUSTED},
        {quota_exceeded, ?STATUS_RESOURCE_EXHAUSTED},
        {rate_limited, ?STATUS_RESOURCE_EXHAUSTED},
        {budget_exceeded, ?STATUS_RESOURCE_EXHAUSTED}
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus}) ->
        {Status, _Message} = router_error:to_grpc(ErrorReason),
        ExpectedStatus = Status,
        ok
    end, TestCases),
    
    ok.

%% @doc Test UNAVAILABLE (14) mapping
test_unavailable_mapping(_Config) ->
    TestCases = [
        {service_down, ?STATUS_UNAVAILABLE},
        {timeout, ?STATUS_UNAVAILABLE},
        {nats_unavailable, ?STATUS_UNAVAILABLE}
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus}) ->
        {Status, _Message} = router_error:to_grpc(ErrorReason),
        ExpectedStatus = Status,
        ok
    end, TestCases),
    
    ok.

%% @doc Test UNAUTHENTICATED (16) mapping
test_unauthenticated_mapping(_Config) ->
    {Status, Message} = router_error:to_grpc(unauthenticated),
    ?STATUS_UNAUTHENTICATED = Status,
    <<"unauthenticated">> = Message,
    ok.

%% @doc Test PERMISSION_DENIED (7) mapping
test_permission_denied_mapping(_Config) ->
    {Status, Message} = router_error:to_grpc(permission_denied),
    ?STATUS_PERMISSION_DENIED = Status,
    <<"permission denied">> = Message,
    ok.

%% @doc Test INTERNAL (13) mapping
test_internal_mapping(_Config) ->
    TestCases = [
        {no_provider_available, ?STATUS_INTERNAL},
        {internal_error, ?STATUS_INTERNAL}
    ],
    
    lists:foreach(fun({ErrorReason, ExpectedStatus}) ->
        {Status, _Message} = router_error:to_grpc(ErrorReason),
        ExpectedStatus = Status,
        ok
    end, TestCases),
    
    ok.

%% @doc Test unknown error mapping (should map to INTERNAL)
test_unknown_error_mapping(_Config) ->
    {Status, Message} = router_error:to_grpc(unknown_error_reason),
    ?STATUS_INTERNAL = Status,
    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0),
    ok.

%% @doc Test error mapping table completeness
%% Verifies that all error types from OPERATIONAL_GUIDE.md are covered
test_error_mapping_table_completeness(_Config) ->
    %% Clear persistent_term to ensure fresh mapping
    persistent_term:erase({router_error, error_mapping}),
    Mapping = router_error:error_mapping_table(),
    
    %% Required error types from decision matrix
    RequiredErrors = [
        %% INVALID_ARGUMENT (3)
        missing_tenant_id,
        invalid_policy,
        invalid_request,
        missing_message,
        
        %% NOT_FOUND (5)
        policy_not_found,
        
        %% RESOURCE_EXHAUSTED (8)
        rate_limit_exceeded,
        quota_exceeded,
        rate_limited,
        budget_exceeded,
        
        %% UNAVAILABLE (14)
        service_down,
        timeout,
        nats_unavailable,
        
        %% UNAUTHENTICATED (16) / PERMISSION_DENIED (7)
        unauthenticated,
        permission_denied,
        
        %% INTERNAL (13)
        no_provider_available,
        internal_error
    ],
    
    %% Verify all required errors are in mapping
    lists:foreach(fun(ErrorReason) ->
        case maps:find(ErrorReason, Mapping) of
            {ok, {Status, Message}} when is_integer(Status), is_binary(Message) ->
                ?assert(Status >= 0),
                ?assert(Status =< 16),
                ok;
            {ok, Value} ->
                ct:fail("Invalid mapping value for ~p: ~p (expected {Status, Message})", [ErrorReason, Value]);
            error ->
                ct:fail("Required error ~p not found in mapping", [ErrorReason])
        end
    end, RequiredErrors),
    
    ok.

%% @doc Test context override for custom error messages
test_context_override(_Config) ->
    %% Test that context message overrides default
    {Status, Message} = router_error:to_grpc(missing_tenant_id, #{
        context => <<"Custom error: tenant_id is required for this operation">>
    }),
    ?STATUS_INVALID_ARGUMENT = Status,
    <<"Custom error: tenant_id is required for this operation">> = Message,
    
    %% Test that default message is used when context is missing
    {Status2, Message2} = router_error:to_grpc(policy_not_found, #{}),
    ?STATUS_NOT_FOUND = Status2,
    <<"policy not found">> = Message2,
    
    ok.

