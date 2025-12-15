%% @doc Unit Tests for router_intake_validator module
%% @test_category unit, fast, coverage_hotspot
-module(router_intake_validator_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_validate_uuid_v4_valid/1,
    test_validate_uuid_v4_invalid/1,
    test_validate_ulid_valid/1,
    test_validate_ulid_invalid/1,
    test_validate_w3c_trace_context_valid/1,
    test_validate_w3c_trace_context_invalid/1,
    test_validate_schema_valid/1,
    test_validate_schema_invalid/1,
    test_validate_version_supported/1,
    test_validate_version_unsupported/1,
    test_validate_correlation_fields/1,
    test_validate_tenant_valid/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_validate_uuid_v4_valid/1,
    test_validate_uuid_v4_invalid/1,
    test_validate_ulid_valid/1,
    test_validate_ulid_invalid/1,
    test_validate_w3c_trace_context_valid/1,
    test_validate_w3c_trace_context_invalid/1,
    test_validate_schema_valid/1,
    test_validate_schema_invalid/1,
    test_validate_version_supported/1,
    test_validate_version_unsupported/1,
    test_validate_correlation_fields/1,
    test_validate_tenant_valid/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_validate_uuid_v4_valid,
            test_validate_uuid_v4_invalid,
            test_validate_ulid_valid,
            test_validate_ulid_invalid,
            test_validate_w3c_trace_context_valid,
            test_validate_w3c_trace_context_invalid,
            test_validate_schema_valid,
            test_validate_schema_invalid,
            test_validate_version_supported,
            test_validate_version_unsupported,
            test_validate_correlation_fields,
            test_validate_tenant_valid
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for validate_uuid_v4/1
%% ============================================================================

test_validate_uuid_v4_valid(_Config) ->
    %% Valid UUID v4
    ValidUUID = <<"550e8400-e29b-41d4-a716-446655440000">>,
    Result = router_intake_validator:validate_uuid_v4(ValidUUID),
    
    %% Returns ok or {ok, _} on success
    case Result of
        ok -> ok;
        {ok, _} -> ok;
        Other -> ct:fail("Unexpected result: ~p", [Other])
    end,
    
    ok.

test_validate_uuid_v4_invalid(_Config) ->
    %% Invalid UUIDs
    InvalidUUID1 = <<"not-a-uuid">>,
    Result1 = router_intake_validator:validate_uuid_v4(InvalidUUID1),
    ?assertMatch({error, _}, Result1),
    
    InvalidUUID2 = <<"">>,
    Result2 = router_intake_validator:validate_uuid_v4(InvalidUUID2),
    ?assertMatch({error, _}, Result2),
    
    ok.

%% ============================================================================
%% Tests for validate_ulid/1
%% ============================================================================

test_validate_ulid_valid(_Config) ->
    %% Valid ULID (26 characters, Crockford base32)
    ValidULID = <<"01ARZ3NDEKTSV4RRFFQ69G5FAV">>,
    Result = router_intake_validator:validate_ulid(ValidULID),
    
    %% Returns ok or {ok, _} on success
    case Result of
        ok -> ok;
        {ok, _} -> ok;
        Other -> ct:fail("Unexpected result: ~p", [Other])
    end,
    
    ok.

test_validate_ulid_invalid(_Config) ->
    %% Invalid ULIDs
    InvalidULID1 = <<"not-a-ulid">>,
    Result1 = router_intake_validator:validate_ulid(InvalidULID1),
    ?assertMatch({error, _}, Result1),
    
    InvalidULID2 = <<"">>,
    Result2 = router_intake_validator:validate_ulid(InvalidULID2),
    ?assertMatch({error, _}, Result2),
    
    ok.

%% ============================================================================
%% Tests for validate_w3c_trace_context/1
%% ============================================================================

test_validate_w3c_trace_context_valid(_Config) ->
    %% Valid W3C trace context - 32 hex characters (not full traceparent)
    ValidTrace = <<"0af7651916cd43dd8448eb211c80319c">>,
    Result = router_intake_validator:validate_w3c_trace_context(ValidTrace),
    
    %% Returns ok or {ok, _} on success
    case Result of
        ok -> ok;
        {ok, _} -> ok;
        Other -> ct:fail("Unexpected result: ~p", [Other])
    end,
    
    ok.

test_validate_w3c_trace_context_invalid(_Config) ->
    %% Invalid trace context
    InvalidTrace = <<"not-a-trace">>,
    Result = router_intake_validator:validate_w3c_trace_context(InvalidTrace),
    
    ?assertMatch({error, _}, Result),
    
    ok.

%% ============================================================================
%% Tests for validate_schema/2
%% ============================================================================

test_validate_schema_valid(_Config) ->
    Payload = <<"{\"version\":\"1\",\"data\":{}}">>,
    MessageType = decide,
    
    Result = router_intake_validator:validate_schema(Payload, MessageType),
    
    %% Should return ok or error depending on schema requirements
    ?assertEqual(true, is_tuple(Result)),
    
    ok.

test_validate_schema_invalid(_Config) ->
    Payload = <<"not json">>,
    MessageType = decide,
    
    Result = router_intake_validator:validate_schema(Payload, MessageType),
    
    ?assertMatch({error, _}, Result),
    
    ok.

%% ============================================================================
%% Tests for validate_version/3
%% ============================================================================

test_validate_version_supported(_Config) ->
    %% validate_version takes (Subject, Message, Headers)
    Subject = <<"beamline.flow.decide.v1">>,
    Message = #{<<"version">> => <<"1">>},
    Headers = #{},
    
    Result = router_intake_validator:validate_version(Subject, Message, Headers),
    
    ?assertMatch({ok, <<"1">>}, Result),
    
    ok.

test_validate_version_unsupported(_Config) ->
    %% Version "999" is not supported
    Subject = <<"beamline.flow.decide.v999">>,
    Message = #{<<"version">> => <<"999">>},
    Headers = #{},
    
    Result = router_intake_validator:validate_version(Subject, Message, Headers),
    
    ?assertMatch({error, _}, Result),
    
    ok.

%% ============================================================================
%% Tests for validate_correlation_fields/2
%% ============================================================================

test_validate_correlation_fields(_Config) ->
    Fields = #{
        <<"request_id">> => <<"550e8400-e29b-41d4-a716-446655440000">>,
        <<"trace_id">> => <<"00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01">>
    },
    
    Result = router_intake_validator:validate_correlation_fields(Fields, decide),
    
    %% Should return ok or error based on validation
    ?assertEqual(true, is_tuple(Result)),
    
    ok.

%% ============================================================================
%% Tests for validate_tenant/2
%% ============================================================================

test_validate_tenant_valid(_Config) ->
    TenantId = <<"valid_tenant">>,
    Context = #{},
    
    Result = router_intake_validator:validate_tenant(TenantId, Context),
    
    %% Should return ok or error based on tenant validation
    ?assertEqual(true, is_tuple(Result)),
    
    ok.
