%% @doc gRPC API Compatibility Tests
%%
%% Tests gRPC API compatibility, message formats, error codes, and protocol compliance.
%% Verifies that the router maintains compatibility with gRPC protocol specifications.
%%
%% @test_category compatibility, grpc
-module(router_grpc_compatibility_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").
-include("../include/flow_pb.hrl").
-include_lib("grpcbox/include/grpcbox.hrl").

-compile([export_all, nowarn_export_all]).

all() -> [
    test_grpc_api_compatibility,
    test_grpc_message_format_route_request,
    test_grpc_message_format_route_decision,
    test_grpc_error_codes,
    test_grpc_metadata_handling,
    test_grpc_version_compatibility,
    test_grpc_service_availability
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

%% @doc Test: gRPC API compatibility verification
test_grpc_api_compatibility(_Config) ->
    case router_protocol_compatibility:verify_grpc_api_compatibility() of
        {ok, Report} ->
            ?assert(maps:is_key(compatible, Report)),
            ?assert(maps:is_key(services, Report)),
            ?assert(maps:is_key(message_formats, Report)),
            ?assert(maps:is_key(error_codes, Report)),
            ?assert(maps:is_key(metadata, Report)),
            ?assert(maps:is_key(version, Report)),
            
            Compatible = maps:get(compatible, Report),
            ?assert(Compatible),
            
            ok;
        {error, Reason} ->
            ct:fail({compatibility_verification_failed, Reason})
    end.

%% @doc Test: RouteRequest message format
test_grpc_message_format_route_request(_Config) ->
    case router_protocol_compatibility:check_grpc_message_format(route_request) of
        {ok, FormatInfo} ->
            ?assert(maps:is_key(message_type, FormatInfo)),
            ?assert(maps:is_key(format, FormatInfo)),
            ?assert(maps:is_key(fields, FormatInfo)),
            ?assertEqual(route_request, maps:get(message_type, FormatInfo)),
            ?assertEqual(valid, maps:get(format, FormatInfo)),
            ok;
        {error, Reason} ->
            ct:fail({message_format_check_failed, Reason})
    end.

%% @doc Test: RouteDecision message format
test_grpc_message_format_route_decision(_Config) ->
    case router_protocol_compatibility:check_grpc_message_format(route_decision) of
        {ok, FormatInfo} ->
            ?assert(maps:is_key(message_type, FormatInfo)),
            ?assert(maps:is_key(format, FormatInfo)),
            ?assert(maps:is_key(fields, FormatInfo)),
            ?assertEqual(route_decision, maps:get(message_type, FormatInfo)),
            ?assertEqual(valid, maps:get(format, FormatInfo)),
            ok;
        {error, Reason} ->
            ct:fail({message_format_check_failed, Reason})
    end.

%% @doc Test: gRPC error codes
test_grpc_error_codes(_Config) ->
    %% Test error code mapping
    ErrorCodes = [
        {invalid_argument, 3},
        {not_found, 5},
        {resource_exhausted, 8},
        {internal, 13},
        {unavailable, 14}
    ],
    
    lists:foreach(fun({Error, ExpectedCode}) ->
        try
            router_error:to_grpc(Error, <<"test">>)
        catch
            {grpc_error, {Code, _}} ->
                ?assertEqual(ExpectedCode, Code)
        end
    end, ErrorCodes),
    
    ok.

%% @doc Test: gRPC metadata handling
test_grpc_metadata_handling(_Config) ->
    %% Test correlation_id extraction
    TestMetadata1 = [
        {<<"x-correlation-id">>, <<"test_corr_id_1">>}
    ],
    TestCtx1 = #{metadata => TestMetadata1},
    
    CorrId1 = router_grpc:extract_correlation_id(TestCtx1),
    ?assertEqual(<<"test_corr_id_1">>, CorrId1),
    
    %% Test alternative header
    TestMetadata2 = [
        {<<"correlation-id">>, <<"test_corr_id_2">>}
    ],
    TestCtx2 = #{metadata => TestMetadata2},
    
    CorrId2 = router_grpc:extract_correlation_id(TestCtx2),
    ?assertEqual(<<"test_corr_id_2">>, CorrId2),
    
    ok.

%% @doc Test: gRPC version compatibility
test_grpc_version_compatibility(_Config) ->
    Version = router_protocol_compatibility:get_grpc_api_version(),
    ?assertEqual(<<"v1">>, Version),
    ok.

%% @doc Test: gRPC service availability
test_grpc_service_availability(_Config) ->
    %% Check Router.Decide service
    ?assert(erlang:function_exported(router_grpc, decide, 2)),
    
    %% Check RouterAdmin service
    ?assert(erlang:function_exported(router_admin_grpc, upsert_policy, 2)),
    ?assert(erlang:function_exported(router_admin_grpc, get_policy, 2)),
    ?assert(erlang:function_exported(router_admin_grpc, list_policies, 2)),
    ?assert(erlang:function_exported(router_admin_grpc, delete_policy, 2)),
    
    ok.

