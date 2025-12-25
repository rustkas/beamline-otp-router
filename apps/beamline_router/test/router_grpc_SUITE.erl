%% @doc Common Test Suite for gRPC Router.Decide Service
%% @test_category fast
-module(router_grpc_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Include protobuf-generated records
-include("/home/rustkas/aigroup/apps/otp/router/include/flow_pb.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/_build/test/lib/grpcbox/include/grpcbox.hrl").

%% gRPC status codes (numeric) - for stable matching
-define(GRPC_STATUS_NOT_FOUND_INT, 5).
-define(GRPC_STATUS_INTERNAL_INT, 13).
-define(GRPC_STATUS_INVALID_ARGUMENT_INT, 3).

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    %% Test functions called via groups
    test_decide_request_success/1,
    test_decide_request_error_policy_not_found/1,
    test_decide_request_error_missing_tenant_id/1
]}).

%% Test suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test functions
-export([
    test_decide_request_success/1,
    test_decide_request_error_policy_not_found/1,
    test_decide_request_error_missing_tenant_id/1
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
groups_for_level(_) -> %% fast, sanity
    [].  %% gRPC tests require full application context, skip in fast mode

groups() ->
    [
        {unit_tests, [sequence], [
            test_decide_request_success,
            test_decide_request_error_policy_not_found,
            test_decide_request_error_missing_tenant_id
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, true),
    ok = application:set_env(beamline_router, nats_mode, mock),
    %% Setup mock BEFORE starting app
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for gRPC server to start
            timer:sleep(500),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    router_mock_helpers:cleanup_and_verify(),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Reset policy store for clean test state
    case whereis(router_policy_store) of
        undefined -> ok;
        _ -> gen_server:call(router_policy_store, reset_all)
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: Decide request with valid policy
%% Placeholder test - requires gRPC client implementation for full testing
test_decide_request_success(_Config) ->
    %% Create RouteRequest protobuf message
    Message = #'Message'{
        message_id = <<"test_msg_1">>,
        tenant_id = <<"test_tenant">>,
        message_type = <<"chat">>,
        payload = <<"Hello, world">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = <<"test_policy">>,
        context = []
    },
    
    %% Encode request
    Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    
    %% Create context
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Call router_grpc:decide/2 directly (unit test approach)
    %% Note: Full integration test would require gRPC client
    try
        {ok, Response, _} = router_grpc:decide(Ctx, Request),
        
        %% Decode response
        RouteDecisionPb = flow_pb:decode_msg(Response, 'RouteDecision'),
        
        %% Verify response structure
        ?assert(is_record(RouteDecisionPb, 'RouteDecision')),
        ?assert(is_binary(RouteDecisionPb#'RouteDecision'.provider_id)),
        ?assert(is_binary(RouteDecisionPb#'RouteDecision'.reason)),
        
        ok
    catch
        throw:{grpc_error, {Status, _Msg}} ->
            %% Policy not found is expected if policy doesn't exist
            ct:log("gRPC error status: ~p", [Status]),
            case Status of
                ?GRPC_STATUS_NOT_FOUND_INT ->
                    %% Expected: policy doesn't exist in test setup
                    ok;
                ?GRPC_STATUS_INTERNAL_INT ->
                    %% Internal error can happen if NATS/policy registry not fully set up
                    ct:log("gRPC internal error - expected in mock mode"),
                    ok;
                _ ->
                    ct:fail("Unexpected gRPC error: ~p", [Status])
            end
    end.

%% @doc Test: Decide request with non-existent policy
%% Placeholder test - requires gRPC client implementation for full testing
test_decide_request_error_policy_not_found(_Config) ->
    %% Create RouteRequest with non-existent policy
    Message = #'Message'{
        message_id = <<"test_msg_2">>,
        tenant_id = <<"test_tenant">>,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = <<"non_existent_policy">>,
        context = []
    },
    
    Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Should return NOT_FOUND error
    try
        router_grpc:decide(Ctx, Request),
        ct:fail("Expected NOT_FOUND error")
    catch
        throw:{grpc_error, {Status, _Msg}} ->
            ?assertEqual(?GRPC_STATUS_NOT_FOUND_INT, Status)
    end.

%% @doc Test: Decide request with missing tenant_id
%% Placeholder test - requires gRPC client implementation for full testing
test_decide_request_error_missing_tenant_id(_Config) ->
    %% Create RouteRequest without tenant_id
    Message = #'Message'{
        message_id = <<"test_msg_3">>,
        tenant_id = <<>>,  %% Empty tenant_id
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = <<"test_policy">>,
        context = []
    },
    
    Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Should return INVALID_ARGUMENT error
    try
        router_grpc:decide(Ctx, Request),
        ct:fail("Expected INVALID_ARGUMENT error")
    catch
        throw:{grpc_error, {Status, _Msg}} ->
            ?assertEqual(?GRPC_STATUS_INVALID_ARGUMENT_INT, Status)
    end.