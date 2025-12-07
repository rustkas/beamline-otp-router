%% @doc Gateway → Router Integration Tests
%%
%% Tests integration between Gateway (NestJS) and Router (Erlang) via gRPC.
%% These tests verify end-to-end request flow from Gateway to Router.
%%
%% @test_category integration, gateway, grpc
-module(router_gateway_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").
-include("../include/flow_pb.hrl").
-include_lib("grpcbox/include/grpcbox.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         test_gateway_to_router_decide/1, test_gateway_to_router_error_handling/1,
         test_gateway_to_router_policy_not_found/1, test_gateway_to_router_rate_limiting/1,
         test_gateway_backpressure_status_query/1, test_gateway_backpressure_notification/1,
         test_gateway_backpressure_health_check/1, test_gateway_to_router_overload_response/1]).

all() -> [
    test_gateway_to_router_decide,
    test_gateway_to_router_error_handling,
    test_gateway_to_router_policy_not_found,
    test_gateway_to_router_rate_limiting,
    test_gateway_backpressure_status_query,
    test_gateway_backpressure_notification,
    test_gateway_backpressure_health_check,
    test_gateway_to_router_overload_response
].

init_per_suite(Config) ->
    %% Start Router application
    ok = router_test_utils:start_router_app(),
    
    %% Create test policy
    TenantId = <<"test_tenant_gateway">>,
    PolicyId = <<"test_policy_gateway">>,
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = undefined,
        sticky = undefined,
        metadata = #{}
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, PolicyId, Policy, undefined),
    
    [{tenant_id, TenantId}, {policy_id, PolicyId} | Config].

end_per_suite(Config) ->
    %% Cleanup
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    router_policy_store:delete_policy(TenantId, PolicyId, undefined),
    router_test_utils:stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: Gateway → Router Decide request
test_gateway_to_router_decide(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Create RouteRequest (as Gateway would)
    Message = #'Message'{
        message_id = <<"msg_gateway_1">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello from Gateway">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    
    %% Create context (Gateway context)
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Call Router.Decide (as Gateway would)
    case router_grpc:decide(Ctx, Request) of
        {ok, Response, _} ->
            RouteDecisionPb = flow_pb:decode_msg(Response, 'RouteDecision'),
            ?assert(is_record(RouteDecisionPb, 'RouteDecision')),
            ?assertNotEqual(undefined, RouteDecisionPb#'RouteDecision'.provider_id),
            ?assertNotEqual(undefined, RouteDecisionPb#'RouteDecision'.reason),
            ok;
        {grpc_error, {Status, Msg}} ->
            ct:fail({unexpected_error, Status, Msg})
    end.

%% @doc Test: Gateway → Router error handling
test_gateway_to_router_error_handling(_Config) ->
    
    %% Create invalid request (missing message)
    RouteRequestPb = #'RouteRequest'{
        message = undefined,
        policy_id = <<"test_policy">>,
        context = []
    },
    Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    
    %% Create context
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Call Router.Decide (should fail)
    case catch router_grpc:decide(Ctx, Request) of
        {grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, _}} ->
            ok;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%% @doc Test: Gateway → Router policy not found
test_gateway_to_router_policy_not_found(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    
    %% Create request with non-existent policy
    Message = #'Message'{
        message_id = <<"msg_gateway_2">>,
        tenant_id = TenantId,
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
    
    %% Create context
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Call Router.Decide (should fail with NOT_FOUND)
    case catch router_grpc:decide(Ctx, Request) of
        {grpc_error, {?GRPC_STATUS_NOT_FOUND, _}} ->
            ok;
        Other ->
            ct:fail({unexpected_result, Other})
    end.

%% @doc Test: Gateway → Router rate limiting
test_gateway_to_router_rate_limiting(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Create multiple requests (to trigger rate limit)
    Message = #'Message'{
        message_id = <<"msg_gateway_3">>,
        tenant_id = TenantId,
        message_type = <<"chat">>,
        payload = <<"Hello">>,
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    RouteRequestPb = #'RouteRequest'{
        message = Message,
        policy_id = PolicyId,
        context = []
    },
    Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Send multiple requests (may hit rate limit)
    Results = lists:map(fun(_) ->
        router_grpc:decide(Ctx, Request)
    end, lists:seq(1, 10)),
    
    %% At least some requests should succeed
    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    ?assert(SuccessCount > 0, "At least some requests should succeed").

%% @doc Test: Gateway backpressure status query
test_gateway_backpressure_status_query(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup: Create ETS tables for backpressure
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 1200, erlang:system_time(millisecond)}),
    
    LatencyTable = router_intake_latency_cache,
    case ets:whereis(LatencyTable) of
        undefined -> ets:new(LatencyTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(LatencyTable, {{Subject, p95}, 5500, erlang:system_time(millisecond)}),
    
    %% Get Gateway-formatted status
    GatewayStatus = router_gateway_backpressure:get_backpressure_status_for_gateway(Subject),
    
    %% Verify status structure
    ?assert(maps:is_key(subject, GatewayStatus)),
    ?assert(maps:is_key(status, GatewayStatus)),
    ?assert(maps:is_key(metrics, GatewayStatus)),
    ?assert(maps:is_key(thresholds, GatewayStatus)),
    ?assert(maps:is_key(policy, GatewayStatus)),
    ?assert(maps:is_key(timestamp, GatewayStatus)),
    
    %% Verify status value
    Status = maps:get(status, GatewayStatus),
    ?assert(Status =:= <<"active">> orelse Status =:= <<"warning">> orelse Status =:= <<"inactive">>),
    
    %% Verify metrics structure
    Metrics = maps:get(metrics, GatewayStatus, #{}),
    ?assert(maps:is_key(pending_messages, Metrics)),
    ?assert(maps:is_key(latency_p95_ms, Metrics)),
    ?assert(maps:is_key(inflight_messages, Metrics)),
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

%% @doc Test: Gateway backpressure notification
test_gateway_backpressure_notification(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    GatewayEndpoint = <<"http://gateway:3000/api/backpressure">>,
    
    %% Setup: Create backpressure status
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% Get Gateway status
    GatewayStatus = router_gateway_backpressure:get_backpressure_status_for_gateway(Subject),
    
    %% Send notification to Gateway
    NotificationResult = router_gateway_backpressure:notify_gateway_backpressure_status(GatewayEndpoint, GatewayStatus),
    ?assertMatch({ok, _}, NotificationResult),
    
    {ok, Notification} = NotificationResult,
    ?assert(maps:is_key(type, Notification)),
    ?assert(maps:is_key(status, Notification)),
    ?assert(maps:is_key(subject, Notification)),
    ?assert(maps:is_key(timestamp, Notification)),
    ?assertEqual(<<"backpressure_status">>, maps:get(type, Notification)),
    
    %% Cleanup
    ets:delete(PendingTable),
    ok.

%% @doc Test: Gateway backpressure health check
test_gateway_backpressure_health_check(_Config) ->
    %% Get health status
    HealthStatus = router_gateway_backpressure:check_gateway_backpressure_health(),
    
    %% Verify health structure
    ?assert(maps:is_key(healthy, HealthStatus)),
    ?assert(maps:is_key(status_counts, HealthStatus)),
    ?assert(maps:is_key(total_subjects, HealthStatus)),
    ?assert(maps:is_key(timestamp, HealthStatus)),
    
    %% Verify status counts
    StatusCounts = maps:get(status_counts, HealthStatus, #{}),
    ?assert(maps:is_key(active, StatusCounts)),
    ?assert(maps:is_key(warning, StatusCounts)),
    ?assert(maps:is_key(inactive, StatusCounts)),
    
    ok.

%% @doc Test: Gateway → Router overload response
test_gateway_to_router_overload_response(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Setup: Create overload condition
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    LatencyTable = router_intake_latency_cache,
    case ets:whereis(LatencyTable) of
        undefined -> ets:new(LatencyTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(LatencyTable, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    %% Check backpressure status
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    
    %% Get Gateway response
    GatewayResponse = router_gateway_backpressure:build_gateway_backpressure_response(Subject),
    ?assert(maps:is_key(success, GatewayResponse)),
    ?assert(maps:is_key(data, GatewayResponse)),
    ?assert(maps:get(success, GatewayResponse)),
    
    %% Verify response data
    Data = maps:get(data, GatewayResponse, #{}),
    ?assert(maps:is_key(status, Data)),
    ?assertEqual(<<"active">>, maps:get(status, Data)),
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

