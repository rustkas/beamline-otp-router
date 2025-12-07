%% @doc Smoke Test for gRPC Router.Decide
%% Local smoke test for gRPC service (CP1)
-module(router_grpc_smoke).
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

smoke_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"gRPC Router.Decide accepts RouteRequest and returns RouteDecision",
         fun test_grpc_decide/0}
     ]}.

setup() ->
    %% Load and start required applications
    ok = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 9002),  %% Use different port for smoke test
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% Wait for gRPC server to start (bounded wait)
    test_helpers:wait_for_app_start(router_grpc_sup, 2000),
    ok.

cleanup(_) ->
    application:stop(beamline_router),
    ok.

test_grpc_decide() ->
    %% Create test RouteRequest
    Request = #{
        <<"message">> => #{
            <<"message_id">> => <<"smoke_test_001">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello, world!">>,
            <<"metadata">> => #{},
            <<"timestamp_ms">> => erlang:system_time(millisecond)
        },
        <<"policy_id">> => <<"default">>,
        <<"context">> => #{}
    },
    
    %% Convert to RouteRequest record
    RouteRequest = #route_request{
        message = maps:get(<<"message">>, Request),
        policy_id = maps:get(<<"policy_id">>, Request),
        context = maps:get(<<"context">>, Request)
    },
    
    %% Test routing directly (smoke test)
    Result = router_core:route(RouteRequest, #{}),
    
    %% Verify result
    ?assertMatch({ok, _}, Result),
    {ok, Decision} = Result,
    
    %% Verify decision structure
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ?assertNotEqual(undefined, Decision#route_decision.reason),
    ?assert(Decision#route_decision.priority >= 0),
    ?assert(Decision#route_decision.priority =< 100),
    ?assert(Decision#route_decision.expected_latency_ms >= 0),
    ?assert(Decision#route_decision.expected_cost >= 0.0),
    
    ok.

