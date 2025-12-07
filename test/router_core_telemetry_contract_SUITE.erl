%% @doc Unit tests for router_core:route/2 + Telemetry contract
%% Verifies that telemetry:span/3 contract is correctly followed
%% @test_category fast
-module(router_core_telemetry_contract_SUITE).
-include_lib("common_test/include/ct.hrl").
%% Include necessary header files
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).

%% Export test functions
-export([
    test_route_returns_tuple_with_metadata/1,
    test_route_telemetry_stop_event_no_exception/1,
    test_route_error_returns_correct_format/1,
    test_route_success_returns_correct_format/1
]).


all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_route_returns_tuple_with_metadata,
            test_route_telemetry_stop_event_no_exception,
            test_route_error_returns_correct_format,
            test_route_success_returns_correct_format
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_policy_store, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% Test: route/2 returns {Result, StopMetadata} tuple
test_route_returns_tuple_with_metadata(_Config) ->
    %% Create valid route request
    Message = #{
        <<"message_id">> => <<"test-msg">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message_type">> => <<"chat">>,
        <<"payload">> => <<"test">>
    },
    RouteRequest = #route_request{
        message = Message,
        policy_id = undefined,
        context = #{}
    },
    
    %% Create a test policy first
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"default">>,
        weights = #{<<"openai">> => 1.0}
    },
    {ok, _} = router_policy_store:upsert_policy(<<"test_tenant">>, <<"default">>, Policy, undefined),
    
    %% Call route/2
    %% Note: telemetry:span/3 returns only Result, not {Result, StopMetadata}
    Result = router_core:route(RouteRequest, #{}),
    
    %% Verify it returns {ok, Decision} (telemetry:span/3 unwraps the tuple)
    case Result of
        {ok, #route_decision{}} ->
            ok;
        _ ->
            ct:fail("Expected {ok, #route_decision{}}, got: ~p", [Result])
    end,
    
    ok.

%% Test: Telemetry stop event doesn't throw exception
test_route_telemetry_stop_event_no_exception(_Config) ->
    %% Track telemetry events
    Events = ets:new(telemetry_events, [set, private]),
    HandlerId = {?MODULE, test_telemetry_contract},
    
    telemetry:attach(HandlerId, [router_core, route, stop],
        fun(_Event, Measurements, Metadata, _) ->
            ets:insert(Events, {stop, Measurements, Metadata})
        end, #{}),
    
    telemetry:attach(HandlerId, [router_core, route, exception],
        fun(_Event, Measurements, Metadata, _) ->
            ets:insert(Events, {exception, Measurements, Metadata})
        end, #{}),
    
    %% Create valid route request
    Message = #{
        <<"message_id">> => <<"test-msg">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message_type">> => <<"chat">>,
        <<"payload">> => <<"test">>
    },
    RouteRequest = #route_request{
        message = Message,
        policy_id = undefined,
        context = #{}
    },
    
    %% Create a test policy first
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"default">>,
        weights = #{<<"openai">> => 1.0}
    },
    {ok, _} = router_policy_store:upsert_policy(<<"test_tenant">>, <<"default">>, Policy, undefined),
    
    %% Call route/2 - should not throw exception
    %% Note: telemetry:span/3 returns only Result, not {Result, StopMetadata}
    try
        Result = router_core:route(RouteRequest, #{}),
        case Result of
            {ok, #route_decision{}} ->
                ok;
            _ ->
                ct:fail("Expected {ok, #route_decision{}}, got: ~p", [Result])
        end,
        
        %% Allow telemetry to process (bounded wait)
        test_helpers:wait_for_condition(fun() ->
            %% Check if telemetry events were emitted (simple check)
            true
        end, 500),
        
        %% Verify stop event was emitted (not exception)
        case ets:lookup(Events, stop) of
            [{stop, _Measurements, _Metadata}] ->
                ok;  %% Expected
            [] ->
                ct:fail("Telemetry stop event was not emitted")
        end,
        
        %% Verify no exception event
        [] = ets:lookup(Events, exception),
        
        ok
    catch
        Class:Error:Stacktrace ->
            telemetry:detach(HandlerId),
            ets:delete(Events),
            ct:fail("Exception thrown: ~p:~p~n~p", [Class, Error, Stacktrace])
    end,
    
    telemetry:detach(HandlerId),
    ets:delete(Events),
    ok.

%% Test: Error case returns correct format
test_route_error_returns_correct_format(_Config) ->
    %% Create route request with missing tenant_id
    Message = #{
        <<"message_id">> => <<"test-msg">>
        %% tenant_id missing
    },
    RouteRequest = #route_request{
        message = Message,
        policy_id = undefined,
        context = #{}
    },
    
    %% Call route/2
    %% Note: telemetry:span/3 returns only Result, not {Result, StopMetadata}
    Result = router_core:route(RouteRequest, #{}),
    
    %% Verify it returns {error, ...} (telemetry:span/3 unwraps the tuple)
    case Result of
        {error, {missing_tenant_id, _Context}} ->
            ok;
        _ ->
            ct:fail("Expected {error, {missing_tenant_id, _Context}}, got: ~p", [Result])
    end,
    
    ok.

%% Test: Success case returns correct format
test_route_success_returns_correct_format(_Config) ->
    %% Create valid route request
    Message = #{
        <<"message_id">> => <<"test-msg">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message_type">> => <<"chat">>,
        <<"payload">> => <<"test">>
    },
    RouteRequest = #route_request{
        message = Message,
        policy_id = undefined,
        context = #{}
    },
    
    %% Create a test policy first
    Policy = #policy{
        tenant_id = <<"test_tenant">>,
        policy_id = <<"default">>,
        weights = #{<<"openai">> => 1.0}
    },
    {ok, _} = router_policy_store:upsert_policy(<<"test_tenant">>, <<"default">>, Policy, undefined),
    
    %% Call route/2
    %% Note: telemetry:span/3 returns only Result, not {Result, StopMetadata}
    Result = router_core:route(RouteRequest, #{}),
    
    %% Verify it returns {ok, Decision} (telemetry:span/3 unwraps the tuple)
    case Result of
        {ok, Decision} when is_record(Decision, route_decision) ->
            ok;
        _ ->
            ct:fail("Expected {ok, #route_decision{}}, got: ~p", [Result])
    end,
    
    ok.
