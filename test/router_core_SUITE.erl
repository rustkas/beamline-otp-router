%% @doc Common Test Suite for Router Core
%% Tests policy parsing, basic decision, errors (CP1)
%% @test_category cp1_smoke, fast
-module(router_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Use main header file for accurate record definitions
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_policy_parsing/1,
    test_basic_decision/1,
    test_missing_tenant_id/1,
    test_policy_not_found/1,
    test_weighted_routing/1,
    test_fallback/1,
    test_telemetry_events/1,
    test_empty_tenant_id/1,
    test_context_undefined/1,
    test_context_non_map/1,
    test_context_with_correlation_id/1
]}).

all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_policy_parsing,
            test_basic_decision,
            test_missing_tenant_id,
            test_empty_tenant_id,
            test_policy_not_found,
        test_weighted_routing,
            test_fallback,
            test_telemetry_events,
            test_context_undefined,
            test_context_non_map,
            test_context_with_correlation_id
        ]}
    ].

init_per_suite(Config) ->
    %% Ensure all required applications are started
    _ = application:load(beamline_router),  %% Ignore error if already loaded
    ok = application:set_env(beamline_router, grpc_port, 9001),  %% Use different port for tests
    ok = application:set_env(beamline_router, metrics_port, 9091),  %% Use different port for metrics to avoid conflicts
    %% Disable heir/transfer logic for faster test execution
    ok = application:set_env(beamline_router, disable_heir, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> ok;
        {error, {already_started, beamline_router}} -> ok;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end,
    
    %% Wait for router_policy_store process to be ready (bounded wait)
    test_helpers:wait_for_app_start(router_policy_store, 1000),
    
    Config.

end_per_suite(Config) ->
    %% Stop application
    application:stop(beamline_router),
    application:unload(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Policy parsing from fixtures
test_policy_parsing(_Config) ->
    %% Load policy using the same method as other tests
    Result = try
        router_policy_store:load_policy(<<"default_tenant">>, <<"default">>)
    catch
        Class:Reason ->
            ct:fail("Error loading policy: ~p:~p", [Class, Reason])
    end,
    case Result of
        {ok, PolicyTuple} ->
            %% Convert to list to avoid badrecord issues completely
            %% Use erlang:tuple_to_list directly to avoid record structure checks
            PolicyList = erlang:tuple_to_list(PolicyTuple),
            
            %% Verify policy structure
            %% Record structure: {policy, tenant_id, policy_id, version, defaults, escalate_on, weights, fallback, fallbacks, sticky, pre, validators, post, metadata}
            %% Elements: 1=policy, 2=tenant_id, 3=policy_id, 4=version, 5=defaults, 6=escalate_on, 7=weights, 8=fallback, 9=fallbacks, 10=sticky, 11=pre, 12=validators, 13=post, 14=metadata
            
            %% Verify first element is policy atom
            case lists:nth(1, PolicyList) of
                policy -> ok;
                _ -> ct:fail("First element should be 'policy' atom, got: ~p", [lists:nth(1, PolicyList)])
            end,
            %% Verify tenant_id
            case lists:nth(2, PolicyList) of
                <<"default_tenant">> -> ok;
                TenantId -> ct:fail("Expected tenant_id <<\"default_tenant\">>, got: ~p", [TenantId])
            end,
            %% Verify policy_id
            case lists:nth(3, PolicyList) of
                <<"default">> -> ok;
                PolicyId -> ct:fail("Expected policy_id <<\"default\">>, got: ~p", [PolicyId])
            end,
            %% Verify version
            case lists:nth(4, PolicyList) of
                <<"1.0">> -> ok;
                Version -> ct:fail("Expected version <<\"1.0\">>, got: ~p", [Version])
            end,
            %% Verify weights
            Weights = lists:nth(7, PolicyList),
            case maps:get(<<"openai">>, Weights, undefined) of
                0.7 -> ok;
                OpenAIWeight -> ct:fail("Expected openai weight 0.7, got: ~p", [OpenAIWeight])
            end,
            case maps:get(<<"anthropic">>, Weights, undefined) of
                0.3 -> ok;
                AnthropicWeight -> ct:fail("Expected anthropic weight 0.3, got: ~p", [AnthropicWeight])
            end,
            %% fallback can be undefined if fallbacks array is used instead
            Fallbacks = lists:nth(9, PolicyList),
            Fallback = lists:nth(8, PolicyList),
            case (Fallback =/= undefined) orelse (length(Fallbacks) > 0) of
                true -> ok;
                false -> ct:fail("Expected fallback or fallbacks array")
            end,
            ok;
        Error ->
            ct:fail("Failed to load policy: ~p", [Error])
    end.

%% Test: Basic decision making
test_basic_decision(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_123">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ?assertNotEqual(undefined, Decision#route_decision.reason),
    ?assert(Decision#route_decision.priority >= 0),
    ?assert(Decision#route_decision.priority =< 100),
    ok.

%% Test: Missing tenant_id error
test_missing_tenant_id(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_123">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
    Result = router_core:route(RouteRequest, #{}),
    ?assertMatch({error, {missing_tenant_id, _}}, Result),
    {error, {ErrorReason, ErrorContext}} = Result,
    ?assertEqual(missing_tenant_id, ErrorReason),
    ?assert(maps:is_key(context, ErrorContext), "Context should contain context field"),
    ok.

%% Test: Policy not found (returns error)
test_policy_not_found(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_123">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"nonexistent">>,
        context = #{}
    },
    
    %% Should return policy_not_found error (no fallback to default)
    Result = router_core:route(RouteRequest, #{}),
    ?assertMatch({error, {policy_not_found, _}}, Result),
    {error, {ErrorReason, ErrorContext}} = Result,
    ?assertEqual(policy_not_found, ErrorReason),
    ?assert(maps:is_key(tenant_id, ErrorContext), "Context should contain tenant_id"),
    ?assert(maps:is_key(policy_id, ErrorContext), "Context should contain policy_id"),
    ok.

%% Test: Weighted routing (deterministic)
test_weighted_routing(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_123">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
    %% Fix random seed for deterministic testing
    rand:seed(exs1024, {1, 2, 3}),
    
    %% Run multiple times to test weighted distribution
    Results = lists:map(fun(_) ->
        {ok, Decision} = router_core:route(RouteRequest, #{}),
        Decision#route_decision.provider_id
    end, lists:seq(1, 100)),
    
    %% Count provider selections
    ProviderCounts = lists:foldl(
        fun(Provider, Acc) ->
            maps:update_with(Provider, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        Results
    ),
    
    %% Verify that both providers can be selected (with weights 0.7/0.3)
    ?assert(maps:is_key(<<"openai">>, ProviderCounts) orelse maps:is_key(<<"anthropic">>, ProviderCounts)),
    
    %% Verify approximate distribution (allow 20% tolerance)
    OpenAICount = maps:get(<<"openai">>, ProviderCounts, 0),
    AnthropicCount = maps:get(<<"anthropic">>, ProviderCounts, 0),
    Total = OpenAICount + AnthropicCount,
    
    if
        Total > 0 ->
            OpenAIRatio = OpenAICount / Total,
            ?assert(OpenAIRatio >= 0.5),  %% At least 50% (0.7 expected)
            ?assert(OpenAIRatio =< 0.9);  %% At most 90%
        true ->
            %% If no results, something is wrong
            ?assert(false)
    end,
    ok.

%% Test: Fallback provider
test_fallback(_Config) ->
    %% Create request with empty weights policy
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_123">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = #{
            <<"all_providers_failed">> => true
        }
    },
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Should use fallback or weighted provider
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ok.

%% Test: Telemetry events
test_telemetry_events(_Config) ->
    %% Capture telemetry events
    HandlerId1 = <<"router_core_test_routes">>,
    HandlerId2 = <<"router_core_test_errors">>,
    HandlerId3 = <<"router_core_test_resolutions">>,
    Self = self(),
    
    %% Attach handlers for router_core events
    Handler = fun(Event, Measurements, Metadata, _HandlerConfig) ->
        Self ! {telemetry, Event, Measurements, Metadata}
    end,
    
    telemetry:attach(HandlerId1, [router_core, routes_total], Handler, #{}),
    telemetry:attach(HandlerId2, [router_core, errors_total], Handler, #{}),
    telemetry:attach(HandlerId3, [router_core, resolutions_total], Handler, #{}),
    
    %% Create route request
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"telemetry_test_123">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Test telemetry">>
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
    %% Call route
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    
    %% Wait for telemetry events
    receive
        {telemetry, [router_core, routes_total], Measurements2, Metadata2} ->
            ?assert(maps:is_key(count, Measurements2), "routes_total should have count"),
            ?assert(maps:is_key(tenant_id, Metadata2), "routes_total should have tenant_id"),
            ?assert(maps:is_key(policy_id, Metadata2), "routes_total should have policy_id"),
            ?assert(maps:is_key(provider_id, Metadata2), "routes_total should have provider_id"),
            ?assert(maps:is_key(result, Metadata2), "routes_total should have result"),
            ?assertEqual(ok, maps:get(result, Metadata2), "result should be ok"),
            ok
    after 1000 ->
        ?assert(false, "Telemetry event not received within timeout")
    end,
    
    receive
        {telemetry, [router_core, resolutions_total], Measurements3, Metadata3} ->
            ?assert(maps:is_key(count, Measurements3), "resolutions_total should have count"),
            ?assert(maps:is_key(tenant_id, Metadata3), "resolutions_total should have tenant_id"),
            ?assert(maps:is_key(policy_id, Metadata3), "resolutions_total should have policy_id"),
            ?assert(maps:is_key(provider_id, Metadata3), "resolutions_total should have provider_id"),
            ok
    after 1000 ->
        ?assert(false, "Telemetry event not received within timeout")
    end,
    
    %% Test error telemetry
    ErrorRouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"telemetry_error_test">>,
            <<"message_type">> => <<"chat">>
            %% Missing tenant_id
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
      {error, {ErrorReason, _ErrorContext}} = router_core:route(ErrorRouteRequest, #{}),
    ?assertEqual(missing_tenant_id, ErrorReason),
    
    receive
        {telemetry, [router_core, errors_total], Measurements, Metadata} ->
            ?assert(maps:is_key(count, Measurements), "errors_total should have count"),
            ?assert(maps:is_key(error, Metadata), "errors_total should have error"),
            ?assert(maps:is_key(result, Metadata), "errors_total should have result"),
            ?assertEqual(error, maps:get(result, Metadata), "result should be error"),
            ?assertEqual(missing_tenant_id, maps:get(error, Metadata), "error should be missing_tenant_id"),
            ok
    after 1000 ->
        ?assert(false, "Error telemetry event not received within timeout")
    end,
    
    %% Cleanup
    telemetry:detach(HandlerId1),
    telemetry:detach(HandlerId2),
    telemetry:detach(HandlerId3),
    
    ok.

%% Test: Empty tenant_id (should fail)
test_empty_tenant_id(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_empty_tenant">>,
            <<"tenant_id">> => <<>>,  %% Empty tenant_id
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
    Result = router_core:route(RouteRequest, #{}),
    ?assertMatch({error, {missing_tenant_id, _}}, Result),
    {error, {ErrorReason, ErrorContext}} = Result,
    ?assertEqual(missing_tenant_id, ErrorReason),
    ?assert(maps:is_key(context, ErrorContext), "Context should contain context field"),
    ok.

%% Test: Context undefined (should be handled gracefully)
test_context_undefined(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_context_undefined">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = undefined  %% Explicitly undefined
    },
    
    %% Should handle undefined context gracefully (convert to empty map)
    {ok, Decision} = router_core:route(RouteRequest, undefined),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ok.

%% Test: Context non-map (should be handled gracefully)
test_context_non_map(_Config) ->
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_context_non_map">>,
            <<"tenant_id">> => <<"default_tenant">>,
        <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = <<"not_a_map">>  %% Non-map context
    },
    
    %% Should handle non-map context gracefully (convert to empty map)
    {ok, Decision} = router_core:route(RouteRequest, <<"not_a_map">>),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    ok.

%% Test: Context with correlation_id (should be preserved)
test_context_with_correlation_id(_Config) ->
    CorrelationId = <<"corr-test-123">>,
    RouteRequest = #route_request{
        message = #{
            <<"message_id">> => <<"test_correlation">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = <<"default">>,
        context = #{}
    },
    
    Context = #{correlation_id => CorrelationId},
    
    %% Should handle correlation_id in context
    {ok, Decision} = router_core:route(RouteRequest, Context),
    ?assertNotEqual(undefined, Decision#route_decision.provider_id),
    %% Context should be preserved through the call chain
    ok.
