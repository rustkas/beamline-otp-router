%% @doc Enhanced tests for router_caf_adapter
%% Tests retries, tenant blocking, deadline calculation, and telemetry
-module(router_caf_adapter_enhanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Include router definitions
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    test_retry_success_on_second_attempt/1,
    test_retry_exhausted/1,
    test_tenant_blocked/1,
    test_global_disable/1,
    test_deadline_min_cap/1,
    test_deadline_max_cap/1,
    test_deadline_calculation/1,
    test_telemetry_span_attributes/1
]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

all() ->
    [
        {group, unit_tests}
    ].

groups() ->
    [
        {unit_tests, [sequence], [
            test_retry_success_on_second_attempt,
            test_retry_exhausted,
            test_tenant_blocked,
            test_global_disable,
            test_deadline_min_cap,
            test_deadline_max_cap,
            test_deadline_calculation,
            test_telemetry_span_attributes
        ]}
    ].

init_per_suite(Config) ->
    %% Start application
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

%% Test: Retry succeeds on second attempt
test_retry_success_on_second_attempt(_Config) ->
    %% Check if meck is available
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            %% Mock router_nats to fail first time, succeed second time
            meck:new(router_nats, [passthrough]),
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) ->
                case get(retry_count) of
                    undefined ->
                        put(retry_count, 1),
                        {error, {timeout, "NATS timeout"}};
                    _ ->
                        put(retry_count, undefined),
                        {ok, <<"ack-retry-success">>}
                end
            end),
    
    %% Set max retries to 3
    application:set_env(beamline_router, caf_max_retries, 3),
    application:set_env(beamline_router, caf_retry_base_ms, 10),  %% Fast for testing
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-retry-test">>,
        <<"tenant_id">> => <<"test-tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    
            Decision = #route_decision{
                provider_id = <<"openai">>,
                reason = <<"weighted">>,
                priority = 50,
                expected_latency_ms = 850,
                expected_cost = 0.012,
                metadata = #{}
            },
            
            %% Capture telemetry events
            Handler = fun(Event, Measurements, Metadata) ->
                case Event of
                    [router_caf_adapter, assignments_retry_total] ->
                        put(telemetry_retry, {Measurements, Metadata});
                    [router_caf_adapter, assignments_published_total] ->
                        put(telemetry_published, {Measurements, Metadata});
                    _ ->
                        ok
                end
            end,
            telemetry:attach("test-handler-retry", [router_caf_adapter, assignments_retry_total], Handler, #{}),
            telemetry:attach("test-handler-published", [router_caf_adapter, assignments_published_total], Handler, #{}),
            
            %% Publish assignment
            Result = router_caf_adapter:publish_assignment(Request, Decision),
            
            %% Verify result
            ?assertEqual(ok, Result),
            
            %% Verify retry telemetry
            RetryTelemetry = get(telemetry_retry),
            ?assertNotEqual(undefined, RetryTelemetry),
            {RetryMeasurements, RetryMetadata} = RetryTelemetry,
            ?assertEqual(1, maps:get(count, RetryMeasurements)),
            ?assertEqual(1, maps:get(retries, RetryMetadata)),
            
            %% Verify published telemetry
            PublishedTelemetry = get(telemetry_published),
            ?assertNotEqual(undefined, PublishedTelemetry),
            
            telemetry:detach("test-handler-retry"),
            telemetry:detach("test-handler-published"),
            meck:unload(router_nats),
            ok
    end.

%% Test: Retries exhausted
test_retry_exhausted(_Config) ->
    %% Check if meck is available
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            %% Mock router_nats to always fail
            meck:new(router_nats, [passthrough]),
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) ->
                {error, {connection_failed, "NATS unavailable"}}
            end),
    
            %% Set max retries to 2
            application:set_env(beamline_router, caf_max_retries, 2),
            application:set_env(beamline_router, caf_retry_base_ms, 10),
            
            Request = #{
                <<"version">> => <<"1">>,
                <<"request_id">> => <<"req-retry-exhausted">>,
                <<"tenant_id">> => <<"test-tenant">>,
                <<"task">> => #{
                    <<"type">> => <<"text.generate">>
                }
            },
            
            Decision = #route_decision{
                provider_id = <<"openai">>,
                reason = <<"weighted">>,
                priority = 50,
                expected_latency_ms = 850,
                expected_cost = 0.012,
                metadata = #{}
            },
            
            %% Capture telemetry
            Handler = fun(Event, Measurements, Metadata) ->
                case Event of
                    [router_caf_adapter, assignments_failed_total] ->
                        put(telemetry_failed, {Measurements, Metadata});
                    _ ->
                        ok
                end
            end,
            telemetry:attach("test-handler", [router_caf_adapter, assignments_failed_total], Handler, #{}),
            
            %% Publish assignment
            Result = router_caf_adapter:publish_assignment(Request, Decision),
            
            %% Verify result
            ?assertEqual(error, Result),
            
            %% Verify failed telemetry
            FailedTelemetry = get(telemetry_failed),
            ?assertNotEqual(undefined, FailedTelemetry),
            {FailedMeasurements, FailedMetadata} = FailedTelemetry,
            ?assertEqual(1, maps:get(count, FailedMeasurements)),
            ?assertEqual(connection_failed, maps:get(error_kind, FailedMetadata)),
            ?assertEqual(2, maps:get(retries, FailedMetadata)),
            
            telemetry:detach("test-handler"),
            meck:unload(router_nats),
            ok
    end.

%% Test: Tenant blocked
test_tenant_blocked(_Config) ->
    %% Set tenant allowlist
    application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [<<"allowed-tenant">>]),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-tenant-blocked">>,
        <<"tenant_id">> => <<"blocked-tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    
    Decision = #route_decision{
        provider_id = <<"openai">>,
        reason = <<"weighted">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        metadata = #{}
    },
    
    %% Capture telemetry
    Handler = fun(Event, Measurements, Metadata) ->
        case Event of
            [router_caf_adapter, assignments_blocked_total] ->
                put(telemetry_blocked, {Measurements, Metadata});
            _ ->
                ok
        end
    end,
    telemetry:attach("test-handler", [router_caf_adapter, assignments_blocked_total], Handler, #{}),
    
    %% Publish assignment
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    
    %% Verify result (should return ok but not publish)
    ?assertEqual(ok, Result),
    
    %% Verify blocked telemetry
    BlockedTelemetry = get(telemetry_blocked),
    ?assertNotEqual(undefined, BlockedTelemetry),
    {BlockedMeasurements, BlockedMetadata} = BlockedTelemetry,
    ?assertEqual(1, maps:get(count, BlockedMeasurements)),
    ?assertEqual(tenant_not_allowed, maps:get(reason, BlockedMetadata)),
    ?assertEqual(<<"blocked-tenant">>, maps:get(tenant_id, BlockedMetadata)),
    
    %% Cleanup
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
            telemetry:detach("test-handler-retry"),
            telemetry:detach("test-handler-published"),
    ok.

%% Test: Global disable
test_global_disable(_Config) ->
    %% Disable globally
    application:set_env(beamline_router, caf_push_assignment_enabled, false),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-global-disable">>,
        <<"tenant_id">> => <<"test-tenant">>,
        <<"push_assignment">> => true,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    
    Decision = #route_decision{
        provider_id = <<"openai">>,
        reason = <<"weighted">>,
        priority = 50,
        expected_latency_ms = 850,
        expected_cost = 0.012,
        metadata = #{}
    },
    
    %% Capture telemetry
    Handler = fun(Event, Measurements, Metadata) ->
        case Event of
            [router_caf_adapter, assignments_skipped_total] ->
                put(telemetry_skipped, {Measurements, Metadata});
            _ ->
                ok
        end
    end,
    telemetry:attach("test-handler", [router_caf_adapter, assignments_skipped_total], Handler, #{}),
    
    %% Publish assignment
    Result = router_caf_adapter:publish_assignment(Request, Decision),
    
    %% Verify result
    ?assertEqual(ok, Result),
    
    %% Verify skipped telemetry
    SkippedTelemetry = get(telemetry_skipped),
    ?assertNotEqual(undefined, SkippedTelemetry),
    {SkippedMeasurements, SkippedMetadata} = SkippedTelemetry,
    ?assertEqual(1, maps:get(count, SkippedMeasurements)),
    ?assertEqual(global_disabled, maps:get(reason, SkippedMetadata)),
    
    %% Cleanup
    application:set_env(beamline_router, caf_push_assignment_enabled, true),
            telemetry:detach("test-handler-retry"),
            telemetry:detach("test-handler-published"),
    ok.

%% Test: Deadline min cap
test_deadline_min_cap(_Config) ->
    %% Set min deadline to 10000ms
    application:set_env(beamline_router, caf_deadline_min_ms, 10000),
    application:set_env(beamline_router, caf_deadline_multiplier, 5),
    
    Decision = #route_decision{
        provider_id = <<"openai">>,
        reason = <<"weighted">>,
        priority = 50,
        expected_latency_ms = 100,  %% Very low latency
        expected_cost = 0.012,
        metadata = #{}
    },
    
    %% Calculate deadline (using internal function via exported test function)
    ExpectedLatency = Decision#route_decision.expected_latency_ms,
    Deadline = router_caf_adapter:calculate_deadline(ExpectedLatency),
    
    %% Verify min cap applied (100 * 5 = 500, but min is 10000)
    ?assertEqual(10000, Deadline),
    
    %% Cleanup
    application:unset_env(beamline_router, caf_deadline_min_ms),
    application:unset_env(beamline_router, caf_deadline_multiplier),
    ok.

%% Test: Deadline max cap
test_deadline_max_cap(_Config) ->
    %% Set max deadline to 20000ms
    application:set_env(beamline_router, caf_deadline_max_ms, 20000),
    application:set_env(beamline_router, caf_deadline_multiplier, 5),
    
    Decision = #route_decision{
        provider_id = <<"openai">>,
        reason = <<"weighted">>,
        priority = 50,
        expected_latency_ms = 10000,  %% High latency
        expected_cost = 0.012,
        metadata = #{}
    },
    
    %% Calculate deadline (using internal function via exported test function)
    ExpectedLatency = Decision#route_decision.expected_latency_ms,
    Deadline = router_caf_adapter:calculate_deadline(ExpectedLatency),
    
    %% Verify max cap applied (10000 * 5 = 50000, but max is 20000)
    ?assertEqual(20000, Deadline),
    
    %% Cleanup
    application:unset_env(beamline_router, caf_deadline_max_ms),
    application:unset_env(beamline_router, caf_deadline_multiplier),
    ok.

%% Test: Deadline calculation
test_deadline_calculation(_Config) ->
    %% Set multiplier to 5
    application:set_env(beamline_router, caf_deadline_multiplier, 5),
    application:set_env(beamline_router, caf_deadline_min_ms, 5000),
    application:set_env(beamline_router, caf_deadline_max_ms, 60000),
    
    TestCases = [
        {100, 5000},   %% Min cap
        {1000, 5000},  %% Min cap
        {2000, 10000}, %% Normal calculation
        {5000, 25000}, %% Normal calculation
        {15000, 60000} %% Max cap
    ],
    
    lists:foreach(fun({ExpectedLatency, ExpectedDeadline}) ->
        Deadline = router_caf_adapter:calculate_deadline(ExpectedLatency),
        ?assertEqual(ExpectedDeadline, Deadline, 
            io_lib:format("Deadline for ~pms should be ~pms, got ~pms", 
                [ExpectedLatency, ExpectedDeadline, Deadline]))
    end, TestCases),
    
    %% Cleanup
    application:unset_env(beamline_router, caf_deadline_multiplier),
    application:unset_env(beamline_router, caf_deadline_min_ms),
    application:unset_env(beamline_router, caf_deadline_max_ms),
    ok.

%% Test: Telemetry span attributes
test_telemetry_span_attributes(_Config) ->
    %% Check if meck is available
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            %% Mock successful publish
            meck:new(router_nats, [passthrough]),
            meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {ok, <<"ack-telemetry">>} end),
            
            Request = #{
                <<"version">> => <<"1">>,
                <<"request_id">> => <<"req-span-test">>,
                <<"tenant_id">> => <<"test-tenant">>,
                <<"task">> => #{
                    <<"type">> => <<"text.generate">>
                }
            },
            
            Decision = #route_decision{
                provider_id = <<"openai">>,
                reason = <<"weighted">>,
                priority = 50,
                expected_latency_ms = 850,
                expected_cost = 0.012,
                metadata = #{}
            },
            
            %% Capture span events
            Handler = fun(Event, Measurements, Metadata) ->
                case Event of
                    [router_caf_adapter, publish_assignment, start] ->
                        put(span_start, Metadata);
                    [router_caf_adapter, publish_assignment, stop] ->
                        put(span_stop, {Measurements, Metadata});
                    _ ->
                        ok
                end
            end,
            telemetry:attach("test-handler-start", [router_caf_adapter, publish_assignment, start], Handler, #{}),
            telemetry:attach("test-handler-stop", [router_caf_adapter, publish_assignment, stop], Handler, #{}),
            
            %% Publish assignment
            router_caf_adapter:publish_assignment(Request, Decision),
            
            %% Verify start metadata
            StartMetadata = get(span_start),
            ?assertNotEqual(undefined, StartMetadata),
            ?assert(maps:is_key(assignment_id, StartMetadata)),
            ?assert(maps:is_key(expected_latency_ms, StartMetadata)),
            ?assert(maps:is_key(deadline_ms, StartMetadata)),
            ?assertEqual(850, maps:get(expected_latency_ms, StartMetadata)),
            
            %% Verify stop metadata
            StopData = get(span_stop),
            ?assertNotEqual(undefined, StopData),
            {StopMeasurements, StopMetadata} = StopData,
            ?assert(maps:is_key(duration, StopMeasurements)),
            ?assert(maps:is_key(result, StopMetadata)),
            ?assert(maps:is_key(deadline_ms, StopMetadata)),
            
            telemetry:detach("test-handler-start"),
            telemetry:detach("test-handler-stop"),
            meck:unload(router_nats),
            ok
    end.
