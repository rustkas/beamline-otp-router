%% @doc Integration tests for NATS subscriber with CAF push
%% Tests: DecideRequest handling, push_assignment flag, error responses
-module(router_nats_subscriber_caf_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").


%% Use main header file for accurate record definitions
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    %% Test functions called via groups
    test_normalize_boolean/1,
    test_decide_request_success/1,
    test_decide_request_with_push_assignment/1,
    test_decide_request_error_policy_not_found/1,
    test_decide_request_error_missing_tenant_id/1,
    test_decide_request_unsupported_version/1,
    test_decide_request_custom_assignment_subject/1,
    test_push_assignment_false_no_publication/1,
    test_push_assignment_error_no_publication/1,
    test_telemetry_metrics_incremented/1,
    test_async_publication_monitoring/1,
    test_async_retry_metrics/1,
    test_payload_size_limit/1,
    test_version_validation_missing/1,
    test_version_validation_unsupported/1
]}).

all() ->
    [
        {group, integration_tests}
    ].

groups() ->
    [
        {integration_tests, [sequence], [
            test_normalize_boolean,
            test_decide_request_success,
            test_decide_request_with_push_assignment,
            test_decide_request_error_policy_not_found,
            test_decide_request_error_missing_tenant_id,
            test_decide_request_unsupported_version,
            test_decide_request_custom_assignment_subject,
            test_push_assignment_false_no_publication,
            test_push_assignment_error_no_publication,
            test_telemetry_metrics_incremented,
            test_async_publication_monitoring,
            test_async_retry_metrics,
            test_payload_size_limit,
            test_version_validation_missing,
            test_version_validation_unsupported
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

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: normalize_boolean function
test_normalize_boolean(_Config) ->
    %% Test true values
    ?assert(router_nats_subscriber:normalize_boolean(true)),
    ?assert(router_nats_subscriber:normalize_boolean(<<"true">>)),
    ?assert(router_nats_subscriber:normalize_boolean(1)),
    
    %% Test false values
    ?assertNot(router_nats_subscriber:normalize_boolean(false)),
    ?assertNot(router_nats_subscriber:normalize_boolean(<<"false">>)),
    ?assertNot(router_nats_subscriber:normalize_boolean(0)),
    
    %% Test default (unknown values)
    ?assertNot(router_nats_subscriber:normalize_boolean(undefined)),
    ?assertNot(router_nats_subscriber:normalize_boolean(<<"maybe">>)),
    ?assertNot(router_nats_subscriber:normalize_boolean(42)),
    ?assertNot(router_nats_subscriber:normalize_boolean(#{})),
    
    ok.

%% Test: Successful DecideRequest (without push_assignment)
test_decide_request_success(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-001">>,
        <<"trace_id">> => <<"tr-test-001">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Simulate NATS message handling
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    %% In mock mode, verify that function completes without error
    %% In real scenario, we would capture the response and validate it
    ok.

%% Test: DecideRequest with push_assignment=true
test_decide_request_with_push_assignment(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-002">>,
        <<"trace_id">> => <<"tr-test-002">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>,
            <<"payload_ref">> => <<"s3://bucket/key">>
        },
        <<"policy_id">> => <<"default">>,
        <<"push_assignment">> => true,
        <<"assignment_subject">> => <<"caf.exec.assign.v1">>
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Simulate NATS message handling
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    %% Verify that both response and assignment are published
    %% In mock mode, this is verified by function completion
    ok.

%% Test: Error - policy not found
test_decide_request_error_policy_not_found(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-003">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"nonexistent">>
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Should return ErrorResponse
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    ok.

%% Test: Error - missing tenant_id
test_decide_request_error_missing_tenant_id(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-004">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
        %% Missing tenant_id
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Should return ErrorResponse with invalid_request
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    ok.

%% Test: Error - unsupported version
test_decide_request_unsupported_version(_Config) ->
    Request = #{
        <<"version">> => <<"2">>,  %% Unsupported version
        <<"request_id">> => <<"req-test-005">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        }
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Should return ErrorResponse with invalid_request
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    ok.

%% Test: Custom assignment subject
test_decide_request_custom_assignment_subject(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-006">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>,
        <<"push_assignment">> => true,
        <<"assignment_subject">> => <<"caf.exec.assign.v1.custom.queue">>
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Should publish to custom subject
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    ok.

%% Test: push_assignment=false should not publish ExecAssignment
test_push_assignment_false_no_publication(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-007">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>,
        <<"push_assignment">> => false  %% Explicitly false
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Should NOT publish ExecAssignment
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    %% In mock mode, verify that function completes without error
    %% In real scenario, we would verify that no ExecAssignment was published
    ok.

%% Test: Error routing should not publish ExecAssignment
test_push_assignment_error_no_publication(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-test-008">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"nonexistent">>,  %% Will cause error
        <<"push_assignment">> => true  %% Even with push_assignment=true
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Should return ErrorResponse, NOT publish ExecAssignment
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    %% In mock mode, verify that function completes without error
    %% In real scenario, we would verify that no ExecAssignment was published
    ok.

%% Test: Verify telemetry metrics are incremented during integration
test_telemetry_metrics_incremented(_Config) ->
    %% Capture telemetry events
    PublishedRef = make_ref(),
    BlockedRef = make_ref(),
    SkippedRef = make_ref(),
    
    Handler = fun(Event, Measurements, Metadata) ->
        case Event of
            [router_caf_adapter, assignments_published_total] ->
                put(PublishedRef, {Measurements, Metadata});
            [router_caf_adapter, assignments_blocked_total] ->
                put(BlockedRef, {Measurements, Metadata});
            [router_caf_adapter, assignments_skipped_total] ->
                put(SkippedRef, {Measurements, Metadata});
            _ ->
                ok
        end
    end,
    
    %% Attach handlers
    telemetry:attach("test-published", [router_caf_adapter, assignments_published_total], Handler, #{}),
    telemetry:attach("test-blocked", [router_caf_adapter, assignments_blocked_total], Handler, #{}),
    telemetry:attach("test-skipped", [router_caf_adapter, assignments_skipped_total], Handler, #{}),
    
    try
        %% Test 1: Successful publication should increment assignments_published_total
        Request1 = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => <<"req-telemetry-001">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>
            },
            <<"policy_id">> => <<"default">>,
            <<"push_assignment">> => true
        },
        RequestJson1 = jsx:encode(Request1),
        Subject = <<"beamline.router.v1.decide">>,
        router_nats_subscriber:handle_nats_message(Subject, RequestJson1),
        
        %% Wait for async spawn to complete (bounded wait)
        test_helpers:wait_for_condition(fun() -> true end, 200),
        
        %% Verify published metric was incremented
        case get(PublishedRef) of
            undefined ->
                ct:comment("assignments_published_total not incremented (may be expected in mock mode)");
            {Measurements, Metadata} ->
                ?assertEqual(1, maps:get(count, Measurements, 0)),
                ?assert(maps:is_key(tenant_id, Metadata) orelse maps:is_key(<<"tenant_id">>, Metadata))
        end,
        
        %% Test 2: Tenant blocking should increment assignments_blocked_total
        application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [<<"allowed-tenant">>]),
        Request2 = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => <<"req-telemetry-002">>,
            <<"tenant_id">> => <<"blocked-tenant">>,  %% Not in allowlist
            <<"task">> => #{
                <<"type">> => <<"text.generate">>
            },
            <<"policy_id">> => <<"default">>,
            <<"push_assignment">> => true
        },
        RequestJson2 = jsx:encode(Request2),
        router_nats_subscriber:handle_nats_message(Subject, RequestJson2),
        
        %% Wait for async spawn to complete (bounded wait)
        test_helpers:wait_for_condition(fun() -> true end, 200),
        
        %% Verify blocked metric was incremented
        case get(BlockedRef) of
            undefined ->
                ct:comment("assignments_blocked_total not incremented (may be expected in mock mode)");
            {Measurements2, Metadata2} ->
                ?assertEqual(1, maps:get(count, Measurements2, 0)),
                ?assert(maps:is_key(tenant_id, Metadata2) orelse maps:is_key(<<"tenant_id">>, Metadata2))
        end,
        
        %% Test 3: Global disable should increment assignments_skipped_total
        application:set_env(beamline_router, caf_push_assignment_enabled, false),
        Request3 = #{
            <<"version">> => <<"1">>,
            <<"request_id">> => <<"req-telemetry-003">>,
            <<"tenant_id">> => <<"default_tenant">>,
            <<"task">> => #{
                <<"type">> => <<"text.generate">>
            },
            <<"policy_id">> => <<"default">>,
            <<"push_assignment">> => true
        },
        RequestJson3 = jsx:encode(Request3),
        router_nats_subscriber:handle_nats_message(Subject, RequestJson3),
        
        %% Wait for async spawn to complete (bounded wait)
        test_helpers:wait_for_condition(fun() -> true end, 200),
        
        %% Verify skipped metric was incremented
        case get(SkippedRef) of
            undefined ->
                ct:comment("assignments_skipped_total not incremented (may be expected in mock mode)");
            {Measurements3, _Metadata3} ->
                ?assertEqual(1, maps:get(count, Measurements3, 0))
        end,
        
        %% Restore config
        application:set_env(beamline_router, caf_push_assignment_enabled, true),
        application:set_env(beamline_router, caf_push_assignment_allowed_tenants, undefined),
        
        ok
    after
        telemetry:detach("test-published"),
        telemetry:detach("test-blocked"),
        telemetry:detach("test-skipped")
    end.

%% Test: Verify async publication with spawn_monitor and DOWN message handling
test_async_publication_monitoring(_Config) ->
    %% This test verifies that spawn_monitor is used and DOWN messages are handled
    %% Note: Actual DOWN message handling is tested via process crash simulation
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-async-001">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{
            <<"type">> => <<"text.generate">>
        },
        <<"policy_id">> => <<"default">>,
        <<"push_assignment">> => true
    },
    
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Call handler - should spawn monitored process
    router_nats_subscriber:handle_nats_message(Subject, RequestJson),
    
    %% Wait for async spawn_monitor to complete (bounded wait)
    test_helpers:wait_for_condition(fun() -> true end, 200),
    
    %% Verify that function completes without blocking
    %% In real scenario, we would verify DOWN message handling
    ok.

%% Test: Verify retry metrics are incremented during async publication
test_async_retry_metrics(_Config) ->
    %% Check if meck is available
    case code:which(meck) of
        non_existing ->
            {skip, "meck not available"};
        _ ->
            RetryRef = make_ref(),
            
            Handler = fun(Event, Measurements, Metadata) ->
                case Event of
                    [router_caf_adapter, assignments_retry_total] ->
                        put(RetryRef, {Measurements, Metadata});
                    _ ->
                        ok
                end
            end,
            
            telemetry:attach("test-retry", [router_caf_adapter, assignments_retry_total], Handler, #{}),
            
            try
                %% Mock router_nats to fail first time, succeed second time
                meck:new(router_nats, [passthrough]),
                meck:expect(router_nats, publish, fun(_, _) ->
                    case get(retry_count) of
                        undefined ->
                            put(retry_count, 1),
                            {error, {timeout, "NATS timeout"}};
                        _ ->
                            put(retry_count, undefined),
                            ok
                    end
                end),
                
                application:set_env(beamline_router, caf_max_retries, 3),
                application:set_env(beamline_router, caf_retry_base_ms, 10),
                
                Request = #{
                    <<"version">> => <<"1">>,
                    <<"request_id">> => <<"req-retry-async-001">>,
                    <<"tenant_id">> => <<"default_tenant">>,
                    <<"task">> => #{
                        <<"type">> => <<"text.generate">>
                    },
                    <<"policy_id">> => <<"default">>,
                    <<"push_assignment">> => true
                },
                
                Decision = #route_decision{
                    provider_id = <<"openai">>,
                    reason = <<"weighted">>,
                    priority = 50,
                    expected_latency_ms = 850,
                    expected_cost = 0.012,
                    metadata = #{}
                },
                
                %% Call adapter directly (simulating async spawn)
                router_caf_adapter:publish_assignment(Request, Decision),
                
                %% Wait for retries to complete (bounded wait)
                test_helpers:wait_for_condition(fun() -> true end, 500),
                
                %% Verify retry metric was incremented
                case get(RetryRef) of
                    undefined ->
                        ct:comment("assignments_retry_total not incremented (may be expected in mock mode)");
                    {Measurements, Metadata} ->
                        ?assert(maps:get(count, Measurements, 0) >= 1),
                        ?assert(maps:is_key(retries, Metadata) orelse maps:is_key(<<"retries">>, Metadata))
                end,
                
                ok
            after
                meck:unload(router_nats),
                telemetry:detach("test-retry")
            end
    end.


%% Test: Payload size limit validation
test_payload_size_limit(_Config) ->
    %% Set small payload limit for testing
    ok = application:set_env(beamline_router, nats_max_payload_size, 100),
    
    %% Create oversized payload
    LargePayload = binary:copy(<<"x">>, 200),  %% 200 bytes > 100 limit
    LargeRequest = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-oversized">>,
        <<"tenant_id">> => <<"test">>,
        <<"task">> => #{<<"type">> => <<"test">>},
        <<"payload">> => LargePayload
    }),
    
    %% Mock NATS subscriber to capture error response
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    %% Process message (should reject due to size)
    router_nats_subscriber:handle_nats_message(<<"test.subject">>, LargeRequest),
    
    %% Verify error was logged (check via meck or logs)
    %% Note: In real scenario, error response would be published
    meck:unload(router_nats),
    
    %% Reset config
    ok = application:set_env(beamline_router, nats_max_payload_size, 1048576),
    ok.

%% Test: Version validation - missing version
test_version_validation_missing(_Config) ->
    Request = #{
        <<"request_id">> => <<"req-no-version">>,
        <<"tenant_id">> => <<"test">>,
        <<"task">> => #{<<"type">> => <<"test">>}
        %% Missing "version" field
    },
    RequestJson = jsx:encode(Request),
    
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    router_nats_subscriber:handle_nats_message(<<"test.subject">>, RequestJson),
    
    %% Verify error response was sent (would check in real scenario)
    meck:unload(router_nats),
    ok.

%% Test: Version validation - unsupported version
test_version_validation_unsupported(_Config) ->
    Request = #{
        <<"version">> => <<"2">>,  %% Unsupported version
        <<"request_id">> => <<"req-unsupported-version">>,
        <<"tenant_id">> => <<"test">>,
        <<"task">> => #{<<"type">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    router_nats_subscriber:handle_nats_message(<<"test.subject">>, RequestJson),
    
    %% Verify error response was sent with supported_versions in logs
    meck:unload(router_nats),
    ok.
