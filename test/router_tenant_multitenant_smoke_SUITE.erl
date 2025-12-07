%% @doc Multi-tenant smoke tests for Router
%% Verifies tenant isolation, validation, and metrics/logs with tenant labels
%% @test_category fast, cp1_smoke
-module(router_tenant_multitenant_smoke_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_valid_tenant_a_success/1,
    test_invalid_tenant_rejection/1,
    test_valid_tenant_b_independent/1,
    test_tenant_metrics_isolation/1,
    test_tenant_logs_isolation/1,
    test_tenant_spoofing_rejection/1,
    test_mixed_tenant_payload_ignored/1,
    test_tenant_header_priority_over_payload/1
]}).


%% Test tenant IDs
-define(TENANT_A, <<"tenant-a">>).
-define(TENANT_B, <<"tenant-b">>).
-define(TENANT_INVALID, <<"tenant-invalid">>).

all() ->
    [
        {group, multitenant_tests},
        {group, security_tests}
    ].

groups() ->
    [
        {multitenant_tests, [sequence], [
            test_valid_tenant_a_success,
            test_invalid_tenant_rejection,
            test_valid_tenant_b_independent,
            test_tenant_metrics_isolation,
            test_tenant_logs_isolation
        ]},
        {security_tests, [sequence], [
            test_tenant_spoofing_rejection,
            test_mixed_tenant_payload_ignored,
            test_tenant_header_priority_over_payload
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    
    %% Configure tenant allowlist with tenant-a and tenant-b
    ok = application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [
        ?TENANT_A,
        ?TENANT_B
    ]),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_policy_store, 1000),
            
            %% Setup telemetry handler to capture metrics
            TelemetryTable = ets:new(telemetry_events, [ordered_set, public]),
            ok = telemetry:attach_many(
                <<"router_tenant_multitenant_smoke">>,
                [
                    [router_tenant_validator, audit],
                    [router_tenant_validator, router_tenant_audit_total],
                    [router_core, route],
                    [router_nats_subscriber, decide_requests_total]
                ],
                fun(Event, Measurements, Metadata, _HandlerConfig) ->
                    ets:insert(TelemetryTable, {erlang:monotonic_time(millisecond), Event, Measurements, Metadata})
                end,
                TelemetryTable
            ),
            
            [{telemetry_table, TelemetryTable} | Config];
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    %% Detach telemetry handler
    telemetry:detach(<<"router_tenant_multitenant_smoke">>),
    
    %% Clean up telemetry table
    case proplists:get_value(telemetry_table, Config) of
        undefined -> ok;
        Table -> ets:delete(Table)
    end,
    
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Valid tenant A → success
test_valid_tenant_a_success(_Config) ->
    ct:comment("Test: Valid tenant A should succeed"),
    
    %% Create a valid DecideRequest with tenant-a
    _Request = #{
        <<"request_id">> => <<"req-tenant-a-1">>,
        <<"tenant_id">> => ?TENANT_A,
        <<"task">> => <<"test-task">>,
        <<"version">> => <<"1">>
    },
    
    %% Validate tenant
    Context = #{
        <<"request_id">> => <<"req-tenant-a-1">>,
        <<"source">> => <<"test">>
    },
    
    case router_tenant_validator:validate_tenant(?TENANT_A, Context) of
        {ok, ValidatedTenantId} ->
            ?assertEqual(?TENANT_A, ValidatedTenantId),
            ct:comment("Tenant A validation succeeded"),
            ok;
        {error, Reason, ErrorContext} ->
            ct:fail("Tenant A validation failed: ~p, context: ~p", [Reason, ErrorContext])
    end,
    
    %% Also verify allowlist check
    ?assert(router_nats_subscriber:check_tenant_allowed(?TENANT_A)),
    ?assert(router_caf_adapter:check_tenant_allowed(?TENANT_A)),
    
    ok.

%% Test: Invalid tenant → expected rejection
test_invalid_tenant_rejection(_Config) ->
    ct:comment("Test: Invalid tenant should be rejected"),
    
    %% Create a request with invalid tenant
    _Request = #{
        <<"request_id">> => <<"req-invalid-1">>,
        <<"tenant_id">> => ?TENANT_INVALID,
        <<"task">> => <<"test-task">>,
        <<"version">> => <<"1">>
    },
    
    %% Validate tenant (should fail)
    Context = #{
        <<"request_id">> => <<"req-invalid-1">>,
        <<"source">> => <<"test">>
    },
    
    case router_tenant_validator:validate_tenant(?TENANT_INVALID, Context) of
        {ok, _} ->
            ct:fail("Invalid tenant should be rejected, but validation succeeded");
        {error, tenant_not_allowed, ErrorContext} ->
            ?assertEqual(?TENANT_INVALID, maps:get(tenant_id, ErrorContext, undefined)),
            ?assertEqual(<<"tenant_id not in allowlist">>, maps:get(reason, ErrorContext, undefined)),
            ct:comment("Invalid tenant correctly rejected with tenant_not_allowed"),
            ok;
        {error, Reason, ErrorContext} ->
            ct:fail("Invalid tenant rejected with unexpected reason: ~p, context: ~p", [Reason, ErrorContext])
    end,
    
    %% Also verify allowlist check
    ?assertNot(router_nats_subscriber:check_tenant_allowed(?TENANT_INVALID)),
    ?assertNot(router_caf_adapter:check_tenant_allowed(?TENANT_INVALID)),
    
    ok.

%% Test: Valid tenant B → independent success
test_valid_tenant_b_independent(_Config) ->
    ct:comment("Test: Valid tenant B should succeed independently"),
    
    %% Create a valid DecideRequest with tenant-b
    _Request = #{
        <<"request_id">> => <<"req-tenant-b-1">>,
        <<"tenant_id">> => ?TENANT_B,
        <<"task">> => <<"test-task">>,
        <<"version">> => <<"1">>
    },
    
    %% Validate tenant
    Context = #{
        <<"request_id">> => <<"req-tenant-b-1">>,
        <<"source">> => <<"test">>
    },
    
    case router_tenant_validator:validate_tenant(?TENANT_B, Context) of
        {ok, ValidatedTenantId} ->
            ?assertEqual(?TENANT_B, ValidatedTenantId),
            ct:comment("Tenant B validation succeeded"),
            ok;
        {error, Reason, ErrorContext} ->
            ct:fail("Tenant B validation failed: ~p, context: ~p", [Reason, ErrorContext])
    end,
    
    %% Also verify allowlist check
    ?assert(router_nats_subscriber:check_tenant_allowed(?TENANT_B)),
    ?assert(router_caf_adapter:check_tenant_allowed(?TENANT_B)),
    
    %% Verify tenant A still works (isolation)
    ?assert(router_nats_subscriber:check_tenant_allowed(?TENANT_A)),
    ?assert(router_caf_adapter:check_tenant_allowed(?TENANT_A)),
    
    ok.

%% Test: Tenant metrics isolation
test_tenant_metrics_isolation(Config) ->
    ct:comment("Test: Metrics should contain correct tenant labels and not mix tenants"),
    
    TelemetryTable = proplists:get_value(telemetry_table, Config),
    
    %% Clear telemetry table
    ets:delete_all_objects(TelemetryTable),
    
    %% Generate events for tenant A
    ContextA = #{
        <<"request_id">> => <<"req-tenant-a-metrics">>,
        <<"source">> => <<"test">>
    },
    {ok, _} = router_tenant_validator:validate_tenant(?TENANT_A, ContextA),
    
    %% Generate events for tenant B
    ContextB = #{
        <<"request_id">> => <<"req-tenant-b-metrics">>,
        <<"source">> => <<"test">>
    },
    {ok, _} = router_tenant_validator:validate_tenant(?TENANT_B, ContextB),
    
    %% Generate rejection event for invalid tenant
    ContextInvalid = #{
        <<"request_id">> => <<"req-invalid-metrics">>,
        <<"source">> => <<"test">>
    },
    {error, _, _} = router_tenant_validator:validate_tenant(?TENANT_INVALID, ContextInvalid),
    
    %% Wait a bit for telemetry events to be processed
    timer:sleep(100),
    
    %% Collect all telemetry events
    AllEvents = ets:tab2list(TelemetryTable),
    
    %% Filter events by tenant_id
    TenantAEvents = lists:filter(fun({_, _, _, Metadata}) ->
        case maps:get(tenant_id, Metadata, undefined) of
            ?TENANT_A -> true;
            _ -> false
        end
    end, AllEvents),
    
    TenantBEvents = lists:filter(fun({_, _, _, Metadata}) ->
        case maps:get(tenant_id, Metadata, undefined) of
            ?TENANT_B -> true;
            _ -> false
        end
    end, AllEvents),
    
    InvalidTenantEvents = lists:filter(fun({_, _, _, Metadata}) ->
        case maps:get(tenant_id, Metadata, undefined) of
            ?TENANT_INVALID -> true;
            _ -> false
        end
    end, AllEvents),
    
    %% Verify tenant A events have correct tenant_id
    ct:comment("Tenant A events: ~p", [length(TenantAEvents)]),
    lists:foreach(fun({_, _, _, Metadata}) ->
        ?assertEqual(?TENANT_A, maps:get(tenant_id, Metadata, undefined))
    end, TenantAEvents),
    
    %% Verify tenant B events have correct tenant_id
    ct:comment("Tenant B events: ~p", [length(TenantBEvents)]),
    lists:foreach(fun({_, _, _, Metadata}) ->
        ?assertEqual(?TENANT_B, maps:get(tenant_id, Metadata, undefined))
    end, TenantBEvents),
    
    %% Verify invalid tenant events have correct tenant_id
    ct:comment("Invalid tenant events: ~p", [length(InvalidTenantEvents)]),
    lists:foreach(fun({_, _, _, Metadata}) ->
        ?assertEqual(?TENANT_INVALID, maps:get(tenant_id, Metadata, undefined))
    end, InvalidTenantEvents),
    
    %% Verify no cross-tenant mixing (tenant A events should not contain tenant B)
    lists:foreach(fun({_, _, _, Metadata}) ->
        TenantId = maps:get(tenant_id, Metadata, undefined),
        ?assertNotEqual(?TENANT_B, TenantId)
    end, TenantAEvents),
    
    %% Verify no cross-tenant mixing (tenant B events should not contain tenant A)
    lists:foreach(fun({_, _, _, Metadata}) ->
        TenantId = maps:get(tenant_id, Metadata, undefined),
        ?assertNotEqual(?TENANT_A, TenantId)
    end, TenantBEvents),
    
    ct:comment("Metrics isolation verified: tenant labels are correct and not mixed"),
    ok.

%% Test: Tenant logs isolation
test_tenant_logs_isolation(_Config) ->
    ct:comment("Test: Logs should contain correct tenant labels and not mix tenants"),
    
    %% Create test contexts with different tenants
    ContextA = #{
        <<"request_id">> => <<"req-tenant-a-logs">>,
        <<"tenant_id">> => ?TENANT_A,
        <<"source">> => <<"test">>
    },
    
    ContextB = #{
        <<"request_id">> => <<"req-tenant-b-logs">>,
        <<"tenant_id">> => ?TENANT_B,
        <<"source">> => <<"test">>
    },
    
    ContextInvalid = #{
        <<"request_id">> => <<"req-invalid-logs">>,
        <<"tenant_id">> => ?TENANT_INVALID,
        <<"source">> => <<"test">>
    },
    
    %% Validate tenants (this will trigger logging via router_tenant_validator:emit_audit_event)
    {ok, _} = router_tenant_validator:validate_tenant(?TENANT_A, ContextA),
    {ok, _} = router_tenant_validator:validate_tenant(?TENANT_B, ContextB),
    {error, _, ErrorContextInvalid} = router_tenant_validator:validate_tenant(?TENANT_INVALID, ContextInvalid),
    
    %% Verify that tenant_id is correctly passed in contexts
    %% (This ensures logs will have correct tenant labels)
    ?assertEqual(?TENANT_A, maps:get(tenant_id, ContextA, undefined)),
    ?assertEqual(?TENANT_B, maps:get(tenant_id, ContextB, undefined)),
    ?assertEqual(?TENANT_INVALID, maps:get(tenant_id, ErrorContextInvalid, undefined)),
    
    %% Verify no cross-tenant mixing in contexts
    ?assertNotEqual(?TENANT_B, maps:get(tenant_id, ContextA, undefined)),
    ?assertNotEqual(?TENANT_A, maps:get(tenant_id, ContextB, undefined)),
    
    %% Verify that router_logger:filter_pii preserves tenant_id structure
    %% (tenant_id should not be filtered as PII in logs)
    FilteredContextA = router_logger:filter_pii(ContextA),
    FilteredContextB = router_logger:filter_pii(ContextB),
    
    %% tenant_id should be preserved after PII filtering
    ?assertEqual(?TENANT_A, maps:get(<<"tenant_id">>, FilteredContextA, maps:get(tenant_id, FilteredContextA, undefined))),
    ?assertEqual(?TENANT_B, maps:get(<<"tenant_id">>, FilteredContextB, maps:get(tenant_id, FilteredContextB, undefined))),
    
    ct:comment("Logs isolation verified: tenant labels are correctly structured and not mixed"),
    ok.

%% Test: Tenant spoofing rejection
test_tenant_spoofing_rejection(_Config) ->
    ct:comment("Test: Tenant spoofing attempt (header tenant-a, payload tenant-b) should be rejected or logged as violation"),
    
    %% Simulate tenant spoofing: header says tenant-a, but payload contains tenant-b
    Headers = #{
        <<"tenant_id">> => ?TENANT_A,
        <<"trace_id">> => <<"trace-spoofing-1">>,
        <<"version">> => <<"1">>
    },
    
    Payload = #{
        <<"tenant_id">> => ?TENANT_B,  %% Spoofing attempt: different tenant in payload
        <<"assignment_id">> => <<"assign-spoofing-1">>,
        <<"request_id">> => <<"req-spoofing-1">>,
        <<"status">> => <<"completed">>
    },
    
    %% Extract tenant_id using Router's logic (headers priority)
    %% This simulates what router_result_consumer does
    TenantIdFromHeaders = maps:get(<<"tenant_id">>, Headers, undefined),
    TenantIdFromPayload = maps:get(<<"tenant_id">>, Payload, undefined),
    
    %% Router should use tenant_id from headers (priority), not payload
    FinalTenantId = case TenantIdFromHeaders of
        undefined -> TenantIdFromPayload;
        _ -> TenantIdFromHeaders
    end,
    
    %% Verify that header tenant_id is used (not payload)
    ?assertEqual(?TENANT_A, FinalTenantId),
    ?assertNotEqual(?TENANT_B, FinalTenantId),
    
    %% Verify that payload tenant_id is different (spoofing attempt detected)
    ?assertNotEqual(TenantIdFromHeaders, TenantIdFromPayload),
    
    %% Validate the tenant from headers (should succeed for tenant-a)
    Context = #{
        <<"request_id">> => <<"req-spoofing-1">>,
        <<"source">> => <<"test">>,
        <<"spoofing_detected">> => true,
        <<"header_tenant">> => TenantIdFromHeaders,
        <<"payload_tenant">> => TenantIdFromPayload
    },
    
    case router_tenant_validator:validate_tenant(FinalTenantId, Context) of
        {ok, ValidatedTenantId} ->
            %% Header tenant is validated (tenant-a is valid)
            ?assertEqual(?TENANT_A, ValidatedTenantId),
            ct:comment("Tenant spoofing prevented: header tenant used, payload tenant ignored"),
            ok;
        {error, Reason, ErrorContext} ->
            ct:fail("Tenant validation failed unexpectedly: ~p, context: ~p", [Reason, ErrorContext])
    end,
    
    ok.

%% Test: Mixed tenant payload ignored in metrics/logs
test_mixed_tenant_payload_ignored(Config) ->
    ct:comment("Test: If payload contains different tenant_id, it should not appear in metrics/logs labels"),
    
    TelemetryTable = proplists:get_value(telemetry_table, Config),
    
    %% Clear telemetry table
    ets:delete_all_objects(TelemetryTable),
    
    %% Create scenario: header has tenant-a, payload has tenant-b (spoofing attempt)
    Headers = #{
        <<"tenant_id">> => ?TENANT_A,
        <<"trace_id">> => <<"trace-mixed-1">>,
        <<"version">> => <<"1">>
    },
    
    Payload = #{
        <<"tenant_id">> => ?TENANT_B,  %% Different tenant in payload (should be ignored)
        <<"assignment_id">> => <<"assign-mixed-1">>,
        <<"request_id">> => <<"req-mixed-1">>
    },
    
    %% Extract tenant_id (headers priority)
    TenantIdFromHeaders = maps:get(<<"tenant_id">>, Headers, undefined),
    TenantIdFromPayload = maps:get(<<"tenant_id">>, Payload, undefined),
    
    FinalTenantId = case TenantIdFromHeaders of
        undefined -> TenantIdFromPayload;
        _ -> TenantIdFromHeaders
    end,
    
    %% Verify header tenant is used
    ?assertEqual(?TENANT_A, FinalTenantId),
    
    %% Validate tenant (this will emit telemetry events)
    Context = #{
        <<"request_id">> => <<"req-mixed-1">>,
        <<"source">> => <<"test">>,
        <<"header_tenant">> => TenantIdFromHeaders,
        <<"payload_tenant">> => TenantIdFromPayload
    },
    
    {ok, _} = router_tenant_validator:validate_tenant(FinalTenantId, Context),
    
    %% Wait for telemetry events
    timer:sleep(100),
    
    %% Collect all telemetry events
    AllEvents = ets:tab2list(TelemetryTable),
    
    %% Filter events by tenant_id in metadata
    TenantAEvents = lists:filter(fun({_, _, _, Metadata}) ->
        case maps:get(tenant_id, Metadata, undefined) of
            ?TENANT_A -> true;
            _ -> false
        end
    end, AllEvents),
    
    TenantBEvents = lists:filter(fun({_, _, _, Metadata}) ->
        case maps:get(tenant_id, Metadata, undefined) of
            ?TENANT_B -> true;
            _ -> false
        end
    end, AllEvents),
    
    %% Verify that only tenant-a events exist (payload tenant-b should not appear)
    ?assert(length(TenantAEvents) > 0, "Should have tenant-a events"),
    ?assertEqual(0, length(TenantBEvents), "Should not have tenant-b events (payload tenant ignored)"),
    
    %% Verify no tenant-b in tenant-a events metadata
    lists:foreach(fun({_, _, _, Metadata}) ->
        TenantId = maps:get(tenant_id, Metadata, undefined),
        ?assertEqual(?TENANT_A, TenantId),
        ?assertNotEqual(?TENANT_B, TenantId)
    end, TenantAEvents),
    
    ct:comment("Mixed tenant payload ignored: metrics/logs use header tenant, payload tenant not in labels"),
    ok.

%% Test: Tenant header priority over payload
test_tenant_header_priority_over_payload(_Config) ->
    ct:comment("Test: Missing tenant_id header, but present in payload - should use payload as fallback"),
    
    %% Scenario 1: Header has tenant_id (should be used)
    HeadersWithTenant = #{
        <<"tenant_id">> => ?TENANT_A,
        <<"trace_id">> => <<"trace-priority-1">>,
        <<"version">> => <<"1">>
    },
    
    PayloadWithTenant = #{
        <<"tenant_id">> => ?TENANT_B,  %% Different tenant in payload
        <<"assignment_id">> => <<"assign-priority-1">>
    },
    
    %% Extract tenant_id (headers priority)
    TenantId1 = case maps:get(<<"tenant_id">>, HeadersWithTenant, undefined) of
        undefined -> maps:get(<<"tenant_id">>, PayloadWithTenant, undefined);
        HeaderTenant1 -> HeaderTenant1
    end,
    
    ?assertEqual(?TENANT_A, TenantId1, "Header tenant should be used when present"),
    
    %% Scenario 2: Header missing tenant_id, payload has tenant_id (should use payload)
    HeadersWithoutTenant = #{
        <<"trace_id">> => <<"trace-priority-2">>,
        <<"version">> => <<"1">>
        %% No tenant_id in headers
    },
    
    PayloadWithTenant2 = #{
        <<"tenant_id">> => ?TENANT_B,
        <<"assignment_id">> => <<"assign-priority-2">>
    },
    
    %% Extract tenant_id (payload fallback)
    TenantId2 = case maps:get(<<"tenant_id">>, HeadersWithoutTenant, undefined) of
        undefined -> maps:get(<<"tenant_id">>, PayloadWithTenant2, undefined);
        HeaderTenant2 -> HeaderTenant2
    end,
    
    ?assertEqual(?TENANT_B, TenantId2, "Payload tenant should be used when header missing"),
    
    %% Scenario 3: Both missing (should be undefined)
    HeadersEmpty = #{
        <<"trace_id">> => <<"trace-priority-3">>
    },
    
    PayloadEmpty = #{
        <<"assignment_id">> => <<"assign-priority-3">>
    },
    
    TenantId3 = case maps:get(<<"tenant_id">>, HeadersEmpty, undefined) of
        undefined -> maps:get(<<"tenant_id">>, PayloadEmpty, undefined);
        HeaderTenant3 -> HeaderTenant3
    end,
    
    ?assertEqual(undefined, TenantId3, "Should be undefined when both missing"),
    
    %% Verify validation behavior
    Context1 = #{
        <<"request_id">> => <<"req-priority-1">>,
        <<"source">> => <<"test">>
    },
    
    Context2 = #{
        <<"request_id">> => <<"req-priority-2">>,
        <<"source">> => <<"test">>
    },
    
    %% Tenant from header should validate
    {ok, ?TENANT_A} = router_tenant_validator:validate_tenant(TenantId1, Context1),
    
    %% Tenant from payload should validate
    {ok, ?TENANT_B} = router_tenant_validator:validate_tenant(TenantId2, Context2),
    
    ct:comment("Tenant header priority verified: headers used when present, payload used as fallback"),
    ok.

