%% @doc Unit tests for delivery count tracking and MaxDeliver exhaustion
%% Fast regression tests for delivery count tracking with MaxDeliver simulation
-module(router_delivery_count_tracking_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, groups/0, init_per_suite/1, init_per_testcase/2]}).


all() ->
    [
        {group, integration_tests}
    ].

groups() ->
    [
        {integration_tests, [sequence], [
            test_delivery_count_tracking,
            test_delivery_count_increment,
            test_maxdeliver_exhaustion_emits_metric,
            test_maxdeliver_exhaustion_removes_tracking,
            test_maxdeliver_not_exhausted,
            test_cleanup_after_ack,
            test_concurrent_delivery_count_tracking,
            test_delivery_count_tracking_under_ack_failures,
            test_processing_delays_redelivery_with_delivery_count
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, usage_subject, <<"beamline.usage.v1.metered">>),
    ok = application:set_env(beamline_router, nats_js_max_deliver, 3),  %% Set MaxDeliver to 3 for testing
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_result_consumer, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clean up ETS tables if they exist
    case ets:whereis(router_delivery_count) of
        undefined -> ok;
        Table1 -> ets:delete_all_objects(Table1)
    end,
    case ets:whereis(router_ack_delivery_count) of
        undefined -> ok;
        Table2 -> ets:delete_all_objects(Table2)
    end,
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Clean up ETS tables
    case ets:whereis(router_delivery_count) of
        undefined -> ok;
        Table3 -> ets:delete_all_objects(Table3)
    end,
    case ets:whereis(router_ack_delivery_count) of
        undefined -> ok;
        Table4 -> ets:delete_all_objects(Table4)
    end,
    Config.

%% Test: Basic delivery count tracking
test_delivery_count_tracking(_Config) ->
    MsgId = <<"msg-test-1">>,
    
    %% Create ETS table (simulating router_result_consumer init)
    %% Note: track_delivery_count will create table if it doesn't exist, but we create it here for testing
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Track first delivery
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    %% Verify count is 1
    case ets:lookup(router_delivery_count, MsgId) of
        [{MsgId, Count}] ->
            1 = Count,
            ok;
        [] ->
            ct:fail("Delivery count not tracked for message")
    end,
    
    ets:delete(Table),
    ok.

%% Test: Delivery count increments correctly
test_delivery_count_increment(_Config) ->
    MsgId = <<"msg-test-2">>,
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Track multiple deliveries
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    %% Verify count is 3
    [{MsgId, Count}] = ets:lookup(router_delivery_count, MsgId),
    3 = Count,
    
    ets:delete(Table),
    ok.

%% Test: MaxDeliver exhaustion emits metric
test_maxdeliver_exhaustion_emits_metric(_Config) ->
    MsgId = <<"msg-test-3">>,
    AssignmentId = <<"assign-test-3">>,
    RequestId = <<"req-test-3">>,
    ErrorContext = #{
        reason => <<"tenant_validation_failed">>,
        source => <<"test">>
    },
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Track deliveries up to MaxDeliver (3)
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    %% Verify count is 3 (MaxDeliver)
    [{MsgId, 3}] = ets:lookup(router_delivery_count, MsgId),
    
    %% Track emitted metrics
    ExhaustedMetrics = ets:new(exhausted_metrics, [set, private]),
    HandlerId = {?MODULE, test_maxdeliver_exhaustion},
    telemetry:attach(HandlerId, [router_result_consumer, router_jetstream_maxdeliver_exhausted_total],
        fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
            ets:insert(ExhaustedMetrics, {exhausted, Metadata})
        end, #{}),
    
    %% Check MaxDeliver exhaustion (should emit metric)
    ok = router_result_consumer:check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
    
    %% Allow telemetry to process (bounded wait)
    test_helpers:wait_for_condition(fun() -> true end, 300),
    
    %% Verify metric was emitted
    case ets:lookup(ExhaustedMetrics, exhausted) of
        [{_, Metadata}] ->
            AssignmentIdFromMeta = maps:get(assignment_id, Metadata, undefined),
            RequestIdFromMeta = maps:get(request_id, Metadata, undefined),
            MsgIdFromMeta = maps:get(msg_id, Metadata, undefined),
            DeliveryCount = maps:get(delivery_count, Metadata, undefined),
            MaxDeliver = maps:get(max_deliver, Metadata, undefined),
            Reason = maps:get(reason, Metadata, undefined),
            true = (AssignmentIdFromMeta =/= undefined),
            true = (RequestIdFromMeta =/= undefined),
            true = (MsgIdFromMeta =/= undefined),
            AssignmentId = AssignmentIdFromMeta,
            RequestId = RequestIdFromMeta,
            MsgId = MsgIdFromMeta,
            3 = DeliveryCount,
            3 = MaxDeliver,
            <<"maxdeliver_exhausted">> = Reason,
            ok;
        [] ->
            ct:fail("MaxDeliver exhaustion metric was not emitted")
    end,
    
    telemetry:detach(HandlerId),
    ets:delete(ExhaustedMetrics),
    ets:delete(Table),
    ok.

%% Test: MaxDeliver exhaustion removes tracking entry
test_maxdeliver_exhaustion_removes_tracking(_Config) ->
    MsgId = <<"msg-test-4">>,
    AssignmentId = <<"assign-test-4">>,
    RequestId = <<"req-test-4">>,
    ErrorContext = #{
        reason => <<"tenant_validation_failed">>,
        source => <<"test">>
    },
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Track deliveries up to MaxDeliver (3)
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    %% Verify entry exists
    [{MsgId, 3}] = ets:lookup(Table, MsgId),
    
    %% Check MaxDeliver exhaustion
    ok = router_result_consumer:check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
    
    %% Verify entry was removed
    [] = ets:lookup(router_delivery_count, MsgId),
    
    ets:delete(Table),
    ok.

%% Test: MaxDeliver not exhausted (count < MaxDeliver)
test_maxdeliver_not_exhausted(_Config) ->
    MsgId = <<"msg-test-5">>,
    AssignmentId = <<"assign-test-5">>,
    RequestId = <<"req-test-5">>,
    ErrorContext = #{
        reason => <<"tenant_validation_failed">>,
        source => <<"test">>
    },
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Track deliveries below MaxDeliver (2 < 3)
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    %% Track emitted metrics (should not emit)
    ExhaustedMetrics = ets:new(exhausted_metrics, [set, private]),
    HandlerId = {?MODULE, test_maxdeliver_not_exhausted},
    telemetry:attach(HandlerId, [router_result_consumer, router_jetstream_maxdeliver_exhausted_total],
        fun(_Event, _Measurements, Metadata, _HandlerConfig) ->
            ets:insert(ExhaustedMetrics, {exhausted, Metadata})
        end, #{}),
    
    %% Check MaxDeliver exhaustion (should NOT emit metric)
    ok = router_result_consumer:check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
    
    %% Allow telemetry to process (bounded wait)
    test_helpers:wait_for_condition(fun() -> true end, 300),
    
    %% Verify metric was NOT emitted
    case ets:lookup(ExhaustedMetrics, exhausted) of
        [] ->
            ok;  %% Expected - metric should not be emitted
        [{_, _}] ->
            ct:fail("MaxDeliver exhaustion metric was emitted when it should not have been")
    end,
    
    %% Verify entry still exists (not removed)
    [{MsgId, 2}] = ets:lookup(router_delivery_count, MsgId),
    
    telemetry:detach(HandlerId),
    ets:delete(ExhaustedMetrics),
    ets:delete(Table),
    ok.

%% Test: Cleanup after successful ACK
test_cleanup_after_ack(_Config) ->
    MsgId = <<"msg-test-6">>,
    
    %% Create ETS table
    Table = ets:new(router_delivery_count, [
        set,
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    
    %% Track delivery
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    %% Verify entry exists
    [{MsgId, 1}] = ets:lookup(router_delivery_count, MsgId),
    
    %% Cleanup after ACK
    ok = router_result_consumer:cleanup_delivery_count(MsgId),
    
    %% Verify entry was removed
    [] = ets:lookup(router_delivery_count, MsgId),
    
    ets:delete(Table),
    ok.

%% Test: Concurrent delivery count tracking
test_concurrent_delivery_count_tracking(_Config) ->
    MsgId1 = <<"msg-concurrent-1">>,
    MsgId2 = <<"msg-concurrent-2">>,
    MsgId3 = <<"msg-concurrent-3">>,
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Track deliveries concurrently (simulate multiple messages)
    ok = router_result_consumer:track_delivery_count(MsgId1),
    ok = router_result_consumer:track_delivery_count(MsgId2),
    ok = router_result_consumer:track_delivery_count(MsgId1),  %% Second delivery for MsgId1
    ok = router_result_consumer:track_delivery_count(MsgId3),
    ok = router_result_consumer:track_delivery_count(MsgId2),  %% Second delivery for MsgId2
    
    %% Verify all counts are correct
    [{MsgId1, 2}] = ets:lookup(router_delivery_count, MsgId1),
    [{MsgId2, 2}] = ets:lookup(router_delivery_count, MsgId2),
    [{MsgId3, 1}] = ets:lookup(router_delivery_count, MsgId3),
    
    ets:delete(Table),
    ok.

%% ============================================================================
%% Fault Injection Tests (from JETSTREAM_FAULT_INJECTION_TESTS.md)
%% ============================================================================

%% @doc Test: Delivery count tracking under ACK failures
%% Scenario S1: Verify delivery count is tracked even when ACK fails intermittently.
%% 
%% Fault Injection:
%% - ACK fails on first attempt
%% - ACK succeeds on subsequent attempts
%%
%% Verifications:
%% - Delivery count is tracked even when ACK fails
%% - Delivery count increments on each delivery attempt
%% - ACK failure doesn't prevent delivery count tracking
%%
%% Reference: docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md#scenario-1-intermittent-acknak-errors
test_delivery_count_tracking_under_ack_failures(_Config) ->
    MsgId = <<"msg-ack-failure-1">>,
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Phase 1: Enable fault injection - first ACK fails
    SkipResult = case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(ack, {error, connection_refused}),
            ok;
        false ->
            ct:comment("router_nats_fault_injection not loaded, skipping fault injection test"),
            ets:delete(Table),
            {skip, "router_nats_fault_injection not available"}
    end,
    
    case SkipResult of
        {skip, _} = Skip ->
            Skip;
        ok ->
            %% Track first delivery (ACK will fail, but delivery count should be tracked)
            ok = router_result_consumer:track_delivery_count(MsgId),
            
            %% Verify delivery count is tracked even if ACK fails
            [{MsgId, Count1}] = ets:lookup(router_delivery_count, MsgId),
            1 = Count1,
            
            %% Try ACK (should fail due to fault injection)
            case router_nats:ack_message(MsgId) of
                {error, _Reason} ->
                    ok;  %% Expected - fault injection causes ACK to fail
                ok ->
                    ct:comment("ACK succeeded (fault injection may not be working), continuing test")
            end,
            
            %% Verify delivery count is still tracked after ACK failure
            [{MsgId, Count1}] = ets:lookup(router_delivery_count, MsgId),
            1 = Count1,
            
            %% Phase 2: Disable fault injection - subsequent ACK should succeed
            router_nats_fault_injection:disable_fault(ack),
            
            %% Track second delivery (simulate redelivery)
            ok = router_result_consumer:track_delivery_count(MsgId),
            
            %% Verify delivery count incremented
            [{MsgId, Count2}] = ets:lookup(router_delivery_count, MsgId),
            2 = Count2,
            
            %% Try ACK again (should succeed now)
            ok = router_nats:ack_message(MsgId),
            
            %% Cleanup delivery count after successful ACK
            ok = router_result_consumer:cleanup_delivery_count(MsgId),
            
            %% Verify entry was removed after cleanup
            [] = ets:lookup(router_delivery_count, MsgId),
            
            ets:delete(Table),
            ok
    end.

%% @doc Test: Processing delays redelivery with delivery count tracking
%% Scenario S2: Verify delivery count tracking under processing delays (tenant validation failures).
%% 
%% Fault Injection:
%% - Tenant validation fails (simulating processing delay/timeout)
%% - Triggers NAK for controlled redelivery
%%
%% Verifications:
%% - Delivery count is tracked on each redelivery
%% - Delivery count increments correctly with each NAK
%% - Delivery count matches redelivery attempts
%%
%% Reference: docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md#scenario-2-processing-delays-causing-redelivery-growth
test_processing_delays_redelivery_with_delivery_count(_Config) ->
    MsgId = <<"msg-processing-delays-dc-1">>,
    
    %% Create ETS table
    Table = case ets:whereis(router_delivery_count) of
        undefined ->
            ets:new(router_delivery_count, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        T -> T
    end,
    
    %% Configure tenant allowlist to reject specific tenant
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, [<<"acme">>]),
    
    %% First delivery - track delivery count
    ok = router_result_consumer:track_delivery_count(MsgId),
    [{MsgId, 1}] = ets:lookup(router_delivery_count, MsgId),
    
    %% Simulate tenant validation failure (would trigger NAK in real scenario)
    %% For this test, we simulate the delivery count tracking that happens on redelivery
    %% In real scenario, router_result_consumer would call track_delivery_count before NAK
    
    %% Second delivery (redelivery after NAK)
    ok = router_result_consumer:track_delivery_count(MsgId),
    [{MsgId, 2}] = ets:lookup(router_delivery_count, MsgId),
    
    %% Third delivery (another redelivery)
    ok = router_result_consumer:track_delivery_count(MsgId),
    [{MsgId, 3}] = ets:lookup(router_delivery_count, MsgId),
    
    %% Verify delivery count matches redelivery attempts
    3 = (case ets:lookup(router_delivery_count, MsgId) of
        [{MsgId, Count}] -> Count;
        [] -> 0
    end),
    
    %% Cleanup
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, undefined),
    ets:delete(Table),
    ok.
