%% @doc Router Intake Overload Test Suite
%% Tests Router behavior under overload conditions (backpressure scenarios)
%% CP2+: Backpressure and queue management testing
-module(router_intake_overload_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, init_per_suite/1, init_per_testcase/2]}).


-define(SUITE_NAME, router_intake_overload_SUITE).

all() ->
    [
        test_overload_jetstream_backlog,
        test_overload_processing_latency,
        test_overload_inflight_messages,
        test_overload_combined,
        test_backpressure_rejection,
        test_backpressure_nak_rejection,
        test_backpressure_recovery,
        test_end_to_end_overload_gateway,
        test_end_to_end_overload_detailed_status,
        test_end_to_end_overload_event_tracking
    ].

init_per_suite(Config) ->
    %% Setup test environment
    ok = application:ensure_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Setup mocks for each test
    meck:new(router_nats, [passthrough]),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(),
    ok.

%% Test: JetStream backlog overload
test_overload_jetstream_backlog(_Config) ->
    %% Setup: Simulate high backlog (pending messages > 1000)
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for pending cache
    Table = router_jetstream_pending_cache,
    case ets:whereis(Table) of
        undefined -> ets:new(Table, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(Table, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure is active
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    
    %% Verify metrics emitted
    ?assert(meck:called(telemetry, execute, '_')),
    
    %% Cleanup
    ets:delete(Table),
    ok.

%% Test: Processing latency overload
test_overload_processing_latency(_Config) ->
    %% Setup: Simulate high latency (p95 > 5000ms)
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for latency cache
    Table = router_intake_latency_cache,
    case ets:whereis(Table) of
        undefined -> ets:new(Table, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(Table, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    %% Set pending to low value (not overloaded)
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 50, erlang:system_time(millisecond)}),
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure warning (latency only, not queue)
    ?assertEqual({backpressure_warning, 0}, {Status, RetryAfter}),
    
    %% Cleanup
    ets:delete(Table),
    ets:delete(PendingTable),
    ok.

%% Test: In-flight messages overload
test_overload_inflight_messages(_Config) ->
    %% Setup: Simulate high in-flight count (> 500)
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for in-flight tracking
    Table = router_intake_inflight,
    case ets:whereis(Table) of
        undefined -> ets:new(Table, [named_table, bag, public]);
        _ -> ok
    end,
    
    %% Add 600 in-flight messages
    [ets:insert(Table, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 600)],
    
    %% Set pending and latency to low values
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 50, erlang:system_time(millisecond)}),
    
    LatencyTable = router_intake_latency_cache,
    case ets:whereis(LatencyTable) of
        undefined -> ets:new(LatencyTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(LatencyTable, {{Subject, p95}, 100, erlang:system_time(millisecond)}),
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure warning (in-flight only)
    ?assertEqual({backpressure_warning, 0}, {Status, RetryAfter}),
    
    %% Cleanup
    ets:delete(Table),
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

%% Test: Combined overload (backlog + latency + in-flight)
test_overload_combined(_Config) ->
    %% Setup: Simulate all overload conditions
    Subject = <<"beamline.router.v1.decide">>,
    
    %% High backlog
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% High latency
    LatencyTable = router_intake_latency_cache,
    case ets:whereis(LatencyTable) of
        undefined -> ets:new(LatencyTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(LatencyTable, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    %% High in-flight
    InFlightTable = router_intake_inflight,
    case ets:whereis(InFlightTable) of
        undefined -> ets:new(InFlightTable, [named_table, bag, public]);
        _ -> ok
    end,
    [ets:insert(InFlightTable, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 600)],
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure is active
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ets:delete(InFlightTable),
    ok.

%% Test: Backpressure rejection
test_backpressure_rejection(_Config) ->
    %% Setup: Simulate backpressure active
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Set backpressure active
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
    Status = router_intake_backpressure:get_backpressure_status(Subject),
    ?assertEqual(backpressure_active, Status),
    
    %% Verify rejection would occur (in real implementation)
    %% This would be tested in integration tests with actual HTTP requests
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

%% Test: Backpressure NAK rejection
test_backpressure_nak_rejection(_Config) ->
    %% Setup: Simulate backpressure active
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"test-msg-backpressure-nak">>,
    
    %% Set backpressure active
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
    
    %% Mock router_nats:nak_message to track calls
    meck:expect(router_nats, nak_message, fun(MId) ->
        ?assertEqual(MsgId, MId),
        ok
    end),
    
    %% Mock telemetry to track rejection metric
    meck:expect(telemetry, execute, fun([router_intake_backpressure, rejections], #{count := 1}, #{subject := S, reason := R}) ->
        ?assertEqual(Subject, S),
        ?assertEqual(backpressure_active, R),
        ok
    end),
    
    %% Simulate message handling with backpressure active
    %% This would normally be called from router_decide_consumer:handle_decide_message_internal
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    
    %% Verify NAK would be called (in real implementation)
    %% In actual code, router_decide_consumer would call nak_message_if_needed(MsgId, RetryAfter)
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

%% Test: Backpressure recovery
test_backpressure_recovery(_Config) ->
    %% Setup: Simulate recovery from backpressure
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Phase 1: Backpressure active
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
    
    {Status1, _} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual(backpressure_active, Status1),
    
    %% Phase 2: Recovery (queue depth and latency decrease)
    ets:insert(PendingTable, {Subject, 50, erlang:system_time(millisecond)}),
    ets:insert(LatencyTable, {{Subject, p95}, 200, erlang:system_time(millisecond)}),
    
    {Status2, _} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual(backpressure_inactive, Status2),
    
    %% Check recovery status
    RecoveryStatus = router_intake_backpressure:get_backpressure_recovery_status(Subject),
    ?assert(maps:get(recovered, RecoveryStatus, false)),
    
    %% Verify metrics show recovery
    ?assert(meck:called(telemetry, execute, '_')),
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

%% Test: End-to-end overload scenario - Gateway integration
test_end_to_end_overload_gateway(_Config) ->
    %% Setup: Simulate full overload scenario with Gateway integration
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create all required ETS tables
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
    
    InFlightTable = router_intake_inflight,
    case ets:whereis(InFlightTable) of
        undefined -> ets:new(InFlightTable, [named_table, bag, public]);
        _ -> ok
    end,
    [ets:insert(InFlightTable, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 600)],
    
    %% Check backpressure status
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    
    %% Get Gateway-formatted status
    GatewayStatus = router_gateway_backpressure:get_backpressure_status_for_gateway(Subject),
    ?assertEqual(<<"active">>, maps:get(status, GatewayStatus)),
    ?assert(maps:is_key(metrics, GatewayStatus)),
    ?assert(maps:is_key(thresholds, GatewayStatus)),
    ?assert(maps:is_key(policy, GatewayStatus)),
    
    %% Test Gateway notification
    GatewayEndpoint = <<"http://gateway:3000/api/backpressure">>,
    NotificationResult = router_gateway_backpressure:notify_gateway_backpressure_status(GatewayEndpoint, GatewayStatus),
    ?assertMatch({ok, _}, NotificationResult),
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ets:delete(InFlightTable),
    ok.

%% Test: End-to-end overload scenario - detailed status
test_end_to_end_overload_detailed_status(_Config) ->
    %% Setup: Simulate overload with detailed status tracking
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS tables
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
    
    %% Get detailed status
    DetailedStatus = router_intake_backpressure:get_detailed_backpressure_status(Subject),
    ?assert(maps:is_key(subject, DetailedStatus)),
    ?assert(maps:is_key(status, DetailedStatus)),
    ?assert(maps:is_key(metrics, DetailedStatus)),
    ?assert(maps:is_key(thresholds, DetailedStatus)),
    ?assert(maps:is_key(policy, DetailedStatus)),
    ?assert(maps:is_key(recovery, DetailedStatus)),
    
    %% Verify metrics
    Metrics = maps:get(metrics, DetailedStatus, #{}),
    ?assert(maps:is_key(pending_messages, Metrics)),
    ?assert(maps:is_key(latency_p95_ms, Metrics)),
    ?assert(maps:is_key(inflight_messages, Metrics)),
    
    %% Verify thresholds
    Thresholds = maps:get(thresholds, DetailedStatus, #{}),
    ?assert(maps:is_key(queue_overload, Thresholds)),
    ?assert(maps:is_key(latency_overload_ms, Thresholds)),
    ?assert(maps:is_key(inflight_overload, Thresholds)),
    
    %% Cleanup
    ets:delete(PendingTable),
    ets:delete(LatencyTable),
    ok.

%% Test: End-to-end overload scenario - event tracking
test_end_to_end_overload_event_tracking(_Config) ->
    %% Setup: Simulate overload with event tracking
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS tables
    PendingTable = router_jetstream_pending_cache,
    case ets:whereis(PendingTable) of
        undefined -> ets:new(PendingTable, [named_table, set, public]);
        _ -> ok
    end,
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% Trigger backpressure check (creates events)
    {Status, _} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual(backpressure_active, Status),
    
    %% Get events
    Events = router_intake_backpressure:get_backpressure_events(Subject),
    ?assert(length(Events) > 0),
    
    %% Verify event structure
    [FirstEvent | _] = Events,
    ?assert(maps:is_key(type, FirstEvent)),
    ?assert(maps:is_key(timestamp, FirstEvent)),
    ?assertEqual(Subject, maps:get(subject, FirstEvent)),
    
    %% Get metrics
    Metrics = router_intake_backpressure:get_backpressure_metrics(Subject),
    ?assert(maps:is_key(total_events, Metrics)),
    ?assert(maps:is_key(active_events, Metrics)),
    
    %% Cleanup
    ets:delete(PendingTable),
    ok.

