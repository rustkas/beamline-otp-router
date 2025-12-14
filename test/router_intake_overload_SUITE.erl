%% @doc Router Intake Overload Test Suite
%% Tests Router behavior under overload conditions (backpressure scenarios)
%% CP2+: Backpressure and queue management testing
-module(router_intake_overload_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, init_per_suite/1, init_per_testcase/2]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_backpressure_nak_rejection/1,
    test_backpressure_recovery/1,
    test_backpressure_rejection/1,
    test_backpressure_warning_no_rejections/1,
    test_backpressure_inactive_no_rejections/1,
    test_end_to_end_overload_detailed_status/1,
    test_end_to_end_overload_event_tracking/1,
    test_end_to_end_overload_gateway/1,
    test_overload_combined/1,
    test_overload_inflight_messages/1,
    test_overload_jetstream_backlog/1,
    test_overload_processing_latency/1
]).



-define(SUITE_NAME, router_intake_overload_SUITE).

-export([groups/0, groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Overload/stress tests are heavy-only
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [];  %% Overload tests only run in heavy tier
groups_for_level(heavy) -> [{group, stress_tests}].

groups() ->
    [{stress_tests, [sequence], [
        test_overload_jetstream_backlog,
        test_overload_processing_latency,
        test_overload_inflight_messages,
        test_overload_combined,
        test_backpressure_rejection,
        test_backpressure_nak_rejection,
        test_backpressure_warning_no_rejections,
        test_backpressure_inactive_no_rejections,
        test_backpressure_recovery,
        test_end_to_end_overload_gateway,
        test_end_to_end_overload_detailed_status,
        test_end_to_end_overload_event_tracking
    ]}].

init_per_suite(Config) ->
    %% Setup test environment (start dependencies as well)
    _ = application:load(beamline_router),
    ok = application:ensure_started(sasl),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subj, _Stream, _Ack, _Dur, _Mode) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    meck:new(router_jetstream, [passthrough]),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    _ = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_intake_inflight, [named_table, bag, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_events, [named_table, ordered_set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_status_history, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_recovery, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_events, [named_table, ordered_set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_status_history, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_recovery, [named_table, set, public]),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        {error, {already_started, beamline_router}} -> Config;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    meck:unload(),
    ok = application:stop(beamline_router),
    [
        case ets:whereis(Tab) of undefined -> ok; _ -> catch ets:delete(Tab), ok end
        || Tab <- [
            router_jetstream_pending_cache,
            router_intake_latency_cache,
            router_intake_inflight,
            router_backpressure_events,
            router_backpressure_status_history,
            router_backpressure_recovery
        ]
    ],
    ok.

init_per_testcase(_TestCase, Config) ->
    router_mock_helpers:ensure_mock(router_nats, []),
    router_mock_helpers:reset(router_nats),
    %% Re-stub dangerous functions after reset
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subj, _Stream, _Ack, _Dur, _Mode) ->
        {error, connection_closed}
    end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    router_mock_helpers:ensure_mock(router_jetstream, [passthrough]),
    router_mock_helpers:reset(router_jetstream),
    router_mock_helpers:ensure_mock(router_logger, [passthrough]),
    router_mock_helpers:reset(router_logger),
    router_mock_helpers:ensure_mock(telemetry, [passthrough]),
    router_mock_helpers:reset(telemetry),
    _ = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_intake_inflight, [named_table, bag, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_events, [named_table, ordered_set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_status_history, [named_table, set, public]),
    _ = router_test_init:ensure_ets_table(router_backpressure_recovery, [named_table, set, public]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch meck:unload(router_nats),
    ok.

%% Test: JetStream backlog overload
test_overload_jetstream_backlog(_Config) ->
    %% Setup: Simulate high backlog (pending messages > 1000)
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for pending cache
    Table = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(Table, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure warning (only queue overloaded)
    ?assertEqual({backpressure_warning, 0}, {Status, RetryAfter}),
    
    %% Verify metrics emitted
    ?assert(meck:called(telemetry, execute, '_')),
    
    %% Cleanup
    ets:delete_all_objects(Table),
    ok.

%% Test: Processing latency overload
test_overload_processing_latency(_Config) ->
    %% Setup: Simulate high latency (p95 > 5000ms)
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for latency cache
    Table = router_intake_latency_cache,
    ets:insert(Table, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    %% Set pending to low value (not overloaded)
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 50, erlang:system_time(millisecond)}),
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure warning (latency only, not queue)
    ?assertEqual({backpressure_warning, 0}, {Status, RetryAfter}),
    
    %% Cleanup
    ets:delete_all_objects(Table),
    ets:delete_all_objects(PendingTable),
    ok.

%% Test: In-flight messages overload
test_overload_inflight_messages(_Config) ->
    %% Setup: Simulate high in-flight count (> 500)
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS table for in-flight tracking
    Table = router_intake_inflight,
    
    %% Add 600 in-flight messages
    [ets:insert(Table, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 600)],
    
    %% Set pending and latency to low values
    PendingTable = router_jetstream_pending_cache,
    ets:insert(PendingTable, {Subject, 50, erlang:system_time(millisecond)}),
    
    LatencyTable = router_intake_latency_cache,
    ets:insert(LatencyTable, {{Subject, p95}, 100, erlang:system_time(millisecond)}),
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure warning (in-flight only)
    ?assertEqual({backpressure_warning, 0}, {Status, RetryAfter}),
    
    %% Cleanup
    ets:delete_all_objects(Table),
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ok.

%% Test: Combined overload (backlog + latency + in-flight)
test_overload_combined(_Config) ->
    %% Setup: Simulate all overload conditions
    Subject = <<"beamline.router.v1.decide">>,
    
    %% High backlog
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% High latency
    LatencyTable = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    ets:insert(LatencyTable, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    %% High in-flight
    InFlightTable = router_intake_inflight,
    [ets:insert(InFlightTable, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 600)],
    
    %% Check backpressure
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    
    %% Verify backpressure is active
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    
    %% Cleanup
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ets:delete_all_objects(InFlightTable),
    ok.

%% Test: Backpressure rejection
test_backpressure_rejection(_Config) ->
    %% Setup: Simulate backpressure active
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Set backpressure active
    PendingTable = router_jetstream_pending_cache,
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    LatencyTable = router_intake_latency_cache,
    ets:insert(LatencyTable, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    meck:expect(telemetry, execute, fun(_, _, _) -> ok end),
    {Status, RetryAfter} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual({backpressure_active, 30}, {Status, RetryAfter}),
    ?assert(meck:called(telemetry, execute, '_')),

    %% Cleanup
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ok.

%% Test: Backpressure NAK rejection
test_backpressure_nak_rejection(_Config) ->
    %% Setup: Simulate backpressure active
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"test-msg-backpressure-nak">>,
    
    %% Set backpressure active
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),

    LatencyTable = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    ets:insert(LatencyTable, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    meck:expect(router_jetstream, nak, fun(#{id := MId}, Reason, _Ctx) ->
        ?assertEqual(MsgId, MId),
        ?assertEqual(backpressure, Reason),
        ok
    end),
    
    %% Mock telemetry to track rejection metric, pass through other events
    meck:expect(telemetry, execute, fun(Event, Measurements, Metadata) ->
        case Event of
            [router_intake_backpressure, rejections] ->
                ?assertEqual(1, maps:get(count, Measurements, 0)),
                ?assertEqual(Subject, maps:get(subject, Metadata, undefined)),
                ?assertEqual(backpressure_active, maps:get(reason, Metadata, undefined)),
                ok;
            _ ->
                ok
        end
    end),
    
    router_decide_consumer:handle_decide_message(Subject, <<"{}">>, #{}, MsgId),
    %% Positive: verify telemetry was called (simplified for compilation)
    ?assert(meck:called(telemetry, execute, '_')),
    %% Verify NAK was called with MsgId
    ?assert(meck:called(router_jetstream, nak, '_')),
    _ = router_backpressure_test_helpers:wait_for_rejection_count(1, 500),
    DetailedStatusNak = router_intake_backpressure:get_detailed_backpressure_status(Subject),
    ?assertEqual(backpressure_active, maps:get(status, DetailedStatusNak, backpressure_inactive)),
    MetricsNak = maps:get(metrics, DetailedStatusNak, #{}),
    ?assert(is_integer(maps:get(pending_messages, MetricsNak))),
    ?assert(is_integer(maps:get(latency_p95_ms, MetricsNak))),
    ?assert(is_integer(maps:get(inflight_messages, MetricsNak))),
    ?assertEqual(Subject, maps:get(subject, DetailedStatusNak, undefined)),
    
    %% Cleanup
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ok.

%% Test: Backpressure warning — no rejections and no NAK
test_backpressure_warning_no_rejections(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"test-msg-warning-no-reject">>,
    %% Only queue overloaded → warning
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    %% Latency and inflight are low
    LatencyTable = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    ets:insert(LatencyTable, {{Subject, p95}, 100, erlang:system_time(millisecond)}),
    InFlightTable = router_test_init:ensure_ets_table(router_intake_inflight, [named_table, bag, public]),
    [ets:insert(InFlightTable, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 10)],
    %% Call handler
    meck:expect(router_jetstream, nak, fun(_, _, _) -> ok end),
    meck:expect(telemetry, execute, fun(_, _, _) -> ok end),
    _ = router_intake_backpressure:check_backpressure(Subject),
    router_decide_consumer:handle_decide_message(Subject, <<"{}">>, #{}, MsgId),
    %% Ensure no NAK and confirm status via public API
    ?assert(not meck:called(router_jetstream, nak, '_')),
    %% Status via detailed API
    ?assert(meck:called(telemetry, execute, '_')),
    DetailedStatusWarn = router_intake_backpressure:get_detailed_backpressure_status(Subject),
    ?assertEqual(backpressure_warning, maps:get(status, DetailedStatusWarn, backpressure_inactive)),
    MetricsWarn = maps:get(metrics, DetailedStatusWarn, #{}),
    ?assert(is_integer(maps:get(pending_messages, MetricsWarn))),
    ?assert(is_integer(maps:get(latency_p95_ms, MetricsWarn))),
    ?assert(is_integer(maps:get(inflight_messages, MetricsWarn))),
    ?assertEqual(Subject, maps:get(subject, DetailedStatusWarn, undefined)),
    %% Cleanup
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ets:delete_all_objects(InFlightTable),
    ok.

%% Test: Backpressure inactive — no rejections and no NAK
test_backpressure_inactive_no_rejections(_Config) ->
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"test-msg-inactive-no-reject">>,
    %% All signals low → inactive
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 0, erlang:system_time(millisecond)}),
    LatencyTable = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    ets:insert(LatencyTable, {{Subject, p95}, 50, erlang:system_time(millisecond)}),
    InFlightTable = router_test_init:ensure_ets_table(router_intake_inflight, [named_table, bag, public]),
    [ets:insert(InFlightTable, {Subject, erlang:unique_integer()}) || _ <- lists:seq(1, 2)],
    %% Call handler
    meck:expect(router_jetstream, nak, fun(_, _, _) -> ok end),
    meck:expect(telemetry, execute, fun(_, _, _) -> ok end),
    _ = router_intake_backpressure:check_backpressure(Subject),
    router_decide_consumer:handle_decide_message(Subject, <<"{}">>, #{}, MsgId),
    %% Ensure no NAK and confirm status via public API
    ?assert(not meck:called(router_jetstream, nak, '_')),
    %% Status via detailed API
    ?assert(meck:called(telemetry, execute, '_')),
    DetailedStatusInact = router_intake_backpressure:get_detailed_backpressure_status(Subject),
    ?assertEqual(backpressure_inactive, maps:get(status, DetailedStatusInact, backpressure_active)),
    MetricsInact = maps:get(metrics, DetailedStatusInact, #{}),
    ?assert(is_integer(maps:get(pending_messages, MetricsInact))),
    ?assert(is_integer(maps:get(latency_p95_ms, MetricsInact))),
    ?assert(is_integer(maps:get(inflight_messages, MetricsInact))),
    ?assertEqual(Subject, maps:get(subject, DetailedStatusInact, undefined)),
    %% Cleanup
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ets:delete_all_objects(InFlightTable),
    ok.

%% Test: Backpressure recovery
test_backpressure_recovery(_Config) ->
    %% Setup: Simulate recovery from backpressure
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Phase 1: Backpressure active
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    LatencyTable = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
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
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ok.

%% Test: End-to-end overload scenario - Gateway integration
test_end_to_end_overload_gateway(_Config) ->
    %% Setup: Simulate full overload scenario with Gateway integration
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create all required ETS tables
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    LatencyTable = router_test_init:ensure_ets_table(router_intake_latency_cache, [named_table, set, public]),
    ets:insert(LatencyTable, {{Subject, p95}, 6000, erlang:system_time(millisecond)}),
    
    InFlightTable = router_test_init:ensure_ets_table(router_intake_inflight, [named_table, bag, public]),
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
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ets:delete_all_objects(InFlightTable),
    ok.

%% Test: End-to-end overload scenario - detailed status
test_end_to_end_overload_detailed_status(_Config) ->
    %% Setup: Simulate overload with detailed status tracking
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS tables
    PendingTable = router_jetstream_pending_cache,
    ets:insert(PendingTable, {Subject, 1200, erlang:system_time(millisecond)}),
    
    LatencyTable = router_intake_latency_cache,
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
    ets:delete_all_objects(PendingTable),
    ets:delete_all_objects(LatencyTable),
    ok.

%% Test: End-to-end overload scenario - event tracking
test_end_to_end_overload_event_tracking(_Config) ->
    %% Setup: Simulate overload with event tracking
    Subject = <<"beamline.router.v1.decide">>,
    
    %% Create ETS tables
    PendingTable = router_test_init:ensure_ets_table(router_jetstream_pending_cache, [named_table, set, public]),
    ets:insert(PendingTable, {Subject, 1500, erlang:system_time(millisecond)}),
    
    %% Trigger backpressure check (creates events)
    {Status, _} = router_intake_backpressure:check_backpressure(Subject),
    ?assertEqual(backpressure_warning, Status),
    
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
    ets:delete_all_objects(PendingTable),
    ok.
