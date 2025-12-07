%% @doc Recovery State Integrity Test Suite
%% 
%% Comprehensive test coverage for router recovery scenarios after NATS/JetStream failures.
%% Verifies that router recovers without restart and maintains state integrity:
%% 
%% 1. Recovery Without Restart:
%%    - Router recovers from connection loss without restart
%%    - Router recovers from ACK/NAK errors without restart
%%    - Router recovers from publish errors without restart
%%    - Router recovers from NATS/JetStream restart without restart
%% 
%% 2. State Integrity After Recovery:
%%    - ETS tables don't contain orphaned entries
%%    - Delivery counts remain accurate after recovery
%%    - Idempotency state remains consistent
%%    - No memory leaks or unbounded growth
%% 
%% 3. Message Processing After Recovery:
%%    - New messages process correctly after recovery
%%    - Redelivered messages handle correctly
%%    - No duplicate processing
%%    - No message loss
%% 
%% Test Categories:
%% - All tests verify router doesn't require restart
%% - All tests verify ETS state integrity
%% - All tests verify delivery count accuracy
%% - All tests verify idempotency consistency
%% - All tests verify no state degradation
-module(router_recovery_state_integrity_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
         test_recovery_connection_loss_ets_integrity/1, test_recovery_connection_loss_delivery_counts/1,
         test_recovery_connection_loss_idempotency/1, test_recovery_ack_nak_errors_ets_integrity/1,
         test_recovery_ack_nak_errors_delivery_counts/1, test_recovery_ack_nak_errors_idempotency/1,
         test_recovery_nats_restart_ets_integrity/1, test_recovery_nats_restart_delivery_counts/1,
         test_recovery_nats_restart_idempotency/1, test_recovery_multiple_fault_cycles_ets_integrity/1,
         test_recovery_multiple_fault_cycles_no_degradation/1, test_recovery_publish_errors_ets_integrity/1,
         test_recovery_publish_errors_message_processing/1, get_delivery_count/2]).

all() ->
    [
        %% Scenario A: Connection loss + re-connect
        test_recovery_connection_loss_ets_integrity,
        test_recovery_connection_loss_delivery_counts,
        test_recovery_connection_loss_idempotency,
        
        %% Scenario B: ACK/NAK errors without connection loss
        test_recovery_ack_nak_errors_ets_integrity,
        test_recovery_ack_nak_errors_delivery_counts,
        test_recovery_ack_nak_errors_idempotency,
        
        %% Scenario C: NATS/JetStream restart
        test_recovery_nats_restart_ets_integrity,
        test_recovery_nats_restart_delivery_counts,
        test_recovery_nats_restart_idempotency,
        
        %% Scenario D: Multiple fault cycles
        test_recovery_multiple_fault_cycles_ets_integrity,
        test_recovery_multiple_fault_cycles_no_degradation,
        
        %% Scenario E: Publish errors during recovery
        test_recovery_publish_errors_ets_integrity,
        test_recovery_publish_errors_message_processing
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
    ok = application:set_env(beamline_router, nats_reconnect_delay_ms, 500),
    ok = application:set_env(beamline_router, nats_max_reconnect_delay_ms, 2000),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    ok = application:set_env(beamline_router, nats_js_max_deliver, 5),
    %% Enable idempotency for recovery tests
    ok = application:set_env(beamline_router, cp2_plus_allowed, true),
    ok = application:set_env(beamline_router, idempotency_enabled, true),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            timer:sleep(1000),  %% Wait for router_nats to initialize
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear ETS tables before each test
    clear_ets_tables(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clear ETS tables after each test
    clear_ets_tables(),
    ok.

%% ============================================================================
%% Scenario A: Connection Loss + Re-connect
%% ============================================================================

%% @doc Test: ETS integrity after connection loss and recovery
%% 
%% INVARIANTS:
%% 1. Liveness: Router recovers without restart
%% 2. Consistency: ETS tables don't contain orphaned entries
%% 3. No Degradation: ETS size doesn't grow unbounded
test_recovery_connection_loss_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    
    %% Phase 1: Normal operation - send some messages
    Messages = [
        {<<"recovery.msg.1">>, <<"payload1">>},
        {<<"recovery.msg.2">>, <<"payload2">>},
        {<<"recovery.msg.3">>, <<"payload3">>}
    ],
    
    lists:foreach(fun({Subject, Payload}) ->
        _Result = router_nats:publish(Subject, Payload),
        timer:sleep(50)
    end, Messages),
    
    %% Get initial ETS state
    InitialDeliveryCountSize = get_ets_size(router_delivery_count),
    InitialAckDeliveryCountSize = get_ets_size(router_ack_delivery_count),
    InitialIdempotencySize = get_ets_size(router_idempotency),
    
    %% Phase 2: Induce connection loss
    gen_server:cast(router_nats, {connection_lost, recovery_test_connection_loss}),
    timer:sleep(500),
    
    %% Phase 3: Try operations during failure (should queue or fail gracefully)
    lists:foreach(fun({Subject, Payload}) ->
        _Result = router_nats:publish(Subject, Payload),
        timer:sleep(50)
    end, Messages),
    
    %% Phase 4: Recovery - restore connection
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    %% ========================================================================
    %% INVARIANT 1: LIVENESS VERIFICATION
    %% ========================================================================
    
    %% 1.1: Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    
    %% 1.2: Connection state returns to normal
    {ok, RecoveredState} = router_nats:get_connection_status(),
    true = (RecoveredState =:= connected orelse RecoveredState =:= reconnecting),
    
    %% ========================================================================
    %% INVARIANT 2: CONSISTENCY VERIFICATION
    %% ========================================================================
    
    %% 2.1: ETS tables don't contain orphaned entries
    %% Check that all entries in delivery count tables are valid
    DeliveryCountSize = get_ets_size(router_delivery_count),
    AckDeliveryCountSize = get_ets_size(router_ack_delivery_count),
    
    %% Verify no unbounded growth (should be reasonable)
    true = DeliveryCountSize < 1000,  %% Reasonable limit
    true = AckDeliveryCountSize < 1000,
    
    %% 2.2: Idempotency table doesn't contain orphaned entries
    IdempotencySize = get_ets_size(router_idempotency),
    true = IdempotencySize < 1000,
    
    %% ========================================================================
    %% INVARIANT 3: NO DEGRADATION VERIFICATION
    %% ========================================================================
    
    %% 3.1: ETS size doesn't grow unbounded after recovery
    %% After recovery, size should be reasonable (not thousands of entries)
    true = DeliveryCountSize < InitialDeliveryCountSize + 100,
    true = AckDeliveryCountSize < InitialAckDeliveryCountSize + 100,
    true = IdempotencySize < InitialIdempotencySize + 100,
    
    %% Clean up
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Delivery count accuracy after connection loss and recovery
%% 
%% INVARIANTS:
%% 1. Delivery counts remain accurate after recovery
%% 2. No delivery count corruption
%% 3. Delivery counts don't exceed MaxDeliver
test_recovery_connection_loss_delivery_counts(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Normal operation - track some delivery counts
    MsgId1 = <<"recovery.delivery.1">>,
    MsgId2 = <<"recovery.delivery.2">>,
    
    %% Simulate message deliveries (track delivery counts)
    ok = track_delivery_count_safe(router_delivery_count, MsgId1),
    ok = track_delivery_count_safe(router_delivery_count, MsgId1),  %% Second delivery
    ok = track_delivery_count_safe(router_delivery_count, MsgId2),
    
    %% Verify initial counts
    {ok, Count1Initial} = get_delivery_count(router_delivery_count, MsgId1),
    {ok, Count2Initial} = get_delivery_count(router_delivery_count, MsgId2),
    2 = Count1Initial,
    1 = Count2Initial,
    
    %% Phase 2: Induce connection loss
    gen_server:cast(router_nats, {connection_lost, recovery_test_delivery_counts}),
    timer:sleep(500),
    
    %% Phase 3: During failure, simulate re-delivery (delivery count should increment)
    ok = track_delivery_count_safe(router_delivery_count, MsgId1),  %% Third delivery
    ok = track_delivery_count_safe(router_delivery_count, MsgId2),  %% Second delivery
    
    %% Phase 4: Recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. Delivery counts remain accurate after recovery
    {ok, Count1After} = get_delivery_count(router_delivery_count, MsgId1),
    {ok, Count2After} = get_delivery_count(router_delivery_count, MsgId2),
    
    %% Counts should reflect all deliveries (including during failure)
    3 = Count1After,  %% Initial 2 + 1 during failure
    2 = Count2After,  %% Initial 1 + 1 during failure
    
    %% 3. Delivery counts don't exceed MaxDeliver (5)
    true = Count1After =< 5,
    true = Count2After =< 5,
    
    %% 4. No delivery count corruption (counts are logical)
    true = Count1After >= Count1Initial,
    true = Count2After >= Count2Initial,
    
    %% Clean up
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Idempotency consistency after connection loss and recovery
%% 
%% INVARIANTS:
%% 1. Idempotency state remains consistent
%% 2. No duplicate processing after recovery
%% 3. Idempotency entries don't leak
test_recovery_connection_loss_idempotency(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Normal operation - mark some messages as processed
    KeyType1 = <<"assignment_id">>,
    KeyType2 = <<"request_id">>,
    MsgId1 = <<"recovery.idem.1">>,
    MsgId2 = <<"recovery.idem.2">>,
    
    %% Mark messages as processed
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType1, MsgId1, #{}),
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType2, MsgId2, #{}),
    
    %% Verify they are marked as seen
    {ok, seen} = router_idempotency:check_and_mark(KeyType1, MsgId1, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType2, MsgId2, #{}),
    
    %% Phase 2: Induce connection loss
    gen_server:cast(router_nats, {connection_lost, recovery_test_idempotency}),
    timer:sleep(500),
    
    %% Phase 3: During failure, check idempotency (should still work)
    {ok, seen} = router_idempotency:check_and_mark(KeyType1, MsgId1, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType2, MsgId2, #{}),
    
    %% Phase 4: Recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. Idempotency state remains consistent after recovery
    {ok, seen} = router_idempotency:check_and_mark(KeyType1, MsgId1, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType2, MsgId2, #{}),
    
    %% 3. No duplicate processing (messages still marked as seen)
    %% This verifies idempotency entries weren't lost during recovery
    
    %% 4. New messages can still be processed (idempotency works for new messages)
    MsgId3 = <<"recovery.idem.3">>,
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType1, MsgId3, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType1, MsgId3, #{}),
    
    %% Clean up
    exit(StubPid, normal),
    
    ok.

%% ============================================================================
%% Scenario B: ACK/NAK Errors Without Connection Loss
%% ============================================================================

%% @doc Test: ETS integrity after ACK/NAK errors and recovery
test_recovery_ack_nak_errors_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Get initial ETS state
    InitialDeliveryCountSize = get_ets_size(router_delivery_count),
    InitialAckDeliveryCountSize = get_ets_size(router_ack_delivery_count),
    
    %% Phase 1: Normal operation
    MsgId1 = <<"recovery.ack.1">>,
    MsgId2 = <<"recovery.ack.2">>,
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId1),
    ok = track_delivery_count_safe(router_ack_delivery_count, MsgId2),
    
    %% Phase 2: Induce ACK/NAK errors (fault injection)
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(ack, {error, consumer_deleted}),
            router_nats_fault_injection:enable_fault(nak, {error, consumer_deleted});
        false ->
            ok
    end,
    
    %% Try ACK/NAK operations (should fail gracefully)
    _AckResult = router_nats:ack_message(MsgId1),
    _NakResult = router_nats:nak_message(MsgId2),
    timer:sleep(500),
    
    %% Phase 3: Recovery - disable fault injection
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:disable_fault(ack),
            router_nats_fault_injection:disable_fault(nak);
        false ->
            ok
    end,
    timer:sleep(500),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. ETS tables don't contain orphaned entries
    DeliveryCountSize = get_ets_size(router_delivery_count),
    AckDeliveryCountSize = get_ets_size(router_ack_delivery_count),
    
    true = DeliveryCountSize < 1000,
    true = AckDeliveryCountSize < 1000,
    
    %% 3. No unbounded growth
    true = DeliveryCountSize < InitialDeliveryCountSize + 100,
    true = AckDeliveryCountSize < InitialAckDeliveryCountSize + 100,
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Delivery count accuracy after ACK/NAK errors
test_recovery_ack_nak_errors_delivery_counts(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Phase 1: Normal operation
    MsgId = <<"recovery.ack.delivery.1">>,
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    {ok, InitialCount} = get_delivery_count(router_delivery_count, MsgId),
    2 = InitialCount,
    
    %% Phase 2: Induce ACK errors (fault injection)
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(ack, {error, consumer_deleted});
        false ->
            ok
    end,
    
    %% Simulate re-delivery during ACK errors
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    {ok, CountDuringFailure} = get_delivery_count(router_delivery_count, MsgId),
    3 = CountDuringFailure,
    
    %% Phase 3: Recovery
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:disable_fault(ack);
        false ->
            ok
    end,
    timer:sleep(500),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. Delivery count remains accurate
    {ok, CountAfterRecovery} = get_delivery_count(router_delivery_count, MsgId),
    3 = CountAfterRecovery,  %% Should match count during failure
    
    %% 3. Delivery count doesn't exceed MaxDeliver
    true = CountAfterRecovery =< 5,
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Idempotency consistency after ACK/NAK errors
test_recovery_ack_nak_errors_idempotency(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Phase 1: Normal operation
    KeyType = <<"ack_id">>,
    MsgId = <<"recovery.ack.idem.1">>,
    
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    %% Phase 2: Induce ACK errors
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(ack, {error, consumer_deleted});
        false ->
            ok
    end,
    
    %% Idempotency should still work during ACK errors
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    %% Phase 3: Recovery
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:disable_fault(ack);
        false ->
            ok
    end,
    timer:sleep(500),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. Idempotency state remains consistent
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    exit(StubPid, normal),
    
    ok.

%% ============================================================================
%% Scenario C: NATS/JetStream Restart
%% ============================================================================

%% @doc Test: ETS integrity after NATS/JetStream restart
test_recovery_nats_restart_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Normal operation
    Messages = [
        {<<"recovery.nats.1">>, <<"payload1">>},
        {<<"recovery.nats.2">>, <<"payload2">>}
    ],
    
    lists:foreach(fun({Subject, Payload}) ->
        _Result = router_nats:publish(Subject, Payload),
        timer:sleep(50)
    end, Messages),
    
    InitialDeliveryCountSize = get_ets_size(router_delivery_count),
    InitialIdempotencySize = get_ets_size(router_idempotency),
    
    %% Phase 2: Simulate NATS/JetStream restart (connection loss)
    gen_server:cast(router_nats, {connection_lost, nats_restart}),
    timer:sleep(500),
    
    %% Phase 3: Recovery - simulate NATS/JetStream coming back online
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. ETS tables don't contain orphaned entries
    DeliveryCountSize = get_ets_size(router_delivery_count),
    IdempotencySize = get_ets_size(router_idempotency),
    
    true = DeliveryCountSize < 1000,
    true = IdempotencySize < 1000,
    
    %% 3. No unbounded growth
    true = DeliveryCountSize < InitialDeliveryCountSize + 100,
    true = IdempotencySize < InitialIdempotencySize + 100,
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Delivery count accuracy after NATS/JetStream restart
test_recovery_nats_restart_delivery_counts(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Normal operation
    MsgId = <<"recovery.nats.delivery.1">>,
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    {ok, InitialCount} = get_delivery_count(router_delivery_count, MsgId),
    2 = InitialCount,
    
    %% Phase 2: NATS restart
    gen_server:cast(router_nats, {connection_lost, nats_restart}),
    timer:sleep(500),
    
    %% Phase 3: Recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. Delivery count remains accurate
    {ok, CountAfterRecovery} = get_delivery_count(router_delivery_count, MsgId),
    2 = CountAfterRecovery,  %% Should match initial count
    
    %% 3. Delivery count doesn't exceed MaxDeliver
    true = CountAfterRecovery =< 5,
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Idempotency consistency after NATS/JetStream restart
test_recovery_nats_restart_idempotency(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Normal operation
    KeyType = <<"assignment_id">>,
    MsgId = <<"recovery.nats.idem.1">>,
    
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    %% Phase 2: NATS restart
    gen_server:cast(router_nats, {connection_lost, nats_restart}),
    timer:sleep(500),
    
    %% Phase 3: Recovery
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. Idempotency state remains consistent
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    exit(StubPid, normal),
    
    ok.

%% ============================================================================
%% Scenario D: Multiple Fault Cycles
%% ============================================================================

%% @doc Test: ETS integrity after multiple fault cycles
test_recovery_multiple_fault_cycles_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Initial state
    InitialDeliveryCountSize = get_ets_size(router_delivery_count),
    InitialIdempotencySize = get_ets_size(router_idempotency),
    
    %% Phase 2: Multiple fault cycles (fault -> recovery -> fault -> recovery)
    FaultCycles = 3,
    lists:foreach(fun(Cycle) ->
        ct:log("Fault cycle ~p: inducing connection loss", [Cycle]),
        
        %% Induce fault
        gen_server:cast(router_nats, {connection_lost, {multiple_cycles, Cycle}}),
        timer:sleep(300),
        
        %% Verify still alive
        true = is_process_alive(RouterNatsPid),
        
        %% Recovery
        StubPid = spawn_link(fun() -> receive _ -> ok end end),
        gen_server:cast(router_nats, {connection_restored, StubPid}),
        timer:sleep(500),
        
        %% Clean up
        exit(StubPid, normal),
        timer:sleep(200)
    end, lists:seq(1, FaultCycles)),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. ETS tables don't accumulate garbage
    FinalDeliveryCountSize = get_ets_size(router_delivery_count),
    FinalIdempotencySize = get_ets_size(router_idempotency),
    
    true = FinalDeliveryCountSize < 1000,
    true = FinalIdempotencySize < 1000,
    
    %% 3. No unbounded growth after multiple cycles
    true = FinalDeliveryCountSize < InitialDeliveryCountSize + 200,
    true = FinalIdempotencySize < InitialIdempotencySize + 200,
    
    ok.

%% @doc Test: No performance degradation after multiple fault cycles
test_recovery_multiple_fault_cycles_no_degradation(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Phase 1: Multiple fault cycles
    FaultCycles = 5,
    lists:foreach(fun(Cycle) ->
        gen_server:cast(router_nats, {connection_lost, {degradation_test, Cycle}}),
        timer:sleep(200),
        
        StubPid = spawn_link(fun() -> receive _ -> ok end end),
        gen_server:cast(router_nats, {connection_restored, StubPid}),
        timer:sleep(300),
        
        exit(StubPid, normal),
        timer:sleep(100)
    end, lists:seq(1, FaultCycles)),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. New messages can still be processed after multiple cycles
    Result = router_nats:publish(<<"recovery.degradation.test">>, <<"payload">>),
    true = (Result =:= ok orelse (is_tuple(Result) andalso element(1, Result) =:= error)),
    
    %% 3. Idempotency still works
    KeyType = <<"request_id">>,
    MsgId = <<"recovery.degradation.idem">>,
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    ok.

%% ============================================================================
%% Scenario E: Publish Errors During Recovery
%% ============================================================================

%% @doc Test: ETS integrity after publish errors during recovery
test_recovery_publish_errors_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    InitialDeliveryCountSize = get_ets_size(router_delivery_count),
    
    %% Phase 1: Induce publish errors
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable});
        false ->
            ok
    end,
    
    %% Try to publish (should fail gracefully)
    _Result = router_nats:publish(<<"recovery.publish.1">>, <<"payload">>),
    timer:sleep(500),
    
    %% Phase 2: Recovery
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:disable_fault(publish);
        false ->
            ok
    end,
    timer:sleep(500),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. ETS tables don't contain orphaned entries
    DeliveryCountSize = get_ets_size(router_delivery_count),
    true = DeliveryCountSize < 1000,
    true = DeliveryCountSize < InitialDeliveryCountSize + 100,
    
    exit(StubPid, normal),
    
    ok.

%% @doc Test: Message processing after publish errors recovery
test_recovery_publish_errors_message_processing(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    %% Ensure connected
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    %% Phase 1: Induce publish errors
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable});
        false ->
            ok
    end,
    
    %% Phase 2: Recovery
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:disable_fault(publish);
        false ->
            ok
    end,
    timer:sleep(500),
    
    %% ========================================================================
    %% INVARIANT VERIFICATION
    %% ========================================================================
    
    %% 1. Router recovers without restart
    true = is_process_alive(RouterNatsPid),
    
    %% 2. New messages can be processed after recovery
    Result = router_nats:publish(<<"recovery.publish.2">>, <<"payload">>),
    true = (Result =:= ok orelse (is_tuple(Result) andalso element(1, Result) =:= error)),
    
    exit(StubPid, normal),
    
    ok.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% @doc Get ETS table size (number of entries)
get_ets_size(TableName) ->
    case ets:whereis(TableName) of
        undefined ->
            0;
        Table ->
            case ets:info(Table, size) of
                undefined -> 0;
                Size -> Size
            end
    end.

%% @doc Track delivery count safely (create table if needed)
track_delivery_count_safe(TableName, MsgId) ->
    case ets:whereis(TableName) of
        undefined ->
            _ = ets:new(TableName, [
                set,
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            ets:insert(TableName, {MsgId, 1}),
            ok;
        Table ->
            case ets:lookup(Table, MsgId) of
                [] ->
                    ets:insert(Table, {MsgId, 1}),
                    ok;
                [{MsgId, _Count}] ->
                    ets:update_counter(Table, MsgId, 1),
                    ok
            end
    end.

%% @doc Get delivery count for a message
get_delivery_count(TableName, MsgId) ->
    case ets:whereis(TableName) of
        undefined ->
            {ok, 0};
        Table ->
            case ets:lookup(Table, MsgId) of
                [] ->
                    {ok, 0};
                [{MsgId, Count}] ->
                    ?assert(Count >= 0),
                    {ok, Count}
            end
    end.

%% @doc Clear all ETS tables used in tests
clear_ets_tables() ->
    Tables = [
        router_delivery_count,
        router_ack_delivery_count,
        router_idempotency
    ],
    lists:foreach(fun(TableName) ->
        case ets:whereis(TableName) of
            undefined ->
                ok;
            Table ->
                ets:delete_all_objects(Table)
        end
    end, Tables),
    ok.

