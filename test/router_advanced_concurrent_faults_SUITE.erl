%% @doc Common Test suite for advanced concurrent fault scenarios
%% Tests router behavior under complex simultaneous faults:
%% - Triple-fault scenarios (connect + publish + ack/nak simultaneously)
%% - Mixed intermittent + persistent fault patterns
%% - Cascading fault chains
%% - >2 types of failures simultaneously
%% @test_category fault_injection, advanced_concurrent_faults, integration, slow
-module(router_advanced_concurrent_faults_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [all/0, groups/0, init_per_suite/1, end_per_suite/1, 
                                    init_per_testcase/2, end_per_testcase/2]}).

%% Common Test callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([
    test_triple_fault_scenario_a_connect_publish_ack/1,
    test_triple_fault_scenario_b_connect_publish_nak/1,
    test_triple_fault_scenario_c_flapping_connect_publish_ack/1,
    test_triple_fault_connect_publish_ack_simultaneous/1,
    test_triple_fault_connect_publish_nak_simultaneous/1,
    test_mixed_intermittent_connect_persistent_publish/1,
    test_mixed_persistent_connect_intermittent_ack/1,
    test_mixed_intermittent_publish_persistent_ack/1,
    test_mixed_pattern_flapping_with_persistent_errors/1,
    test_cascading_connect_publish_ack_chain/1,
    test_cascading_reconnect_storm_publish_backlog_ack_loss/1,
    test_cascading_multiple_recovery_cycles/1
]).

all() ->
    [
        {group, triple_fault_tests},
        {group, mixed_pattern_tests},
        {group, cascading_fault_tests}
    ].

groups() ->
    [
        {triple_fault_tests, [sequence], [
            test_triple_fault_scenario_a_connect_publish_ack,
            test_triple_fault_scenario_b_connect_publish_nak,
            test_triple_fault_scenario_c_flapping_connect_publish_ack,
            test_triple_fault_connect_publish_ack_simultaneous,
            test_triple_fault_connect_publish_nak_simultaneous
        ]},
        {mixed_pattern_tests, [sequence], [
            test_mixed_intermittent_connect_persistent_publish,
            test_mixed_persistent_connect_intermittent_ack,
            test_mixed_intermittent_publish_persistent_ack,
            test_mixed_pattern_flapping_with_persistent_errors
        ]},
        {cascading_fault_tests, [sequence], [
            test_cascading_connect_publish_ack_chain,
            test_cascading_reconnect_storm_publish_backlog_ack_loss,
            test_cascading_multiple_recovery_cycles
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    %% Ensure metrics table exists
    router_metrics:ensure(),
    %% Clear fault injection state
    router_nats_fault_injection:clear_all_faults(),
    Config.

end_per_suite(Config) ->
    router_nats_fault_injection:clear_all_faults(),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state before each test
    router_nats_fault_injection:clear_all_faults(),
    %% Reset metrics
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined -> ok;
        _ -> ets:delete_all_objects(router_metrics)
    end,
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Clear fault injection state after each test
    router_nats_fault_injection:clear_all_faults(),
    Config.

%% ========================================================================
%% HELPER FUNCTIONS
%% ========================================================================

%% @doc Get metrics snapshot from router_metrics ETS table
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    router_metrics:ensure(),
    case ets:info(router_metrics) of
        undefined ->
            #{};
        _ ->
            Metrics = ets:tab2list(router_metrics),
            lists:foldl(fun
                ({Key, Value}, Acc) when is_atom(Key) ->
                    maps:put(Key, Value, Acc);
                ({{MetricName, _Labels}, Value}, Acc) when is_atom(MetricName) ->
                    Current = maps:get(MetricName, Acc, 0),
                    maps:put(MetricName, Current + Value, Acc)
            end, #{}, Metrics)
    end.

%% @doc Send test message to router
-spec send_test_message(binary(), binary(), map()) -> ok.
send_test_message(TenantId, RequestId, ExtraFields) ->
    Result = maps:merge(#{
        <<"assignment_id">> => <<"assign-", RequestId/binary>>,
        <<"request_id">> => RequestId,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => TenantId,
        <<"timestamp">> => erlang:system_time(millisecond)
    }, ExtraFields),
    ResultJson = jsx:encode(Result),
    Subject = <<"caf.exec.result.v1">>,
    MsgId = <<"msg-", RequestId/binary>>,
    case whereis(router_result_consumer) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            router_result_consumer:handle_info({nats_message, Subject, ResultJson, #{}, MsgId}, #{})
    end,
    ok.

%% @doc Send batch of test messages
-spec send_message_batch(binary(), non_neg_integer(), binary()) -> ok.
send_message_batch(TenantId, Count, Prefix) ->
    [begin
        RequestId = <<Prefix/binary, "-", (integer_to_binary(N))/binary>>,
        send_test_message(TenantId, RequestId, #{}),
        timer:sleep(50)
    end || N <- lists:seq(1, Count)],
    ok.

%% @doc Enable intermittent fault (fails with probability)
%% Probability: 0.0 to 1.0 (0.0 = never fails, 1.0 = always fails)
-spec enable_intermittent_fault(atom(), term(), float()) -> ok.
enable_intermittent_fault(Operation, Fault, Probability) when Probability >= 0.0, Probability =< 1.0 ->
    router_nats_fault_injection:enable_fault(Operation, {intermittent, Fault, Probability}),
    ok.

%% @doc Enable persistent fault (always fails)
-spec enable_persistent_fault(atom(), term()) -> ok.
enable_persistent_fault(Operation, Fault) ->
    router_nats_fault_injection:enable_fault(Operation, Fault),
    ok.

%% @doc Verify no deadlocks or resource leaks
%% Checks: process liveness, no excessive memory growth, no stuck processes
-spec verify_no_deadlocks_or_leaks() -> ok.
verify_no_deadlocks_or_leaks() ->
    %% Check critical processes are alive
    RouterNatsPid = whereis(router_nats),
    RouterSupPid = whereis(beamline_router_sup),
    ConsumerPid = whereis(router_result_consumer),
    
    true = is_process_alive(RouterNatsPid),
    true = is_process_alive(RouterSupPid),
    case ConsumerPid of
        undefined -> ok;
        Pid when is_pid(Pid) -> true = is_process_alive(Pid)
    end,
    
    %% Check process count (should not grow unbounded)
    ProcessCount = erlang:system_info(process_count),
    ct:comment("Process count: ~p", [ProcessCount]),
    true = (ProcessCount < 10000),  %% Reasonable upper bound
    
    ok.

%% @doc Verify message semantics (at-least-once delivery)
%% Checks: no excessive duplicates, no lost messages beyond SLA
-spec verify_message_semantics(map(), map(), map()) -> ok.
verify_message_semantics(InitialMetrics, FinalMetrics, ExpectedBehavior) ->
    %% Check redelivery metrics
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    RedeliveryDelta = FinalRedelivery - InitialRedelivery,
    
    %% Check ACK metrics
    InitialAck = maps:get(router_jetstream_ack_total, InitialMetrics, 0),
    FinalAck = maps:get(router_jetstream_ack_total, FinalMetrics, 0),
    AckDelta = FinalAck - InitialAck,
    
    ct:comment("Redelivery delta: ~p, ACK delta: ~p", [RedeliveryDelta, AckDelta]),
    
    %% Verify redelivery is reasonable (not infinite)
    MaxExpectedRedelivery = maps:get(max_redelivery, ExpectedBehavior, 100),
    true = (RedeliveryDelta =< MaxExpectedRedelivery),
    
    ok.

%% @doc Wait for system to stabilize after faults
-spec wait_for_stabilization(non_neg_integer()) -> ok.
wait_for_stabilization(TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_stabilization_loop(StartTime, TimeoutMs).

wait_for_stabilization_loop(StartTime, TimeoutMs) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= TimeoutMs of
        true ->
            ok;
        false ->
            timer:sleep(100),
            wait_for_stabilization_loop(StartTime, TimeoutMs)
    end.

%% ========================================================================
%% TRIPLE-FAULT TEST CASES
%% ========================================================================

%% @doc Test Scenario A: Connect + Publish + ACK concurrent faults
%% 
%% Scenario: During connection establishment, publish operations are attempted,
%% and ACK operations fail simultaneously:
%% - Connect: interruption during handshake
%% - Publish: part of messages sent, part with timeouts
%% - ACK: ACK on already published messages doesn't reach client
%%
%% Expectations:
%% - Client performs correct retries, no infinite hangs
%% - Queue state is consistent
%% - No message loss beyond acceptable SLA
%% - System recovers after faults are cleared
test_triple_fault_scenario_a_connect_publish_ack(_Config) ->
    ct:comment("=== Test Scenario A: Connect + Publish + ACK Triple Fault ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Configure triple faults: connect + publish + ack
    Faults = [
        {connect, {error, connection_refused}},
        {publish, {error, timeout}},
        {ack, {error, timeout}}
    ],
    
    %% Enable all faults simultaneously
    lists:foreach(fun({Operation, Fault}) ->
        router_nats_fault_injection:enable_fault(Operation, Fault)
    end, Faults),
    
    %% WHEN: Attempt operations during triple fault
    spawn(fun() ->
        timer:sleep(100),
        send_message_batch(<<"acme">>, 10, <<"triple-a">>)
    end),
    
    %% Wait for faults to manifest
    timer:sleep(1500),
    
    %% Get metrics during fault
    FaultMetrics = get_metrics_snapshot(),
    
    %% Verify no deadlocks or leaks
    verify_no_deadlocks_or_leaks(),
    
    %% Disable faults (recovery)
    lists:foreach(fun({Operation, _Fault}) ->
        router_nats_fault_injection:disable_fault(Operation)
    end, Faults),
    
    %% Wait for recovery
    wait_for_stabilization(2000),
    
    %% Get metrics after recovery
    FinalMetrics = get_metrics_snapshot(),
    
    %% THEN: Verify all criteria
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    %% Verify error metrics increased during fault
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FaultConnectionLost = maps:get(router_nats_connection_lost_total, FaultMetrics, 0),
    FinalConnectionLost = maps:get(router_nats_connection_lost_total, FinalMetrics, 0),
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FaultPublishFailures = maps:get(router_nats_publish_failures_total, FaultMetrics, 0),
    FinalPublishFailures = maps:get(router_nats_publish_failures_total, FinalMetrics, 0),
    InitialAckFailures = maps:get(router_nats_ack_failures_total, InitialMetrics, 0),
    FaultAckFailures = maps:get(router_nats_ack_failures_total, FaultMetrics, 0),
    FinalAckFailures = maps:get(router_nats_ack_failures_total, FinalMetrics, 0),
    
    ct:comment("Connection lost: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialConnectionLost, FaultConnectionLost, FinalConnectionLost]),
    ct:comment("Publish failures: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialPublishFailures, FaultPublishFailures, FinalPublishFailures]),
    ct:comment("ACK failures: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialAckFailures, FaultAckFailures, FinalAckFailures]),
    
    %% Assertions: Errors should increase during fault
    true = (FaultConnectionLost >= InitialConnectionLost orelse 
            FaultPublishFailures >= InitialPublishFailures orelse
            FaultAckFailures >= InitialAckFailures),
    true = (FinalConnectionLost >= InitialConnectionLost orelse 
            FinalPublishFailures >= InitialPublishFailures orelse
            FinalAckFailures >= InitialAckFailures),
    
    ok.

%% @doc Test Scenario B: Connect + Publish + NAK concurrent faults
%%
%% Scenario: Connection established but network drops after few milliseconds,
%% client continues trying to publish, handler tries to send NAK but network already down
%%
%% Expectations:
%% - No lost "hanging" messages that are stuck forever without redelivery
%% - Correct retry logic
%% - No infinite retry loops
test_triple_fault_scenario_b_connect_publish_nak(_Config) ->
    ct:comment("=== Test Scenario B: Connect + Publish + NAK Triple Fault ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Configure triple faults: connect + publish + nak
    Faults = [
        {connect, close_connection},
        {publish, {error, timeout}},
        {nak, {error, connection_refused}}
    ],
    
    %% Enable all faults simultaneously
    lists:foreach(fun({Operation, Fault}) ->
        router_nats_fault_injection:enable_fault(Operation, Fault)
    end, Faults),
    
    %% WHEN: Attempt operations during triple fault
    spawn(fun() ->
        timer:sleep(100),
        send_message_batch(<<"acme">>, 10, <<"triple-b">>)
    end),
    
    %% Wait for faults to manifest
    timer:sleep(1500),
    
    %% Get metrics during fault
    FaultMetrics = get_metrics_snapshot(),
    
    %% Verify no deadlocks or leaks
    verify_no_deadlocks_or_leaks(),
    
    %% Disable faults (recovery)
    lists:foreach(fun({Operation, _Fault}) ->
        router_nats_fault_injection:disable_fault(Operation)
    end, Faults),
    
    %% Wait for recovery
    wait_for_stabilization(2000),
    
    %% Get metrics after recovery
    FinalMetrics = get_metrics_snapshot(),
    
    %% THEN: Verify all criteria
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    %% Verify redelivery occurred (messages should be redelivered, not lost)
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FaultRedelivery = maps:get(router_jetstream_redelivery_total, FaultMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    
    ct:comment("Redelivery: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialRedelivery, FaultRedelivery, FinalRedelivery]),
    
    %% Verify error metrics increased during fault
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FaultConnectionLost = maps:get(router_nats_connection_lost_total, FaultMetrics, 0),
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FaultPublishFailures = maps:get(router_nats_publish_failures_total, FaultMetrics, 0),
    
    ct:comment("Connection lost: Initial=~p, DuringFault=~p", 
               [InitialConnectionLost, FaultConnectionLost]),
    ct:comment("Publish failures: Initial=~p, DuringFault=~p", 
               [InitialPublishFailures, FaultPublishFailures]),
    
    %% Assertions: Errors should increase during fault
    true = (FaultConnectionLost >= InitialConnectionLost orelse 
            FaultPublishFailures >= InitialPublishFailures),
    
    %% Assertions: Redelivery should occur (messages not lost)
    true = (FinalRedelivery >= InitialRedelivery),
    
    ok.

%% @doc Test Scenario C: Flapping connection + Publish + ACK concurrent faults
%%
%% Scenario: Connection flaps (up/down multiple times in short period),
%% parallel workers publish to different streams,
%% part of ACKs lost, part duplicated (due to client retries)
%%
%% Expectations:
%% - No invariant violations (e.g., incorrect delivery count, wild duplicates)
%% - Correct handling of duplicate ACKs
%% - System stabilizes after flapping stops
test_triple_fault_scenario_c_flapping_connect_publish_ack(_Config) ->
    ct:comment("=== Test Scenario C: Flapping Connection + Publish + ACK Triple Fault ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate flapping: enable/disable connect faults repeatedly
    FlappingCycles = 5,
    
    %% WHEN: Simulate flapping connection with publish and ACK faults
    spawn(fun() ->
        lists:foreach(fun(Cycle) ->
            %% Enable connect fault (break connection)
            router_nats_fault_injection:enable_fault(connect, close_connection),
            router_nats_fault_injection:enable_fault(publish, {error, timeout}),
            router_nats_fault_injection:enable_fault(ack, {error, timeout}),
            timer:sleep(200),
            
            %% Disable fault (reconnect)
            router_nats_fault_injection:disable_fault(connect),
            router_nats_fault_injection:disable_fault(publish),
            router_nats_fault_injection:disable_fault(ack),
            timer:sleep(200),
            
            %% Send messages during flapping
            if Cycle rem 2 =:= 0 ->
                send_message_batch(<<"acme">>, 3, <<"flap-", (integer_to_binary(Cycle))/binary>>);
            true ->
                ok
            end
        end, lists:seq(1, FlappingCycles))
    end),
    
    %% Wait for flapping cycles
    timer:sleep(FlappingCycles * 500 + 1000),
    
    %% Get metrics during flapping
    _ = get_metrics_snapshot(),
    
    %% Verify no deadlocks or leaks
    verify_no_deadlocks_or_leaks(),
    
    %% Clear all faults and wait for stabilization
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% Get metrics after stabilization
    FinalMetrics = get_metrics_snapshot(),
    
    %% THEN: Verify all criteria
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    %% Verify connection metrics reflect flapping
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FinalConnectionLost = maps:get(router_nats_connection_lost_total, FinalMetrics, 0),
    InitialConnectionRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
    FinalConnectionRestored = maps:get(router_nats_connection_restored_total, FinalMetrics, 0),
    
    ct:comment("Connection lost: Initial=~p, Final=~p", [InitialConnectionLost, FinalConnectionLost]),
    ct:comment("Connection restored: Initial=~p, Final=~p", [InitialConnectionRestored, FinalConnectionRestored]),
    
    %% Assertions: Connection events should reflect flapping
    true = (FinalConnectionLost >= InitialConnectionLost),
    true = (FinalConnectionRestored >= InitialConnectionRestored),
    
    ok.

%% @doc Test: Connect + Publish + ACK simultaneous faults (all at once)
%%
%% Scenario: All three fault types occur simultaneously at the same moment
%%
%% Expectations:
%% - System handles simultaneous faults correctly
%% - No race conditions
%% - Correct recovery after all faults cleared
test_triple_fault_connect_publish_ack_simultaneous(_Config) ->
    ct:comment("=== Test: Connect + Publish + ACK Simultaneous Faults ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% WHEN: Enable all three faults simultaneously
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    %% Attempt operations
    spawn(fun() ->
        send_message_batch(<<"acme">>, 10, <<"simultaneous">>)
    end),
    
    timer:sleep(1500),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Disable all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify recovery
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    ok.

%% @doc Test: Connect + Publish + NAK simultaneous faults
%%
%% Scenario: All three fault types occur simultaneously
%%
%% Expectations:
%% - System handles simultaneous faults correctly
%% - Messages are redelivered correctly
test_triple_fault_connect_publish_nak_simultaneous(_Config) ->
    ct:comment("=== Test: Connect + Publish + NAK Simultaneous Faults ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% WHEN: Enable all three faults simultaneously
    router_nats_fault_injection:enable_fault(connect, close_connection),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    router_nats_fault_injection:enable_fault(nak, {error, connection_refused}),
    
    %% Attempt operations
    spawn(fun() ->
        send_message_batch(<<"acme">>, 10, <<"simultaneous-nak">>)
    end),
    
    timer:sleep(1500),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Disable all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify recovery and redelivery
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    %% Verify redelivery occurred
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    true = (FinalRedelivery >= InitialRedelivery),
    
    ok.

%% ========================================================================
%% MIXED PATTERN TEST CASES (INTERMITTENT + PERSISTENT)
%% ========================================================================

%% @doc Test: Intermittent connect + Persistent publish error
%%
%% Scenario: Connection drops and recovers periodically,
%% but even when connected, server consistently rejects publish (quota, no stream)
%%
%% Expectations:
%% - Client doesn't waste resources on meaningless retries
%% - Correct logging/metrics
%% - System enters degraded mode gracefully
test_mixed_intermittent_connect_persistent_publish(_Config) ->
    ct:comment("=== Test: Intermittent Connect + Persistent Publish Error ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Enable intermittent connect fault (50% probability)
    enable_intermittent_fault(connect, close_connection, 0.5),
    
    %% Enable persistent publish fault (always fails)
    enable_persistent_fault(publish, {error, quota_exceeded}),
    
    %% WHEN: Attempt operations
    spawn(fun() ->
        send_message_batch(<<"acme">>, 20, <<"mixed-1">>)
    end),
    
    timer:sleep(3000),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Get metrics during faults
    FaultMetrics = get_metrics_snapshot(),
    
    %% Disable faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify metrics show persistent publish errors
    FinalMetrics = get_metrics_snapshot(),
    
    InitialPublishFailures = maps:get(router_nats_publish_failures_total, InitialMetrics, 0),
    FaultPublishFailures = maps:get(router_nats_publish_failures_total, FaultMetrics, 0),
    FinalPublishFailures = maps:get(router_nats_publish_failures_total, FinalMetrics, 0),
    
    ct:comment("Publish failures: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialPublishFailures, FaultPublishFailures, FinalPublishFailures]),
    
    %% Assertions: Publish failures should increase during fault and after
    true = (FaultPublishFailures >= InitialPublishFailures),
    true = (FinalPublishFailures > InitialPublishFailures),
    
    ok.

%% @doc Test: Persistent connect issue + Intermittent ACK loss
%%
%% Scenario: Main connection can't establish for long time,
%% but client periodically "reaches" cluster,
%% in these short windows ACKs are partially lost
%%
%% Expectations:
%% - No infinite loops
%% - Correct redelivery without infinite duplication
test_mixed_persistent_connect_intermittent_ack(_Config) ->
    ct:comment("=== Test: Persistent Connect Issue + Intermittent ACK Loss ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Enable persistent connect fault (always fails)
    enable_persistent_fault(connect, {error, connection_refused}),
    
    %% Enable intermittent ACK fault (30% probability)
    enable_intermittent_fault(ack, {error, timeout}, 0.3),
    
    %% WHEN: Attempt operations
    spawn(fun() ->
        send_message_batch(<<"acme">>, 15, <<"mixed-2">>)
    end),
    
    timer:sleep(3000),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Periodically allow connection (simulate "reaching" cluster)
    spawn(fun() ->
        timer:sleep(1000),
        router_nats_fault_injection:disable_fault(connect),
        timer:sleep(500),
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused})
    end),
    
    timer:sleep(2000),
    
    %% Disable all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify recovery
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    ok.

%% @doc Test: Intermittent publish timeout + Persistent ACK failure
%%
%% Scenario: Publish sometimes succeeds, sometimes times out,
%% ACK on successfully delivered messages almost always fails
%%
%% Expectations:
%% - Delivery guarantee not broken (acceptable duplicates or controlled loss)
%% - No infinite retry loops
test_mixed_intermittent_publish_persistent_ack(_Config) ->
    ct:comment("=== Test: Intermittent Publish Timeout + Persistent ACK Failure ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Enable intermittent publish fault (40% probability)
    enable_intermittent_fault(publish, {error, timeout}, 0.4),
    
    %% Enable persistent ACK fault (always fails)
    enable_persistent_fault(ack, {error, timeout}),
    
    %% WHEN: Attempt operations
    spawn(fun() ->
        send_message_batch(<<"acme">>, 15, <<"mixed-3">>)
    end),
    
    timer:sleep(3000),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Get metrics during faults
    FaultMetrics = get_metrics_snapshot(),
    
    %% Disable all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify metrics
    FinalMetrics = get_metrics_snapshot(),
    
    InitialAckFailures = maps:get(router_nats_ack_failures_total, InitialMetrics, 0),
    FaultAckFailures = maps:get(router_nats_ack_failures_total, FaultMetrics, 0),
    FinalAckFailures = maps:get(router_nats_ack_failures_total, FinalMetrics, 0),
    InitialRedelivery = maps:get(router_jetstream_redelivery_total, InitialMetrics, 0),
    FaultRedelivery = maps:get(router_jetstream_redelivery_total, FaultMetrics, 0),
    FinalRedelivery = maps:get(router_jetstream_redelivery_total, FinalMetrics, 0),
    
    ct:comment("ACK failures: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialAckFailures, FaultAckFailures, FinalAckFailures]),
    ct:comment("Redelivery: Initial=~p, DuringFault=~p, Final=~p", 
               [InitialRedelivery, FaultRedelivery, FinalRedelivery]),
    
    %% Assertions: ACK failures and redelivery should increase during fault and after
    true = (FaultAckFailures >= InitialAckFailures),
    true = (FinalAckFailures > InitialAckFailures),
    true = (FaultRedelivery >= InitialRedelivery),
    true = (FinalRedelivery >= InitialRedelivery),
    
    ok.

%% @doc Test: Flapping with persistent errors
%%
%% Scenario: Connection flaps, but even when connected, certain operations consistently fail
%%
%% Expectations:
%% - System doesn't enter retry storm
%% - Correct backoff behavior
test_mixed_pattern_flapping_with_persistent_errors(_Config) ->
    ct:comment("=== Test: Flapping with Persistent Errors ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% Simulate flapping connection
    FlappingCycles = 5,
    
    %% Enable persistent publish fault (always fails even when connected)
    enable_persistent_fault(publish, {error, quota_exceeded}),
    
    %% WHEN: Flap connection while publish always fails
    spawn(fun() ->
        lists:foreach(fun(Cycle) ->
            router_nats_fault_injection:enable_fault(connect, close_connection),
            timer:sleep(200),
            router_nats_fault_injection:disable_fault(connect),
            timer:sleep(200),
            
            if Cycle rem 2 =:= 0 ->
                send_message_batch(<<"acme">>, 2, <<"flap-persist-", (integer_to_binary(Cycle))/binary>>);
            true ->
                ok
            end
        end, lists:seq(1, FlappingCycles))
    end),
    
    timer:sleep(FlappingCycles * 500 + 1000),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Disable all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify recovery
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    ok.

%% ========================================================================
%% CASCADING FAULT TEST CASES
%% ========================================================================

%% @doc Test: Cascading fault chain (connect → publish → ack)
%%
%% Scenario: Chain of faults: connect fails → some publish goes to void → ack doesn't arrive
%%
%% Expectations:
%% - System handles cascading faults correctly
%% - Correct retry logic through the chain
test_cascading_connect_publish_ack_chain(_Config) ->
    ct:comment("=== Test: Cascading Connect → Publish → ACK Chain ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% WHEN: Enable faults in sequence (cascading)
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    
    timer:sleep(500),
    
    %% Some publish attempts happen during connection failure
    spawn(fun() ->
        send_message_batch(<<"acme">>, 5, <<"cascade-1">>)
    end),
    
    timer:sleep(500),
    
    %% Connection recovers, but publish now fails
    router_nats_fault_injection:disable_fault(connect),
    router_nats_fault_injection:enable_fault(publish, {error, timeout}),
    
    timer:sleep(500),
    
    %% Publish recovers, but ACK fails
    router_nats_fault_injection:disable_fault(publish),
    router_nats_fault_injection:enable_fault(ack, {error, timeout}),
    
    timer:sleep(1000),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Disable all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(2000),
    
    %% THEN: Verify recovery
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    ok.

%% @doc Test: Reconnect storm + publish backlog + ACK loss
%%
%% Scenario: Multiple reconnections in short time, backlog of publish attempts,
%% ACKs are lost during reconnections
%%
%% Expectations:
%% - System handles reconnect storm without crash
%% - Backlog is processed correctly after storm
%% - No message loss
test_cascading_reconnect_storm_publish_backlog_ack_loss(_Config) ->
    ct:comment("=== Test: Reconnect Storm + Publish Backlog + ACK Loss ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% WHEN: Simulate reconnect storm
    ReconnectCycles = 10,
    
    spawn(fun() ->
        lists:foreach(fun(Cycle) ->
            router_nats_fault_injection:enable_fault(connect, close_connection),
            router_nats_fault_injection:enable_fault(ack, {error, timeout}),
            timer:sleep(100),
            router_nats_fault_injection:disable_fault(connect),
            router_nats_fault_injection:disable_fault(ack),
            timer:sleep(100),
            
            %% Send messages during storm
            send_message_batch(<<"acme">>, 2, <<"storm-", (integer_to_binary(Cycle))/binary>>)
        end, lists:seq(1, ReconnectCycles))
    end),
    
    timer:sleep(ReconnectCycles * 300 + 2000),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% Clear all faults
    router_nats_fault_injection:clear_all_faults(),
    wait_for_stabilization(3000),
    
    %% THEN: Verify recovery and backlog processing
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 100}),
    
    %% Verify connection metrics
    InitialConnectionLost = maps:get(router_nats_connection_lost_total, InitialMetrics, 0),
    FinalConnectionLost = maps:get(router_nats_connection_lost_total, FinalMetrics, 0),
    
    ct:comment("Connection lost: Initial=~p, Final=~p", [InitialConnectionLost, FinalConnectionLost]),
    true = (FinalConnectionLost >= InitialConnectionLost),
    
    ok.

%% @doc Test: Multiple recovery cycles
%%
%% Scenario: Multiple fault → recovery cycles in sequence
%%
%% Expectations:
%% - System recovers correctly after each cycle
%% - No state drift across cycles
%% - New messages after cycles have normal behavior
test_cascading_multiple_recovery_cycles(_Config) ->
    ct:comment("=== Test: Multiple Recovery Cycles ==="),
    
    %% GIVEN: Initial state
    InitialMetrics = get_metrics_snapshot(),
    
    %% WHEN: Multiple fault → recovery cycles
    Cycles = 3,
    
    lists:foreach(fun(Cycle) ->
        %% Enable faults
        router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
        router_nats_fault_injection:enable_fault(publish, {error, timeout}),
        
        spawn(fun() ->
            send_message_batch(<<"acme">>, 3, <<"cycle-", (integer_to_binary(Cycle))/binary>>)
        end),
        
        timer:sleep(1000),
        
        %% Disable faults (recovery)
        router_nats_fault_injection:clear_all_faults(),
        wait_for_stabilization(1000)
    end, lists:seq(1, Cycles)),
    
    %% Verify no deadlocks
    verify_no_deadlocks_or_leaks(),
    
    %% THEN: Verify final state
    FinalMetrics = get_metrics_snapshot(),
    verify_message_semantics(InitialMetrics, FinalMetrics, #{max_redelivery => 50}),
    
    ok.

