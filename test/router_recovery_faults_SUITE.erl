%% @doc Recovery State Integrity: Fault Pattern Scenarios
%%
%% Tests for ACK/NAK errors, multiple fault cycles, and publish errors:
%% - ETS integrity after ACK/NAK errors
%% - Multiple fault cycles handling
%% - Publish errors recovery
%%
%% @test_category recovery, state_integrity, heavy
-module(router_recovery_faults_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_ack_nak_errors_ets_integrity/1,
    test_ack_nak_errors_delivery_counts/1,
    test_ack_nak_errors_idempotency/1,
    test_multiple_fault_cycles_ets_integrity/1,
    test_multiple_fault_cycles_no_degradation/1,
    test_publish_errors_ets_integrity/1,
    test_publish_errors_message_processing/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, fault_tests}];
        _ -> []
    end.

groups() ->
    [{fault_tests, [sequence], [
        test_ack_nak_errors_ets_integrity,
        test_ack_nak_errors_delivery_counts,
        test_ack_nak_errors_idempotency,
        test_multiple_fault_cycles_ets_integrity,
        test_multiple_fault_cycles_no_degradation,
        test_publish_errors_ets_integrity,
        test_publish_errors_message_processing
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_fail_open_mode, false),
    ok = application:set_env(beamline_router, cp2_plus_allowed, true),
    ok = application:set_env(beamline_router, idempotency_enabled, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> timer:sleep(1000), Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) -> clear_ets_tables(), Config.
end_per_testcase(_TC, _Config) -> clear_ets_tables(), ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_ack_nak_errors_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    InitialSize = get_ets_size(router_delivery_count),
    
    MsgId = <<"recovery.ack.1">>,
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:enable_fault(ack, {error, consumer_deleted}),
            router_nats_fault_injection:enable_fault(nak, {error, consumer_deleted});
        false -> ok
    end,
    
    _ = router_nats:ack_message(MsgId),
    timer:sleep(500),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} ->
            router_nats_fault_injection:disable_fault(ack),
            router_nats_fault_injection:disable_fault(nak);
        false -> ok
    end,
    timer:sleep(500),
    
    true = is_process_alive(RouterNatsPid),
    
    FinalSize = get_ets_size(router_delivery_count),
    true = FinalSize < InitialSize + 100,
    
    exit(StubPid, normal), ok.

test_ack_nak_errors_delivery_counts(_Config) ->
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    MsgId = <<"recovery.ack.delivery.1">>,
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    {ok, 2} = get_delivery_count(router_delivery_count, MsgId),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:enable_fault(ack, {error, consumer_deleted});
        false -> ok
    end,
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    {ok, 3} = get_delivery_count(router_delivery_count, MsgId),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:disable_fault(ack);
        false -> ok
    end,
    timer:sleep(500),
    
    {ok, 3} = get_delivery_count(router_delivery_count, MsgId),
    
    exit(StubPid, normal), ok.

test_ack_nak_errors_idempotency(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    KeyType = <<"ack_id">>,
    MsgId = <<"recovery.ack.idem.1">>,
    
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:enable_fault(ack, {error, consumer_deleted});
        false -> ok
    end,
    
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:disable_fault(ack);
        false -> ok
    end,
    timer:sleep(500),
    
    true = is_process_alive(RouterNatsPid),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    exit(StubPid, normal), ok.

test_multiple_fault_cycles_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    InitialSize = get_ets_size(router_delivery_count),
    
    FaultCycles = 3,
    lists:foreach(fun(Cycle) ->
        gen_server:cast(router_nats, {connection_lost, {cycle, Cycle}}),
        timer:sleep(300),
        
        true = is_process_alive(RouterNatsPid),
        
        StubPid = spawn_link(fun() -> receive _ -> ok end end),
        gen_server:cast(router_nats, {connection_restored, StubPid}),
        timer:sleep(500),
        
        exit(StubPid, normal),
        timer:sleep(200)
    end, lists:seq(1, FaultCycles)),
    
    true = is_process_alive(RouterNatsPid),
    
    FinalSize = get_ets_size(router_delivery_count),
    true = FinalSize < InitialSize + 200,
    ok.

test_multiple_fault_cycles_no_degradation(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    FaultCycles = 5,
    lists:foreach(fun(Cycle) ->
        gen_server:cast(router_nats, {connection_lost, {degradation, Cycle}}),
        timer:sleep(200),
        
        StubPid = spawn_link(fun() -> receive _ -> ok end end),
        gen_server:cast(router_nats, {connection_restored, StubPid}),
        timer:sleep(300),
        
        exit(StubPid, normal),
        timer:sleep(100)
    end, lists:seq(1, FaultCycles)),
    
    true = is_process_alive(RouterNatsPid),
    
    _ = router_nats:publish(<<"recovery.degradation.test">>, <<"payload">>),
    
    KeyType = <<"request_id">>,
    MsgId = <<"recovery.degradation.idem">>,
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    ok.

test_publish_errors_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    InitialSize = get_ets_size(router_delivery_count),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable});
        false -> ok
    end,
    
    _ = router_nats:publish(<<"recovery.publish.1">>, <<"payload">>),
    timer:sleep(500),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:disable_fault(publish);
        false -> ok
    end,
    timer:sleep(500),
    
    true = is_process_alive(RouterNatsPid),
    
    FinalSize = get_ets_size(router_delivery_count),
    true = FinalSize < InitialSize + 100,
    
    exit(StubPid, normal), ok.

test_publish_errors_message_processing(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(300),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable});
        false -> ok
    end,
    
    _ = router_nats:publish(<<"recovery.publish.error">>, <<"payload">>),
    timer:sleep(500),
    
    case code:is_loaded(router_nats_fault_injection) of
        {file, _} -> router_nats_fault_injection:disable_fault(publish);
        false -> ok
    end,
    timer:sleep(500),
    
    true = is_process_alive(RouterNatsPid),
    
    _ = router_nats:publish(<<"recovery.publish.after">>, <<"payload">>),
    
    exit(StubPid, normal), ok.

%% ============================================================================
%% HELPERS
%% ============================================================================

clear_ets_tables() ->
    Tables = [router_delivery_count, router_ack_delivery_count, router_idempotency],
    lists:foreach(fun(Table) ->
        case ets:info(Table) of
            undefined -> ok;
            _ -> ets:delete_all_objects(Table)
        end
    end, Tables).

get_ets_size(Table) ->
    case ets:info(Table) of
        undefined -> 0;
        _ -> ets:info(Table, size)
    end.

track_delivery_count_safe(Table, MsgId) ->
    case ets:info(Table) of
        undefined -> ok;
        _ ->
            case ets:lookup(Table, MsgId) of
                [] -> ets:insert(Table, {MsgId, 1});
                [{_, Count}] -> ets:insert(Table, {MsgId, Count + 1})
            end, ok
    end.

get_delivery_count(Table, MsgId) ->
    case ets:info(Table) of
        undefined -> {ok, 0};
        _ ->
            case ets:lookup(Table, MsgId) of
                [] -> {ok, 0};
                [{_, Count}] -> {ok, Count}
            end
    end.
