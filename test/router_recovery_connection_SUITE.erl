%% @doc Recovery State Integrity: Connection Scenarios
%%
%% Tests for connection loss/recovery scenarios:
%% - ETS integrity after connection loss
%% - Delivery count accuracy after connection loss
%% - Idempotency consistency after connection loss
%% - NATS restart scenarios
%%
%% @test_category recovery, state_integrity, heavy
-module(router_recovery_connection_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_connection_loss_ets_integrity/1,
    test_connection_loss_delivery_counts/1,
    test_connection_loss_idempotency/1,
    test_nats_restart_ets_integrity/1,
    test_nats_restart_delivery_counts/1,
    test_nats_restart_idempotency/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, connection_tests}];
        _ -> []
    end.

groups() ->
    [{connection_tests, [sequence], [
        test_connection_loss_ets_integrity,
        test_connection_loss_delivery_counts,
        test_connection_loss_idempotency,
        test_nats_restart_ets_integrity,
        test_nats_restart_delivery_counts,
        test_nats_restart_idempotency
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_reconnect_attempts, 5),
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

test_connection_loss_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    
    Messages = [{<<"recovery.msg.1">>, <<"payload1">>}, {<<"recovery.msg.2">>, <<"payload2">>}],
    lists:foreach(fun({Subject, Payload}) ->
        _ = router_nats:publish(Subject, Payload), timer:sleep(50)
    end, Messages),
    
    InitialSize = get_ets_size(router_delivery_count),
    
    gen_server:cast(router_nats, {connection_lost, test_connection_loss}),
    timer:sleep(500),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    true = is_process_alive(RouterNatsPid),
    
    FinalSize = get_ets_size(router_delivery_count),
    true = FinalSize < 1000,
    true = FinalSize < InitialSize + 100,
    
    exit(StubPid, normal), ok.

test_connection_loss_delivery_counts(_Config) ->
    MsgId = <<"recovery.delivery.1">>,
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    {ok, 2} = get_delivery_count(router_delivery_count, MsgId),
    
    gen_server:cast(router_nats, {connection_lost, test_delivery}),
    timer:sleep(500),
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    {ok, 3} = get_delivery_count(router_delivery_count, MsgId),
    
    exit(StubPid, normal), ok.

test_connection_loss_idempotency(_Config) ->
    KeyType = <<"assignment_id">>,
    MsgId = <<"recovery.idem.1">>,
    
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    gen_server:cast(router_nats, {connection_lost, test_idem}),
    timer:sleep(500),
    
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    exit(StubPid, normal), ok.

test_nats_restart_ets_integrity(_Config) ->
    RouterNatsPid = whereis(router_nats),
    InitialSize = get_ets_size(router_delivery_count),
    
    gen_server:cast(router_nats, {connection_lost, nats_restart}),
    timer:sleep(500),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    true = is_process_alive(RouterNatsPid),
    
    FinalSize = get_ets_size(router_delivery_count),
    true = FinalSize < InitialSize + 100,
    
    exit(StubPid, normal), ok.

test_nats_restart_delivery_counts(_Config) ->
    MsgId = <<"recovery.nats.delivery.1">>,
    
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    ok = track_delivery_count_safe(router_delivery_count, MsgId),
    
    {ok, 2} = get_delivery_count(router_delivery_count, MsgId),
    
    gen_server:cast(router_nats, {connection_lost, nats_restart}),
    timer:sleep(500),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    {ok, 2} = get_delivery_count(router_delivery_count, MsgId),
    
    exit(StubPid, normal), ok.

test_nats_restart_idempotency(_Config) ->
    KeyType = <<"assignment_id">>,
    MsgId = <<"recovery.nats.idem.1">>,
    
    {ok, not_seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
    gen_server:cast(router_nats, {connection_lost, nats_restart}),
    timer:sleep(500),
    
    StubPid = spawn_link(fun() -> receive _ -> ok end end),
    gen_server:cast(router_nats, {connection_restored, StubPid}),
    timer:sleep(1000),
    
    {ok, seen} = router_idempotency:check_and_mark(KeyType, MsgId, #{}),
    
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
