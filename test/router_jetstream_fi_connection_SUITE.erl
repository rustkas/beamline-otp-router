%% @doc JetStream Fault Injection - Connection Tests
%% 
%% Tests for NATS connection loss/recovery and consumer reconnection.
%% Runs only with ROUTER_TEST_LEVEL=heavy.
%%
%% @test_category fault_injection, heavy, slow
-module(router_jetstream_fi_connection_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_nats_connection_loss_recovery/1,
    test_jetstream_consumer_reconnection/1,
    test_stream_availability_after_recovery/1,
    test_ets_state_preservation_during_nats_restart/1
]).

suite() -> [{timetrap, {minutes, 10}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, connection_tests}];
        _ -> []
    end.

groups() ->
    [{connection_tests, [sequence], [
        test_nats_connection_loss_recovery,
        test_jetstream_consumer_reconnection,
        test_stream_availability_after_recovery,
        test_ets_state_preservation_during_nats_restart
    ]}].

init_per_suite(Config) ->
    router_jetstream_fi_helper:init_common_suite(Config).

end_per_suite(Config) ->
    router_jetstream_fi_helper:end_common_suite(Config).

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_nats_connection_loss_recovery(_Config) ->
    Result = #{
        <<"assignment_id">> => <<"assign-conn-loss">>,
        <<"request_id">> => <<"req-conn-loss">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => <<"acme">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    MsgId = <<"msg-conn-loss">>,
    
    ConnState = router_test_init:ensure_ets_table(connection_state, [named_table, set, public]),
    ets:insert(ConnState, {connected, false}),
    ets:insert(ConnState, {connection_count, 0}),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        [{connected, Connected}] = ets:lookup(ConnState, connected),
        case Connected of
            false -> {error, connection_lost};
            true -> ok
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        [{connection_count, Count}] = ets:lookup(ConnState, connection_count),
        NewCount = Count + 1,
        ets:insert(ConnState, {connection_count, NewCount}),
        case NewCount =< 1 of
            true ->
                ets:insert(ConnState, {connected, false}),
                {error, connection_lost};
            false ->
                ets:insert(ConnState, {connected, true}),
                {ok, <<"consumer-reconnected">>}
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        [{connected, Connected}] = ets:lookup(ConnState, connected),
        case Connected of false -> {error, connection_lost}; true -> ok end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) ->
        [{connected, Connected}] = ets:lookup(ConnState, connected),
        case Connected of false -> {error, connection_lost}; true -> ok end
    end),
    
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) -> router_nats:ack_message(Id) end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) -> router_nats:nak_message(Id) end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(_TenantId, _Context) -> {ok, <<"acme">>} end),
    
    ConsumerPid = whereis(router_result_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson, #{}, MsgId}, #{}),
    timer:sleep(1000),
    
    ?assert(is_process_alive(ConsumerPid)),
    
    ets:insert(ConnState, {connected, true}),
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson, #{}, MsgId}, #{}),
    timer:sleep(1000),
    
    ?assert(is_process_alive(ConsumerPid)),
    [{connection_count, FinalCount}] = ets:lookup(ConnState, connection_count),
    ?assert(FinalCount >= 1),
    
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    ets:delete_all_objects(ConnState),
    ok.

test_jetstream_consumer_reconnection(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-consumer-reconnect">>,
        <<"trace_id">> => <<"tr-consumer-reconnect">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-consumer-reconnect">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    SubState = router_test_init:ensure_ets_table(subscription_state, [named_table, set, public]),
    ets:insert(SubState, {subscribed, false}),
    ets:insert(SubState, {subscription_count, 0}),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        [{subscription_count, Count}] = ets:lookup(SubState, subscription_count),
        NewCount = Count + 1,
        ets:insert(SubState, {subscription_count, NewCount}),
        case NewCount =< 1 of
            true ->
                ets:insert(SubState, {subscribed, false}),
                {error, consumer_not_found};
            false ->
                ets:insert(SubState, {subscribed, true}),
                {ok, <<"consumer-reconnected">>}
        end
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    
    meck:new(router_jetstream, [passthrough]),
    meck:expect(router_jetstream, ack, fun(#{id := Id}) -> router_nats:ack_message(Id) end),
    meck:expect(router_jetstream, nak, fun(#{id := Id}, _Reason) -> router_nats:nak_message(Id) end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(_TenantId, _Context) -> {ok, <<"acme">>} end),
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(1000),
    
    ?assert(is_process_alive(ConsumerPid)),
    
    ets:insert(SubState, {subscribed, true}),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, #{}),
    timer:sleep(1000),
    
    ?assert(is_process_alive(ConsumerPid)),
    [{subscription_count, FinalCount}] = ets:lookup(SubState, subscription_count),
    ?assert(FinalCount >= 1),
    
    meck:unload(router_nats),
    meck:unload(router_jetstream),
    meck:unload(router_tenant_validator),
    meck:unload(router_policy_store),
    ets:delete_all_objects(SubState),
    ok.

test_stream_availability_after_recovery(_Config) ->
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) ->
        {ok, <<"consumer-1">>}
    end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    ConsumerPid = whereis(router_result_consumer),
    ?assert(is_pid(ConsumerPid)),
    ?assert(is_process_alive(ConsumerPid)),
    
    meck:unload(router_nats),
    ok.

test_ets_state_preservation_during_nats_restart(_Config) ->
    TestTable = router_test_init:ensure_ets_table(router_jetstream_fi_test_ets, [named_table, public, set]),
    
    ets:insert(TestTable, {test_key, test_value}),
    
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(1000),
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(1000),
    
    [{test_key, test_value}] = ets:lookup(TestTable, test_key),
    
    ets:delete_all_objects(TestTable),
    ok.
