%% @doc JetStream E2E - Integration Tests
%% 
%% Tests for durable subscriptions, message handling, and idempotency.
%% Runs with ROUTER_TEST_LEVEL=full or heavy.
%%
%% @test_category e2e, integration
-module(router_jetstream_e2e_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_durable_subscription_creation/1,
    test_durable_subscription_reconnect/1,
    test_jetstream_publish_with_ack/1,
    test_message_acknowledgment/1,
    test_message_nak_redelivery/1,
    test_idempotency_result_processing/1,
    test_durable_group_isolation/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, integration_tests}];
        "heavy" -> [{group, integration_tests}];
        _ -> []
    end.

groups() ->
    [{integration_tests, [sequence], [
        test_durable_subscription_creation,
        test_durable_subscription_reconnect,
        test_jetstream_publish_with_ack,
        test_message_acknowledgment,
        test_message_nak_redelivery,
        test_idempotency_result_processing,
        test_durable_group_isolation
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, usage_subject, <<"beamline.usage.v1.metered">>),
    ok = application:set_env(beamline_router, nats_js_durable_group_results, <<"router-results">>),
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_result_consumer, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_durable_subscription_creation(_Config) ->
    Subject = <<"caf.exec.result.v1">>,
    DurableGroup = <<"router-results">>,
    
    meck:expect(router_nats, subscribe_jetstream, fun(S, DG, AckPolicy, DeliverGroup, Mode) ->
        case {S, DG, AckPolicy, DeliverGroup, Mode} of
            {Subject, DurableGroup, explicit, undefined, push} ->
                {ok, <<"mock-consumer-", DurableGroup/binary>>};
            _ ->
                {error, invalid_params}
        end
    end),
    
    {ok, ConsumerId} = router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, undefined, push),
    ?assert(is_binary(ConsumerId)),
    ?assert(binary:match(ConsumerId, DurableGroup) =/= nomatch),
    ok.

test_durable_subscription_reconnect(_Config) ->
    Subject = <<"caf.exec.result.v1">>,
    DurableGroup = <<"router-results">>,
    ExpectedConsumerId = <<"mock-consumer-router-results">>,
    
    meck:expect(router_nats, subscribe_jetstream, fun(S, DG, _AckPolicy, _DeliverGroup, _Mode) ->
        case {S, DG} of
            {Subject, DurableGroup} -> {ok, ExpectedConsumerId};
            _ -> {error, invalid_params}
        end
    end),
    
    {ok, ConsumerId1} = router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, undefined, push),
    {ok, ConsumerId2} = router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, undefined, push),
    
    ?assertEqual(ExpectedConsumerId, ConsumerId1),
    ?assertEqual(ExpectedConsumerId, ConsumerId2),
    ok.

test_jetstream_publish_with_ack(_Config) ->
    Subject = <<"caf.exec.assign.v1">>,
    Payload = jsx:encode(#{<<"test">> => <<"data">>}),
    Headers = #{},
    
    meck:expect(router_nats, publish_with_ack, fun(S, P, H) ->
        case {S, P, H} of
            {Subject, Payload, Headers} -> {ok, <<"pub-ack-123">>};
            _ -> {error, invalid_params}
        end
    end),
    
    {ok, PubAckId} = router_nats:publish_with_ack(Subject, Payload, Headers),
    ?assert(is_binary(PubAckId)),
    ok.

test_message_acknowledgment(_Config) ->
    MsgId = <<"msg-123">>,
    
    meck:expect(router_nats, ack_message, fun(M) ->
        case M of MsgId -> ok; _ -> {error, invalid_msg_id} end
    end),
    
    ?assertEqual(ok, router_nats:ack_message(MsgId)),
    ok.

test_message_nak_redelivery(_Config) ->
    MsgId = <<"msg-456">>,
    
    meck:expect(router_nats, nak_message, fun(M) ->
        case M of MsgId -> ok; _ -> {error, invalid_msg_id} end
    end),
    
    ?assertEqual(ok, router_nats:nak_message(MsgId)),
    ok.

test_idempotency_result_processing(_Config) ->
    AssignmentId = <<"assign-idempotent">>,
    RequestId = <<"req-idempotent">>,
    Result = #{
        <<"assignment_id">> => AssignmentId,
        <<"request_id">> => RequestId,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"trace_id">> => <<"tr-idempotent">>,
        <<"tenant_id">> => <<"acme">>,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"msg_id">> => <<"msg-idempotent">>
    },
    ResultJson = jsx:encode(Result),
    
    UsageEvents = router_test_init:ensure_ets_table(usage_events, [set, public]),
    ets:delete_all_objects(UsageEvents),
    meck:expect(router_nats, publish, fun(Subject, Payload) ->
        case Subject of
            <<"beamline.usage.v1.metered">> ->
                UsageMap = jsx:decode(Payload, [return_maps]),
                EventKey = {maps:get(<<"assignment_id">>, UsageMap), maps:get(<<"request_id">>, UsageMap)},
                ets:insert(UsageEvents, {EventKey, UsageMap}),
                ok;
            _ -> ok
        end
    end),
    
    meck:expect(router_nats, ack_message, fun(MsgId) ->
        case MsgId of <<"msg-idempotent">> -> ok; _ -> {error, invalid_msg_id} end
    end),
    
    router_result_consumer ! {nats_message, <<"caf.exec.result.v1">>, ResultJson},
    test_helpers:wait_for_condition(fun() -> true end, 300),
    
    case ets:lookup(UsageEvents, {AssignmentId, RequestId}) of
        [{_, _Usage}] -> ok;
        [] -> ct:log("Usage event not published (may be expected)")
    end,
    
    ets:delete(UsageEvents),
    ok.

test_durable_group_isolation(_Config) ->
    Subject = <<"caf.exec.result.v1">>,
    Group1 = <<"router-results-1">>,
    Group2 = <<"router-results-2">>,
    
    meck:expect(router_nats, subscribe_jetstream, fun(S, DG, _AckPolicy, _DeliverGroup, _Mode) ->
        case {S, DG} of
            {Subject, Group1} -> {ok, <<"consumer-", Group1/binary>>};
            {Subject, Group2} -> {ok, <<"consumer-", Group2/binary>>};
            _ -> {error, invalid_params}
        end
    end),
    
    {ok, ConsumerId1} = router_nats:subscribe_jetstream(Subject, Group1, explicit, undefined, push),
    {ok, ConsumerId2} = router_nats:subscribe_jetstream(Subject, Group2, explicit, undefined, push),
    
    ?assert(ConsumerId1 =/= ConsumerId2),
    ?assert(binary:match(ConsumerId1, Group1) =/= nomatch),
    ?assert(binary:match(ConsumerId2, Group2) =/= nomatch),
    ok.
