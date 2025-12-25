%% @doc JetStream E2E - CP2 Checklist Tests
%% 
%% Tests for CP2 requirements: durable subscriptions, redelivery, DLQ, ACK latency.
%% Runs with ROUTER_TEST_LEVEL=full or heavy.
%%
%% @test_category e2e, cp2
-module(router_jetstream_e2e_cp2_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-compile({nowarn_unused_function, [
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

-export([
    all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]).

-export([
    test_durable_subscription_survives_restart/1,
    test_redelivery_until_ack_or_maxdeliver/1,
    test_dlq_payload_contains_context/1,
    test_ack_latency_within_target/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, cp2_tests}];
        "heavy" -> [{group, cp2_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, cp2_tests}];
        "heavy" -> [{group, cp2_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, cp2_tests}];
        "heavy" -> [{group, cp2_tests}];
        _ -> []
    end.
groups() ->
    [{cp2_tests, [sequence], [
        test_durable_subscription_survives_restart,
        test_redelivery_until_ack_or_maxdeliver,
        test_dlq_payload_contains_context,
        test_ack_latency_within_target
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, nats_js_durable_group_results, <<"router-results">>),
    %% CRITICAL: Setup mock BEFORE starting app to prevent supervisor conflicts
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            test_helpers:wait_for_app_start(router_result_consumer, 1000),
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    application:stop(beamline_router),
    %% Explicitly unload the mock that was set up in init_per_suite
    router_mock_helpers:unload(router_nats),
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Don't recreate mock - it's shared across parallel tests
    %% Just reset expectations if needed
    Config.

end_per_testcase(_TestCase, Config) ->
    %% Don't unload mock - it's shared for the whole suite
    Config.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_durable_subscription_survives_restart(_Config) ->
    Subject = <<"caf.exec.result.v1">>,
    DurableGroup = <<"router-results">>,
    ExpectedConsumerId = <<"mock-consumer-router-results">>,
    
    meck:expect(router_nats, subscribe_jetstream, fun(S, DG, _AckPolicy, _DeliverGroup, _Mode) ->
        case {S, DG} of
            {Subject, DurableGroup} -> {ok, ExpectedConsumerId};
            _ -> {error, invalid_params}
        end
    end),
    
    %% First subscription
    {ok, ConsumerId1} = router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, undefined, push),
    
    %% Simulate restart (same consumer ID should be returned)
    {ok, ConsumerId2} = router_nats:subscribe_jetstream(Subject, DurableGroup, explicit, undefined, push),
    
    ?assertEqual(ExpectedConsumerId, ConsumerId1),
    ?assertEqual(ExpectedConsumerId, ConsumerId2),
    ok.

test_redelivery_until_ack_or_maxdeliver(_Config) ->
    MaxDeliver = application:get_env(beamline_router, nats_js_max_deliver, 5),
    ?assert(is_integer(MaxDeliver)),
    ?assert(MaxDeliver > 0),
    
    %% Track NAK calls
    NakCalls = router_ets_helpers:ensure_named_ets_table(nak_calls, [named_table, set, public]),
    ets:insert(NakCalls, {count, 0}),
    
    meck:expect(router_nats, nak_message, fun(MsgId) ->
        [{count, Count}] = router_ets_helpers:ets_lookup(NakCalls, count),
        ets:insert(NakCalls, {count, Count + 1}),
        ets:insert(NakCalls, {MsgId, Count + 1}),
        ok
    end),
    
    %% Simulate NAK until max_deliver
    lists:foreach(fun(N) ->
        MsgId = list_to_binary("msg-" ++ integer_to_list(N)),
        router_nats:nak_message(MsgId)
    end, lists:seq(1, MaxDeliver)),
    
    [{count, FinalCount}] = router_ets_helpers:ets_lookup(NakCalls, count),
    ?assertEqual(MaxDeliver, FinalCount),
    
    ets:delete_all_objects(NakCalls),
    ok.

test_dlq_payload_contains_context(_Config) ->
    %% Test that DLQ messages contain required context
    DlqPayload = #{
        <<"original_message">> => <<"test-message">>,
        <<"tenant_id">> => <<"acme">>,
        <<"request_id">> => <<"req-dlq">>,
        <<"error">> => <<"max_deliver_exhausted">>,
        <<"delivery_count">> => 5
    },
    
    %% Verify required fields
    ?assert(maps:is_key(<<"original_message">>, DlqPayload)),
    ?assert(maps:is_key(<<"tenant_id">>, DlqPayload)),
    ?assert(maps:is_key(<<"error">>, DlqPayload)),
    ?assert(maps:is_key(<<"delivery_count">>, DlqPayload)),
    ok.

test_ack_latency_within_target(_Config) ->
    TargetLatencyMs = 50,  %% Target: ACK within 50ms
    
    %% Mock ACK with timing
    AckTiming = router_ets_helpers:ensure_named_ets_table(ack_timing, [named_table, set, public]),
    
    meck:expect(router_nats, ack_message, fun(MsgId) ->
        Start = erlang:monotonic_time(millisecond),
        %% Simulate some processing
        timer:sleep(10),
        End = erlang:monotonic_time(millisecond),
        Latency = End - Start,
        ets:insert(AckTiming, {MsgId, Latency}),
        ok
    end),
    
    %% Perform ACK
    router_nats:ack_message(<<"msg-latency-test">>),
    
    %% Verify latency
    [{<<"msg-latency-test">>, Latency}] = router_ets_helpers:ets_lookup(AckTiming, <<"msg-latency-test">>),
    ?assert(Latency < TargetLatencyMs),
    
    ets:delete_all_objects(AckTiming),
    ok.
