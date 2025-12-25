%% @doc Error/Resilience Tests for router_decide_consumer
%% 
%% Tests for error handling, resilience to publish failures, and concurrent message processing.
%% Runs on full CI.
%%
%% @test_category fault_injection, medium
-module(router_decide_consumer_faults_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
%% Include state record
-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_reply_publish_error_return/1,
    test_reply_publish_error_exception/1,
    test_reply_publish_error_consumer_resilience/1,
    test_decide_ack_error_with_tenant_validation_fail_same_message/1
]).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, fault_tests}];
groups_for_level(full) ->
    [{group, fault_tests}];
groups_for_level(_) ->
    [].
groups() ->
    [{fault_tests, [sequence], [
        test_reply_publish_error_return,
        test_reply_publish_error_exception,
        test_reply_publish_error_consumer_resilience,
        test_decide_ack_error_with_tenant_validation_fail_same_message
    ]}].

init_per_suite(Config) ->
    meck:new(router_rate_limiter, [passthrough, no_link]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    Base = router_test_bootstrap:init_per_suite(Config, #{
        start => none,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => false,
            nats_mode => mock,
            decide_subject => <<"beamline.router.v1.decide">>,
            tracing_enabled => false
        }
    }),
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_suite_helpers:start_router_suite(),
    Base.

end_per_suite(Config) ->
    Base = router_test_bootstrap:end_per_suite(Config, #{
        start => none,
        stop => router_suite
    }),
    catch meck:unload(router_rate_limiter),
    Base.

init_per_testcase(TestCase, Config) ->
    Base = router_test_bootstrap:init_per_testcase(TestCase, Config, #{
        clear_faults => true
    }),
    router_metrics:ensure(),
    case catch meck:validate(router_nats) of
        true -> ok;
        false -> ok = router_mock_helpers:setup_router_nats_mock();
        {'EXIT', _} -> ok = router_mock_helpers:setup_router_nats_mock()
    end,
    Base.

end_per_testcase(_TestCase, Config) ->
    Base = router_test_bootstrap:end_per_testcase(_TestCase, Config, #{cleanup_mocks => false}),
    catch meck:unload(router_logger),
    catch meck:unload(router_policy_store),
    Base.

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

setup_basic_state() ->
    #state{
        connection = undefined,
        decide_subject = <<"beamline.router.v1.decide">>,
        js_durable_group = <<"test-group">>,
        publication_monitors = #{}
    }.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_reply_publish_error_return(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-publish-error">>,
        <<"trace_id">> => <<"tr-publish-error">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"msg-publish-error">>,
    
    meck:new(router_logger, [passthrough]),
    LogCalls = log_calls,
    _ = router_ets_helpers:ensure_named_ets_table(LogCalls, [set, public]),
    ets:delete_all_objects(LogCalls),
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, timeout} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
    timer:sleep(300),
    
    ?assert(is_process_alive(ConsumerPid)),
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    %% Reset router_nats mock for next test (don't unload - it's suite-level)
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    ets:delete(LogCalls),
    ok.

test_reply_publish_error_exception(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-publish-exception">>,
        <<"trace_id">> => <<"tr-publish-exception">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId = <<"msg-publish-exception">>,
    
    meck:new(router_logger, [passthrough]),
    LogCalls = log_calls,
    _ = router_ets_helpers:ensure_named_ets_table(LogCalls, [set, public]),
    ets:delete_all_objects(LogCalls),
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> erlang:error(connection_lost) end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    try
        State = setup_basic_state(),
        router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
        timer:sleep(300)
    catch
        _:Exception ->
            ct:log("Exception caught: ~p", [Exception])
    end,
    
    ?assert(is_process_alive(ConsumerPid)),
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_policy_store),
    meck:unload(router_logger),
    ets:delete(LogCalls),
    ok.

test_reply_publish_error_consumer_resilience(_Config) ->
    Request1 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-resilience-1">>,
        <<"trace_id">> => <<"tr-resilience-1">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>},
        <<"policy_id">> => <<"default">>
    },
    Request2 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-resilience-2">>,
        <<"trace_id">> => <<"tr-resilience-2">>,
        <<"tenant_id">> => <<"default_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>},
        <<"policy_id">> => <<"default">>
    },
    Request1Json = jsx:encode(Request1),
    Request2Json = jsx:encode(Request2),
    Subject = <<"beamline.router.v1.decide">>,
    MsgId1 = <<"msg-resilience-1">>,
    MsgId2 = <<"msg-resilience-2">>,
    
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),

    PublishCallCount = publish_calls,
    _ = router_ets_helpers:ensure_named_ets_table(PublishCallCount, [set, public]),
    ets:delete_all_objects(PublishCallCount),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        Count = case router_ets_helpers:ets_lookup(publish_calls, count) of
            [{count, C}] -> C + 1;
            [] -> 1
        end,
        ets:insert(publish_calls, {count, Count}),
        case Count of
            1 -> {error, timeout};
            _ -> ok
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    ok = application:set_env(beamline_router, nats_mode, mock),

    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    State1 = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, Request1Json, #{}, MsgId1}, State1),
    timer:sleep(200),
    ?assert(is_process_alive(ConsumerPid)),
    
    State2 = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, Request2Json, #{}, MsgId2}, State2),
    timer:sleep(200),
    
    ?assert(is_process_alive(ConsumerPid)),
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    timer:sleep(200),
    [{count, FinalCount}] = router_ets_helpers:ets_lookup(publish_calls, count),
    ?assert(FinalCount >= 2),
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    meck:unload(router_policy_store),
    ets:delete(publish_calls),
    ok.

test_decide_ack_error_with_tenant_validation_fail_same_message(_Config) ->
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-ack-tenant">>,
        <<"trace_id">> => <<"tr-ack-tenant">>,
        <<"tenant_id">> => <<"valid_tenant">>,
        <<"task">> => #{<<"type">> => <<"text.generate">>, <<"payload_ref">> => <<"s3://bucket/key">>},
        <<"policy_id">> => <<"default">>
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-ack-tenant">>,
    Subject = <<"beamline.router.v1.decide">>,
    
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    
    AckCalls = ack_calls,
    _ = router_ets_helpers:ensure_named_ets_table(AckCalls, [set, public]),
    ets:delete_all_objects(AckCalls),
    ets:insert(AckCalls, {count, 0}),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        [{count, C}] = router_ets_helpers:ets_lookup(AckCalls, count),
        ets:insert(AckCalls, {count, C + 1}),
        case C of
            0 -> {error, timeout};
            _ -> ok
        end
    end),
    meck:expect(router_nats, nak_message, fun(_MsgIdBin) -> ok end),
    meck:expect(router_nats, request, fun(_Subj, _Pay, _Time) -> {ok, <<>>} end),
    ok = application:set_env(beamline_router, nats_mode, mock),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    
    ConsumerPid = whereis(router_decide_consumer),
    ?assert(is_pid(ConsumerPid)),
    
    State = setup_basic_state(),
    router_decide_consumer:handle_info({nats_message, Subject, RequestJson, #{}, MsgId}, State),
    timer:sleep(300),
    
    ?assert(is_process_alive(ConsumerPid)),
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    meck:unload(router_policy_store),
    ets:delete(AckCalls),
    ok.
