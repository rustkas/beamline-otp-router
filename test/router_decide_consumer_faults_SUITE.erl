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
-include("../include/beamline_router.hrl").

%% Include state record
-record(state, {
    connection :: pid() | undefined,
    decide_subject :: binary(),
    js_durable_group :: binary(),
    publication_monitors :: #{pid() => map()}
}).

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2
]}).

%% Common Test exports
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

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
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, fault_tests}];
        "heavy" -> [{group, fault_tests}];
        _ -> []  %% Skip in fast mode
    end.

groups() ->
    [{fault_tests, [sequence], [
        test_reply_publish_error_return,
        test_reply_publish_error_exception,
        test_reply_publish_error_consumer_resilience,
        test_decide_ack_error_with_tenant_validation_fail_same_message
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, decide_subject, <<"beamline.router.v1.decide">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    %% CRITICAL: Mock router_nats BEFORE starting app to avoid undef on start_link/0
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    ok = router_suite_helpers:start_router_suite(),
    Config.

end_per_suite(_Config) ->
    router_suite_helpers:stop_router_suite(),
    catch meck:unload(router_rate_limiter),
    ok.

init_per_testcase(_TestCase, Config) ->
    router_suite_helpers:ensure_no_faults(),
    %% NOTE: router_nats is fully mocked, don't check for live process
    %% ok = router_test_utils:ensure_router_nats_alive(),
    router_metrics:ensure(),
    %% Ensure router_nats mock is setup (may have been unloaded by previous test)
    case meck:validate(router_nats) of
        true -> ok;
        false -> ok = router_mock_helpers:setup_router_nats_mock()
    end,
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Only unload test-specific mocks, NOT router_nats which is suite-level
    catch meck:unload(router_logger),
    catch meck:unload(router_policy_store),
    ok.

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
    LogCalls = router_test_init:ensure_ets_table(log_calls, [set, named_table, public]),
    ets:delete_all_objects(LogCalls),
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    %% Ensure router_nats mock is properly reset (may be mocked from init_per_suite)
    case meck:validate(router_nats) of
        true -> 
            %% Already mocked, just reset expectations
            meck:reset(router_nats);
        false ->
            %% Not mocked or unloaded, create fresh mock (no passthrough to avoid gen_server:call)
            meck:new(router_nats, [])
    end,
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
    meck:reset(router_nats),
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
    LogCalls = router_test_init:ensure_ets_table(log_calls, [set, named_table, public]),
    ets:delete_all_objects(LogCalls),
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    %% Ensure router_nats mock is properly reset (may be mocked from init_per_suite)
    case meck:validate(router_nats) of
        true -> meck:reset(router_nats);
        false -> meck:new(router_nats, [])
    end,
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
    
    meck:reset(router_nats),
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
    
    %% Ensure router_nats mock is properly reset (may be mocked from init_per_suite)
    case meck:validate(router_nats) of
        true -> meck:reset(router_nats);
        false -> meck:new(router_nats, [])
    end,
    %% NOTE: Do NOT call ensure_router_nats_started() with mock - start_link not stubbed
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),

    PublishCallCount = router_test_init:ensure_ets_table(publish_calls, [set, named_table, public]),
    ets:delete_all_objects(PublishCallCount),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        Count = case ets:lookup(publish_calls, count) of
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
    [{count, FinalCount}] = ets:lookup(publish_calls, count),
    ?assert(FinalCount >= 2),
    test_helpers:wait_for_meck_call(router_nats, ack_message, '_', 1000),
    
    meck:reset(router_nats),
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
    
    %% Ensure router_nats mock is properly reset (may be mocked from init_per_suite)
    case meck:validate(router_nats) of
        true -> meck:reset(router_nats);
        false -> meck:new(router_nats, [])
    end,
    %% NOTE: Do NOT call ensure_router_nats_started() with mock - start_link not stubbed
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    
    AckCalls = router_test_init:ensure_ets_table(ack_calls, [set, named_table, public]),
    ets:delete_all_objects(AckCalls),
    ets:insert(AckCalls, {count, 0}),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, ack_message, fun(_MsgIdBin) ->
        [{count, C}] = ets:lookup(AckCalls, count),
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
    
    meck:reset(router_nats),
    meck:unload(router_policy_store),
    ets:delete(AckCalls),
    ok.
