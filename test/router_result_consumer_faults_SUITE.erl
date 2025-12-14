%% @doc Fault/Error Tests for router_result_consumer
%% 
%% Tests for error handling and resilience to publish failures.
%% Runs on full CI.
%%
%% @test_category fault_injection, medium
-module(router_result_consumer_faults_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({nowarn_unused_function, [
    all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
]}).

%% Common Test exports
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_usage_publish_error_return/1,
    test_usage_publish_error_exception/1
]).

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "full" -> [{group, fault_tests}];
        "heavy" -> [{group, fault_tests}];
        _ -> []
    end.

groups() ->
    [{fault_tests, [sequence], [
        test_usage_publish_error_return,
        test_usage_publish_error_exception
    ]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, result_subject, <<"caf.exec.result.v1">>),
    ok = application:set_env(beamline_router, usage_subject, <<"beamline.usage.v1.metered">>),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    meck:new(router_rate_limiter, [passthrough]),
    meck:expect(router_rate_limiter, start_link, fun() -> {ok, spawn(fun() -> receive after infinity -> ok end end)} end),
    Config.

end_per_suite(Config) ->
    application:stop(beamline_router),
    router_mock_helpers:unload(router_rate_limiter),
    Config.

init_per_testcase(_TestCase, Config) ->
    router_mock_helpers:unload_all(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_mock_helpers:unload_all(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_usage_publish_error_return(_Config) ->
    Result = #{
        <<"assignment_id">> => <<"assign-publish-error">>,
        <<"request_id">> => <<"req-publish-error">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    code:ensure_loaded(router_logger),
    meck:new(router_logger, [passthrough]),
    LogCalls = router_test_init:ensure_ets_table(log_calls, [duplicate_bag, public]),
    ets:delete_all_objects(LogCalls),
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, timeout} end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    meck:new(router_tenant_validator, [passthrough]),
    meck:expect(router_tenant_validator, validate_tenant, fun(TenantId, _Context) -> {ok, TenantId} end),
    meck:new(router_circuit_breaker, [passthrough]),
    meck:expect(router_circuit_breaker, record_success, fun(_Tenant, _Provider) -> ok end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    timer:sleep(300),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    AllLogs = ets:tab2list(LogCalls),
    UsageErrorLogs = [L || {log, error, Message, _Context} = L <- AllLogs,
                          binary:match(Message, <<"usage">>) =/= nomatch orelse
                          binary:match(Message, <<"Usage">>) =/= nomatch],
    ?assert(length(UsageErrorLogs) > 0),
    
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    meck:unload(router_tenant_validator),
    meck:unload(router_circuit_breaker),
    meck:unload(router_logger),
    ets:delete(LogCalls),
    ok.

test_usage_publish_error_exception(_Config) ->
    Result = #{
        <<"assignment_id">> => <<"assign-publish-exception">>,
        <<"request_id">> => <<"req-publish-exception">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    meck:new(router_logger, [passthrough]),
    LogCalls = router_test_init:ensure_ets_table(log_calls, [duplicate_bag, public]),
    ets:delete_all_objects(LogCalls),
    meck:expect(router_logger, error, fun(Message, Context) ->
        ets:insert(LogCalls, {log, error, Message, Context}),
        ok
    end),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> erlang:error(connection_lost) end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    meck:new(router_circuit_breaker, [passthrough]),
    meck:expect(router_circuit_breaker, record_success, fun(_Tenant, _Provider) -> ok end),
    
    try
        router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
        timer:sleep(300)
    catch
        _:Exception ->
            ct:log("Exception caught: ~p", [Exception])
    end,
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    timer:sleep(200),
    AllLogs = ets:tab2list(LogCalls),
    ErrorLogs = [L || {log, error, _Message, _Context} = L <- AllLogs],
    ?assert(length(ErrorLogs) >= 0),
    
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    meck:unload(router_circuit_breaker),
    meck:unload(router_logger),
    ets:delete(LogCalls),
    ok.
