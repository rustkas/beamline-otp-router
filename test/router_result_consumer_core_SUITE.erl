%% @doc Core Unit Tests for router_result_consumer
%% 
%% Fast, stable, deterministic tests for basic result processing.
%% Runs on every CI push.
%%
%% @test_category unit, fast
-module(router_result_consumer_core_SUITE).
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
    test_result_parsing/1,
    test_result_correlation/1,
    test_usage_emission/1,
    test_result_validation/1,
    test_malformed_json/1,
    test_missing_fields/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [{group, unit_tests}].

groups() ->
    [{unit_tests, [sequence], [
        test_result_parsing,
        test_result_correlation,
        test_usage_emission,
        test_result_validation,
        test_malformed_json,
        test_missing_fields
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
    %% Ensure no stale mocks leak between tests
    router_mock_helpers:unload_all(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    router_mock_helpers:unload_all(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_result_parsing(_Config) ->
    Result = #{
        <<"assignment_id">> => <<"assign-123">>,
        <<"request_id">> => <<"req-456">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"trace_id">> => <<"00000000000000000000000000000001">>,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    ResultJson = jsx:encode(Result),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    ok.

test_result_correlation(_Config) ->
    AssignmentId = <<"assign-correlate">>,
    RequestId = <<"req-correlate">>,
    Result = #{
        <<"assignment_id">> => AssignmentId,
        <<"request_id">> => RequestId,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>
    },
    ResultJson = jsx:encode(Result),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, Payload) ->
        UsageMap = jsx:decode(Payload, [return_maps]),
        ?assertEqual(AssignmentId, maps:get(<<"assignment_id">>, UsageMap)),
        ?assertEqual(RequestId, maps:get(<<"request_id">>, UsageMap)),
        ok
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    ok.

test_usage_emission(_Config) ->
    Result = #{
        <<"assignment_id">> => <<"assign-usage">>,
        <<"request_id">> => <<"req-usage">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"job">> => #{<<"type">> => <<"text.generate">>},
        <<"latency_ms">> => 850,
        <<"cost">> => 0.012,
        <<"tenant_id">> => <<"acme">>,
        <<"version">> => <<"1">>
    },
    ResultJson = jsx:encode(Result),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(Subject, _Payload) ->
        case Subject of
            <<"beamline.usage.v1.metered">> -> ok;
            _ -> ok
        end
    end),
    meck:expect(router_nats, subscribe_jetstream, fun(_Subject, _Durable, _AckPolicy, _DeliverGroup, _Mode) -> {ok, <<"consumer-1">>} end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:new(router_policy_store, [passthrough]),
    meck:expect(router_policy_store, load_policy, fun(_Tenant, _Policy) -> {error, not_found} end),
    meck:new(router_idempotency, [passthrough]),
    meck:expect(router_idempotency, check_and_mark, fun(_KeyType, _MessageId, _Data) -> {ok, not_seen} end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    meck:unload(router_nats),
    meck:unload(router_idempotency),
    meck:unload(router_policy_store),
    ok.

test_result_validation(_Config) ->
    Result1 = #{
        <<"status">> => <<"success">>,
        <<"version">> => <<"1">>
    },
    Result1Json = jsx:encode(Result1),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, Result1Json}, #{}),
    test_helpers:wait_for_no_meck_call(router_nats, publish, '_', 200),
    meck:unload(router_nats),
    ok.

test_malformed_json(_Config) ->
    MalformedJson = <<"{invalid json}">>,
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, MalformedJson}, #{}),
    test_helpers:wait_for_no_meck_call(router_nats, publish, '_', 200),
    meck:unload(router_nats),
    ok.

test_missing_fields(_Config) ->
    Result = #{
        <<"assignment_id">> => <<"assign-missing">>,
        <<"request_id">> => <<"req-missing">>,
        <<"version">> => <<"1">>
    },
    ResultJson = jsx:encode(Result),
    
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, ResultJson}, #{}),
    test_helpers:wait_for_no_meck_call(router_nats, publish, '_', 200),
    meck:unload(router_nats),
    ok.
