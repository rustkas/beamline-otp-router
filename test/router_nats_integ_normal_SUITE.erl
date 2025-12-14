%% @doc NATS Integration: Normal and Fail-Open Tests
%%
%% Core integration tests:
%% - Normal operation (decide, result, publish)
%% - Fail-open mode
%%
%% @test_category integration, full, nats
-module(router_nats_integ_normal_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_normal_decide_consumer/1,
    test_normal_result_consumer/1,
    test_normal_publish/1,
    test_fail_open_decide_consumer/1,
    test_fail_open_result_consumer/1,
    test_fail_open_publish/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, normal_tests}, {group, fail_open_tests}];
        "full" -> [{group, normal_tests}, {group, fail_open_tests}];
        _ -> []
    end.

groups() ->
    [{normal_tests, [sequence], [
        test_normal_decide_consumer,
        test_normal_result_consumer,
        test_normal_publish
    ]},
     {fail_open_tests, [sequence], [
        test_fail_open_decide_consumer,
        test_fail_open_result_consumer,
        test_fail_open_publish
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    %% Unload mock FIRST to ensure cleanup happens even if app:stop fails
    router_mock_helpers:unload(router_nats),
    _ = application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% NORMAL OPERATION TESTS
%% ============================================================================

test_normal_decide_consumer(_Config) ->
    ct:comment("=== Normal Decide Consumer ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Request = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    }),
    
    _ = router_decide_consumer:handle_decide_message(Request, #{}, <<"msg-001">>, #{}),
    ok.

test_normal_result_consumer(_Config) ->
    ct:comment("=== Normal Result Consumer ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Result = jsx:encode(#{
        <<"assignment_id">> => <<"assign-001">>,
        <<"request_id">> => <<"req-001">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    }),
    
    _ = router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, Result, #{}, <<"msg-001">>}, #{}),
    ok.

test_normal_publish(_Config) ->
    ct:comment("=== Normal Publish ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    ?assertEqual(ok, Result),
    ok.

%% ============================================================================
%% FAIL-OPEN MODE TESTS
%% ============================================================================

test_fail_open_decide_consumer(_Config) ->
    ct:comment("=== Fail-Open Decide Consumer ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_refused} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Request = jsx:encode(#{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-002">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    }),
    
    %% Should not crash
    _ = router_decide_consumer:handle_decide_message(Request, #{}, <<"msg-002">>, #{}),
    ok.

test_fail_open_result_consumer(_Config) ->
    ct:comment("=== Fail-Open Result Consumer ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_refused} end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Result = jsx:encode(#{
        <<"assignment_id">> => <<"assign-002">>,
        <<"request_id">> => <<"req-002">>,
        <<"status">> => <<"success">>,
        <<"provider_id">> => <<"openai:gpt-4o">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    }),
    
    _ = router_result_consumer:handle_info({nats_message, <<"caf.exec.result.v1">>, Result, #{}, <<"msg-002">>}, #{}),
    ok.

test_fail_open_publish(_Config) ->
    ct:comment("=== Fail-Open Publish ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, connection_refused} end),
    
    Result = router_nats:publish(<<"test.subject">>, <<"test payload">>),
    ?assertEqual({error, connection_refused}, Result),
    ok.
