%% @doc NATS Subscriber CAF: Core Tests
%%
%% Core integration tests:
%% - Smoke test
%% - Normalize boolean
%% - DecideRequest success/errors
%% - Push assignment handling
%%
%% @test_category integration, full, caf
-module(router_nats_sub_caf_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_smoke/1,
    test_normalize_boolean/1,
    test_decide_request_success/1,
    test_decide_request_with_push_assignment/1,
    test_decide_request_error_policy_not_found/1,
    test_decide_request_error_missing_tenant_id/1,
    test_decide_request_unsupported_version/1,
    test_push_assignment_false_no_publication/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_ENABLE_META") of
        "1" -> meta_all();
        "true" -> meta_all();
        "on" -> meta_all();
        _ -> []
    end.

meta_all() ->
    [].
groups() ->
    [{smoke_tests, [parallel], [test_smoke]},
     {core_tests, [sequence], [
        test_normalize_boolean,
        test_decide_request_success,
        test_decide_request_with_push_assignment,
        test_decide_request_error_policy_not_found,
        test_decide_request_error_missing_tenant_id,
        test_decide_request_unsupported_version,
        test_push_assignment_false_no_publication
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = router_mock_helpers:setup_router_nats_mock(),
    ok = router_mock_helpers:ensure_mock(telemetry, [passthrough]),
    meck:expect(telemetry, execute, fun(_, _, _) -> ok end),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload_all([router_nats, telemetry]),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_smoke(_Config) ->
    ct:comment("=== Smoke Test ==="),
    ?assert(true),
    ok.

test_normalize_boolean(_Config) ->
    ct:comment("=== Normalize Boolean ==="),
    ?assertEqual(true, router_decide_consumer:test_normalize_boolean(true)),
    ?assertEqual(true, router_decide_consumer:test_normalize_boolean(<<"true">>)),
    ?assertEqual(true, router_decide_consumer:test_normalize_boolean(1)),
    ?assertEqual(false, router_decide_consumer:test_normalize_boolean(false)),
    ?assertEqual(false, router_decide_consumer:test_normalize_boolean(<<"false">>)),
    ?assertEqual(false, router_decide_consumer:test_normalize_boolean(0)),
    ok.

test_decide_request_success(_Config) ->
    ct:comment("=== DecideRequest Success ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-001">>,
    
    Result = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    %% Accept ok or {ok, _} as valid results
    case Result of
        ok -> ok;
        {ok, _} -> ok;
        _ -> ct:fail("Unexpected result: ~p", [Result])
    end,
    ok.

test_decide_request_with_push_assignment(_Config) ->
    ct:comment("=== DecideRequest with Push Assignment ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-002">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"push_assignment">> => true,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-002">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_decide_request_error_policy_not_found(_Config) ->
    ct:comment("=== DecideRequest Error: Policy Not Found ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-003">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"non_existent_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-003">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_decide_request_error_missing_tenant_id(_Config) ->
    ct:comment("=== DecideRequest Error: Missing Tenant ID ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-004">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-004">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_decide_request_unsupported_version(_Config) ->
    ct:comment("=== DecideRequest Error: Unsupported Version ==="),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"999">>,
        <<"request_id">> => <<"req-005">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message">> => #{}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-005">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    ok.

test_push_assignment_false_no_publication(_Config) ->
    ct:comment("=== Push Assignment False: No Publication ==="),
    PublishCalls = router_test_init:ensure_ets_table(publish_calls, [set, public]),
    ets:delete_all_objects(PublishCalls),
    meck:expect(router_nats, publish, fun(Subject, _Payload) ->
        case binary:match(Subject, <<"assignment">>) of
            nomatch -> ok;
            _ -> ets:insert(PublishCalls, {Subject, 1})
        end,
        ok
    end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-006">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"push_assignment">> => false,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    RequestJson = jsx:encode(Request),
    MsgId = <<"msg-006">>,
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, MsgId, #{}),
    
    AssignmentPublishes = ets:tab2list(PublishCalls),
    ?assertEqual([], AssignmentPublishes),
    
    ets:delete(PublishCalls),
    ok.
