%% @doc Intake Error Handler: Core Tests
%%
%% Core error handling tests:
%% - Schema validation error
%% - Version unsupported error
%% - DLQ publication
%% - Error response building
%%
%% @test_category intake, full
-module(router_intake_error_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_schema_validation_failed/1,
    test_version_unsupported/1,
    test_send_to_dlq/1,
    test_build_error_response/1,
    test_nats_message_fate_ack/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, core_tests}];
        "full" -> [{group, core_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, core_tests}];
        "full" -> [{group, core_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, core_tests}];
        "full" -> [{group, core_tests}];
        _ -> []
    end.
groups() ->
    [{core_tests, [sequence], [
        test_schema_validation_failed,
        test_version_unsupported,
        test_send_to_dlq,
        test_build_error_response,
        test_nats_message_fate_ack
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, dlq_enabled, true),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) ->
    catch meck:unload(),
    Config.

end_per_testcase(_TC, _Config) ->
    catch meck:unload(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_schema_validation_failed(_Config) ->
    ct:comment("=== Schema Validation Failed ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {ok, <<"ack">>} end),
    meck:expect(router_logger, error, fun(_Msg, _Ctx) -> ok end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    ok = router_intake_error_handler:handle_intake_error(
        schema_validation_failed, <<"Schema validation failed">>,
        <<"beamline.router.v1.decide">>, <<"payload">>, #{},
        <<"msg-001">>, #{<<"request_id">> => <<"req-001">>}
    ),
    
    meck:unload(),
    ok.

test_version_unsupported(_Config) ->
    ct:comment("=== Version Unsupported ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {ok, <<"ack">>} end),
    meck:expect(router_logger, error, fun(_Msg, _Ctx) -> ok end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    ok = router_intake_error_handler:handle_intake_error(
        version_unsupported, <<"Version unsupported">>,
        <<"beamline.router.v1.decide">>, <<"payload">>, #{},
        <<"msg-002">>, #{<<"request_id">> => <<"req-002">>}
    ),
    
    meck:unload(),
    ok.

test_send_to_dlq(_Config) ->
    ct:comment("=== Send to DLQ ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(telemetry, [passthrough]),
    
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(Subject, _Payload, _Headers) ->
        ct:comment("DLQ Subject: ~p", [Subject]),
        {ok, <<"dlq-ack">>}
    end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    ok = router_intake_error_handler:send_to_dlq(
        <<"beamline.router.v1.decide">>, <<"payload">>,
        schema_validation_failed, <<"error msg">>, #{}
    ),
    
    meck:unload(),
    ok.

test_build_error_response(_Config) ->
    ct:comment("=== Build Error Response ==="),
    Context = #{<<"request_id">> => <<"req-123">>, <<"trace_id">> => <<"trace-456">>},
    
    Response = router_intake_error_handler:build_error_response(
        schema_validation_failed, <<"Schema validation failed">>, Context
    ),
    
    %% Response structure: #{ok => false, error => #{code, message, intake_error_code}, context => ...}
    ?assertEqual(false, maps:get(<<"ok">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(<<"invalid_request">>, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Schema validation failed">>, maps:get(<<"message">>, Error)),
    ?assertEqual(<<"SCHEMA_VALIDATION_FAILED">>, maps:get(<<"intake_error_code">>, Error)),
    
    ResponseContext = maps:get(<<"context">>, Response),
    ?assertEqual(<<"req-123">>, maps:get(<<"request_id">>, ResponseContext)),
    ok.

test_nats_message_fate_ack(_Config) ->
    ct:comment("=== NATS Message Fate ACK ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, publish_with_ack, fun(_S, _P, _H) -> {error, connection_closed} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    
    meck:expect(router_nats, ack_message, fun(MsgId) ->
        ?assertEqual(<<"msg-ack">>, MsgId),
        ok
    end),
    
    %% Use schema_validation_failed error code which triggers ACK
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    ok = router_intake_error_handler:handle_nats_message_fate(schema_validation_failed, <<"msg-ack">>, Context),
    
    meck:unload(),
    ok.
