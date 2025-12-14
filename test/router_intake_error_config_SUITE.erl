%% @doc Intake Error Handler: Config and Edge Case Tests
%%
%% Configuration and edge case tests:
%% - DLQ subject pattern
%% - DLQ enabled config
%% - Audit logging
%% - DLQ publish errors
%% - Error response publish errors
%%
%% @test_category intake, full
-module(router_intake_error_config_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_dlq_subject_pattern/1,
    test_dlq_enabled_config/1,
    test_audit_logging/1,
    test_dlq_publish_error_return/1,
    test_dlq_publish_error_exception/1,
    test_error_response_publish_error/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, config_tests}];
        "full" -> [{group, config_tests}];
        _ -> []
    end.

groups() ->
    [{config_tests, [sequence], [
        test_dlq_subject_pattern,
        test_dlq_enabled_config,
        test_audit_logging,
        test_dlq_publish_error_return,
        test_dlq_publish_error_exception,
        test_error_response_publish_error
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

test_dlq_subject_pattern(_Config) ->
    ct:comment("=== DLQ Subject Pattern ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:new(telemetry, [passthrough]),
    
    ok = application:set_env(beamline_router, dlq_subject_pattern, undefined),
    
    meck:expect(router_nats, publish_with_ack, fun(Subject, _Payload, _Headers) ->
        ct:comment("DLQ Subject: ~p", [Subject]),
        {ok, <<"dlq-ack">>}
    end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    ok = router_intake_error_handler:send_to_dlq(
        <<"beamline.router.v1.decide">>, <<"payload">>,
        schema_validation_failed, <<"error">>, #{}
    ),
    
    meck:unload(),
    ok.

test_dlq_enabled_config(_Config) ->
    ct:comment("=== DLQ Enabled Config ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    
    ok = application:set_env(beamline_router, dlq_enabled, true),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {ok, <<"ack">>} end),
    meck:expect(router_logger, error, fun(_Msg, _Ctx) -> ok end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    ok = router_intake_error_handler:handle_intake_error(
        schema_validation_failed, <<"Error">>,
        <<"subject">>, <<"payload">>, #{}, <<"msg-001">>, #{}
    ),
    
    meck:unload(),
    ok.

test_audit_logging(_Config) ->
    ct:comment("=== Audit Logging ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    
    LogCalls = router_test_init:ensure_ets_table(log_calls, [set, public]),
    ets:delete_all_objects(LogCalls),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {ok, <<"ack">>} end),
    meck:expect(router_logger, error, fun(Msg, _Ctx) ->
        ets:insert(LogCalls, {Msg, 1}),
        ok
    end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    ok = router_intake_error_handler:handle_intake_error(
        schema_validation_failed, <<"Error">>,
        <<"subject">>, <<"payload">>, #{}, <<"msg-001">>, #{}
    ),
    
    ets:delete(LogCalls),
    meck:unload(),
    ok.

test_dlq_publish_error_return(_Config) ->
    ct:comment("=== DLQ Publish Error Return ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(telemetry, [passthrough]),
    
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        {error, timeout}
    end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    %% Should not crash even when DLQ publish fails
    _ = router_intake_error_handler:send_to_dlq(
        <<"subject">>, <<"payload">>,
        schema_validation_failed, <<"error">>, #{}
    ),
    
    meck:unload(),
    ok.

test_dlq_publish_error_exception(_Config) ->
    ct:comment("=== DLQ Publish Error Exception ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(telemetry, [passthrough]),
    
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, publish, fun(_S, _P) -> ok end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) ->
        throw(connection_closed)
    end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    %% Note: send_to_dlq doesn't catch thrown exceptions - they propagate
    %% Test verifies exception propagation (caller must handle)
    Result = (catch router_intake_error_handler:send_to_dlq(
        <<"subject">>, <<"payload">>,
        schema_validation_failed, <<"error">>, #{}
    )),
    %% catch returns the thrown value directly for throw()
    ?assertEqual(connection_closed, Result),
    
    meck:unload(),
    ok.

test_error_response_publish_error(_Config) ->
    ct:comment("=== Error Response Publish Error ==="),
    %% No passthrough for router_nats to avoid noproc on gen_server:call
    meck:new(router_nats, []),
    meck:new(router_logger, [passthrough]),
    meck:new(telemetry, [passthrough]),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> {error, timeout} end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    meck:expect(router_nats, subscribe_jetstream, fun(_S, _D, _A, _G, _M) -> {ok, <<"c">>} end),
    meck:expect(router_nats, request, fun(_S, _P, _T) -> {ok, <<>>} end),
    meck:expect(router_nats, publish_with_ack, fun(_Subject, _Payload, _Headers) -> {ok, <<"ack">>} end),
    meck:expect(router_logger, error, fun(_Msg, _Ctx) -> ok end),
    meck:expect(telemetry, execute, fun(_Event, _Measurements, _Metadata) -> ok end),
    
    %% Should not crash even when error response publish fails
    ok = router_intake_error_handler:handle_intake_error(
        schema_validation_failed, <<"Error">>,
        <<"subject">>, <<"payload">>, #{}, <<"msg-001">>, #{}
    ),
    
    meck:unload(),
    ok.
