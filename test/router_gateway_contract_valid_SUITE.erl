%% @doc Gateway Contract Smoke: Validation Tests
%%
%% Validation and error tests:
%% - Invalid request missing fields
%% - Invalid request wrong version
%% - Tenant rejected
%% - Internal router error
%%
%% @test_category contract, full, smoke
-module(router_gateway_contract_valid_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_invalid_request_missing_fields/1,
    test_invalid_request_wrong_version/1,
    test_tenant_rejected/1,
    test_internal_router_error/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, validation_tests}];
        "full" -> [{group, validation_tests}];
        _ -> []
    end.

groups() ->
    %% Run sequentially to avoid mock race conditions
    [{validation_tests, [sequence], [
        test_invalid_request_missing_fields,
        test_invalid_request_wrong_version,
        test_tenant_rejected,
        test_internal_router_error
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    router_mock_helpers:cleanup_and_verify(),
    ok.

init_per_testcase(_TC, Config) ->
    %% Ensure mock is active for each test
    %% Re-setup to handle case where previous test corrupted it
    ok = router_mock_helpers:setup_router_nats_mock(#{
        publish => fun(_Subject, _Payload) -> ok end,
        nak_message => fun(_MsgId) -> ok end,
        ack_message => fun(_MsgId) -> ok end
    }),
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_invalid_request_missing_fields(_Config) ->
    ct:comment("=== Invalid Request: Missing Fields ==="),
    
    %% Missing tenant_id
    Request1 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"message">> => #{<<"type">> => <<"chat">>}
    },
    _ = router_decide_consumer:handle_decide_message(jsx:encode(Request1), #{}, <<"msg-001">>, #{}),
    
    %% Missing message
    Request2 = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-002">>,
        <<"tenant_id">> => <<"test_tenant">>
    },
    _ = router_decide_consumer:handle_decide_message(jsx:encode(Request2), #{}, <<"msg-002">>, #{}),
    
    %% Missing request_id
    Request3 = #{
        <<"version">> => <<"1">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message">> => #{<<"type">> => <<"chat">>}
    },
    _ = router_decide_consumer:handle_decide_message(jsx:encode(Request3), #{}, <<"msg-003">>, #{}),
    ok.

test_invalid_request_wrong_version(_Config) ->
    ct:comment("=== Invalid Request: Wrong Version ==="),
    
    Request = #{
        <<"version">> => <<"999">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"message">> => #{<<"type">> => <<"chat">>}
    },
    _ = router_decide_consumer:handle_decide_message(jsx:encode(Request), #{}, <<"msg-001">>, #{}),
    ok.

test_tenant_rejected(_Config) ->
    ct:comment("=== Tenant Rejected ==="),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"blocked_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    _ = router_decide_consumer:handle_decide_message(jsx:encode(Request), #{}, <<"msg-001">>, #{}),
    ok.

test_internal_router_error(_Config) ->
    ct:comment("=== Internal Router Error ==="),
    
    meck:expect(router_nats, publish, fun(_Subject, _Payload) ->
        {error, internal_error}
    end),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    _ = router_decide_consumer:handle_decide_message(jsx:encode(Request), #{}, <<"msg-001">>, #{}),
    ok.
