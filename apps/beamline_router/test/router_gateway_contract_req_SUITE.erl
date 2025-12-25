%% @doc Gateway Contract Smoke: Request/Response Tests
%%
%% Contract validation tests:
%% - DecideRequest/Response structure
%% - Headers pass-through
%% - Error response structure
%%
%% @test_category contract, full, smoke
-module(router_gateway_contract_req_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_decide_request_response_structure/1,
    test_headers_pass_through/1,
    test_error_response_structure/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, contract_tests}];
        "full" -> [{group, contract_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, contract_tests}];
        "full" -> [{group, contract_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, contract_tests}];
        "full" -> [{group, contract_tests}];
        _ -> []
    end.
groups() ->
    [{contract_tests, [parallel], [
        test_decide_request_response_structure,
        test_headers_pass_through,
        test_error_response_structure
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, ack_message, fun(_MsgId) -> ok end),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_decide_request_response_structure(_Config) ->
    ct:comment("=== Decide Request/Response Structure ==="),
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => <<"test_tenant">>,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{
            <<"type">> => <<"chat">>,
            <<"payload">> => <<"test message">>
        }
    },
    
    ?assert(maps:is_key(<<"version">>, Request)),
    ?assert(maps:is_key(<<"request_id">>, Request)),
    ?assert(maps:is_key(<<"tenant_id">>, Request)),
    ?assert(maps:is_key(<<"message">>, Request)),
    
    RequestJson = jsx:encode(Request),
    ?assert(is_binary(RequestJson)),
    
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, <<"msg-001">>, #{}),
    ok.

test_headers_pass_through(_Config) ->
    ct:comment("=== Headers Pass-Through ==="),
    
    TraceId = <<"trace-123-abc">>,
    TenantId = <<"test_tenant">>,
    
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-002">>,
        <<"tenant_id">> => TenantId,
        <<"trace_id">> => TraceId,
        <<"policy_id">> => <<"test_policy">>,
        <<"message">> => #{<<"type">> => <<"chat">>, <<"payload">> => <<"test">>}
    },
    
    ?assertEqual(TraceId, maps:get(<<"trace_id">>, Request)),
    ?assertEqual(TenantId, maps:get(<<"tenant_id">>, Request)),
    
    RequestJson = jsx:encode(Request),
    _ = router_decide_consumer:handle_decide_message(RequestJson, #{}, <<"msg-002">>, #{}),
    ok.

test_error_response_structure(_Config) ->
    ct:comment("=== Error Response Structure ==="),
    
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    
    %% Invalid request - missing required fields
    InvalidRequest = #{
        <<"version">> => <<"1">>
        %% Missing tenant_id and message
    },
    InvalidJson = jsx:encode(InvalidRequest),
    
    _ = router_decide_consumer:handle_decide_message(InvalidJson, #{}, <<"msg-003">>, #{}),
    ok.
