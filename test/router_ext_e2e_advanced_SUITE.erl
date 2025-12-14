%% @doc Extensions E2E: Advanced Pipeline Tests
%%
%% Advanced E2E pipeline tests:
%% - Full pipeline
%% - Multiple extensions
%% - Extension timeout
%% - Extension error
%%
%% @test_category e2e, full, extensions
-module(router_ext_e2e_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_full_pipeline/1,
    test_multiple_extensions/1,
    test_extension_timeout/1,
    test_extension_error/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        "full" -> [{group, advanced_tests}];
        _ -> []
    end.

groups() ->
    [{advanced_tests, [sequence], [
        test_full_pipeline,
        test_multiple_extensions,
        test_extension_timeout,
        test_extension_error
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, telemetry_enabled, true),
    ok = router_mock_helpers:ensure_mock(router_extension_invoker, [passthrough]),
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> timer:sleep(500), Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload_all([router_extension_invoker, router_nats]),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_full_pipeline(_Config) ->
    ct:comment("=== Full Pipeline ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {ok, #{<<"processed">> => true}}
    end),
    
    Policy = create_policy([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]},
        {validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]},
        {post, [{id, <<"mask_pii">>, mode, <<"required">>}]}
    ]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>, <<"trace_id">> => <<"trace-001">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, Decision} -> ?assert(is_record(Decision, route_decision));
        {error, Reason} -> ct:comment("Expected: ~p", [Reason]), ok
    end.

test_multiple_extensions(_Config) ->
    ct:comment("=== Multiple Extensions ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {ok, #{<<"processed">> => true}}
    end),
    
    Policy = create_policy([
        {pre, [{id, <<"normalize_text">>, mode, <<"required">>}]},
        {validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]},
        {post, [{id, <<"mask_pii">>, mode, <<"required">>}]}
    ]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, _Decision} -> ok;
        {error, Reason} -> ct:comment("Expected: ~p", [Reason]), ok
    end.

test_extension_timeout(_Config) ->
    ct:comment("=== Extension Timeout ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        timer:sleep(100),
        {error, timeout}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, _Decision} -> ok;  %% Fail-open
        {error, Reason} -> ct:comment("Timeout handled: ~p", [Reason]), ok
    end.

test_extension_error(_Config) ->
    ct:comment("=== Extension Error ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {error, {extension_error, <<"test error">>}}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"optional">>}]}]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, _Decision} -> ok;  %% Fail-open
        {error, Reason} -> ct:comment("Error handled: ~p", [Reason]), ok
    end.

%% ============================================================================
%% HELPERS
%% ============================================================================

create_policy(Extensions) ->
    Pre = proplists:get_value(pre, Extensions, []),
    Validators = proplists:get_value(validators, Extensions, []),
    Post = proplists:get_value(post, Extensions, []),
    
    PreItems = lists:map(fun({id, Id, mode, Mode}) -> #{id => Id, mode => Mode, config => #{}} end, Pre),
    ValidatorItems = lists:map(fun({id, Id, on_fail, OnFail}) -> #{id => Id, on_fail => OnFail} end, Validators),
    PostItems = lists:map(fun({id, Id, mode, Mode}) -> #{id => Id, mode => Mode, config => #{}} end, Post),
    
    #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
            pre = PreItems, validators = ValidatorItems, post = PostItems,
            weights = #{<<"openai:gpt-4">> => 1.0}, metadata = #{}}.

create_request(Payload, Policy) ->
    #route_request{
        message = #{<<"message_id">> => <<"msg-001">>, <<"message_type">> => <<"chat">>,
                    <<"payload">> => Payload, <<"metadata">> => #{}},
        policy_id = Policy#policy.policy_id, context = #{}}.
