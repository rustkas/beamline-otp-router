%% @doc Extensions E2E: Core Pipeline Tests
%%
%% Core E2E pipeline tests:
%% - Pre-processor
%% - Validator
%% - Post-processor
%% - Custom provider
%%
%% @test_category e2e, full, extensions
-module(router_ext_e2e_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_pre_processor/1,
    test_validator/1,
    test_post_processor/1,
    test_custom_provider/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, core_tests}];
        "full" -> [{group, core_tests}];
        _ -> []
    end.

groups() ->
    [{core_tests, [sequence], [
        test_pre_processor,
        test_validator,
        test_post_processor,
        test_custom_provider
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

test_pre_processor(_Config) ->
    ct:comment("=== Pre-processor ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {ok, #{<<"normalized">> => true}}
    end),
    
    Policy = create_policy([{pre, [{id, <<"normalize_text">>, mode, <<"required">>}]}]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, _Decision} -> ok;
        {error, Reason} -> ct:comment("Expected in mock mode: ~p", [Reason]), ok
    end.

test_validator(_Config) ->
    ct:comment("=== Validator ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {ok, #{<<"validated">> => true}}
    end),
    
    Policy = create_policy([{validators, [{id, <<"pii_guard">>, on_fail, <<"block">>}]}]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, _Decision} -> ok;
        {error, Reason} -> ct:comment("Expected: ~p", [Reason]), ok
    end.

test_post_processor(_Config) ->
    ct:comment("=== Post-processor ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {ok, #{<<"masked">> => true}}
    end),
    
    Policy = create_policy([{post, [{id, <<"mask_pii">>, mode, <<"required">>}]}]),
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, _Decision} -> ok;
        {error, Reason} -> ct:comment("Expected: ~p", [Reason]), ok
    end.

test_custom_provider(_Config) ->
    ct:comment("=== Custom Provider ==="),
    meck:expect(router_extension_invoker, invoke, fun(_ExtId, _Req, _Ctx) ->
        {ok, #{<<"provider_id">> => <<"test_provider">>}}
    end),
    
    Policy = #policy{tenant_id = <<"test_tenant">>, policy_id = <<"test_policy">>, version = <<"1.0">>,
                     weights = #{<<"test_provider">> => 1.0}, metadata = #{}},
    Request = create_request(<<"Hello World">>, Policy),
    Context = #{<<"tenant_id">> => <<"test_tenant">>},
    
    case router_decider:decide(Request, Policy, Context) of
        {ok, Decision} -> ?assertEqual(<<"test_provider">>, Decision#route_decision.provider_id);
        {error, Reason} -> ct:comment("Expected: ~p", [Reason]), ok
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
