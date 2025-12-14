%% @doc Router → Provider Integration Tests
%%
%% Tests integration between Router (Erlang) and Provider services.
%% These tests verify provider selection and routing logic.
%%
%% @test_category integration, provider
-module(router_provider_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

-export([groups/0, suite/0]).
-compile([export_all, nowarn_export_all]).

suite() ->
    [
        {timetrap, {minutes, 2}}
    ].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(fast) ->
    [];
groups_for_level(_) -> %% full, heavy
    [{group, provider_integration_tests}].

groups() ->
    [
        {provider_integration_tests, [], [
            test_provider_selection,
            test_provider_fallback,
            test_provider_sticky_session
        ]}
    ].

init_per_suite(Config) ->
    %% Start Router application
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, sticky_store_enabled, true),
    ok = router_suite_helpers:start_router_suite(),
    
    %% Create test policy
    TenantId = <<"test_tenant_provider">>,
    PolicyId = <<"test_policy_provider">>,
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = #{
            <<"openai">> => 0.7,
            <<"anthropic">> => 0.3
        },
        fallback = <<"openai">>,
        sticky = #{
            <<"enabled">> => true,
            <<"type">> => <<"header">>,
            <<"params">> => #{<<"key">> => <<"session_id">>}
        },
        metadata = #{}
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    [{tenant_id, TenantId}, {policy_id, PolicyId} | Config].

end_per_suite(Config) ->
    %% Cleanup
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    router_policy_store:delete_policy(TenantId, PolicyId),
    router_suite_helpers:stop_router_suite(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: Provider selection based on weights
test_provider_selection(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Create route request
    Request = #route_request{
        message = #{
            <<"tenant_id">> => TenantId,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = PolicyId,
        context = #{}
    },
    
    %% Route request
    case router_core:route(Request, #{}) of
        {ok, Decision} ->
            ?assert(is_record(Decision, route_decision)),
            ?assertNotEqual(undefined, Decision#route_decision.provider_id),
            ?assert(lists:member(Decision#route_decision.provider_id, [<<"openai">>, <<"anthropic">>])),
            ok;
        Error ->
            ct:fail({routing_failed, Error})
    end.

%% @doc Test: Provider fallback
test_provider_fallback(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Create route request
    Request = #route_request{
        message = #{
            <<"tenant_id">> => TenantId,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = PolicyId,
        context = #{}
    },
    
    %% Route request (should use fallback if primary fails)
    case router_core:route(Request, undefined) of
        {ok, Decision} ->
            ?assert(is_record(Decision, route_decision)),
            ?assertNotEqual(undefined, Decision#route_decision.provider_id),
            ok;
        Error ->
            ct:fail({routing_failed, Error})
    end.

%% @doc Test: Provider sticky session
test_provider_sticky_session(Config) ->
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    
    %% Create route request with session
    Request = #route_request{
        message = #{
            <<"tenant_id">> => TenantId,
            <<"message_type">> => <<"chat">>,
            <<"payload">> => <<"Hello">>
        },
        policy_id = PolicyId,
        context = #{
            <<"session_id">> => <<"session_1">>
        }
    },
    
    %% Route request (should use sticky session)
    case router_core:route(Request, undefined) of
        {ok, Decision1} ->
            Provider1 = Decision1#route_decision.provider_id,
            ?assert(is_record(Decision1, route_decision)),
            ?assertNotEqual(undefined, Provider1),
            
            %% Route another request with same session (should get same provider)
            case router_core:route(Request, undefined) of
                {ok, Decision2} ->
                    Provider2 = Decision2#route_decision.provider_id,
                    ?assertEqual(Provider1, Provider2),
                    ok;
                Error ->
                    ct:fail({routing_failed, Error})
            end;
        Error ->
            ct:fail({routing_failed, Error})
    end.
