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

-compile([export_all, nowarn_export_all]).

all() -> [
    test_provider_selection,
    test_provider_fallback,
    test_provider_sticky_session
].

init_per_suite(Config) ->
    %% Start Router application
    ok = router_test_utils:start_router_app(),
    
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
            <<"session_id">> => <<"session_1">>
        },
        metadata = #{}
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, PolicyId, Policy, undefined),
    
    [{tenant_id, TenantId}, {policy_id, PolicyId} | Config].

end_per_suite(Config) ->
    %% Cleanup
    TenantId = proplists:get_value(tenant_id, Config),
    PolicyId = proplists:get_value(policy_id, Config),
    router_policy_store:delete_policy(TenantId, PolicyId, undefined),
    router_test_utils:stop_router_app(),
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

