-module(router_cp2_dry_run_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dry_run_basic/1, test_dry_run_with_extensions/1, test_dsl_compilation/1]).

all() -> [test_dry_run_basic, test_dry_run_with_extensions, test_dsl_compilation].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, nats_mode, mock),
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

%% Test 1: Basic Dry-Run
%% Verify that setting dry_run=true in context returns a decision without side effects
test_dry_run_basic(_Config) ->
    TenantId = <<"dry_run_tenant">>,
    PolicyId = <<"basic_policy">>,
    
    %% Create a simple policy
    PolicyMap = #{
        <<"policy_id">> => PolicyId,
        <<"weights">> => #{<<"provider_a">> => 1.0}
    },
    {ok, _} = router_policy_store:upsert_policy_map(TenantId, PolicyMap),
    
    %% Request with dry_run=true
    RouteRequest = #route_request{
        message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_1">>},
        policy_id = PolicyId,
        context = #{<<"dry_run">> => true}
    },
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Assertions
    ?assertMatch(#route_decision{provider_id = <<"provider_a">>}, Decision),
    ?assertMatch(#{<<"dry_run">> := true}, Decision#route_decision.metadata),
    
    ok.

%% Test 2: Dry-Run with Extensions
%% Verify that extensions are "skipped" (return default success) in dry-run mode
test_dry_run_with_extensions(_Config) ->
    TenantId = <<"dry_run_ext_tenant">>,
    PolicyId = <<"ext_policy">>,
    
    %% Policy with pre-processor and validator
    PolicyMap = #{
        <<"policy_id">> => PolicyId,
        <<"weights">> => #{<<"provider_b">> => 1.0},
        <<"pre">> => [
            #{<<"id">> => <<"ext_pre_1">>, <<"mode">> => <<"mandatory">>}
        ],
        <<"validators">> => [
            #{<<"id">> => <<"ext_val_1">>, <<"on_fail">> => <<"block">>}
        ]
    },
    {ok, _} = router_policy_store:upsert_policy_map(TenantId, PolicyMap),
    
    %% Request with dry_run=true
    RouteRequest = #route_request{
        message = #{<<"tenant_id">> => TenantId, <<"message_id">> => <<"msg_2">>},
        policy_id = PolicyId,
        context = #{<<"dry_run">> => true}
    },
    
    %% Note: We do NOT register mocks for extensions. 
    %% If dry-run logic fails, it will try to call NATS and fail (or hang).
    
    {ok, Decision} = router_core:route(RouteRequest, #{}),
    
    %% Assertions
    ?assertMatch(#route_decision{provider_id = <<"provider_b">>}, Decision),
    
    %% Verify extensions were "executed" (logged in metadata)
    %% In dry-run, they should still be tracked as part of the path
    Metadata = Decision#route_decision.metadata,
    Executed = maps:get(<<"executed_extensions">>, Metadata, []),
    
    %% Check that our extensions are in the executed list
    PreFound = lists:keymember(<<"ext_pre_1">>, 2, Executed),
    ValFound = lists:keymember(<<"ext_val_1">>, 2, Executed),
    
    ct:pal("Executed extensions: ~p", [Executed]),

    ?assert(PreFound, "Pre-processor should be in executed list"),
    ?assert(ValFound, "Validator should be in executed list"),
    
    ok.

%% Test 3: DSL Compilation
%% Verify that map-based DSL is correctly compiled to #policy{}
test_dsl_compilation(_Config) ->
    TenantId = <<"dsl_tenant">>,
    PolicyMap = #{
        <<"policy_id">> => <<"dsl_policy">>,
        <<"weights">> => #{<<"p1">> => 0.8, <<"p2">> => 0.2},
        <<"fallback">> => #{<<"provider">> => <<"p3">>},
        <<"sticky">> => #{<<"enabled">> => true, <<"strategy">> => <<"header">>, <<"key">> => <<"x-user-id">>},
        <<"pre">> => [#{<<"id">> => <<"pre1">>, <<"mode">> => <<"mandatory">>}],
        <<"metadata">> => #{<<"owner">> => <<"team-a">>}
    },
    
    {ok, Policy} = router_policy_store:compile_policy(TenantId, PolicyMap),
    
    ?assertMatch(#policy{tenant_id = TenantId, policy_id = <<"dsl_policy">>}, Policy),
    ?assertMatch(#{<<"p1">> := 0.8}, Policy#policy.weights),
    ?assertMatch(#{<<"provider">> := <<"p3">>}, Policy#policy.fallback),
    ?assertMatch(#{<<"strategy">> := <<"header">>}, Policy#policy.sticky),
    ?assertMatch([#{id := <<"pre1">>}], Policy#policy.pre),
    ?assertMatch(#{<<"owner">> := <<"team-a">>}, Policy#policy.metadata),
    
    ok.
