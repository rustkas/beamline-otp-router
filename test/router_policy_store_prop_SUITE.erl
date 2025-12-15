%% @doc Test Suite for Router Policy Store with concurrent and random operations
%% Tests concurrent operations and basic invariants
-module(router_policy_store_prop_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").
-include("../include/flow_pb.hrl").

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]}).

%% Test suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    prop_upsert_get_roundtrip/1,
    prop_delete_removes_policy/1,
    prop_list_returns_all_policies/1,
    prop_concurrent_upserts_no_loss/1,
    prop_concurrent_deletes_idempotent/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, property_tests}];
groups_for_level(_) -> %% fast, full
    [].

groups() ->
    [
        {property_tests, [sequence], [
            prop_upsert_get_roundtrip,
            prop_delete_removes_policy,
            prop_list_returns_all_policies,
            prop_concurrent_upserts_no_loss,
            prop_concurrent_deletes_idempotent
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("### init_per_suite: starting beamline_router for property tests", []),
    
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, admin_api_key, <<"test-key">>),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_Case, Config) ->
    ok = router_policy_store:reset(),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%====================================================================
%% Property-like Tests (randomized/concurrent operations)
%%====================================================================

%% Property: Upsert then get should return the same policy
prop_upsert_get_roundtrip(_Config) ->
    %% Run multiple random tests manually
    NumTests = 20,
    Results = lists:map(fun(I) ->
        TenantId = iolist_to_binary(["tenant_prop_", integer_to_list(I)]),
        PolicyId = iolist_to_binary(["policy_prop_", integer_to_list(I)]),
        AdminPolicyPb = #'AdminPolicy'{
            policy_id = PolicyId,
            providers = [#'AdminProvider'{id = <<"openai:gpt-4">>, weight = 0.5}],
            sticky = false,
            rules = []
        },
        
        %% Convert and upsert
        {ok, Policy} = router_admin_grpc:convert_admin_policy_to_policy(TenantId, AdminPolicyPb),
        {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
        
        %% Get and verify
        case router_policy_store:get_policy(TenantId, PolicyId) of
            {ok, Retrieved} ->
                Retrieved#policy.policy_id =:= PolicyId andalso
                Retrieved#policy.tenant_id =:= TenantId;
            _ ->
                false
        end
    end, lists:seq(1, NumTests)),
    
    ?assert(lists:all(fun(R) -> R =:= true end, Results)).

%% Property: Delete should remove policy
prop_delete_removes_policy(_Config) ->
    NumTests = 20,
    Results = lists:map(fun(I) ->
        TenantId = iolist_to_binary(["tenant_del_", integer_to_list(I)]),
        PolicyId = iolist_to_binary(["policy_del_", integer_to_list(I)]),
        AdminPolicyPb = #'AdminPolicy'{
            policy_id = PolicyId,
            providers = [#'AdminProvider'{id = <<"anthropic:claude">>, weight = 0.8}],
            sticky = false,
            rules = []
        },
        
        %% Upsert
        {ok, Policy} = router_admin_grpc:convert_admin_policy_to_policy(TenantId, AdminPolicyPb),
        {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
        
        %% Delete
        ok = router_policy_store:delete_policy(TenantId, PolicyId),
        
        %% Verify deleted
        case router_policy_store:get_policy(TenantId, PolicyId) of
            {error, not_found} -> true;
            _ -> false
        end
    end, lists:seq(1, NumTests)),
    
    ?assert(lists:all(fun(R) -> R =:= true end, Results)).

%% Property: List should return all upserted policies for a tenant
prop_list_returns_all_policies(_Config) ->
    TenantId = <<"list_prop_tenant">>,
    NumPolicies = 10,
    
    %% Create N policies
    PolicyIds = [iolist_to_binary(["lp", integer_to_list(I)]) 
                || I <- lists:seq(1, NumPolicies)],
    
    lists:foreach(fun(PolicyId) ->
        AdminPolicyPb = #'AdminPolicy'{
            policy_id = PolicyId,
            providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
            sticky = false,
            rules = []
        },
        {ok, Policy} = router_admin_grpc:convert_admin_policy_to_policy(TenantId, AdminPolicyPb),
        {ok, _} = router_policy_store:upsert_policy(TenantId, Policy)
    end, PolicyIds),
    
    %% List and verify count
    {ok, Policies} = router_policy_store:list_policies(TenantId),
    ?assertEqual(NumPolicies, length(Policies)).

%% Property: Concurrent upserts should not lose data
prop_concurrent_upserts_no_loss(_Config) ->
    TenantId = <<"concurrent_tenant">>,
    NumPolicies = 20,
    
    PolicyIds = [iolist_to_binary(["cp", integer_to_list(I)]) 
                || I <- lists:seq(1, NumPolicies)],
    
    Parent = self(),
    
    %% Spawn concurrent upserts
    Pids = lists:map(fun(PolicyId) ->
        spawn_link(fun() ->
            AdminPolicyPb = #'AdminPolicy'{
                policy_id = PolicyId,
                providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
                sticky = false,
                rules = []
            },
            {ok, Policy} = router_admin_grpc:convert_admin_policy_to_policy(TenantId, AdminPolicyPb),
            Result = router_policy_store:upsert_policy(TenantId, Policy),
            Parent ! {done, self(), Result}
        end)
    end, PolicyIds),
    
    %% Wait for all
    Results = [receive {done, Pid, R} -> R after router_test_timeouts:long_wait() -> timeout end || Pid <- Pids],
    
    %% All should succeed
    AllOk = lists:all(fun(R) -> element(1, R) =:= ok end, Results),
    
    %% All policies should be present
    {ok, Policies} = router_policy_store:list_policies(TenantId),
    AllPresent = length(Policies) =:= NumPolicies,
    
    ?assert(AllOk andalso AllPresent).

%% Property: Concurrent deletes should be idempotent
prop_concurrent_deletes_idempotent(_Config) ->
    TenantId = <<"delete_tenant">>,
    PolicyId = <<"delete_target">>,
    
    %% Create policy
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
        sticky = false,
        rules = []
    },
    {ok, Policy} = router_admin_grpc:convert_admin_policy_to_policy(TenantId, AdminPolicyPb),
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy),
    
    Parent = self(),
    NumDeletes = 10,
    
    %% Spawn concurrent deletes of same policy
    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            Result = router_policy_store:delete_policy(TenantId, PolicyId),
            Parent ! {done, self(), Result}
        end)
    end, lists:seq(1, NumDeletes)),
    
    %% Wait for all
    Results = [receive {done, Pid, R} -> R after router_test_timeouts:long_wait() -> timeout end || Pid <- Pids],
    
    %% Exactly one should succeed with `ok`, rest with `{error, not_found}` or `ok`
    OkCount = length([R || R <- Results, R =:= ok]),
    NotFoundCount = length([R || R <- Results, R =:= {error, not_found}]),
    
    %% All responses should be valid (either ok or not_found)
    ?assertEqual(NumDeletes, OkCount + NotFoundCount),
    
    %% Policy should be deleted
    ?assertEqual({error, not_found}, router_policy_store:get_policy(TenantId, PolicyId)).
