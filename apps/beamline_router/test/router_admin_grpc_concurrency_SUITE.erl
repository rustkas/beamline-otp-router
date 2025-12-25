%% @doc Common Test Suite for RouterAdmin Concurrency Tests
%% Tests concurrent policy updates and race conditions using REAL policy store
-module(router_admin_grpc_concurrency_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Include necessary header files
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/flow_pb.hrl").

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
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([
    test_upsert_sanity/1,
    test_concurrent_upsert_different_tenants/1,
    test_concurrent_upsert_same_tenant/1,
    test_concurrent_get_list/1,
    test_concurrent_delete/1
]).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, concurrency_tests}];
groups_for_level(full) ->
    [{group, concurrency_tests}];
groups_for_level(_) ->
    [{group, concurrency_tests}].
%% no tier branching

groups() ->
    [
        {concurrency_tests, [sequence], [
            test_upsert_sanity,
            test_concurrent_upsert_different_tenants,
            test_concurrent_upsert_same_tenant,
            test_concurrent_get_list,
            test_concurrent_delete
        ]}
    ].

init_per_suite(Config) ->
    router_test_bootstrap:init_per_suite(Config, #{
        start => ensure_all_started
    }).

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{
        start => ensure_all_started,
        stop => stop_app
    }).

init_per_testcase(_Case, Config) ->
    BaseConfig = router_test_bootstrap:init_per_testcase(_Case, Config, #{}),
    ok = router_policy_store:reset(),
    TestUserId = <<"test-user">>,
    [{test_user_id, TestUserId} | BaseConfig].

end_per_testcase(_Case, Config) ->
    router_test_bootstrap:end_per_testcase(_Case, Config, #{}),
    ok.

%% Helpers

upsert_via_store(TenantId, AdminPolicyPb) ->
    {ok, Policy} = router_admin_grpc:convert_admin_policy_to_policy(TenantId, AdminPolicyPb),
    router_policy_store:upsert_policy(TenantId, Policy).

test_upsert_sanity(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy_sanity">>,
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
        sticky = false,
        rules = []
    },

    ct:pal("### sanity: calling router_policy_store directly", []),
    Result = upsert_via_store(TenantId, AdminPolicyPb),
    ct:pal("### sanity: result = ~p", [Result]),

    ?assertMatch({ok, _}, Result),

    %% Verify store content
    {ok, Policy} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(PolicyId, Policy#policy.policy_id),
    ?assertEqual(TenantId, Policy#policy.tenant_id).

test_concurrent_upsert_different_tenants(_Config) ->
    NumRequests = 10,
    Parent = self(),

    Pids = lists:map(fun(I) ->
        TenantId = iolist_to_binary(["tenant_", integer_to_list(I)]),
        PolicyId = iolist_to_binary(["policy_", integer_to_list(I)]),
        spawn_link(fun() ->
            AdminPolicyPb = #'AdminPolicy'{
                policy_id = PolicyId,
                providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
                sticky = false,
                rules = []
            },
            Result = upsert_via_store(TenantId, AdminPolicyPb),
            Parent ! {upsert_result, self(), Result}
        end)
    end, lists:seq(1, NumRequests)),

    Results = [receive {upsert_result, Pid, Result} -> {Pid, Result} end || Pid <- Pids],
    lists:foreach(fun({_Pid, Result}) ->
        ?assertMatch({ok, _}, Result)
    end, Results),

    %% Verify all policies are stored
    lists:foreach(fun(I) ->
        TenantId = iolist_to_binary(["tenant_", integer_to_list(I)]),
        PolicyId = iolist_to_binary(["policy_", integer_to_list(I)]),
        {ok, _Policy} = router_policy_store:get_policy(TenantId, PolicyId)
    end, lists:seq(1, NumRequests)).

test_concurrent_upsert_same_tenant(_Config) ->
    NumRequests = 10,
    Parent = self(),
    TenantId = <<"same_tenant">>,

    Pids = lists:map(fun(I) ->
        PolicyId = iolist_to_binary(["policy_", integer_to_list(I)]),
        spawn_link(fun() ->
            AdminPolicyPb = #'AdminPolicy'{
                policy_id = PolicyId,
                providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
                sticky = false,
                rules = []
            },
            Result = upsert_via_store(TenantId, AdminPolicyPb),
            Parent ! {upsert_result, self(), Result}
        end)
    end, lists:seq(1, NumRequests)),

    Results = [receive {upsert_result, Pid, Result} -> {Pid, Result} end || Pid <- Pids],
    lists:foreach(fun({_Pid, Result}) ->
        ?assertMatch({ok, _}, Result)
    end, Results).

test_concurrent_get_list(_Config) ->
    NumPolicies = 10,
    TenantId = <<"get_list_tenant">>,

    %% Setup policies
    lists:foreach(fun(I) ->
        PolicyId = iolist_to_binary(["p", integer_to_list(I)]),
        AdminPolicyPb = #'AdminPolicy'{
            policy_id = PolicyId,
            providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
            sticky = false,
            rules = []
        },
        {ok, _} = upsert_via_store(TenantId, AdminPolicyPb)
    end, lists:seq(1, NumPolicies)),

    Parent = self(),
    NumRequests = 5,
    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            Result = router_policy_store:list_policies(TenantId),
            Parent ! {list_result, self(), Result}
        end)
    end, lists:seq(1, NumRequests)),

    Results = [receive {list_result, Pid, Result} -> {Pid, Result} end || Pid <- Pids],
    lists:foreach(fun({_Pid, Result}) ->
        ?assertMatch({ok, _}, Result),
        {ok, Policies} = Result,
        ?assertEqual(NumPolicies, length(Policies))
    end, Results).

test_concurrent_delete(_Config) ->
    NumPolicies = 20,
    TenantId = <<"delete_tenant">>,
    Parent = self(),

    PolicyIds = [iolist_to_binary(["p", integer_to_list(I)]) || I <- lists:seq(1, NumPolicies)],
    lists:foreach(fun(PolicyId) ->
        AdminPolicyPb = #'AdminPolicy'{
            policy_id = PolicyId,
            providers = [#'AdminProvider'{id = <<"p1">>, weight = 1.0}],
            sticky = false,
            rules = []
        },
        {ok, _} = upsert_via_store(TenantId, AdminPolicyPb)
    end, PolicyIds),

    Pids = lists:map(fun(PolicyId) ->
        spawn_link(fun() ->
            Result = router_policy_store:delete_policy(TenantId, PolicyId),
            Parent ! {delete_result, self(), Result}
        end)
    end, PolicyIds),

    Results = [receive {delete_result, Pid, Result} -> {Pid, Result} end || Pid <- Pids],
    lists:foreach(fun({_Pid, Result}) ->
        ?assertEqual(ok, Result)
    end, Results),

    %% Verify all policies deleted
    lists:foreach(fun(PolicyId) ->
        ?assertMatch({error, not_found}, router_policy_store:get_policy(TenantId, PolicyId))
    end, PolicyIds).
