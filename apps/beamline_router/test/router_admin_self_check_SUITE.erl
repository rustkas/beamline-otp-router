%% # Admin self-check suite
%%
%% Verifies that the admin control-plane endpoints are healthy and support expected operations.

-module(router_admin_self_check_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([
    all/0, groups_for_level/1,
    groups/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_policy_store_started/1,
    test_policy_store_contains_default_policy/1,
    test_add_update_delete_policy/1,
    test_all_admin_subjects_respond/1,
    test_tenant_isolation/1,
    test_endpoint_supports_required_rpcs/1
]).

-define(TENANT_ID, <<"tenant_123">>).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, self_check_tests}];
groups_for_level(full) ->
    [{group, self_check_tests}];
groups_for_level(_) ->
    [{group, self_check_tests}].

groups() ->
    [
        {self_check_tests, [sequence], [
            test_policy_store_started,
            test_policy_store_contains_default_policy,
            test_add_update_delete_policy,
            test_all_admin_subjects_respond,
            test_tenant_isolation,
            test_endpoint_supports_required_rpcs
        ]}
    ].

init_per_suite(Config) ->
    application:ensure_all_started(beamline_router),
    {ok, _} = router_nats_server:ensure_running(),
    Config.

end_per_suite(_Config) ->
    safe_ignore(fun() -> router_nats_server:stop() end),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_admin_policy_store:reset(),
    DefaultPolicy = ensure_policy_weights(default_policy()),
    DefaultTenantId = DefaultPolicy#policy.tenant_id,
    safe_ignore(fun() -> router_policy_store:delete_policy(DefaultTenantId, DefaultTenantId) end),
    {ok, _} = router_policy_store:upsert_policy(DefaultTenantId, DefaultPolicy),
    Config.

end_per_testcase(_TestCase, Config) ->
    ok = router_admin_policy_store:reset(),
    DefaultPolicy = ensure_policy_weights(default_policy()),
    DefaultTenantId = DefaultPolicy#policy.tenant_id,
    safe_ignore(fun() -> router_policy_store:delete_policy(DefaultTenantId, DefaultTenantId) end),
    Config.

test_policy_store_started(_Config) ->
    ?assert(erlang:is_pid(whereis(router_policy_store))),
    ok.

test_policy_store_contains_default_policy(_Config) ->
    DefaultPolicy = default_policy(),
    TenantId = DefaultPolicy#policy.tenant_id,
    {ok, Policy} = router_policy_store:get_policy(TenantId, TenantId),
    ?assertMatch(#policy{}, Policy),
    ok.

test_add_update_delete_policy(_Config) ->
    TenantId = ?TENANT_ID,
    PolicyId = TenantId,  % Using TenantId as PolicyId for simplicity

    Policy1 = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        weights = #{<<"openai:gpt-4">> => 1.0},
        allow_rules = [{"subject1", "pub"}]
    },
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy1),
    {ok, Stored1} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(Policy1#policy.allow_rules, Stored1#policy.allow_rules),

    Policy2 = Policy1#policy{allow_rules = [{"subject2", "sub"}]},
    {ok, _} = router_policy_store:upsert_policy(TenantId, Policy2),
    {ok, Stored2} = router_policy_store:get_policy(TenantId, PolicyId),
    ?assertEqual(Policy2#policy.allow_rules, Stored2#policy.allow_rules),

    ok = router_policy_store:delete_policy(TenantId, PolicyId),
    {error, not_found} = router_policy_store:get_policy(TenantId, PolicyId),
    ok.

test_all_admin_subjects_respond(_Config) ->
    Subjects = [
        ~"beamline.router.admin.get_policy",
        ~"beamline.router.admin.put_policy",
        ~"beamline.router.admin.delete_policy",
        ~"beamline.router.admin.list_policies",
        ~"beamline.router.admin.health"
    ],
    lists:foreach(
        fun(Subject) ->
            Response =
                try router_admin_cp_status:handle_admin_request(Subject, #{} ) of
                    R -> R
                catch
                    Class:Reason -> {error, {Class, Reason}}
                end,
            ?assertMatch({ok, _}, Response)
        end,
        Subjects
    ),
    ok.

test_tenant_isolation(_Config) ->
    Tenant1 = <<"tenant1">>,
    Tenant2 = <<"tenant2">>,
    Policy1Id = Tenant1,
    Policy2Id = Tenant2,

    Policy1 = #policy{tenant_id = Tenant1, policy_id = Policy1Id, weights = #{<<"p1">> => 1.0}, allow_rules = [{"s1", "pub"}]},
    Policy2 = #policy{tenant_id = Tenant2, policy_id = Policy2Id, weights = #{<<"p2">> => 1.0}, allow_rules = [{"s2", "pub"}]},

    {ok, _} = router_policy_store:upsert_policy(Tenant1, Policy1),
    {ok, _} = router_policy_store:upsert_policy(Tenant2, Policy2),

    {ok, Stored1} = router_policy_store:get_policy(Tenant1, Policy1Id),
    {ok, Stored2} = router_policy_store:get_policy(Tenant2, Policy2Id),

    ?assertEqual(Policy1#policy.allow_rules, Stored1#policy.allow_rules),
    ?assertEqual(Policy2#policy.allow_rules, Stored2#policy.allow_rules),
    ok.

test_endpoint_supports_required_rpcs(_Config) ->
    Supported = router_admin_cp_status:supported_admin_subjects(),
    Required = [
        ~"beamline.router.admin.get_policy",
        ~"beamline.router.admin.put_policy",
        ~"beamline.router.admin.delete_policy",
        ~"beamline.router.admin.list_policies",
        ~"beamline.router.admin.health"
    ],
    lists:foreach(
        fun(Subject) ->
            ?assert(lists:member(Subject, Supported))
        end,
        Required
    ),
    ok.

%% -- helpers

default_policy() ->
    TenantId = <<"default">>,
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = TenantId,
        weights = #{<<"openai:gpt-4">> => 1.0},
        allow_rules = [{"*", "pub"}, {"*", "sub"}]
    },
    Policy.

ensure_policy_weights(Policy) ->
    case Policy#policy.weights of
        #{} ->
            Policy#policy{weights = #{<<"openai:gpt-4">> => 1.0}};
        _ ->
            Policy
    end.

safe_ignore(Fun) when is_function(Fun, 0) ->
    try Fun() of
        Res -> Res
    catch
        _:_ -> ok
    end.
