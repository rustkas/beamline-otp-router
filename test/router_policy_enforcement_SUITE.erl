%% @doc Common Test suite for Policy Enforcement (RBAC + Quotas)
%% @test_category cp1_smoke, fast
-module(router_policy_enforcement_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [all/0, end_per_suite/1, end_per_testcase/2, init_per_suite/1, init_per_testcase/2, suite/0]}).


%% Test suite configuration
suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [
        test_policy_quota_enforcement,
        test_cross_tenant_isolation,
        test_audit_log_completeness,
        test_rbac_with_quota,
        test_rate_limit_integration
    ].

init_per_suite(Config) ->
    %% Start required services (if not already started)
    case whereis(router_rbac) of
        undefined -> {ok, _} = router_rbac:start_link();
        _ -> ok
    end,
    case whereis(router_rate_limiter) of
        undefined -> {ok, _} = router_rate_limiter:start_link();
        _ -> ok
    end,
    case whereis(router_policy_store) of
        undefined -> {ok, _} = router_policy_store:start_link();
        _ -> ok
    end,
    %% Reset RBAC state (uses proper API instead of direct ETS access)
    case whereis(router_rbac) of
        undefined -> ok;
        _ -> router_rbac:reset()
    end,
    %% Clear quota table for clean test state (using API instead of direct ETS access)
    router_quota:clear_all_quotas(),
    %% Clear audit table for clean test state (using API instead of direct ETS access)
    router_audit:clear_all_audit_entries(),
    Config.

end_per_suite(_Config) ->
    %% Stop services
    catch gen_server:stop(router_rbac),
    catch gen_server:stop(router_rate_limiter),
    catch gen_server:stop(router_policy_store),
    %% Reset RBAC state before stopping (uses proper API instead of direct ETS access)
    case whereis(router_rbac) of
        undefined -> ok;
        _ -> router_rbac:reset()
    end,
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Policy quota enforcement
test_policy_quota_enforcement(_Config) ->
    TenantId = <<"test_tenant">>,
    
    %% Mock router_policy_store:list_policies to return empty list initially
    meck:new(router_policy_store, [passthrough]),
    PolicyCountRef = erlang:make_ref(),
    put(PolicyCountRef, 0),
    
    meck:expect(router_policy_store, list_policies, fun(Tid) ->
        Tid = TenantId,  %% Verify tenant ID
        Count = get(PolicyCountRef),
        {ok, lists:duplicate(Count, #policy{tenant_id = Tid})}
    end),
    
    %% Set quota to 2 policies
    Quota = #quota{
        tenant_id = TenantId,
        max_policies = 2,
        max_rules_per_policy = 50,
        max_providers_per_policy = 20
    },
    router_quota:set_tenant_quota(TenantId, Quota),
    
    %% Create first policy (should succeed)
    {ok, Remaining1} = router_quota:check_policy_quota(TenantId),
    ?assertEqual(2, Remaining1),
    put(PolicyCountRef, 1),
    
    %% Create second policy (should succeed)
    {ok, Remaining2} = router_quota:check_policy_quota(TenantId),
    ?assertEqual(1, Remaining2),
    put(PolicyCountRef, 2),
    
    %% Try to create third policy (should fail)
    {error, quota_exceeded, 2} = router_quota:check_policy_quota(TenantId),
    
    meck:unload(router_policy_store),
    ok.

%% Test: Cross-tenant isolation
test_cross_tenant_isolation(_Config) ->
    TenantId1 = <<"tenant1">>,
    TenantId2 = <<"tenant2">>,
    UserId = <<"test_user">>,
    PolicyId = <<"test_policy">>,
    
    %% Assign operator role for tenant1
    ok = router_rbac:assign_role(UserId, TenantId1, <<"operator">>),
    
    %% User can access policies in tenant1
    ?assert(router_permissions:check_policy_access(UserId, TenantId1, PolicyId, #{})),
    
    %% User cannot access policies in tenant2 (no role assigned)
    ?assertNot(router_permissions:check_policy_access(UserId, TenantId2, PolicyId, #{})),
    
    %% Assign operator role for tenant2
    ok = router_rbac:assign_role(UserId, TenantId2, <<"operator">>),
    
    %% Now user can access policies in tenant2
    ?assert(router_permissions:check_policy_access(UserId, TenantId2, PolicyId, #{})),
    
    ok.

%% Test: Audit log completeness
test_audit_log_completeness(_Config) ->
    TenantId = <<"test_tenant">>,
    UserId = <<"test_user">>,
    PolicyId = <<"test_policy">>,
    
    %% Log policy action
    Context = #{
        <<"trace_id">> => <<"test_trace">>,
        <<"action">> => <<"test_action">>
    },
    ok = router_audit:log_policy_action(TenantId, UserId, <<"upsert">>, PolicyId, Context),
    
    %% Wait a bit for async operations
    timer:sleep(50),
    
    %% Get audit entries
    Entries = router_audit:get_audit_entries(TenantId, 10, 0),
    
    %% Verify entry exists
    ?assert(length(Entries) > 0, "Audit entry should be created"),
    
    %% Verify entry structure
    [Entry | _] = Entries,
    ?assert(maps:is_key(<<"ts">>, Entry), "Entry should have ts field"),
    ?assert(maps:is_key(<<"tenant_id">>, Entry), "Entry should have tenant_id field"),
    ?assert(maps:is_key(<<"user_id">>, Entry), "Entry should have user_id field"),
    ?assert(maps:is_key(<<"action">>, Entry), "Entry should have action field"),
    ?assert(maps:is_key(<<"resource_type">>, Entry), "Entry should have resource_type field"),
    ?assertEqual(UserId, maps:get(<<"user_id">>, Entry), "User ID should match"),
    ?assertEqual(<<"upsert">>, maps:get(<<"action">>, Entry), "Action should match"),
    
    ok.

%% Test: RBAC with quota
test_rbac_with_quota(_Config) ->
    TenantId = <<"test_tenant">>,
    UserId = <<"test_user">>,
    PolicyId = <<"test_policy">>,
    
    %% Set quota to 1 policy
    Quota = #quota{
        tenant_id = TenantId,
        max_policies = 1,
        max_rules_per_policy = 50,
        max_providers_per_policy = 20
    },
    router_quota:set_tenant_quota(TenantId, Quota),
    
    %% Assign operator role
    ok = router_rbac:assign_role(UserId, TenantId, <<"operator">>),
    
    %% User has permission but quota is exceeded
    ?assert(router_permissions:check_policy_write(UserId, TenantId, PolicyId, #{})),
    {error, quota_exceeded, 1} = router_quota:check_policy_quota(TenantId),
    
    ok.

%% Test: Rate limit integration
test_rate_limit_integration(_Config) ->
    %% Ensure rate limiter is running
    case whereis(router_rate_limiter) of
        undefined ->
            {ok, _} = router_rate_limiter:start_link(),
            timer:sleep(100);
        _ ->
            ok
    end,
    
    TenantId = <<"test_tenant">>,
    Endpoint = <<"decide">>,
    UserId = <<"test_user">>,
    
    %% Set custom rate limit
    Limits = #{
        <<"requests_per_minute">> => 5,
        <<"ttl_seconds">> => 60
    },
    ok = router_rate_limiter:set_rate_limits(TenantId, Limits),
    
    %% Make 5 requests (should succeed)
    Results = lists:map(fun(_) ->
        router_rate_limiter:check_rate_limit(TenantId, Endpoint, UserId)
    end, lists:seq(1, 5)),
    
    %% Verify all 5 requests succeeded
    lists:foreach(fun(Result) ->
        ?assertMatch({ok, _Remaining}, Result, "Rate limit should allow first 5 requests")
    end, Results),
    
    %% 6th request should fail
    {error, rate_limit_exceeded, 0} = router_rate_limiter:check_rate_limit(TenantId, Endpoint, UserId),
    
    ok.

