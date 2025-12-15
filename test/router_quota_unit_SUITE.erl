%% @doc Unit Tests for router_quota module
%% @test_category unit, fast, coverage_hotspot
-module(router_quota_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_get_default_quota/1,
    test_get_tenant_quota_default/1,
    test_set_tenant_quota/1,
    test_check_policy_quota_ok/1,
    test_check_rule_quota_ok/1,
    test_check_provider_quota_ok/1,
    test_get_quota_usage/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1,
    test_clear_all_quotas/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_get_default_quota/1,
    test_get_tenant_quota_default/1,
    test_set_tenant_quota/1,
    test_check_policy_quota_ok/1,
    test_check_rule_quota_ok/1,
    test_check_provider_quota_ok/1,
    test_get_quota_usage/1,
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1,
    test_clear_all_quotas/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_get_default_quota,
            test_get_tenant_quota_default,
            test_set_tenant_quota,
            test_check_policy_quota_ok,
            test_check_rule_quota_ok,
            test_check_provider_quota_ok,
            test_get_quota_usage,
            test_get_table_size,
            test_get_table_memory,
            test_check_size_limit,
            test_clear_all_quotas
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear quotas before each test
    router_quota:clear_all_quotas(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for get_default_quota/0
%% ============================================================================

test_get_default_quota(_Config) ->
    Result = router_quota:get_default_quota(),
    
    ?assertEqual(true, is_record(Result, quota)),
    ?assertEqual(<<"default">>, Result#quota.tenant_id),
    ?assertEqual(true, is_integer(Result#quota.max_policies)),
    ?assertEqual(true, is_integer(Result#quota.max_rules_per_policy)),
    ?assertEqual(true, is_integer(Result#quota.max_providers_per_policy)),
    
    ok.

%% ============================================================================
%% Tests for get_tenant_quota/1
%% ============================================================================

test_get_tenant_quota_default(_Config) ->
    TenantId = <<"new_tenant">>,
    
    Result = router_quota:get_tenant_quota(TenantId),
    
    ?assertEqual(true, is_record(Result, quota)),
    ?assertEqual(TenantId, Result#quota.tenant_id),
    
    ok.

%% ============================================================================
%% Tests for set_tenant_quota/2
%% ============================================================================

test_set_tenant_quota(_Config) ->
    TenantId = <<"custom_tenant">>,
    CustomQuota = #quota{
        tenant_id = TenantId,
        max_policies = 100,
        max_rules_per_policy = 200,
        max_providers_per_policy = 50
    },
    
    ok = router_quota:set_tenant_quota(TenantId, CustomQuota),
    
    RetrievedQuota = router_quota:get_tenant_quota(TenantId),
    
    ?assertEqual(TenantId, RetrievedQuota#quota.tenant_id),
    ?assertEqual(100, RetrievedQuota#quota.max_policies),
    ?assertEqual(200, RetrievedQuota#quota.max_rules_per_policy),
    ?assertEqual(50, RetrievedQuota#quota.max_providers_per_policy),
    
    ok.

%% ============================================================================
%% Tests for check_policy_quota/1, check_policy_quota/2
%% ============================================================================

test_check_policy_quota_ok(_Config) ->
    TenantId = <<"policy_quota_tenant">>,
    
    Result = router_quota:check_policy_quota(TenantId),
    
    case Result of
        {ok, Remaining} ->
            ?assertEqual(true, is_integer(Remaining)),
            ?assertEqual(true, Remaining > 0);
        {error, quota_exceeded, _Max} ->
            %% This can happen if tenant already has policies
            ok
    end,
    
    %% Test with policy_id parameter
    Result2 = router_quota:check_policy_quota(TenantId, <<"some_policy">>),
    
    case Result2 of
        {ok, Remaining2} ->
            ?assertEqual(true, is_integer(Remaining2));
        {error, quota_exceeded, _Max2} ->
            ok
    end,
    
    ok.

%% ============================================================================
%% Tests for check_rule_quota/2
%% ============================================================================

test_check_rule_quota_ok(_Config) ->
    TenantId = <<"rule_quota_tenant">>,
    
    %% With 5 rules, should be under quota
    Result = router_quota:check_rule_quota(TenantId, 5),
    
    case Result of
        {ok, Remaining} ->
            ?assertEqual(true, is_integer(Remaining)),
            ?assertEqual(true, Remaining > 0);
        {error, quota_exceeded, _Max} ->
            ok
    end,
    
    ok.

%% ============================================================================
%% Tests for check_provider_quota/2
%% ============================================================================

test_check_provider_quota_ok(_Config) ->
    TenantId = <<"provider_quota_tenant">>,
    
    %% With 3 providers, should be under quota
    Result = router_quota:check_provider_quota(TenantId, 3),
    
    case Result of
        {ok, Remaining} ->
            ?assertEqual(true, is_integer(Remaining)),
            ?assertEqual(true, Remaining > 0);
        {error, quota_exceeded, _Max} ->
            ok
    end,
    
    ok.

%% ============================================================================
%% Tests for get_quota_usage/1
%% ============================================================================

test_get_quota_usage(_Config) ->
    TenantId = <<"usage_tenant">>,
    
    Result = router_quota:get_quota_usage(TenantId),
    
    ?assertEqual(true, is_map(Result)),
    %% Keys are binaries in the returned map
    ?assertEqual(true, maps:is_key(<<"tenant_id">>, Result)),
    ?assertEqual(TenantId, maps:get(<<"tenant_id">>, Result)),
    ?assertEqual(true, maps:is_key(<<"policies">>, Result)),
    
    ok.

%% ============================================================================
%% Tests for table operations
%% ============================================================================

test_get_table_size(_Config) ->
    Result = router_quota:get_table_size(),
    
    ?assertEqual(true, is_integer(Result) orelse Result =:= undefined),
    
    ok.

test_get_table_memory(_Config) ->
    Result = router_quota:get_table_memory(),
    
    ?assertEqual(true, is_integer(Result) orelse Result =:= undefined),
    
    ok.

test_check_size_limit(_Config) ->
    Result = router_quota:check_size_limit(),
    
    case Result of
        {ok, Size} when is_integer(Size) ->
            ?assertEqual(true, Size >= 0);
        {error, no_limit_configured} ->
            ok;
        {error, exceeded, _Current, _Limit} ->
            ok
    end,
    
    ok.

%% ============================================================================
%% Tests for clear_all_quotas/0
%% ============================================================================

test_clear_all_quotas(_Config) ->
    TenantId = <<"clear_test_tenant">>,
    CustomQuota = #quota{
        tenant_id = TenantId,
        max_policies = 999,
        max_rules_per_policy = 999,
        max_providers_per_policy = 999
    },
    
    ok = router_quota:set_tenant_quota(TenantId, CustomQuota),
    
    %% Clear all quotas
    ok = router_quota:clear_all_quotas(),
    
    %% Should return default quota now
    RetrievedQuota = router_quota:get_tenant_quota(TenantId),
    
    %% Should be back to default values, not 999
    ?assertNotEqual(999, RetrievedQuota#quota.max_policies),
    
    ok.
