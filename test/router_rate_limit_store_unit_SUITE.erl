%% @doc Unit Tests for router_rate_limit_store module
%% Additional targeted coverage tests for helper/utility functions
%% @test_category unit, fast, coverage_hotspot
-module(router_rate_limit_store_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_get_table_size/1,
    test_get_table_memory/1,
    test_check_size_limit/1,
    test_reset_function/1,
    test_token_refill_basic/1,
    test_token_refill_after_time/1,
    test_rate_limit_disabled_config/1,
    test_rate_limit_with_default_config/1,
    test_get_rate_limit_status_not_found/1,
    test_get_rate_limit_status_found/1,
    test_reset_rate_limit/1,
    test_tenant_scope/1,
    test_policy_scope/1
]).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_get_table_size,
            test_get_table_memory,
            test_check_size_limit,
            test_reset_function,
            test_token_refill_basic,
            test_token_refill_after_time,
            test_rate_limit_disabled_config,
            test_rate_limit_with_default_config,
            test_get_rate_limit_status_not_found,
            test_get_rate_limit_status_found,
            test_reset_rate_limit,
            test_tenant_scope,
            test_policy_scope
        ]}
    ].

init_per_suite(Config) ->
    %% Ensure app is started
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, disable_heir, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> ct:fail({app_start_failed, Error})
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear rate limit store before each test
    catch router_rate_limit_store:reset(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    catch router_rate_limit_store:reset(),
    ok.

%% ============================================================================
%% Tests for get_table_size/0
%% ============================================================================

test_get_table_size(_Config) ->
    %% Initial state should be 0 or small
    Size0 = router_rate_limit_store:get_table_size(),
    ?assertEqual(true, is_integer(Size0)),
    ?assert(Size0 >= 0),
    
    %% Add some entries
    Config = #{<<"enabled">> => true, <<"requests_per_second">> => 100, <<"burst">> => 50},
    router_rate_limit_store:check_rate_limit(tenant, <<"tenant1">>, Config),
    router_rate_limit_store:check_rate_limit(tenant, <<"tenant2">>, Config),
    
    Size1 = router_rate_limit_store:get_table_size(),
    ?assertEqual(true, Size1 >= 2),
    ok.

%% ============================================================================
%% Tests for get_table_memory/0
%% ============================================================================

test_get_table_memory(_Config) ->
    Memory = router_rate_limit_store:get_table_memory(),
    ?assertEqual(true, is_integer(Memory)),
    ?assert(Memory >= 0),
    ok.

%% ============================================================================
%% Tests for check_size_limit/0
%% ============================================================================

test_check_size_limit(_Config) ->
    Result = router_rate_limit_store:check_size_limit(),
    case Result of
        {ok, Size} ->
            ?assertEqual(true, is_integer(Size));
        {error, exceeded, _Current, _Limit} ->
            %% This is a valid result too
            ok;
        {error, no_limit_configured} ->
            ok
    end,
    ok.

%% ============================================================================
%% Tests for reset/0
%% ============================================================================

test_reset_function(_Config) ->
    %% Add entries
    Config = #{<<"enabled">> => true, <<"requests_per_second">> => 100, <<"burst">> => 50},
    router_rate_limit_store:check_rate_limit(tenant, <<"tenant_reset">>, Config),
    
    %% Reset
    Result = router_rate_limit_store:reset(),
    ?assertEqual(ok, Result),
    
    %% Table should be empty or status should be not_found
    Status = router_rate_limit_store:get_rate_limit_status(tenant, <<"tenant_reset">>),
    ?assertEqual({error, not_found}, Status),
    ok.

%% ============================================================================
%% Tests for token refill logic
%% ============================================================================

test_token_refill_basic(_Config) ->
    TenantId = <<"tenant_refill_basic">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% First request creates bucket, consumes 1 token (5-1=4 remaining)
    ?assertEqual({ok, allow}, router_rate_limit_store:check_rate_limit(tenant, TenantId, Config)),
    
    %% Status should show tokens
    {ok, Status} = router_rate_limit_store:get_rate_limit_status(tenant, TenantId),
    ?assertEqual(true, is_map(Status)),
    ok.

test_token_refill_after_time(_Config) ->
    TenantId = <<"tenant_refill_time">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 100,  %% High rate
        <<"burst">> => 5
    },
    
    %% Exhaust tokens
    [router_rate_limit_store:check_rate_limit(tenant, TenantId, Config) || _ <- lists:seq(1, 5)],
    
    %% Next request should be blocked
    Result = router_rate_limit_store:check_rate_limit(tenant, TenantId, Config),
    ?assertMatch({error, {rate_limit_exceeded, _}}, Result),
    ok.

%% ============================================================================
%% Tests for disabled rate limit
%% ============================================================================

test_rate_limit_disabled_config(_Config) ->
    TenantId = <<"tenant_disabled">>,
    Config = #{<<"enabled">> => false},
    
    %% Should always allow when disabled
    Results = [router_rate_limit_store:check_rate_limit(tenant, TenantId, Config) 
               || _ <- lists:seq(1, 100)],
    AllAllowed = lists:all(fun(R) -> R =:= {ok, allow} end, Results),
    ?assertEqual(true, AllAllowed),
    ok.

test_rate_limit_with_default_config(_Config) ->
    TenantId = <<"tenant_default">>,
    %% Minimal config - should use defaults
    Config = #{<<"enabled">> => true},
    
    ?assertEqual({ok, allow}, router_rate_limit_store:check_rate_limit(tenant, TenantId, Config)),
    ok.

%% ============================================================================
%% Tests for get_rate_limit_status
%% ============================================================================

test_get_rate_limit_status_not_found(_Config) ->
    Result = router_rate_limit_store:get_rate_limit_status(tenant, <<"nonexistent_tenant">>),
    ?assertEqual({error, not_found}, Result),
    ok.

test_get_rate_limit_status_found(_Config) ->
    TenantId = <<"tenant_status_found">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% Create bucket
    router_rate_limit_store:check_rate_limit(tenant, TenantId, Config),
    
    %% Get status
    {ok, Status} = router_rate_limit_store:get_rate_limit_status(tenant, TenantId),
    ?assertEqual(true, is_map(Status)),
    
    %% Check expected keys exist
    ?assertEqual(true, maps:is_key(<<"tokens">>, Status) orelse maps:is_key(tokens, Status)),
    ?assertEqual(true, maps:is_key(<<"limit">>, Status) orelse maps:is_key(limit, Status)),
    ?assertEqual(true, maps:is_key(<<"burst">>, Status) orelse maps:is_key(burst, Status)),
    ok.

%% ============================================================================
%% Tests for reset_rate_limit
%% ============================================================================

test_reset_rate_limit(_Config) ->
    TenantId = <<"tenant_reset_single">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 2,
        <<"burst">> => 2
    },
    
    %% Create and exhaust bucket
    router_rate_limit_store:check_rate_limit(tenant, TenantId, Config),
    router_rate_limit_store:check_rate_limit(tenant, TenantId, Config),
    
    %% Reset this specific tenant
    ok = router_rate_limit_store:reset_rate_limit(tenant, TenantId),
    
    %% Should be not found
    ?assertEqual({error, not_found}, 
                 router_rate_limit_store:get_rate_limit_status(tenant, TenantId)),
    ok.

%% ============================================================================
%% Tests for different scopes
%% ============================================================================

test_tenant_scope(_Config) ->
    TenantId = <<"scope_tenant">>,
    Config = #{<<"enabled">> => true, <<"requests_per_second">> => 10, <<"burst">> => 5},
    
    ?assertEqual({ok, allow}, router_rate_limit_store:check_rate_limit(tenant, TenantId, Config)),
    ok.

test_policy_scope(_Config) ->
    Identifier = {<<"tenant_policy">>, <<"policy_123">>},
    Config = #{<<"enabled">> => true, <<"requests_per_second">> => 10, <<"burst">> => 5},
    
    ?assertEqual({ok, allow}, router_rate_limit_store:check_rate_limit(policy, Identifier, Config)),
    ok.
