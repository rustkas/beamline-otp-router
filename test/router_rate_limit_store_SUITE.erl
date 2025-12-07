%% @doc Common Test Suite for Router Rate Limit Store
%% Tests rate limiting with token bucket algorithm
%% @test_category cp2_rate_limiting, fast
-module(router_rate_limit_store_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_rate_limit_allowed/1,
    test_rate_limit_exceeded/1,
    test_rate_limit_burst/1,
    test_rate_limit_refill/1,
    test_rate_limit_disabled/1,
    test_rate_limit_reset/1,
    test_rate_limit_status/1,
    test_rate_limit_concurrent/1,
    test_rate_limit_config_change/1,
    test_rate_limit_scope_isolation/1,
    test_rate_limit_error_handling/1,
    test_rate_limit_restart_behavior/1
]}).

all() ->
    [
        {group, unit_tests},
        {group, edge_case_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_rate_limit_allowed,
            test_rate_limit_exceeded,
            test_rate_limit_burst,
            test_rate_limit_refill,
            test_rate_limit_disabled,
            test_rate_limit_reset,
            test_rate_limit_status
        ]},
        {edge_case_tests, [parallel], [
            test_rate_limit_concurrent,
            test_rate_limit_config_change,
            test_rate_limit_scope_isolation,
            test_rate_limit_error_handling,
            test_rate_limit_restart_behavior
        ]}
    ].

init_per_suite(Config) ->
    %% Ensure all required applications are started
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, disable_heir, true),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> ok;
        {error, {already_started, beamline_router}} -> ok;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end,
    
    %% Wait for router_rate_limit_store process to be ready
    test_helpers:wait_for_app_start(router_rate_limit_store, 1000),
    
    Config.

end_per_suite(Config) ->
    application:stop(beamline_router),
    application:unload(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% Test: Rate limit allows requests within limit
test_rate_limit_allowed(_Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% First request should be allowed
    Result1 = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    ?assertEqual({ok, allow}, Result1),
    
    %% Second request should also be allowed
    Result2 = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    ?assertEqual({ok, allow}, Result2),
    
    ok.

%% Test: Rate limit blocks requests when exceeded
test_rate_limit_exceeded(_Config) ->
    TenantId = <<"test_tenant_exceeded">>,
    PolicyId = <<"test_policy_exceeded">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 2,
        <<"burst">> => 2
    },
    
    %% Consume all tokens (burst = 2)
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    
    %% Next request should be blocked
    Result = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    ?assertMatch({error, {rate_limit_exceeded, _}}, Result),
    
    ok.

%% Test: Burst capacity allows initial burst
test_rate_limit_burst(_Config) ->
    TenantId = <<"test_tenant_burst">>,
    PolicyId = <<"test_policy_burst">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% Consume burst capacity (5 requests)
    Results = [router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config) || _ <- lists:seq(1, 5)],
    ?assertEqual([{ok, allow}, {ok, allow}, {ok, allow}, {ok, allow}, {ok, allow}], Results),
    
    %% Next request should be blocked (no tokens left, no time elapsed)
    Result = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    ?assertMatch({error, {rate_limit_exceeded, _}}, Result),
    
    ok.

%% Test: Tokens refill over time
test_rate_limit_refill(_Config) ->
    TenantId = <<"test_tenant_refill">>,
    PolicyId = <<"test_policy_refill">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 10
    },
    
    %% Consume all tokens
    [router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config) || _ <- lists:seq(1, 10)],
    
    %% Wait 1 second for refill
    timer:sleep(1100),
    
    %% Should have tokens available now (at least 10 tokens refilled)
    Result = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    ?assertEqual({ok, allow}, Result),
    
    ok.

%% Test: Rate limit disabled allows all requests
test_rate_limit_disabled(_Config) ->
    TenantId = <<"test_tenant_disabled">>,
    PolicyId = <<"test_policy_disabled">>,
    Config = #{
        <<"enabled">> => false,
        <<"requests_per_second">> => 1,
        <<"burst">> => 1
    },
    
    %% Even with very low limits, should allow when disabled
    Results = [router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config) || _ <- lists:seq(1, 10)],
    ?assertEqual([{ok, allow} || _ <- lists:seq(1, 10)], Results),
    
    ok.

%% Test: Reset rate limit clears bucket
test_rate_limit_reset(_Config) ->
    TenantId = <<"test_tenant_reset">>,
    PolicyId = <<"test_policy_reset">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 2,
        <<"burst">> => 2
    },
    
    %% Consume all tokens
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    
    %% Should be blocked
    ?assertMatch({error, {rate_limit_exceeded, _}}, 
                 router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config)),
    
    %% Reset
    ok = router_rate_limit_store:reset_rate_limit(policy, {TenantId, PolicyId}),
    
    %% Should be allowed again
    ?assertEqual({ok, allow}, 
                 router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config)),
    
    ok.

%% Test: Get rate limit status
test_rate_limit_status(_Config) ->
    TenantId = <<"test_tenant_status">>,
    PolicyId = <<"test_policy_status">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% Check status before any requests
    {error, not_found} = router_rate_limit_store:get_rate_limit_status(policy, {TenantId, PolicyId}),
    
    %% Make a request to create bucket
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    
    %% Check status
    {ok, Status} = router_rate_limit_store:get_rate_limit_status(policy, {TenantId, PolicyId}),
    ?assert(is_map(Status)),
    ?assert(maps:is_key(<<"tokens">>, Status) orelse maps:is_key(tokens, Status)),
    ?assert(maps:is_key(<<"limit">>, Status) orelse maps:is_key(limit, Status)),
    ?assert(maps:is_key(<<"burst">>, Status) orelse maps:is_key(burst, Status)),
    
    ok.

%% Test: Concurrent rate limit checks (race conditions)
test_rate_limit_concurrent(_Config) ->
    TenantId = <<"test_tenant_concurrent">>,
    PolicyId = <<"test_policy_concurrent">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% Spawn multiple concurrent requests
    Pids = [spawn(fun() ->
        Result = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
        self() ! {result, Result}
    end) || _ <- lists:seq(1, 10)],
    
    %% Collect results
    Results = [receive {result, R} -> R end || _ <- Pids],
    
    %% Verify: exactly 5 should be allowed (burst = 5), rest should be exceeded
    AllowedCount = length([R || R <- Results, R =:= {ok, allow}]),
    ExceededCount = length([R || R <- Results, R =/= {ok, allow}]),
    
    ?assertEqual(5, AllowedCount, "Burst capacity should allow exactly 5 requests"),
    ?assertEqual(5, ExceededCount, "Remaining requests should be rate limited"),
    
    ok.

%% Test: Configuration change handling
test_rate_limit_config_change(_Config) ->
    TenantId = <<"test_tenant_config_change">>,
    PolicyId = <<"test_policy_config_change">>,
    Config1 = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    Config2 = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 20,
        <<"burst">> => 10
    },
    
    %% Use first configuration
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config1),
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config1),
    
    %% Reset bucket
    ok = router_rate_limit_store:reset_rate_limit(policy, {TenantId, PolicyId}),
    
    %% Use second configuration (different RPS and burst)
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config2),
    
    %% Verify new configuration is used (burst = 10, so we can make more requests)
    Results = [router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config2) || _ <- lists:seq(1, 10)],
    AllowedCount = length([R || R <- Results, R =:= {ok, allow}]),
    
    ?assert(AllowedCount >= 9, "New burst capacity (10) should allow at least 9 requests"),
    
    ok.

%% Test: Scope isolation (policy vs tenant)
test_rate_limit_scope_isolation(_Config) ->
    TenantId = <<"test_tenant_scope">>,
    PolicyId1 = <<"test_policy_1">>,
    PolicyId2 = <<"test_policy_2">>,
    PolicyConfig = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 2,
        <<"burst">> => 2
    },
    TenantConfig = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 5,
        <<"burst">> => 5
    },
    
    %% Consume all tokens for Policy 1
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId1}, PolicyConfig),
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId1}, PolicyConfig),
    ?assertMatch({error, {rate_limit_exceeded, _}}, 
                 router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId1}, PolicyConfig)),
    
    %% Policy 2 should still work (independent bucket)
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId2}, PolicyConfig),
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId2}, PolicyConfig),
    
    %% Tenant scope should also be independent
    {ok, allow} = router_rate_limit_store:check_rate_limit(tenant, TenantId, TenantConfig),
    {ok, allow} = router_rate_limit_store:check_rate_limit(tenant, TenantId, TenantConfig),
    
    ok.

%% Test: Error handling (fail open)
test_rate_limit_error_handling(_Config) ->
    TenantId = <<"test_tenant_error">>,
    PolicyId = <<"test_policy_error">>,
    
    %% Test: Invalid configuration (missing required fields)
    InvalidConfig1 = #{
        <<"enabled">> => true
        %% Missing requests_per_second and burst
    },
    %% Should use defaults or fail open
    Result1 = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, InvalidConfig1),
    ?assertMatch({ok, allow}, Result1, "Invalid config should fail open (use defaults)"),
    
    %% Test: Negative values (should be validated)
    InvalidConfig2 = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => -10,
        <<"burst">> => -5
    },
    Result2 = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, InvalidConfig2),
    %% Should validate and use defaults (min 1)
    ?assertMatch({ok, allow}, Result2, "Negative values should be validated to minimum 1"),
    
    %% Test: Zero values (should be validated)
    InvalidConfig3 = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 0,
        <<"burst">> => 0
    },
    Result3 = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, InvalidConfig3),
    %% Should validate and use defaults (min 1)
    ?assertMatch({ok, allow}, Result3, "Zero values should be validated to minimum 1"),
    
    %% Test: Very large values (should be capped)
    InvalidConfig4 = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 999999999,
        <<"burst">> => 999999999
    },
    Result4 = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, InvalidConfig4),
    %% Should be capped at maximum (1000000)
    ?assertMatch({ok, allow}, Result4, "Very large values should be capped"),
    
    ok.

%% Test: Restart behavior (bucket recreation)
test_rate_limit_restart_behavior(_Config) ->
    TenantId = <<"test_tenant_restart">>,
    PolicyId = <<"test_policy_restart">>,
    Config = #{
        <<"enabled">> => true,
        <<"requests_per_second">> => 10,
        <<"burst">> => 5
    },
    
    %% Consume all tokens
    [router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config) || _ <- lists:seq(1, 5)],
    
    %% Verify bucket is exhausted
    ?assertMatch({error, {rate_limit_exceeded, _}}, 
                 router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config)),
    
    %% Simulate restart: reset bucket
    ok = router_rate_limit_store:reset_rate_limit(policy, {TenantId, PolicyId}),
    
    %% After restart, first request should get full burst capacity
    {ok, allow} = router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config),
    
    %% Should be able to consume burst again
    Results = [router_rate_limit_store:check_rate_limit(policy, {TenantId, PolicyId}, Config) || _ <- lists:seq(1, 5)],
    AllowedCount = length([R || R <- Results, R =:= {ok, allow}]),
    
    ?assert(AllowedCount >= 4, "After restart, should get full burst capacity again"),
    
    ok.

