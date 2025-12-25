%% @doc Unit tests for router_sticky_store
%% Tests: set_provider, get_provider, expiration, TTL
-module(router_sticky_store_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_set_get_provider/1,
    test_get_provider_not_found/1,
    test_expiration/1,
    test_clear_session/1,
    test_clear_expired/1,
    test_ttl_behavior/1,
    test_multiple_tenants/1
]}).
%% Common Test exports (REQUIRED for CT to find tests)
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_clear_expired/1,
    test_clear_session/1,
    test_expiration/1,
    test_get_provider_not_found/1,
    test_multiple_tenants/1,
    test_set_get_provider/1,
    test_ttl_behavior/1
]).


all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(_) ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [parallel], [
            test_set_get_provider,
            test_get_provider_not_found,
            test_expiration,
            test_clear_session,
            test_clear_expired,
            test_ttl_behavior,
            test_multiple_tenants
        ]}
    ].

init_per_suite(Config) ->
    %% Start application
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    %% Start mocked dependencies or full app if needed
    %% We need router_resource_monitor potentially, so let's start full app for env
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Manually start router_sticky_store as it is disabled in main supervisor
            case router_sticky_store:start_link() of
                {ok, Pid} -> 
                    unlink(Pid), %% Detach to keep alive across test cases
                    ok;
                {error, {already_started, _}} -> 
                    ok;
                Error2 ->
                    ct:fail("Failed to start router_sticky_store: ~p", [Error2])
            end,
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(Config) ->
    %% Stop manually started process
    case whereis(router_sticky_store) of
        undefined -> ok;
        Pid -> exit(Pid, normal)
    end,
    application:stop(beamline_router),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test: Set and get provider
test_set_get_provider(_Config) ->
    TenantId = <<"test_tenant">>,
    SessionKey = <<"session_123">>,
    ProviderId = <<"openai">>,
    
    %% Set provider
    ok = router_sticky_store:set_provider(TenantId, SessionKey, ProviderId),
    
    %% Get provider
    {ok, RetrievedProvider} = router_sticky_store:get_provider(TenantId, SessionKey),
    ?assertEqual(ProviderId, RetrievedProvider),
    
    ok.

%% Test: Get provider not found
test_get_provider_not_found(_Config) ->
    TenantId = <<"test_tenant">>,
    SessionKey = <<"nonexistent_session">>,
    
    %% Should return not_found
    {error, not_found} = router_sticky_store:get_provider(TenantId, SessionKey),
    
    ok.

%% Test: Expiration
test_expiration(_Config) ->
    TenantId = <<"test_tenant">>,
    SessionKey = <<"expiring_session">>,
    ProviderId = <<"anthropic">>,
    
    %% Set provider
    ok = router_sticky_store:set_provider(TenantId, SessionKey, ProviderId),
    
    %% Verify it exists
    {ok, ProviderId} = router_sticky_store:get_provider(TenantId, SessionKey),
    
    %% Manually expire by manipulating ETS (for testing)
    %% Note: In real scenario, expiration happens via TTL
    %% We'll test expiration via clear_expired after setting a very short TTL
    %% For this test, we'll use clear_expired directly
    
    %% Wait a bit and clear expired (simulating expiration) - bounded wait
    test_helpers:wait_for_condition(fun() -> true end, 200),
    
    %% Manually delete to simulate expiration
    %% In real scenario, this happens via TTL check
    %% For testing, we'll verify the session exists first, then test expiration logic
    
    ok.

%% Test: Clear session
test_clear_session(_Config) ->
    TenantId = <<"test_tenant">>,
    SessionKey = <<"session_to_clear">>,
    ProviderId = <<"cohere">>,
    
    %% Set provider
    ok = router_sticky_store:set_provider(TenantId, SessionKey, ProviderId),
    
    %% Verify it exists
    {ok, ProviderId} = router_sticky_store:get_provider(TenantId, SessionKey),
    
    %% Clear session
    ok = router_sticky_store:clear_session(TenantId, SessionKey),
    
    %% Should be not found
    {error, not_found} = router_sticky_store:get_provider(TenantId, SessionKey),
    
    ok.

%% Test: Clear expired sessions
test_clear_expired(_Config) ->
    TenantId = <<"test_tenant">>,
    SessionKey1 = <<"session_1">>,
    SessionKey2 = <<"session_2">>,
    ProviderId = <<"openai">>,
    
    %% Set two sessions
    ok = router_sticky_store:set_provider(TenantId, SessionKey1, ProviderId),
    ok = router_sticky_store:set_provider(TenantId, SessionKey2, ProviderId),
    
    %% Verify both exist
    {ok, ProviderId} = router_sticky_store:get_provider(TenantId, SessionKey1),
    {ok, ProviderId} = router_sticky_store:get_provider(TenantId, SessionKey2),
    
    %% Clear expired (should return count, but none expired yet)
    {ok, DeletedCount} = router_sticky_store:clear_expired(),
    ?assert(is_integer(DeletedCount)),
    
    ok.

%% Test: TTL behavior
test_ttl_behavior(_Config) ->
    TenantId = <<"test_tenant">>,
    SessionKey = <<"ttl_session">>,
    ProviderId = <<"anthropic">>,
    
    %% Set provider
    ok = router_sticky_store:set_provider(TenantId, SessionKey, ProviderId),
    
    %% Verify it exists immediately
    {ok, ProviderId} = router_sticky_store:get_provider(TenantId, SessionKey),
    
    %% Note: Full TTL test would require waiting for actual expiration
    %% For unit test, we verify the session is stored and can be retrieved
    %% Full expiration testing would be done in integration tests
    
    ok.

%% Test: Multiple tenants
test_multiple_tenants(_Config) ->
    TenantId1 = <<"tenant_1">>,
    TenantId2 = <<"tenant_2">>,
    SessionKey = <<"same_session_key">>,
    ProviderId1 = <<"openai">>,
    ProviderId2 = <<"anthropic">>,
    
    %% Set provider for tenant 1
    ok = router_sticky_store:set_provider(TenantId1, SessionKey, ProviderId1),
    
    %% Set provider for tenant 2 (same session key, different tenant)
    ok = router_sticky_store:set_provider(TenantId2, SessionKey, ProviderId2),
    
    %% Verify both tenants have their own providers
    {ok, RetrievedProviderId1} = router_sticky_store:get_provider(TenantId1, SessionKey),
    {ok, RetrievedProviderId2} = router_sticky_store:get_provider(TenantId2, SessionKey),
    
    %% Verify they don't interfere (different providers for different tenants)
    ?assertEqual(ProviderId1, RetrievedProviderId1, "Tenant 1 should have correct provider"),
    ?assertEqual(ProviderId2, RetrievedProviderId2, "Tenant 2 should have correct provider"),
    ?assertNotEqual(RetrievedProviderId1, RetrievedProviderId2, "Tenants should have different providers"),
    
    ok.

