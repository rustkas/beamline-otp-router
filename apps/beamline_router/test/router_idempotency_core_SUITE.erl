%% @doc Idempotency: Core Integration Tests
%%
%% Core idempotency integration tests
%%
%% @test_category idempotency, integration
-module(router_idempotency_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_basic_idempotency/1,
    test_duplicate_request/1,
    test_ttl_expiration/1,
    test_cleanup/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [];
groups_for_level(full) ->
    [];
groups_for_level(_) ->
    [].
groups() ->
    [{core_tests, [sequence], [
        test_basic_idempotency,
        test_duplicate_request,
        test_ttl_expiration,
        test_cleanup
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) ->
    %% Ensure ETS table exists
    case ets:info(router_idem) of
        undefined -> router_idem:init([]);
        _ -> catch router_idem:evict_all()
    end,
    Config.

end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_basic_idempotency(_Config) ->
    ct:comment("=== Basic Idempotency ==="),
    Key = <<"idem-key-1">>,
    
    ?assertNot(router_idem:is_dup(Key)),
    ok = router_idem:remember(Key, 1000),
    ?assert(router_idem:is_dup(Key)),
    ok.

test_duplicate_request(_Config) ->
    ct:comment("=== Duplicate Request ==="),
    Key = <<"idem-key-2">>,
    
    {ok, not_seen} = router_idem:check_and_mark(Key, 1000),
    {ok, seen, _Timestamp} = router_idem:check_and_mark(Key, 1000),
    ok.

test_ttl_expiration(_Config) ->
    ct:comment("=== TTL Expiration ==="),
    Key = <<"idem-key-3">>,
    
    ok = router_idem:remember(Key, 100),
    ?assert(router_idem:is_dup(Key)),
    timer:sleep(150),
    ?assertNot(router_idem:is_dup(Key)),
    ok.

test_cleanup(_Config) ->
    ct:comment("=== Cleanup ==="),
    Keys = [<<"cleanup-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 10)],
    
    lists:foreach(fun(K) -> ok = router_idem:remember(K, 1000) end, Keys),
    _EvictedCount = router_idem:evict_all(),
    lists:foreach(fun(K) -> ?assertNot(router_idem:is_dup(K)) end, Keys),
    ok.
