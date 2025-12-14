%% @doc Idempotency: Core Tests
%%
%% Core idempotency tests:
%% - Basic duplicate check
%% - Remember and evict
%% - TTL expiration
%% - TTL edge cases
%%
%% @test_category idempotency, unit
-module(router_idem_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_basic_dup_check/1,
    test_remember_and_evict/1,
    test_ttl_expiration/1,
    test_ttl_edge_case_zero/1,
    test_ttl_edge_case_negative/1,
    test_ttl_edge_case_very_large/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    [].

groups() ->
    [{core_tests, [sequence], [
        test_basic_dup_check,
        test_remember_and_evict,
        test_ttl_expiration,
        test_ttl_edge_case_zero,
        test_ttl_edge_case_negative,
        test_ttl_edge_case_very_large
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    case ets:info(router_idem) of
        undefined -> ok;
        _ -> catch router_idem:evict_all(), ets:delete(router_idem)
    end,
    Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_TC, Config) ->
    case ets:info(router_idem) of
        undefined -> router_idem:init([]);
        _ -> catch router_idem:evict_all()
    end,
    Config.

end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_basic_dup_check(_Config) ->
    ct:comment("=== Basic Duplicate Check ==="),
    ok = router_idem:init([]),
    Key = <<"test-key-1">>,
    
    ?assertNot(router_idem:is_dup(Key)),
    ok = router_idem:remember(Key, 1000),
    ?assert(router_idem:is_dup(Key)),
    ok = router_idem:evict(Key),
    ?assertNot(router_idem:is_dup(Key)),
    ok.

test_remember_and_evict(_Config) ->
    ct:comment("=== Remember and Evict ==="),
    ok = router_idem:init([]),
    Key1 = <<"key-1">>,
    Key2 = <<"key-2">>,
    
    ok = router_idem:remember(Key1, 1000),
    ok = router_idem:remember(Key2, 2000),
    
    ?assert(router_idem:is_dup(Key1)),
    ?assert(router_idem:is_dup(Key2)),
    
    ok = router_idem:evict(Key1),
    ?assertNot(router_idem:is_dup(Key1)),
    ?assert(router_idem:is_dup(Key2)),
    ok.

test_ttl_expiration(_Config) ->
    ct:comment("=== TTL Expiration ==="),
    ok = router_idem:init([]),
    Key = <<"ttl-key">>,
    
    ok = router_idem:remember(Key, 100),
    ?assert(router_idem:is_dup(Key)),
    
    timer:sleep(150),
    
    ?assertNot(router_idem:is_dup(Key)),
    ok.

test_ttl_edge_case_zero(_Config) ->
    ct:comment("=== TTL Edge Case: Zero ==="),
    ok = router_idem:init([]),
    Key = <<"zero-ttl-key">>,
    
    %% TTL=0 is treated as invalid (must be > 0)
    {error, invalid_ttl} = router_idem:remember(Key, 0),
    ?assertNot(router_idem:is_dup(Key)),
    ok.

test_ttl_edge_case_negative(_Config) ->
    ct:comment("=== TTL Edge Case: Negative ==="),
    ok = router_idem:init([]),
    Key = <<"negative-ttl-key">>,
    
    {error, invalid_ttl} = router_idem:remember(Key, -100),
    ?assertNot(router_idem:is_dup(Key)),
    ok.

test_ttl_edge_case_very_large(_Config) ->
    ct:comment("=== TTL Edge Case: Very Large ==="),
    ok = router_idem:init([]),
    Key = <<"large-ttl-key">>,
    
    LargeTTL = 365 * 24 * 60 * 60 * 1000,
    ok = router_idem:remember(Key, LargeTTL),
    ?assert(router_idem:is_dup(Key)),
    ok.
