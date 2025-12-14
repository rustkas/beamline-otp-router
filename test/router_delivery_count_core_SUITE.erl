%% @doc Delivery Count Tracking: Core Tests
%%
%% Core delivery count tests:
%% - Basic tracking
%% - Increment
%% - MaxDeliver exhaustion
%%
%% @test_category delivery, unit
-module(router_delivery_count_core_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_basic_tracking/1,
    test_delivery_count_increment/1,
    test_maxdeliver_exhaustion_emits_metric/1,
    test_maxdeliver_exhaustion_removes_tracking/1,
    test_maxdeliver_not_exhausted/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    [].

groups() ->
    [{core_tests, [sequence], [
        test_basic_tracking,
        test_delivery_count_increment,
        test_maxdeliver_exhaustion_emits_metric,
        test_maxdeliver_exhaustion_removes_tracking,
        test_maxdeliver_not_exhausted
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    _ = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) ->
    _ = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    ets:delete_all_objects(router_delivery_count),
    Config.

end_per_testcase(_TC, _Config) ->
    ets:delete_all_objects(router_delivery_count),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_basic_tracking(_Config) ->
    ct:comment("=== Basic Tracking ==="),
    MsgId = <<"msg-test-1">>,
    
    Table = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    ets:delete_all_objects(Table),
    
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    case ets:lookup(Table, MsgId) of
        [{MsgId, 1}] -> ok;
        [] -> ct:fail("Delivery count not tracked")
    end,
    ok.

test_delivery_count_increment(_Config) ->
    ct:comment("=== Delivery Count Increment ==="),
    MsgId = <<"msg-test-2">>,
    
    _ = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    [{MsgId, 3}] = ets:lookup(router_delivery_count, MsgId),
    ok.

test_maxdeliver_exhaustion_emits_metric(_Config) ->
    ct:comment("=== MaxDeliver Exhaustion Emits Metric ==="),
    MsgId = <<"msg-test-3">>,
    AssignmentId = <<"assign-test-3">>,
    RequestId = <<"req-test-3">>,
    ErrorContext = #{reason => <<"test">>, source => <<"test">>},
    
    Table = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    MaxDeliver = 5,
    ets:insert(Table, {MsgId, MaxDeliver}),
    
    ok = router_result_consumer:check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
    ok.

test_maxdeliver_exhaustion_removes_tracking(_Config) ->
    ct:comment("=== MaxDeliver Exhaustion Removes Tracking ==="),
    MsgId = <<"msg-test-4">>,
    AssignmentId = <<"assign-test-4">>,
    RequestId = <<"req-test-4">>,
    ErrorContext = #{reason => <<"test">>, source => <<"test">>},
    
    Table = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    MaxDeliver = 5,
    ets:insert(Table, {MsgId, MaxDeliver}),
    
    ok = router_result_consumer:check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
    
    [] = ets:lookup(router_delivery_count, MsgId),
    ok.

test_maxdeliver_not_exhausted(_Config) ->
    ct:comment("=== MaxDeliver Not Exhausted ==="),
    MsgId = <<"msg-test-5">>,
    AssignmentId = <<"assign-test-5">>,
    RequestId = <<"req-test-5">>,
    ErrorContext = #{reason => <<"test">>, source => <<"test">>},
    
    Table = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    ets:insert(Table, {MsgId, 2}),
    
    ok = router_result_consumer:check_maxdeliver_exhaustion(MsgId, AssignmentId, RequestId, ErrorContext),
    
    [{MsgId, 2}] = ets:lookup(router_delivery_count, MsgId),
    ok.
