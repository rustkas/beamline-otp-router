%% @doc Delivery Count Tracking: Advanced Tests
%%
%% Advanced delivery count tests:
%% - Cleanup after ACK
%% - Concurrent tracking
%% - ACK failure scenarios
%% - Processing delays
%%
%% @test_category delivery, unit, heavy
-module(router_delivery_count_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, groups_for_level/1, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_cleanup_after_ack/1,
    test_concurrent_tracking/1,
    test_under_ack_failures/1,
    test_processing_delays_redelivery/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    groups_for_level(test_level()).

groups_for_level(heavy) -> [{group, advanced_tests}];
groups_for_level(full) -> [{group, advanced_tests}];
groups_for_level(_) -> [].

test_level() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end.

groups() ->
    [{advanced_tests, [sequence], [
        test_cleanup_after_ack,
        test_concurrent_tracking,
        test_under_ack_failures,
        test_processing_delays_redelivery
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

end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_cleanup_after_ack(_Config) ->
    ct:comment("=== Cleanup After ACK ==="),
    MsgId = <<"msg-test-6">>,
    
    Table = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    ok = router_result_consumer:track_delivery_count(MsgId),
    [{MsgId, 1}] = ets:lookup(Table, MsgId),
    
    ok = router_result_consumer:cleanup_delivery_count(MsgId),
    [] = ets:lookup(Table, MsgId),
    ok.

test_concurrent_tracking(_Config) ->
    ct:comment("=== Concurrent Tracking ==="),
    
    _ = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    NumConcurrent = 10,
    MsgIds = [<<"concurrent-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, NumConcurrent)],
    
    Pids = lists:map(fun(MsgId) ->
        spawn(fun() ->
            lists:foreach(fun(_) ->
                ok = router_result_consumer:track_delivery_count(MsgId),
                timer:sleep(10)
            end, lists:seq(1, 5))
        end)
    end, MsgIds),
    
    lists:foreach(fun(Pid) ->
        MRef = monitor(process, Pid),
        receive {'DOWN', MRef, process, Pid, _} -> ok after 5000 -> ok end
    end, Pids),
    
    lists:foreach(fun(MsgId) ->
        case ets:lookup(router_delivery_count, MsgId) of
            [{MsgId, Count}] -> ?assertEqual(5, Count);
            [] -> ok
        end
    end, MsgIds),
    ok.

test_under_ack_failures(_Config) ->
    ct:comment("=== Under ACK Failures ==="),
    MsgId = <<"ack-fail-msg">>,
    
    _ = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    lists:foreach(fun(_) ->
        ok = router_result_consumer:track_delivery_count(MsgId),
        timer:sleep(50)
    end, lists:seq(1, 3)),
    
    [{MsgId, 3}] = ets:lookup(router_delivery_count, MsgId),
    ok.

test_processing_delays_redelivery(_Config) ->
    ct:comment("=== Processing Delays Redelivery ==="),
    MsgId = <<"delay-msg">>,
    
    _ = router_test_init:ensure_ets_table(router_delivery_count, [set, named_table, public, {write_concurrency, true}]),
    
    ok = router_result_consumer:track_delivery_count(MsgId),
    timer:sleep(100),
    ok = router_result_consumer:track_delivery_count(MsgId),
    timer:sleep(100),
    ok = router_result_consumer:track_delivery_count(MsgId),
    
    [{MsgId, 3}] = ets:lookup(router_delivery_count, MsgId),
    ok.
