%% @doc Idempotency: Advanced Tests
%%
%% Advanced idempotency tests:
%% - Evict expired mass cleanup
%% - Evict all emergency
%% - Concurrent operations
%% - Metrics integration
%% - Telemetry events
%% - Check and mark atomic
%%
%% @test_category idempotency, unit, heavy
-module(router_idem_advanced_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_evict_expired_mass_cleanup/1,
    test_evict_all_emergency/1,
    test_concurrent_operations/1,
    test_metrics_integration/1,
    test_telemetry_events/1,
    test_check_and_mark_atomic/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, advanced_tests}];
        "full" -> [{group, advanced_tests}];
        _ -> []
    end.

groups() ->
    [{advanced_tests, [sequence], [
        test_evict_expired_mass_cleanup,
        test_evict_all_emergency,
        test_concurrent_operations,
        test_metrics_integration,
        test_telemetry_events,
        test_check_and_mark_atomic
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

end_per_testcase(_TC, _Config) ->
    catch meck:unload(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_evict_expired_mass_cleanup(_Config) ->
    ct:comment("=== Evict Expired Mass Cleanup ==="),
    ok = router_idem:init([]),
    
    ExpiredKeys = [<<"expired-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 50)],
    ValidKeys = [<<"valid-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 50)],
    
    lists:foreach(fun(Key) -> ok = router_idem:remember(Key, 50) end, ExpiredKeys),
    lists:foreach(fun(Key) -> ok = router_idem:remember(Key, 10000) end, ValidKeys),
    
    Start = erlang:monotonic_time(millisecond),
    ok = test_helpers:wait_for_condition(
        fun() -> erlang:monotonic_time(millisecond) - Start >= 60 end, 200),
    
    _ = router_idem:evict_expired(),
    
    lists:foreach(fun(Key) -> ?assertNot(router_idem:is_dup(Key)) end, ExpiredKeys),
    lists:foreach(fun(Key) -> ?assert(router_idem:is_dup(Key)) end, ValidKeys),
    ok.

test_evict_all_emergency(_Config) ->
    ct:comment("=== Evict All Emergency ==="),
    ok = router_idem:init([]),
    
    Keys = [<<"key-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 20)],
    lists:foreach(fun(Key) -> ok = router_idem:remember(Key, 1000) end, Keys),
    
    ?assert(ets:info(router_idem, size) > 0),
    %% evict_all returns the count of evicted entries
    EvictedCount = router_idem:evict_all(),
    ?assertEqual(20, EvictedCount),
    ?assertEqual(0, ets:info(router_idem, size)),
    ok.

test_concurrent_operations(_Config) ->
    ct:comment("=== Concurrent Operations ==="),
    ok = router_idem:init([]),
    
    Keys = [<<"concurrent-", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 50)],
    
    Pids = lists:map(fun(Key) ->
        spawn(fun() ->
            ok = router_idem:remember(Key, 1000),
            timer:sleep(rand:uniform(100)),
            _ = router_idem:is_dup(Key)
        end)
    end, Keys),
    
    lists:foreach(fun(Pid) ->
        MRef = monitor(process, Pid),
        receive {'DOWN', MRef, process, Pid, _} -> ok after 5000 -> ok end
    end, Pids),
    ok.

test_metrics_integration(_Config) ->
    ct:comment("=== Metrics Integration ==="),
    ok = router_idem:init([]),
    
    meck:new(router_metrics, [passthrough]),
    meck:expect(router_metrics, inc, fun(_Metric) -> ok end),
    
    Key = <<"metrics-key">>,
    ?assertNot(router_idem:is_dup(Key)),
    ok = router_idem:remember(Key, 1000),
    ?assert(router_idem:is_dup(Key)),
    
    meck:unload(router_metrics),
    ok.

test_telemetry_events(_Config) ->
    ct:comment("=== Telemetry Events ==="),
    ok = router_idem:init([]),
    
    TelemetryEvents = router_test_init:ensure_ets_table(telemetry_events, [named_table, set, private]),
    
    meck:new(telemetry, [passthrough]),
    meck:expect(telemetry, execute, fun(Event, _Measurements, _Metadata) ->
        ets:insert(TelemetryEvents, {Event, 1}),
        ok
    end),
    
    Key = <<"telemetry-key">>,
    ?assertNot(router_idem:is_dup(Key)),
    ok = router_idem:remember(Key, 1000),
    
    ets:delete_all_objects(TelemetryEvents),
    meck:unload(telemetry),
    ok.

test_check_and_mark_atomic(_Config) ->
    ct:comment("=== Check and Mark Atomic ==="),
    ok = router_idem:init([]),
    Key = <<"atomic-key-1">>,
    TTL = 1000,
    
    {ok, not_seen} = router_idem:check_and_mark(Key, TTL),
    {ok, seen, _Timestamp} = router_idem:check_and_mark(Key, TTL),
    {ok, seen, _Timestamp2} = router_idem:check_and_mark(Key, TTL),
    ok.
