%% @doc Test suite for router_test_init ETS helpers
%% Tests ETS lifecycle helpers under normal and concurrent usage
%% @test_category cp1_smoke, fast
-module(router_test_init_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_ensure_ets_table_creates_new/1,
    test_ensure_ets_table_clears_existing/1,
    test_reset_ets_table_clears_data/1,
    test_reset_ets_table_on_missing/1,
    test_delete_ets_table/1,
    test_ets_exists/1,
    test_concurrent_ensure_ets/1,
    %% CP1-level concurrency sanity test (Task 7)
    test_concurrent_reset_sanity/1,
    %% ETS snapshot helpers (Task 4)
    test_ets_snapshot_diff/1,
    %% await_router_started regression (Task 6)
    test_await_router_started_timeout/1,
    %% Task 14: Verify runner module is loadable
    test_order_runner_loadable/1
]).

-define(TEST_TABLE, router_test_init_suite_table).

suite() -> [{timetrap, {minutes, 1}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}, {group, stress_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_ensure_ets_table_creates_new,
            test_ensure_ets_table_clears_existing,
            test_reset_ets_table_clears_data,
            test_reset_ets_table_on_missing,
            test_delete_ets_table,
            test_ets_exists,
            %% CP1-level concurrency sanity test (fast, deterministic)
            test_concurrent_reset_sanity,
            %% ETS snapshot helpers
            test_ets_snapshot_diff,
            %% await_router_started timeout regression
            test_await_router_started_timeout,
            %% Task 14: Verify runner module is loadable
            test_order_runner_loadable
        ]},
        {stress_tests, [sequence], [
            test_concurrent_ensure_ets
        ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    %% Clean up any leftover tables
    catch ets:delete(?TEST_TABLE),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Ensure clean state
    catch ets:delete(?TEST_TABLE),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up
    catch ets:delete(?TEST_TABLE),
    ok.

%% ============================================================================
%% UNIT TESTS
%% ============================================================================

%% @doc Test ensure_ets_table creates new table
test_ensure_ets_table_creates_new(_Config) ->
    ?assertEqual(false, router_test_init:ets_exists(?TEST_TABLE)),
    
    _Ref = router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    
    ?assertEqual(true, router_test_init:ets_exists(?TEST_TABLE)),
    ok.

%% @doc Test ensure_ets_table clears existing table
test_ensure_ets_table_clears_existing(_Config) ->
    %% Create table and insert data
    _ = router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    ets:insert(?TEST_TABLE, {key1, value1}),
    ets:insert(?TEST_TABLE, {key2, value2}),
    ?assertEqual(2, ets:info(?TEST_TABLE, size)),
    
    %% ensure_ets_table should clear existing data
    _Ref = router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    
    ?assertEqual(0, ets:info(?TEST_TABLE, size)),
    ok.

%% @doc Test reset_ets_table clears data
test_reset_ets_table_clears_data(_Config) ->
    %% Create table and insert data
    _ = router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    ets:insert(?TEST_TABLE, {key1, value1}),
    ?assertEqual(1, ets:info(?TEST_TABLE, size)),
    
    %% reset should clear
    ok = router_test_init:reset_ets_table(?TEST_TABLE),
    
    ?assertEqual(0, ets:info(?TEST_TABLE, size)),
    ok.

%% @doc Test reset_ets_table on missing table
test_reset_ets_table_on_missing(_Config) ->
    %% Should not crash on missing table
    ?assertEqual(false, router_test_init:ets_exists(?TEST_TABLE)),
    ok = router_test_init:reset_ets_table(?TEST_TABLE),
    ok.

%% @doc Test delete_ets_table
test_delete_ets_table(_Config) ->
    %% Create table
    _ = router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    ?assertEqual(true, router_test_init:ets_exists(?TEST_TABLE)),
    
    %% Delete
    ok = router_test_init:delete_ets_table(?TEST_TABLE),
    
    ?assertEqual(false, router_test_init:ets_exists(?TEST_TABLE)),
    
    %% Delete again should be safe
    ok = router_test_init:delete_ets_table(?TEST_TABLE),
    ok.

%% @doc Test ets_exists
test_ets_exists(_Config) ->
    %% Non-existent
    ?assertEqual(false, router_test_init:ets_exists(?TEST_TABLE)),
    
    %% Create
    _ = router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    ?assertEqual(true, router_test_init:ets_exists(?TEST_TABLE)),
    
    %% Delete
    ets:delete(?TEST_TABLE),
    ?assertEqual(false, router_test_init:ets_exists(?TEST_TABLE)),
    ok.

%% @doc CP1-level sanity test: concurrent reset/insert operations don't crash (Task 7)
%% Smaller scale than heavy test, fast and deterministic.
test_concurrent_reset_sanity(_Config) ->
    %% Create initial table
    router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    
    %% Small concurrency test (5 workers, 20 iterations each)
    NumWorkers = 5,
    NumIterations = 20,
    
    Parent = self(),
    Workers = [spawn_link(fun() ->
        concurrent_reset_sanity_worker(NumIterations),
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, NumWorkers)],
    
    %% Wait for all workers (with short timeout)
    lists:foreach(fun(Pid) ->
        receive
            {done, Pid} -> ok
        after 2000 ->
            ct:fail({timeout_waiting_for, Pid})
        end
    end, Workers),
    
    %% Table should still be usable after concurrent operations
    ?assertEqual(true, router_test_init:ets_exists(?TEST_TABLE)),
    ets:insert(?TEST_TABLE, {sanity_check, pass}),
    ?assertEqual([{sanity_check, pass}], ets:lookup(?TEST_TABLE, sanity_check)),
    ok.

concurrent_reset_sanity_worker(0) -> ok;
concurrent_reset_sanity_worker(N) ->
    %% Simpler operations than heavy test
    case rand:uniform(2) of
        1 -> router_test_init:reset_ets_table(?TEST_TABLE);
        2 -> catch ets:insert(?TEST_TABLE, {make_ref(), N})
    end,
    concurrent_reset_sanity_worker(N - 1).

%% ============================================================================
%% STRESS TESTS (heavy level only)
%% ============================================================================

%% @doc Test concurrent ETS operations don't cause crashes
test_concurrent_ensure_ets(_Config) ->
    %% Create initial table
    router_test_init:ensure_ets_table(?TEST_TABLE, [named_table, set, public]),
    
    %% Spawn multiple processes that concurrently reset the table
    NumWorkers = 10,
    NumIterations = 100,
    
    Parent = self(),
    Workers = [spawn_link(fun() ->
        concurrent_ets_worker(NumIterations),
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, NumWorkers)],
    
    %% Wait for all workers
    lists:foreach(fun(Pid) ->
        receive
            {done, Pid} -> ok
        after 10000 ->
            ct:fail({timeout_waiting_for, Pid})
        end
    end, Workers),
    
    %% Table should still be accessible
    ?assertEqual(true, router_test_init:ets_exists(?TEST_TABLE)),
    
    ok.

concurrent_ets_worker(0) -> ok;
concurrent_ets_worker(N) ->
    %% Random operations
    case rand:uniform(3) of
        1 -> router_test_init:reset_ets_table(?TEST_TABLE);
        2 -> ets:insert(?TEST_TABLE, {make_ref(), N});
        3 -> catch ets:lookup(?TEST_TABLE, make_ref())
    end,
    concurrent_ets_worker(N - 1).

%% ============================================================================
%% ETS SNAPSHOT TESTS (Task 4)
%% ============================================================================

%% @doc Test: ETS snapshot and diff functions work correctly
test_ets_snapshot_diff(_Config) ->
    TestTable1 = router_test_init_snapshot_1,
    TestTable2 = router_test_init_snapshot_2,
    
    %% Clean slate
    catch ets:delete(TestTable1),
    catch ets:delete(TestTable2),
    
    %% Snapshot before
    Before = router_test_init:snapshot_ets([router_test_init_]),
    ?assert(is_list(Before)),
    ?assertNot(lists:member(TestTable1, Before)),
    ?assertNot(lists:member(TestTable2, Before)),
    
    %% Create tables
    ets:new(TestTable1, [named_table, set, public]),
    ets:new(TestTable2, [named_table, set, public]),
    
    %% Snapshot after
    After = router_test_init:snapshot_ets([router_test_init_]),
    ?assert(lists:member(TestTable1, After)),
    ?assert(lists:member(TestTable2, After)),
    
    %% Diff should show additions
    {Added, Removed} = router_test_init:diff_ets_snapshot(Before, After),
    ?assert(lists:member(TestTable1, Added)),
    ?assert(lists:member(TestTable2, Added)),
    ?assertEqual([], Removed),
    
    %% Cleanup
    ets:delete(TestTable1),
    ets:delete(TestTable2),
    
    %% Diff in reverse direction
    AfterCleanup = router_test_init:snapshot_ets([router_test_init_]),
    {Added2, Removed2} = router_test_init:diff_ets_snapshot(After, AfterCleanup),
    ?assertEqual([], Added2),
    ?assert(lists:member(TestTable1, Removed2)),
    ?assert(lists:member(TestTable2, Removed2)),
    ok.

%% ============================================================================
%% AWAIT_ROUTER_STARTED TESTS (Task 6)
%% ============================================================================

%% @doc Regression: await_router_started fails fast with informative error
%% when router is not started
test_await_router_started_timeout(_Config) ->
    %% Save current router state
    WasRunning = whereis(beamline_router_sup) =/= undefined,
    
    %% If router is running, stop it for this test
    case WasRunning of
        true -> application:stop(beamline_router);
        false -> ok
    end,
    
    %% Verify router is not running
    ?assertEqual(undefined, whereis(beamline_router_sup)),
    
    %% await_router_started should fail with timeout
    try
        router_suite_helpers:await_router_started(100),  %% 100ms timeout
        ct:fail(expected_timeout_error)
    catch
        exit:{test_case_failed, ErrorInfo} ->
            %% Verify error is informative
            ?assert(is_tuple(ErrorInfo)),
            ?assertMatch({router_not_started_within_timeout, _, _, _, _}, ErrorInfo),
            ok
    end,
    
    %% Restore router if it was running
    case WasRunning of
        true ->
            _ = application:load(beamline_router),
            ok = application:set_env(beamline_router, grpc_port, 0),
            ok = application:set_env(beamline_router, grpc_enabled, false),
            ok = application:set_env(beamline_router, nats_mode, mock),
            {ok, _} = application:ensure_all_started(beamline_router);
        false -> 
            ok
    end,
    ok.

%% ============================================================================
%% ORDER RUNNER TESTS (Task 14)
%% ============================================================================

%% @doc Task 14: Verify router_cp1_order_runner module is loadable and functional
%% This ensures the stability runner helper doesn't rot.
test_order_runner_loadable(_Config) ->
    %% Verify module can be loaded
    {module, router_cp1_order_runner} = code:ensure_loaded(router_cp1_order_runner),
    
    %% Verify exported functions exist
    Exports = router_cp1_order_runner:module_info(exports),
    ?assert(lists:member({run, 0}, Exports)),
    ?assert(lists:member({run, 1}, Exports)),
    ?assert(lists:member({repeat_run, 0}, Exports)),
    ?assert(lists:member({repeat_run, 1}, Exports)),
    ?assert(lists:member({main, 1}, Exports)),
    
    ok.

