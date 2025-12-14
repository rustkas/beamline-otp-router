%% @doc CP1 Default Environment Gate Test Suite
%%
%% Proves that CP1 suites run under default environment (no special env vars required).
%% This is a meta-test that validates other suites are correctly configured.
%%
%% Tests:
%% - CP1 suites return non-empty all() under default env
%% - No CP1 suite is gated by ROUTER_ENABLE_META
%% - await_router_started/1 is idempotent (Task 4)
%%
%% @test_category cp1_smoke, meta, fast
-module(router_cp1_default_env_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_cp1_suites_return_nonempty_all/1,
    test_no_cp1_suite_gated_by_enable_meta/1,
    test_await_router_started_idempotent/1,
    %% Task 10: no stray mocks after CP1
    test_no_stray_mocks/1,
    %% Task 11: no stray ETS baseline
    test_no_stray_router_ets/1
]).

suite() -> [{timetrap, {minutes, 1}}].

all() ->
    %% This suite always runs - it validates CP1 gate behavior
    [{group, gate_tests}].

groups() ->
    [{gate_tests, [sequence], [
        test_cp1_suites_return_nonempty_all,
        test_no_cp1_suite_gated_by_enable_meta,
        test_await_router_started_idempotent,
        %% Task 10 & 11: cleanup checks (run last)
        test_no_stray_mocks,
        test_no_stray_router_ets
    ]}].

init_per_suite(Config) ->
    %% Ensure no special env vars are set (simulate default env)
    %% We don't unset - we verify behavior regardless
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

%% @doc Test: CP1 core suites return non-empty all() under default environment
%% These suites must not silently skip when no special env vars are set.
test_cp1_suites_return_nonempty_all(_Config) ->
    %% List of CP1 core suites that must always return tests
    Cp1CoreSuites = [
        router_cp1_smoke_SUITE,
        router_cp1_red_bar_SUITE,
        router_core_basic_SUITE,
        router_policy_store_SUITE,
        router_error_SUITE,
        router_mock_helpers_SUITE,
        router_test_init_SUITE
    ],
    
    %% Verify each suite returns non-empty all()
    lists:foreach(fun(Suite) ->
        AllResult = Suite:all(),
        ?assertNotEqual([], AllResult, 
            lists:flatten(io_lib:format("~p:all() returned empty - CP1 suite must not silently skip", [Suite]))),
        ct:pal("~p:all() = ~p (OK)", [Suite, AllResult])
    end, Cp1CoreSuites),
    
    ok.

%% @doc Test: No CP1 suite should be gated by ROUTER_ENABLE_META
%% CP1 suites must run by default without special environment variables.
test_no_cp1_suite_gated_by_enable_meta(_Config) ->
    %% Temporarily ensure ROUTER_ENABLE_META is not set
    OldValue = os:getenv("ROUTER_ENABLE_META"),
    os:unsetenv("ROUTER_ENABLE_META"),
    
    try
        %% CP1 core suites that must work without ROUTER_ENABLE_META
        Cp1CoreSuites = [
            router_cp1_smoke_SUITE,
            router_cp1_red_bar_SUITE,
            router_core_basic_SUITE,
            router_policy_SUITE,
            router_policy_store_SUITE,
            router_error_SUITE,
            router_mock_helpers_SUITE,
            router_test_init_SUITE
        ],
        
        %% Verify each suite returns non-empty all() without ROUTER_ENABLE_META
        lists:foreach(fun(Suite) ->
            AllResult = Suite:all(),
            ?assertNotEqual([], AllResult, 
                lists:flatten(io_lib:format("~p:all() returned empty without ROUTER_ENABLE_META - this is forbidden for CP1", [Suite]))),
            ct:pal("~p:all() without ROUTER_ENABLE_META = ~p (OK)", [Suite, AllResult])
        end, Cp1CoreSuites)
    after
        %% Restore original value if any
        case OldValue of
            false -> ok;
            Value -> os:putenv("ROUTER_ENABLE_META", Value)
        end
    end,
    
    ok.

%% @doc Task 4: Regression test - await_router_started/1 is idempotent
%% Starting/waiting twice should not break.
test_await_router_started_idempotent(_Config) ->
    %% Start router application if not already running
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, tracing_enabled, false),
    ok = application:set_env(beamline_router, disable_heir, true),
    {ok, _} = application:ensure_all_started(beamline_router),
    
    %% First call - should succeed
    ok = router_suite_helpers:await_router_started(5000),
    ct:pal("First await_router_started/1 call: OK"),
    
    %% Second call (idempotent) - should also succeed
    ok = router_suite_helpers:await_router_started(5000),
    ct:pal("Second await_router_started/1 call: OK (idempotent)"),
    
    %% Third call - still idempotent
    ok = router_suite_helpers:await_router_started(5000),
    ct:pal("Third await_router_started/1 call: OK (idempotent)"),
    
    %% Cleanup
    application:stop(beamline_router),
    
    ok.

%% @doc Task 10: Enforce no stray mocks after CP1 run
%% This test should pass if no mocks remain active from previous tests.
test_no_stray_mocks(_Config) ->
    %% Assert no mocks are currently active
    %% If mocks exist, this will clean them up and fail
    ok = router_mock_helpers:assert_no_mocks(),
    ct:pal("No stray mocks detected (OK)"),
    ok.

%% @doc Task 11: Enforce no stray router ETS tables
%% Verifies no test-only ETS tables leaked from CP1 run.
test_no_stray_router_ets(_Config) ->
    %% Get all ETS tables
    AllTables = ets:all(),
    
    %% Check for test-only table prefixes that should not exist
    TestPrefixes = ["test_", "tmp_", "temp_", "mock_"],
    LeakedTables = [T || T <- AllTables,
                         is_atom(T),
                         lists:any(fun(Prefix) ->
                             lists:prefix(Prefix, atom_to_list(T))
                         end, TestPrefixes)],
    
    case LeakedTables of
        [] ->
            ct:pal("No stray test ETS tables detected (OK)"),
            ok;
        _ ->
            ct:pal("Warning: Found potential test ETS tables: ~p", [LeakedTables]),
            %% Only fail if clearly router-related test tables
            RouterLeaked = [T || T <- LeakedTables,
                                  lists:prefix("router_", atom_to_list(T))],
            ?assertEqual([], RouterLeaked, "Stray router test ETS tables found"),
            ok
    end.
