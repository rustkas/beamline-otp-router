%% @doc Regression tests for router_mock_helpers
%%
%% Ensures router_mock_helpers correctly handles meck lifecycle
%% and protects against common meck usage errors.
%%
%% @test_category unit, helpers
-module(router_mock_helpers_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_ensure_mock_creates_new/1,
    test_ensure_mock_idempotent/1,
    test_reset_on_mocked_module/1,
    test_reset_on_unmocked_module/1,
    test_unload_on_mocked_module/1,
    test_unload_on_unmocked_module/1,
    test_expect_wrapper/1,
    test_reset_all/1,
    test_unload_all/1,
    %% Additional coverage for previously untested exported functions
    test_get_mocked_modules/1,
    test_assert_no_mocks/1,
    %% Meck strictness regression (Task 5)
    test_strict_mock_unexpected_call_fails/1,
    %% Idempotency regression (Task 14)
    test_unload_all_idempotent/1,
    %% router_nats mock hardening tests
    test_router_nats_mock_idempotent_across_testcases/1,
    test_router_nats_stray_mock_reported/1,
    test_router_nats_mock_no_passthrough/1,
    %% Stub coverage test (Task 5)
    test_router_nats_stub_coverage/1
]).

suite() -> [{timetrap, {minutes, 1}}].

all() ->
    %% Test infrastructure helpers run by default for CP1
    [{group, unit_tests}].

groups() ->
    [{unit_tests, [sequence], [
        test_ensure_mock_creates_new,
        test_ensure_mock_idempotent,
        test_reset_on_mocked_module,
        test_reset_on_unmocked_module,
        test_unload_on_mocked_module,
        test_unload_on_unmocked_module,
        test_expect_wrapper,
        test_reset_all,
        test_unload_all,
        test_get_mocked_modules,
        test_assert_no_mocks,
        test_strict_mock_unexpected_call_fails,
        test_unload_all_idempotent,
        test_router_nats_mock_idempotent_across_testcases,
        test_router_nats_stray_mock_reported,
        test_router_nats_mock_no_passthrough,
        test_router_nats_stub_coverage
    ]}].

init_per_suite(Config) ->
    ok = router_mock_helpers:ensure_test_deps(),
    Config.

end_per_suite(_Config) ->
    %% Clean up any remaining mocks
    router_mock_helpers:unload_all(),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Ensure clean state before each test
    router_mock_helpers:unload_all(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up after each test (Task 3: no stray mocks)
    router_mock_helpers:cleanup_and_verify(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

%% @doc Test: ensure_mock creates a new mock for unmocked module
test_ensure_mock_creates_new(_Config) ->
    Module = test_mock_target_1,
    
    %% Verify module is not mocked initially
    ?assertNot(lists:member(Module, meck:mocked())),
    
    %% Create mock
    ok = router_mock_helpers:ensure_mock(Module, [non_strict]),
    
    %% Verify module is now mocked
    ?assert(lists:member(Module, meck:mocked())),
    
    %% Cleanup
    router_mock_helpers:unload(Module),
    ok.

%% @doc Test: ensure_mock is idempotent - doesn't crash on already mocked module
test_ensure_mock_idempotent(_Config) ->
    Module = test_mock_target_2,
    
    %% Create mock first time
    ok = router_mock_helpers:ensure_mock(Module, [non_strict]),
    ?assert(lists:member(Module, meck:mocked())),
    
    %% Call again - should not crash and module should still be mocked
    ok = router_mock_helpers:ensure_mock(Module, [non_strict]),
    ?assert(lists:member(Module, meck:mocked())),
    
    %% Cleanup
    router_mock_helpers:unload(Module),
    ok.

%% @doc Test: reset clears expectations on mocked module
test_reset_on_mocked_module(_Config) ->
    Module = test_mock_target_3,
    
    %% Create mock and set expectation
    ok = router_mock_helpers:ensure_mock(Module, [non_strict]),
    meck:expect(Module, test_fun, fun() -> expected_value end),
    
    %% Verify expectation works
    ?assertEqual(expected_value, Module:test_fun()),
    
    %% Reset should not crash
    ok = router_mock_helpers:reset(Module),
    
    %% Module should still be mocked after reset
    ?assert(lists:member(Module, meck:mocked())),
    
    %% Cleanup
    router_mock_helpers:unload(Module),
    ok.

%% @doc Test: reset on unmocked module does not crash
test_reset_on_unmocked_module(_Config) ->
    Module = test_mock_target_4,
    
    %% Verify module is not mocked
    ?assertNot(lists:member(Module, meck:mocked())),
    
    %% Reset should not crash
    ok = router_mock_helpers:reset(Module),
    
    %% Module should still not be mocked
    ?assertNot(lists:member(Module, meck:mocked())),
    ok.

%% @doc Test: unload on mocked module removes mock
test_unload_on_mocked_module(_Config) ->
    Module = test_mock_target_5,
    
    %% Create mock
    ok = router_mock_helpers:ensure_mock(Module, [non_strict]),
    ?assert(lists:member(Module, meck:mocked())),
    
    %% Unload should succeed
    ok = router_mock_helpers:unload(Module),
    
    %% Module should no longer be mocked
    ?assertNot(lists:member(Module, meck:mocked())),
    ok.

%% @doc Test: unload on unmocked module does not crash
test_unload_on_unmocked_module(_Config) ->
    Module = test_mock_target_6,
    
    %% Verify module is not mocked
    ?assertNot(lists:member(Module, meck:mocked())),
    
    %% Unload should not crash
    ok = router_mock_helpers:unload(Module),
    
    %% Module should still not be mocked
    ?assertNot(lists:member(Module, meck:mocked())),
    ok.

%% @doc Test: expect wrapper correctly sets expectations
test_expect_wrapper(_Config) ->
    Module = test_mock_target_7,
    
    %% Create mock
    ok = router_mock_helpers:ensure_mock(Module, [non_strict]),
    
    %% Set expectation via wrapper
    router_mock_helpers:expect(Module, test_fun, fun(X) -> X * 2 end),
    
    %% Verify expectation works
    ?assertEqual(4, Module:test_fun(2)),
    ?assertEqual(10, Module:test_fun(5)),
    
    %% Cleanup
    router_mock_helpers:unload(Module),
    ok.

%% @doc Test: reset_all resets multiple modules safely
test_reset_all(_Config) ->
    Mod1 = test_mock_target_8,
    Mod2 = test_mock_target_9,
    Mod3 = test_mock_target_unmocked,
    
    %% Create mocks for first two
    ok = router_mock_helpers:ensure_mock(Mod1, [non_strict]),
    ok = router_mock_helpers:ensure_mock(Mod2, [non_strict]),
    
    %% Set expectations
    meck:expect(Mod1, test_fun, fun() -> 1 end),
    meck:expect(Mod2, test_fun, fun() -> 2 end),
    
    %% Reset all including unmocked - should not crash
    ok = router_mock_helpers:reset_all([Mod1, Mod2, Mod3]),
    
    %% Mocked modules should still be mocked
    ?assert(lists:member(Mod1, meck:mocked())),
    ?assert(lists:member(Mod2, meck:mocked())),
    
    %% Cleanup
    router_mock_helpers:unload(Mod1),
    router_mock_helpers:unload(Mod2),
    ok.

%% @doc Test: unload_all cleans up all mocks
test_unload_all(_Config) ->
    Mod1 = test_mock_target_10,
    Mod2 = test_mock_target_11,
    
    %% Create mocks
    ok = router_mock_helpers:ensure_mock(Mod1, [non_strict]),
    ok = router_mock_helpers:ensure_mock(Mod2, [non_strict]),
    
    %% Verify both are mocked
    ?assert(lists:member(Mod1, meck:mocked())),
    ?assert(lists:member(Mod2, meck:mocked())),
    
    %% Unload all
    ok = router_mock_helpers:unload_all(),
    
    %% All should be unloaded
    ?assertEqual([], meck:mocked()),
    ok.

%% @doc Test: get_mocked_modules returns list of currently mocked modules
%% Note: We don't assume initial state is empty - only assert on our own mocks
test_get_mocked_modules(_Config) ->
    Mod1 = test_mock_target_get_1,
    Mod2 = test_mock_target_get_2,
    
    %% Capture initial state (may not be empty if other suites ran)
    InitialMocked = router_mock_helpers:get_mocked_modules(),
    ?assert(is_list(InitialMocked)),
    
    %% Our modules should NOT be mocked yet
    ?assertNot(lists:member(Mod1, InitialMocked)),
    ?assertNot(lists:member(Mod2, InitialMocked)),
    
    %% Create mocks
    ok = router_mock_helpers:ensure_mock(Mod1, [non_strict]),
    ok = router_mock_helpers:ensure_mock(Mod2, [non_strict]),
    
    %% Get mocked modules - our mocks should now be present
    MockedModules = router_mock_helpers:get_mocked_modules(),
    ?assert(is_list(MockedModules)),
    ?assert(lists:member(Mod1, MockedModules)),
    ?assert(lists:member(Mod2, MockedModules)),
    
    %% Cleanup our mocks only
    router_mock_helpers:unload(Mod1),
    router_mock_helpers:unload(Mod2),
    
    %% Verify our mocks are gone (not that list is empty)
    FinalMocked = router_mock_helpers:get_mocked_modules(),
    ?assertNot(lists:member(Mod1, FinalMocked)),
    ?assertNot(lists:member(Mod2, FinalMocked)),
    ok.

%% @doc Test: assert_no_mocks semantics
%% Contract: assert_no_mocks/0 returns ok if no mocks, otherwise:
%%   - Cleans up all stray mocks
%%   - Raises error:{stray_mocks_found, ListOfModules}
%% This is "assert+cleanup" semantics, ensuring test isolation even on failure.
test_assert_no_mocks(_Config) ->
    Module1 = test_mock_target_assert_1,
    Module2 = test_mock_target_assert_2,
    
    %% Should succeed when no mocks are active (after init_per_testcase cleanup)
    ok = router_mock_helpers:assert_no_mocks(),
    
    %% Create two mocks to verify error payload lists all stray mocks
    ok = router_mock_helpers:ensure_mock(Module1, [non_strict]),
    ok = router_mock_helpers:ensure_mock(Module2, [non_strict]),
    ?assert(lists:member(Module1, meck:mocked())),
    ?assert(lists:member(Module2, meck:mocked())),
    
    %% Should fail with stray mocks (and clean them up)
    try
        router_mock_helpers:assert_no_mocks(),
        ct:fail({expected_error_for_stray_mocks, meck:mocked()})
    catch
        error:{stray_mocks_found, StrayMocks} ->
            %% Task 6: Verify error payload is informative - contains all stray modules
            ?assert(is_list(StrayMocks)),
            ?assert(lists:member(Module1, StrayMocks)),
            ?assert(lists:member(Module2, StrayMocks)),
            %% Verify mocks were cleaned up (assert+cleanup semantics)
            ?assertEqual([], meck:mocked())
    end,
    
    %% Should succeed again now that mocks are cleaned
    ok = router_mock_helpers:assert_no_mocks(),
    ok.

%% @doc Regression test: strict meck expects unexpected calls to fail (Task 5)
%% This ensures behavior drift is detected when code calls unexpected functions.
%% NOTE: non_strict is REQUIRED to mock non-existent modules, but the behavior
%% we're testing (undefined functions raising undef) still applies.
test_strict_mock_unexpected_call_fails(_Config) ->
    Module = test_mock_target_strict,
    
    %% Create mock with non_strict (required for non-existent modules)
    %% Calling undefined functions should still raise undef error
    meck:new(Module, [non_strict]),
    
    %% Set specific expectation for one function
    meck:expect(Module, expected_fun, fun() -> expected_result end),
    
    %% Verify the expected function works
    ?assertEqual(expected_result, Module:expected_fun()),
    
    %% Try to call an unexpected function (should fail with undef)
    try
        Module:unexpected_fun(),
        ct:fail(expected_error_for_unexpected_call)
    catch
        error:undef ->
            %% This is expected behavior - undefined function
            ok;
        error:{undef, _} ->
            %% Alternative error format
            ok
    end,
    
    %% Verify expected function still works after failed call
    ?assertEqual(expected_result, Module:expected_fun()),
    
    %% Cleanup
    meck:unload(Module),
    ok.

%% @doc Regression test: unload_all is idempotent
%% Calling unload_all multiple times should not crash, even with no mocks active.
test_unload_all_idempotent(_Config) ->
    Mod = test_mock_target_idem,
    
    %% First call with mocks
    ok = router_mock_helpers:ensure_mock(Mod, [non_strict]),
    ?assert(lists:member(Mod, meck:mocked())),
    
    ok = router_mock_helpers:unload_all(),
    ?assertEqual([], meck:mocked()),
    
    %% Second call with no mocks - should not crash
    ok = router_mock_helpers:unload_all(),
    ?assertEqual([], meck:mocked()),
    
    %% Third call - still idempotent
    ok = router_mock_helpers:unload_all(),
    ?assertEqual([], meck:mocked()),
    
    ok.

%% ============================================================================
%% router_nats mock hardening tests
%% ============================================================================

%% @doc Regression test: router_nats mock is idempotent across testcases
%% Verifies setup_router_nats_mock/0 then unload can be repeated
%% without causing already_started or noproc errors.
test_router_nats_mock_idempotent_across_testcases(_Config) ->
    %% First cycle: setup then cleanup
    ok = router_mock_helpers:setup_router_nats_mock(),
    ?assert(lists:member(router_nats, meck:mocked())),
    
    %% Verify stubs work
    ?assertEqual(ok, router_nats:ack_message(<<"msg-1">>)),
    ?assertEqual(ok, router_nats:nak_message(<<"msg-2">>)),
    ?assertEqual({ok, <<"mock-msg-id">>}, router_nats:publish_with_ack(<<"s">>, <<"p">>, #{})),

    router_mock_helpers:unload(router_nats),
    ?assertNot(lists:member(router_nats, meck:mocked())),
    
    %% Second cycle: repeat - should not crash with already_started
    ok = router_mock_helpers:setup_router_nats_mock(),
    ?assert(lists:member(router_nats, meck:mocked())),
    
    %% Verify stubs still work
    ?assertEqual(ok, router_nats:ack_message(<<"msg-3">>)),
    
    router_mock_helpers:unload(router_nats),
    ?assertNot(lists:member(router_nats, meck:mocked())),
    
    %% Third cycle: one more to prove idempotency
    ok = router_mock_helpers:setup_router_nats_mock(#{
        publish_with_ack => fun(_, _, _) -> {ok, <<"custom-ack">>} end
    }),
    ?assert(lists:member(router_nats, meck:mocked())),
    ?assertEqual({ok, <<"custom-ack">>}, router_nats:publish_with_ack(<<"s">>, <<"p">>, #{})),
    
    router_mock_helpers:unload(router_nats),
    ?assertNot(lists:member(router_nats, meck:mocked())),
    
    ok.

%% @doc Regression test: stray router_nats mock is detected
%% Verifies that router_nats appears in meck:mocked() when left active,
%% and can be cleaned up properly.
test_router_nats_stray_mock_reported(_Config) ->
    %% Setup router_nats mock
    ok = router_mock_helpers:setup_router_nats_mock(),
    ?assert(lists:member(router_nats, meck:mocked())),
    
    %% Verify router_nats is in the mocked modules list (would be reported as stray)
    MockedModules = router_mock_helpers:get_mocked_modules(),
    ?assert(lists:member(router_nats, MockedModules)),
    
    %% Clean up directly to avoid warning output
    router_mock_helpers:unload(router_nats),
    
    %% After cleanup, router_nats should no longer be mocked
    ?assertNot(lists:member(router_nats, meck:mocked())),
    
    ok.

%% @doc Regression test: router_nats mock does not use passthrough
%% Verifies that setup_router_nats_mock creates a mock without passthrough
%% so that calls don't reach gen_server:call(router_nats,...).
test_router_nats_mock_no_passthrough(_Config) ->
    %% Setup mock
    ok = router_mock_helpers:setup_router_nats_mock(),
    ?assert(lists:member(router_nats, meck:mocked())),
    
    %% These calls should NOT crash with noproc even though router_nats
    %% gen_server is not running. They should return our stubbed values.
    ?assertEqual(ok, router_nats:ack_message(<<"msg-1">>)),
    ?assertEqual(ok, router_nats:nak_message(<<"msg-2">>)),
    ?assertEqual(ok, router_nats:publish(<<"subject">>, <<"payload">>)),
    ?assertEqual({ok, <<>>}, router_nats:request(<<"subj">>, <<"pay">>, 1000)),
    %% subscribe_jetstream returns success by default (for app startup)
    ?assertEqual({ok, <<"mock-consumer">>}, 
                 router_nats:subscribe_jetstream(<<"s">>, <<"st">>, explicit, undefined, push)),
    %% publish_with_ack returns success by default
    ?assertEqual({ok, <<"mock-msg-id">>}, 
                 router_nats:publish_with_ack(<<"s">>, <<"p">>, #{})),
    
    %% Cleanup - use unload directly to avoid warning
    router_mock_helpers:unload(router_nats),
    ok.

%% @doc Stub coverage test: verify ALL router_nats stubs return expected values
%% This test ensures setup_router_nats_mock() covers all router_nats functions
%% that are commonly called in tests, preventing noproc errors.
test_router_nats_stub_coverage(_Config) ->
    %% Setup mock
    ok = router_mock_helpers:setup_router_nats_mock(),
    ?assert(lists:member(router_nats, meck:mocked())),
    
    %% Test all stubbed functions - these should NOT crash
    %% Core publish functions
    ?assertEqual(ok, router_nats:publish(<<"test.subject">>, <<"payload">>)),
    ?assertEqual({ok, <<"mock-msg-id">>}, router_nats:publish_with_ack(<<"s">>, <<"p">>, #{})),
    ?assertEqual({ok, <<>>}, router_nats:request(<<"s">>, <<"p">>, 5000)),
    
    %% ACK/NAK functions
    ?assertEqual(ok, router_nats:ack_message(<<"msg-id">>)),
    ?assertEqual(ok, router_nats:nak_message(<<"msg-id">>)),
    
    %% Subscription functions
    ?assertEqual(ok, router_nats:subscribe(<<"test.subject">>, fun(_) -> ok end, 5000)),
    ?assertEqual({ok, <<"mock-consumer">>}, 
                 router_nats:subscribe_jetstream(<<"s">>, <<"st">>, explicit, undefined, push)),
    
    %% Connection status functions
    ?assertEqual({ok, #{state => connected, fail_open_mode => false}}, router_nats:get_connection_status()),
    ?assertMatch(#{status := healthy}, router_nats:get_connection_health()),
    
    %% Reconnect function
    ?assertEqual(ok, router_nats:reconnect()),
    
    %% JetStream consumers
    ?assertEqual([], router_nats:get_jetstream_consumers()),
    
    %% TEST-only function (simulate_connection_lost)
    ?assertEqual(ok, router_nats:simulate_connection_lost()),
    
    %% Cleanup
    router_mock_helpers:unload(router_nats),
    ?assertNot(lists:member(router_nats, meck:mocked())),
    
    ok.

