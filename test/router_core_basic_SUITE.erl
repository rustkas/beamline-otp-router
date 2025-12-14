%% @doc Core Router: Basic Tests
%%
%% Basic router tests
%%
%% @test_category core, unit
-module(router_core_basic_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_app_start/1,
    test_supervisor_running/1,
    test_process_registration/1,
    test_policy_store/1,
    test_policy_store_bad_input/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, basic_tests}];
        "full" -> [{group, basic_tests}];
        _ -> [{group, basic_tests}]
    end.

groups() ->
    [{basic_tests, [sequence], [
        test_app_start,
        test_supervisor_running,
        test_process_registration,
        test_policy_store,
        test_policy_store_bad_input
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

init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> 
    %% Task 3: no stray mocks - use cleanup_and_verify
    router_mock_helpers:cleanup_and_verify(),
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_app_start(_Config) ->
    ct:comment("=== App Start ==="),
    Apps = application:which_applications(),
    ?assert(lists:keymember(beamline_router, 1, Apps)),
    ok.

test_supervisor_running(_Config) ->
    ct:comment("=== Supervisor Running ==="),
    %% Check for any router related supervisor or key process
    case whereis(router_sup) of
        undefined ->
            %% Try beamline_router_sup or just verify app is running
            case whereis(beamline_router_sup) of
                undefined ->
                    ct:comment("No named supervisor found, checking if app is alive"),
                    Apps = application:which_applications(),
                    ?assert(lists:keymember(beamline_router, 1, Apps)),
                    ok;
                Pid ->
                    ?assert(is_process_alive(Pid)),
                    ok
            end;
        Pid ->
            ?assert(is_process_alive(Pid)),
            ok
    end.

test_process_registration(_Config) ->
    ct:comment("=== Process Registration ==="),
    %% Robust check: verify supervision tree has running children
    %% This avoids brittleness from specific registered name strings
    SupPid = whereis(beamline_router_sup),
    ?assertNotEqual(undefined, SupPid, "beamline_router_sup should be registered"),
    ?assert(is_process_alive(SupPid), "beamline_router_sup should be alive"),
    
    %% Get supervised children from the router supervision tree
    Children = supervisor:which_children(beamline_router_sup),
    ?assert(is_list(Children), "supervisor:which_children should return a list"),
    ?assert(length(Children) > 0, "Expected at least one supervised child"),
    
    %% Verify all supervised children are alive (using supervision tree, not global registry)
    AliveChildren = [Id || {Id, Pid, _Type, _Modules} <- Children,
                            is_pid(Pid) andalso is_process_alive(Pid)],
    ?assert(length(AliveChildren) > 0, "Expected at least one alive supervised process"),
    
    ct:pal("Found ~p supervised router processes: ~p", [length(AliveChildren), AliveChildren]),
    ok.

test_policy_store(_Config) ->
    ct:comment("=== Policy Store ==="),
    %% Backend-agnostic test: verify policy store API contract, not implementation
    TestTenantId = <<"core_basic_test_tenant">>,
    TestPolicyId = <<"core_basic_test_policy">>,
    
    %% Cleanup any leftover from previous runs
    _ = router_policy_store:delete_policy(TestTenantId, TestPolicyId),
    
    %% Contract: get_policy returns {error, not_found} for non-existent policy
    NotFoundResult = router_policy_store:get_policy(TestTenantId, TestPolicyId),
    ?assertMatch({error, not_found}, NotFoundResult, "Expected not_found for non-existent policy"),
    
    %% Contract: delete_policy on non-existent is safe (returns ok or {error, not_found})
    DeleteResult = router_policy_store:delete_policy(TestTenantId, TestPolicyId),
    case DeleteResult of
        ok -> ok;
        {error, not_found} -> ok;
        Other -> ct:fail({unexpected_delete_result, Other})
    end,
    ok.

%% @doc Negative test: policy store must handle bad input gracefully
%% Contract: API should return controlled errors (not_found or validation error), not crash
%% Task 7: Tests match actual API contract - empty IDs return not_found
test_policy_store_bad_input(_Config) ->
    ct:comment("=== Policy Store Bad Input ==="),
    
    %% Empty tenant/policy IDs should return error (not_found or validation), not crash
    EmptyTenantResult = router_policy_store:get_policy(<<>>, <<"any_policy">>),
    ?assertMatch({error, _}, EmptyTenantResult),
    
    EmptyPolicyResult = router_policy_store:get_policy(<<"any_tenant">>, <<>>),
    ?assertMatch({error, _}, EmptyPolicyResult),
    
    %% Both empty - should return error, not crash
    BothEmptyResult = router_policy_store:get_policy(<<>>, <<>>),
    ?assertMatch({error, _}, BothEmptyResult),
    
    %% Verify policy store process is still alive after bad input
    ?assertNotEqual(undefined, whereis(router_policy_store)),
    
    ok.

