%% @doc Deployment Tests
%%
%% Tests deployment automation, validation, and rollback procedures.
%% Verifies that deployment processes work correctly.
%%
%% @test_category deployment
-module(router_deployment_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("beamline_router.hrl").

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
-compile({nowarn_unused_function, [
    all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2,
    test_validate_deployment/1,
    test_check_pre_deployment/1,
    test_check_post_deployment/1,
    test_get_deployment_status/1,
    test_get_deployment_history/1,
    test_rollback_available/1
]}).

%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

%% Test functions
-export([
    test_validate_deployment/1,
    test_check_pre_deployment/1,
    test_check_post_deployment/1,
    test_get_deployment_status/1,
    test_get_deployment_history/1,
    test_rollback_available/1
]).

-export([groups_for_level/1]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "sanity" -> sanity;
        "heavy" -> heavy;
        "full" -> full;
        _ -> fast
    end,
    groups_for_level(Level).

%% @doc Deployment tests check operational status, run in heavy tier
groups_for_level(sanity) -> [];
groups_for_level(fast) -> [];
groups_for_level(full) -> [];
groups_for_level(heavy) -> [{group, deployment_tests}].

groups() -> [{deployment_tests, [sequence], [
    test_validate_deployment,
    test_check_pre_deployment,
    test_check_post_deployment,
    test_get_deployment_status,
    test_get_deployment_history,
    test_rollback_available
]}].

init_per_suite(Config) ->
    ok = router_suite_helpers:start_router_suite(),
    Config.

end_per_suite(_Config) ->
    router_suite_helpers:stop_router_suite(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Test: Validate deployment
test_validate_deployment(_Config) ->
    Version = <<"v1.0.0">>,
    case router_deployment:validate_deployment(Version) of
        {ok, Report} ->
            ?assert(maps:is_key(passed, Report)),
            ?assert(maps:is_key(version_format, Report)),
            ?assert(maps:is_key(health, Report)),
            ?assert(maps:is_key(configuration, Report)),
            ?assert(maps:is_key(dependencies, Report)),
            ?assert(maps:is_key(resources, Report)),
            ok;
        {error, Reason} ->
            ct:fail({validation_failed, Reason})
    end.

%% @doc Test: Check pre-deployment
test_check_pre_deployment(_Config) ->
    case router_deployment:check_pre_deployment() of
        {ok, Results} ->
            ?assert(maps:is_key(passed, Results)),
            ?assert(maps:is_key(application_running, Results)),
            ?assert(maps:is_key(supervisor_running, Results)),
            ?assert(maps:is_key(nats_connection, Results)),
            ?assert(maps:is_key(disk_space, Results)),
            ok;
        {error, Reason} ->
            ct:fail({pre_deployment_check_failed, Reason})
    end.

%% @doc Test: Check post-deployment
test_check_post_deployment(_Config) ->
    case router_deployment:check_post_deployment() of
        {ok, Results} ->
            ?assert(maps:is_key(passed, Results)),
            ?assert(maps:is_key(health, Results)),
            ?assert(maps:is_key(grpc_endpoint, Results)),
            ?assert(maps:is_key(metrics_endpoint, Results)),
            ok;
        {error, Reason} ->
            ct:fail({post_deployment_check_failed, Reason})
    end.

%% @doc Test: Get deployment status
test_get_deployment_status(_Config) ->
    case router_deployment:get_deployment_status() of
        {ok, Status} ->
            ?assert(maps:is_key(status, Status)),
            ok;
        {error, Reason} ->
            ct:fail({get_status_failed, Reason})
    end.

%% @doc Test: Get deployment history
test_get_deployment_history(_Config) ->
    case router_deployment:get_deployment_history() of
        {ok, History} ->
            ?assert(is_list(History)),
            ok;
        {error, Reason} ->
            ct:fail({get_history_failed, Reason})
    end.

%% @doc Test: Rollback available
test_rollback_available(_Config) ->
    %% Test rollback with undefined version (should try to get previous)
    case router_deployment:rollback(undefined) of
        {ok, _RollbackResult} ->
            ok;
        {error, no_previous_version} ->
            %% Expected if no previous deployment
            ok;
        {error, Reason} ->
            ct:fail({rollback_failed, Reason})
    end.
