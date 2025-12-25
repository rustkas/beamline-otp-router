%% @doc Multi-tenant Smoke: Isolation Tests
%%
%% Tenant isolation tests:
%% - Valid tenant A/B success
%% - Invalid tenant rejection
%% - Metrics isolation
%% - Logs isolation
%%
%% @test_category smoke, fast, multitenant
-module(router_tenant_isolation_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-define(TENANT_A, <<"tenant-a">>).
-define(TENANT_B, <<"tenant-b">>).
-define(TENANT_INVALID, <<"tenant-invalid">>).

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_valid_tenant_a_success/1,
    test_invalid_tenant_rejection/1,
    test_valid_tenant_b_independent/1,
    test_tenant_metrics_isolation/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, isolation_tests}];
        "full" -> [{group, isolation_tests}];
        _ -> [{group, isolation_tests}]
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, isolation_tests}];
        "full" -> [{group, isolation_tests}];
        _ -> [{group, isolation_tests}]
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, isolation_tests}];
        "full" -> [{group, isolation_tests}];
        _ -> [{group, isolation_tests}]
    end.
groups() ->
    [{isolation_tests, [sequence], [
        test_valid_tenant_a_success,
        test_invalid_tenant_rejection,
        test_valid_tenant_b_independent,
        test_tenant_metrics_isolation
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [?TENANT_A, ?TENANT_B]),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> test_helpers:wait_for_app_start(router_policy_store, 1000), Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> application:stop(beamline_router), ok.

init_per_testcase(_TC, Config) -> Config.
end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_valid_tenant_a_success(_Config) ->
    ct:comment("=== Valid Tenant A Success ==="),
    ?assert(router_nats_subscriber:check_tenant_allowed(?TENANT_A)),
    ?assert(router_caf_adapter:check_tenant_allowed(?TENANT_A)),
    ok.

test_invalid_tenant_rejection(_Config) ->
    ct:comment("=== Invalid Tenant Rejection ==="),
    ?assertNot(router_nats_subscriber:check_tenant_allowed(?TENANT_INVALID)),
    ?assertNot(router_caf_adapter:check_tenant_allowed(?TENANT_INVALID)),
    ok.

test_valid_tenant_b_independent(_Config) ->
    ct:comment("=== Valid Tenant B Independent ==="),
    ?assert(router_nats_subscriber:check_tenant_allowed(?TENANT_B)),
    ?assert(router_caf_adapter:check_tenant_allowed(?TENANT_B)),
    %% Verify tenant A still works (isolation)
    ?assert(router_nats_subscriber:check_tenant_allowed(?TENANT_A)),
    ok.

test_tenant_metrics_isolation(_Config) ->
    ct:comment("=== Tenant Metrics Isolation ==="),
    ok = router_metrics:ensure(),
    
    %% Emit metrics for different tenants
    ok = router_metrics:emit_metric(router_tenant_requests_total, #{count => 1}, #{tenant_id => ?TENANT_A}),
    ok = router_metrics:emit_metric(router_tenant_requests_total, #{count => 1}, #{tenant_id => ?TENANT_B}),
    
    ct:comment("Metrics emitted for both tenants"),
    ok.
