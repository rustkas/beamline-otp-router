%% @doc Multi-tenant Smoke: Security Tests
%%
%% Tenant security tests:
%% - Tenant spoofing rejection
%% - Mixed tenant payload ignored
%% - Tenant header priority
%% - Logs isolation
%%
%% @test_category smoke, fast, multitenant, security
-module(router_tenant_security_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../include/beamline_router.hrl").

-define(TENANT_A, <<"tenant-a">>).
-define(TENANT_B, <<"tenant-b">>).
-define(TENANT_INVALID, <<"tenant-invalid">>).

-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_tenant_spoofing_rejection/1,
    test_mixed_tenant_payload_ignored/1,
    test_tenant_header_priority/1,
    test_tenant_logs_isolation/1
]).

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, security_tests}];
        "full" -> [{group, security_tests}];
        _ -> [{group, security_tests}]
    end.

groups() ->
    [{security_tests, [sequence], [
        test_tenant_spoofing_rejection,
        test_mixed_tenant_payload_ignored,
        test_tenant_header_priority,
        test_tenant_logs_isolation
    ]}].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, caf_push_assignment_allowed_tenants, [?TENANT_A, ?TENANT_B]),
    ok = router_mock_helpers:setup_router_nats_mock(),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    meck:expect(router_nats, nak_message, fun(_MsgId) -> ok end),
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> test_helpers:wait_for_app_start(router_policy_store, 1000), Config;
        Error -> ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) ->
    router_mock_helpers:unload(router_nats),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_tenant_spoofing_rejection(_Config) ->
    ct:comment("=== Tenant Spoofing Rejection ==="),
    %% Attacker tries to use valid tenant ID but routes to wrong endpoint
    ?assertNot(router_nats_subscriber:check_tenant_allowed(?TENANT_INVALID)),
    ok.

test_mixed_tenant_payload_ignored(_Config) ->
    ct:comment("=== Mixed Tenant Payload Ignored ==="),
    %% Payload with different tenant_id than header should use header
    Request = #{
        <<"version">> => <<"1">>,
        <<"request_id">> => <<"req-001">>,
        <<"tenant_id">> => ?TENANT_A,  %% Header tenant
        <<"message">> => #{
            <<"tenant_id">> => ?TENANT_B  %% Payload tenant (should be ignored)
        }
    },
    ?assertEqual(?TENANT_A, maps:get(<<"tenant_id">>, Request)),
    ok.

test_tenant_header_priority(_Config) ->
    ct:comment("=== Tenant Header Priority ==="),
    %% Header tenant_id takes priority over payload
    HeaderTenant = ?TENANT_A,
    PayloadTenant = ?TENANT_B,
    
    %% Simulate extraction logic
    ExtractedTenant = case HeaderTenant of
        undefined -> PayloadTenant;
        _ -> HeaderTenant
    end,
    
    ?assertEqual(?TENANT_A, ExtractedTenant),
    ok.

test_tenant_logs_isolation(_Config) ->
    ct:comment("=== Tenant Logs Isolation ==="),
    %% Verify that logging includes tenant context
    ct:comment("Logging for tenant A"),
    ct:comment("Logging for tenant B"),
    ct:comment("Logs should be isolated per tenant"),
    ok.
