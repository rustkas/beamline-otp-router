%% @doc Unit Tests for router_tenant_validator module
%% Targeted coverage tests for tenant validation and audit functions
%% @test_category unit, fast, coverage_hotspot
-module(router_tenant_validator_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_validate_tenant_undefined/1,
    test_validate_tenant_empty/1,
    test_validate_tenant_valid_binary/1,
    test_validate_tenant_invalid_format/1,
    test_validate_tenant_not_in_allowlist/1,
    test_validate_tenant_in_allowlist/1,
    test_validate_tenant_no_policy/1,
    test_validate_tenant_with_policy/1,
    test_emit_audit_event/1,
    test_allowlist_with_map/1,
    test_allowlist_with_list/1,
    test_allowlist_mixed_formats/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1, 
    init_per_testcase/2, end_per_testcase/2,
    test_validate_tenant_undefined/1,
    test_validate_tenant_empty/1,
    test_validate_tenant_valid_binary/1,
    test_validate_tenant_invalid_format/1,
    test_validate_tenant_not_in_allowlist/1,
    test_validate_tenant_in_allowlist/1,
    test_validate_tenant_no_policy/1,
    test_validate_tenant_with_policy/1,
    test_emit_audit_event/1,
    test_allowlist_with_map/1,
    test_allowlist_with_list/1,
    test_allowlist_mixed_formats/1
]}).

all() ->
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_validate_tenant_undefined,
            test_validate_tenant_empty,
            test_validate_tenant_valid_binary,
            test_validate_tenant_invalid_format,
            test_validate_tenant_not_in_allowlist,
            test_validate_tenant_in_allowlist,
            test_validate_tenant_no_policy,
            test_validate_tenant_with_policy,
            test_emit_audit_event,
            test_allowlist_with_map,
            test_allowlist_with_list,
            test_allowlist_mixed_formats
        ]}
    ].

init_per_suite(Config) ->
    %% Start application
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    %% Clear any existing allowlist configuration
    application:unset_env(beamline_router, result_ack_allowed_tenants),
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    
    %% Start mocked dependencies or full app
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    %% Clear environment
    application:unset_env(beamline_router, result_ack_allowed_tenants),
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Clear allowlist for each test
    application:unset_env(beamline_router, result_ack_allowed_tenants),
    application:unset_env(beamline_router, caf_push_assignment_allowed_tenants),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for validate_tenant/2 - Basic cases
%% ============================================================================

test_validate_tenant_undefined(_Config) ->
    Context = #{},
    Result = router_tenant_validator:validate_tenant(undefined, Context),
    
    ?assertMatch({error, tenant_missing, _}, Result),
    {error, tenant_missing, ErrorContext} = Result,
    ?assertEqual(true, maps:is_key(reason, ErrorContext)),
    
    ok.

test_validate_tenant_empty(_Config) ->
    Context = #{},
    Result = router_tenant_validator:validate_tenant(<<>>, Context),
    
    ?assertMatch({error, tenant_empty, _}, Result),
    {error, tenant_empty, ErrorContext} = Result,
    ?assertEqual(true, maps:is_key(reason, ErrorContext)),
    
    ok.

test_validate_tenant_valid_binary(_Config) ->
    Context = #{},
    TenantId = <<"valid_tenant_123">>,
    
    %% Without allowlist, should pass validation
    Result = router_tenant_validator:validate_tenant(TenantId, Context),
    
    ?assertMatch({ok, _}, Result),
    {ok, ReturnedTenantId} = Result,
    ?assertEqual(TenantId, ReturnedTenantId),
    
    ok.

test_validate_tenant_invalid_format(_Config) ->
    Context = #{},
    
    %% Non-binary tenant_id (atom)
    Result1 = router_tenant_validator:validate_tenant(invalid_atom, Context),
    ?assertMatch({error, tenant_invalid_format, _}, Result1),
    
    %% Non-binary tenant_id (integer)
    Result2 = router_tenant_validator:validate_tenant(12345, Context),
    ?assertMatch({error, tenant_invalid_format, _}, Result2),
    
    %% Non-binary tenant_id (list - should fail because not binary)
    Result3 = router_tenant_validator:validate_tenant("string_tenant", Context),
    ?assertMatch({error, tenant_invalid_format, _}, Result3),
    
    ok.

%% ============================================================================
%% Tests for validate_tenant/2 - Allowlist cases
%% ============================================================================

test_validate_tenant_not_in_allowlist(_Config) ->
    %% Set up allowlist
    AllowedTenants = [<<"tenant_a">>, <<"tenant_b">>],
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, AllowedTenants),
    
    Context = #{},
    TenantId = <<"tenant_c">>,  %% Not in allowlist
    
    Result = router_tenant_validator:validate_tenant(TenantId, Context),
    
    ?assertMatch({error, tenant_not_allowed, _}, Result),
    {error, tenant_not_allowed, ErrorContext} = Result,
    ?assertEqual(TenantId, maps:get(tenant_id, ErrorContext)),
    
    ok.

test_validate_tenant_in_allowlist(_Config) ->
    %% Set up allowlist
    AllowedTenants = [<<"tenant_a">>, <<"tenant_b">>],
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, AllowedTenants),
    
    Context = #{},
    TenantId = <<"tenant_a">>,  %% In allowlist
    
    Result = router_tenant_validator:validate_tenant(TenantId, Context),
    
    ?assertMatch({ok, _}, Result),
    {ok, ReturnedTenantId} = Result,
    ?assertEqual(TenantId, ReturnedTenantId),
    
    ok.

%% ============================================================================
%% Tests for validate_tenant/2 - Policy cases
%% ============================================================================

test_validate_tenant_no_policy(_Config) ->
    Context = #{},
    TenantId = <<"tenant_without_policy">>,
    
    %% Without allowlist restriction, tenant passes but may have no policy
    Result = router_tenant_validator:validate_tenant(TenantId, Context),
    
    %% Should still return ok (allows tenant without policy)
    ?assertMatch({ok, _}, Result),
    
    ok.

test_validate_tenant_with_policy(_Config) ->
    Context = #{},
    TenantId = <<"default_tenant">>,  %% This tenant should have a policy loaded
    
    Result = router_tenant_validator:validate_tenant(TenantId, Context),
    
    ?assertMatch({ok, _}, Result),
    {ok, ReturnedTenantId} = Result,
    ?assertEqual(TenantId, ReturnedTenantId),
    
    ok.

%% ============================================================================
%% Tests for emit_audit_event/3
%% ============================================================================

test_emit_audit_event(_Config) ->
    TenantId = <<"audit_test_tenant">>,
    Context = #{
        request_id => <<"req_123">>,
        source => <<"test">>
    },
    
    %% Call emit_audit_event directly
    Result = router_tenant_validator:emit_audit_event(tenant_test_event, TenantId, Context),
    
    ?assertEqual(ok, Result),
    
    ok.

%% ============================================================================
%% Tests for allowlist formats
%% ============================================================================

test_allowlist_with_map(_Config) ->
    %% Set up allowlist as map
    AllowedTenants = #{
        <<"tenant_x">> => true,
        <<"tenant_y">> => true
    },
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, AllowedTenants),
    
    Context = #{},
    
    %% Tenant in map allowlist
    Result1 = router_tenant_validator:validate_tenant(<<"tenant_x">>, Context),
    ?assertMatch({ok, _}, Result1),
    
    %% Tenant not in map allowlist
    Result2 = router_tenant_validator:validate_tenant(<<"tenant_z">>, Context),
    ?assertMatch({error, tenant_not_allowed, _}, Result2),
    
    ok.

test_allowlist_with_list(_Config) ->
    %% Set up allowlist as list of binaries
    AllowedTenants = [<<"tenant_1">>, <<"tenant_2">>, <<"tenant_3">>],
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, AllowedTenants),
    
    Context = #{},
    
    %% Tenant in list allowlist
    Result1 = router_tenant_validator:validate_tenant(<<"tenant_2">>, Context),
    ?assertMatch({ok, _}, Result1),
    
    %% Tenant not in list allowlist
    Result2 = router_tenant_validator:validate_tenant(<<"tenant_99">>, Context),
    ?assertMatch({error, tenant_not_allowed, _}, Result2),
    
    ok.

test_allowlist_mixed_formats(_Config) ->
    %% Set up allowlist with mixed string and binary formats
    AllowedTenants = [<<"tenant_bin">>, "tenant_str"],
    ok = application:set_env(beamline_router, result_ack_allowed_tenants, AllowedTenants),
    
    Context = #{},
    
    %% Binary tenant matching binary in allowlist
    Result1 = router_tenant_validator:validate_tenant(<<"tenant_bin">>, Context),
    ?assertMatch({ok, _}, Result1),
    
    %% Binary tenant matching string in allowlist (should still work)
    Result2 = router_tenant_validator:validate_tenant(<<"tenant_str">>, Context),
    ?assertMatch({ok, _}, Result2),
    
    ok.
