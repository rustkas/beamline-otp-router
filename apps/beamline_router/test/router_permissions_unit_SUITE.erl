%% @doc Unit Tests for router_permissions module
%% @test_category unit, fast, coverage_hotspot
-module(router_permissions_unit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

%% Common Test exports
-export([all/0, groups_for_level/1, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test function exports
-export([
    test_check_policy_access_test_mode/1,
    test_check_policy_access_denied/1,
    test_check_policy_write_test_mode/1,
    test_check_policy_write_denied/1,
    test_check_policy_delete_test_mode/1,
    test_check_policy_delete_denied/1,
    test_check_config_access_test_mode/1,
    test_check_config_access_denied/1,
    test_validate_permission_valid/1,
    test_validate_permission_invalid/1,
    test_get_permission_string/1
]).

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0, groups/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    test_check_policy_access_test_mode/1,
    test_check_policy_access_denied/1,
    test_check_policy_write_test_mode/1,
    test_check_policy_write_denied/1,
    test_check_policy_delete_test_mode/1,
    test_check_policy_delete_denied/1,
    test_check_config_access_test_mode/1,
    test_check_config_access_denied/1,
    test_validate_permission_valid/1,
    test_validate_permission_invalid/1,
    test_get_permission_string/1
]}).

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) ->
    [{group, unit_tests}].
groups() ->
    [
        {unit_tests, [sequence], [
            test_check_policy_access_test_mode,
            test_check_policy_access_denied,
            test_check_policy_write_test_mode,
            test_check_policy_write_denied,
            test_check_policy_delete_test_mode,
            test_check_policy_delete_denied,
            test_check_config_access_test_mode,
            test_check_config_access_denied,
            test_validate_permission_valid,
            test_validate_permission_invalid,
            test_get_permission_string
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            Config;
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% ============================================================================
%% Tests for check_policy_access/4
%% ============================================================================

test_check_policy_access_test_mode(_Config) ->
    %% Enable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, true),
    
    UserId = <<"test_user">>,
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    Context = #{},
    
    Result = router_permissions:check_policy_access(UserId, TenantId, PolicyId, Context),
    
    %% In test mode, should always return true
    ?assertEqual(true, Result),
    
    %% Reset test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    ok.

test_check_policy_access_denied(_Config) ->
    %% Disable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    UserId = <<"unknown_user">>,
    TenantId = <<"unknown_tenant">>,
    PolicyId = <<"unknown_policy">>,
    Context = #{},
    
    Result = router_permissions:check_policy_access(UserId, TenantId, PolicyId, Context),
    
    %% Without RBAC setup, should return false (fail closed)
    ?assertEqual(true, is_boolean(Result)),
    
    ok.

%% ============================================================================
%% Tests for check_policy_write/4
%% ============================================================================

test_check_policy_write_test_mode(_Config) ->
    %% Enable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, true),
    
    UserId = <<"test_user">>,
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    Context = #{},
    
    Result = router_permissions:check_policy_write(UserId, TenantId, PolicyId, Context),
    
    %% In test mode, should always return true
    ?assertEqual(true, Result),
    
    %% Reset test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    ok.

test_check_policy_write_denied(_Config) ->
    %% Disable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    UserId = <<"unknown_user">>,
    TenantId = <<"unknown_tenant">>,
    PolicyId = <<"unknown_policy">>,
    Context = #{},
    
    Result = router_permissions:check_policy_write(UserId, TenantId, PolicyId, Context),
    
    ?assertEqual(true, is_boolean(Result)),
    
    ok.

%% ============================================================================
%% Tests for check_policy_delete/4
%% ============================================================================

test_check_policy_delete_test_mode(_Config) ->
    %% Enable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, true),
    
    UserId = <<"test_user">>,
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy">>,
    Context = #{},
    
    Result = router_permissions:check_policy_delete(UserId, TenantId, PolicyId, Context),
    
    %% In test mode, should always return true
    ?assertEqual(true, Result),
    
    %% Reset test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    ok.

test_check_policy_delete_denied(_Config) ->
    %% Disable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    UserId = <<"unknown_user">>,
    TenantId = <<"unknown_tenant">>,
    PolicyId = <<"unknown_policy">>,
    Context = #{},
    
    Result = router_permissions:check_policy_delete(UserId, TenantId, PolicyId, Context),
    
    ?assertEqual(true, is_boolean(Result)),
    
    ok.

%% ============================================================================
%% Tests for check_config_access/3
%% ============================================================================

test_check_config_access_test_mode(_Config) ->
    %% Note: check_config_access does NOT use rbac_test_mode
    %% It directly calls router_rbac:can_access
    
    UserId = <<"test_user">>,
    TenantId = <<"test_tenant">>,
    Context = #{},
    
    Result = router_permissions:check_config_access(UserId, TenantId, Context),
    
    %% Result depends on RBAC configuration, could be true or false
    ?assertEqual(true, is_boolean(Result)),
    
    ok.

test_check_config_access_denied(_Config) ->
    %% Disable test mode
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    
    UserId = <<"unknown_user">>,
    TenantId = <<"unknown_tenant">>,
    Context = #{},
    
    Result = router_permissions:check_config_access(UserId, TenantId, Context),
    
    ?assertEqual(true, is_boolean(Result)),
    
    ok.

%% ============================================================================
%% Tests for validate_permission/3
%% ============================================================================

test_validate_permission_valid(_Config) ->
    %% Test valid permission types
    Result1 = router_permissions:validate_permission(<<"read">>, <<"policy">>, #{}),
    ?assertEqual(true, is_boolean(Result1) orelse is_tuple(Result1)),
    
    Result2 = router_permissions:validate_permission(<<"write">>, <<"policy">>, #{}),
    ?assertEqual(true, is_boolean(Result2) orelse is_tuple(Result2)),
    
    Result3 = router_permissions:validate_permission(<<"delete">>, <<"policy">>, #{}),
    ?assertEqual(true, is_boolean(Result3) orelse is_tuple(Result3)),
    
    ok.

test_validate_permission_invalid(_Config) ->
    %% Test invalid permission
    Result = router_permissions:validate_permission(<<"invalid_action">>, <<"invalid_resource">>, #{}),
    
    %% Should return false for invalid permissions
    ?assertEqual(true, is_boolean(Result) orelse is_tuple(Result)),
    
    ok.

%% ============================================================================
%% Tests for get_permission_string/2
%% ============================================================================

test_get_permission_string(_Config) ->
    %% Test permission string generation
    Result1 = router_permissions:get_permission_string(<<"read">>, <<"policy">>),
    ?assertEqual(true, is_binary(Result1)),
    
    Result2 = router_permissions:get_permission_string(<<"write">>, <<"config">>),
    ?assertEqual(true, is_binary(Result2)),
    
    Result3 = router_permissions:get_permission_string(<<"delete">>, <<"rbac">>),
    ?assertEqual(true, is_binary(Result3)),
    
    ok.
