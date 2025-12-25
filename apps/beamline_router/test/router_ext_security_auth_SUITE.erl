%% @doc Extensions Security: Authorization Tests
%%
%% Authorization and RBAC tests:
%% - Unauthorized read/write/delete/admin
%% - Operator and viewer roles
%%
%% @test_category security, full, extensions
-module(router_ext_security_auth_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("/home/rustkas/aigroup/apps/otp/router/include/beamline_router.hrl").

-export([all/0, groups_for_level/1, groups/0, suite/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([
    test_unauthorized_read/1,
    test_unauthorized_write/1,
    test_unauthorized_delete/1,
    test_unauthorized_admin/1,
    test_operator_can_read_write/1,
    test_viewer_can_only_read/1
]).

suite() -> [{timetrap, {minutes, 5}}].

all() ->
    Level = router_test_utils:get_test_level(),
    groups_for_level(Level).

groups_for_level(heavy) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, auth_tests}];
        "full" -> [{group, auth_tests}];
        _ -> []
    end;
groups_for_level(full) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, auth_tests}];
        "full" -> [{group, auth_tests}];
        _ -> []
    end;
groups_for_level(_) ->
    case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> [{group, auth_tests}];
        "full" -> [{group, auth_tests}];
        _ -> []
    end.
groups() ->
    [{auth_tests, [sequence], [
        test_unauthorized_read,
        test_unauthorized_write,
        test_unauthorized_delete,
        test_unauthorized_admin,
        test_operator_can_read_write,
        test_viewer_can_only_read
    ]}].

init_per_suite(Config) ->
    %% Clean up any stuck ETS tables from previous crashed tests
    lists:foreach(fun(TableName) ->
        case ets:whereis(TableName) of
            undefined -> ok;
            _ -> catch ets:delete(TableName)
        end
    end, [rbac_roles, rbac_user_roles, rbac_permissions, permission_cache]),
    
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, rbac_enabled, true),
    %% Setup mock BEFORE starting app to prevent supervisor conflicts
    ok = router_mock_helpers:setup_router_nats_mock(),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for RBAC server to be ready
            timer:sleep(100),
            %% Verify RBAC is running
            case whereis(router_rbac) of
                undefined ->
                    ct:fail("router_rbac not started");
                Pid ->
                    ct:pal("router_rbac started: ~p", [Pid]),
                    %% Reset RBAC to ensure default roles are initialized
                    ok = router_rbac:reset(),
                    Config
            end;
        Error -> 
            ct:fail("Failed to start: ~p", [Error])
    end.

end_per_suite(_Config) -> 
    application:stop(beamline_router), 
    router_mock_helpers:cleanup_and_verify(),
    ok.

init_per_testcase(_TC, Config) -> 
    %% Each test uses unique user IDs, so no state leakage between tests
    %% reset() can now be safely used if needed (fixed to always reinitialize default roles)
    Config.

end_per_testcase(_TC, _Config) -> ok.

%% ============================================================================
%% TEST CASES
%% ============================================================================

test_unauthorized_read(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    Result = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"extension_registry">>),
    ?assertNot(Result),
    ok.

test_unauthorized_write(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    Result = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"extension_registry">>),
    ?assertNot(Result),
    ok.

test_unauthorized_delete(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    Result = router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"extension_registry">>),
    ?assertNot(Result),
    ok.

test_unauthorized_admin(_Config) ->
    UserId = <<"unauthorized_user">>,
    TenantId = <<"test_tenant">>,
    Result = router_rbac:can_access(UserId, TenantId, <<"admin">>, <<"extension_registry">>),
    ?assertNot(Result),
    ok.

test_operator_can_read_write(_Config) ->
    UserId = <<"operatoruser">>,
    TenantId = <<"testtenant">>,
    
    %% Verify RBAC server is running
    RbacPid = whereis(router_rbac),
    ct:pal("=== RBAC DEBUG ==="),
    ct:pal("router_rbac pid: ~p", [RbacPid]),
    ?assertNotEqual(undefined, RbacPid),
    
    %% Debug: Check if roles table exists
    RolesTableRef = ets:whereis(rbac_roles),
    ct:pal("rbac_roles whereis: ~p", [RolesTableRef]),
    
    %% If table exists, show its content
    case RolesTableRef of
        undefined ->
            ct:pal("ERROR: rbac_roles table does not exist!");
        _ ->
            RolesData = ets:tab2list(rbac_roles),
            ct:pal("rbac_roles data (~p entries): ~p", [length(RolesData), RolesData])
    end,
    
    %% Assign role - must succeed
    AssignResult = router_rbac:assign_role(UserId, TenantId, <<"operator">>),
    ct:pal("assign_role result: ~p", [AssignResult]),
    ?assertEqual(ok, AssignResult),
    
    %% Debug: Check user_roles table
    UserRolesTableRef = ets:whereis(rbac_user_roles),
    ct:pal("rbac_user_roles whereis: ~p", [UserRolesTableRef]),
    case UserRolesTableRef of
        undefined ->
            ct:pal("ERROR: rbac_user_roles table does not exist!");
        _ ->
            UserRolesData = ets:tab2list(rbac_user_roles),
            ct:pal("rbac_user_roles data: ~p", [UserRolesData])
    end,
    
    %% Verify role was assigned
    IsOp = router_rbac:is_operator(UserId, TenantId),
    ct:pal("is_operator: ~p", [IsOp]),
    ?assert(IsOp),
    
    %% Debug: Check can_access result
    ReadResult = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"extension_registry">>),
    ct:pal("can_access read result: ~p", [ReadResult]),
    
    %% Operator should have read/write access to extension_registry
    ?assert(ReadResult),
    ?assert(router_rbac:can_access(UserId, TenantId, <<"write">>, <<"extension_registry">>)),
    
    %% Operator should NOT have delete/admin access
    ?assertNot(router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"extension_registry">>)),
    ?assertNot(router_rbac:can_access(UserId, TenantId, <<"admin">>, <<"extension_registry">>)),
    ok.

test_viewer_can_only_read(_Config) ->
    UserId = <<"vieweruser">>,
    TenantId = <<"testtenant">>,
    
    %% Assign viewer role - must succeed
    AssignResult = router_rbac:assign_role(UserId, TenantId, <<"viewer">>),
    ct:log("assign_role viewer result: ~p", [AssignResult]),
    ?assertEqual(ok, AssignResult),
    
    %% Verify role was assigned
    ?assert(router_rbac:is_viewer(UserId, TenantId)),
    
    %% Viewer should have read access
    ?assert(router_rbac:can_access(UserId, TenantId, <<"read">>, <<"extension_registry">>)),
    
    %% Viewer should NOT have write/delete access
    ?assertNot(router_rbac:can_access(UserId, TenantId, <<"write">>, <<"extension_registry">>)),
    ?assertNot(router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"extension_registry">>)),
    ok.
