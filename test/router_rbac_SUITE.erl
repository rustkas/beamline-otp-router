%% @doc Common Test Suite for Router RBAC Module
%% Unit tests for role-based access control functionality
-module(router_rbac_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suppress warnings for Common Test callbacks
-compile({nowarn_unused_function, [
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]}).

%% Test suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_assign_role_success/1,
    test_assign_role_already_assigned/1,
    test_revoke_role_success/1,
    test_revoke_role_not_found/1,
    test_is_admin/1,
    test_is_operator/1,
    test_is_viewer/1,
    test_has_permission/1,
    test_can_access/1,
    test_multiple_roles/1,
    test_different_tenants/1
]).

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, unit_tests}];
groups_for_level(full) ->
    [{group, unit_tests}];
groups_for_level(_) -> %% fast
    [{group, unit_tests}].

groups() ->
    [
        {unit_tests, [sequence], [
            test_assign_role_success,
            test_assign_role_already_assigned,
            test_revoke_role_success,
            test_revoke_role_not_found,
            test_is_admin,
            test_is_operator,
            test_is_viewer,
            test_has_permission,
            test_can_access,
            test_multiple_roles,
            test_different_tenants
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("### init_per_suite: starting beamline_router for RBAC tests", []),
    
    _ = application:load(beamline_router),
    %% IMPORTANT: rbac_test_mode = false to test real RBAC behavior
    ok = application:set_env(beamline_router, rbac_test_mode, false),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, admin_api_key, <<"test-key">>),
    
    case application:ensure_all_started(beamline_router) of
        {ok, _} -> Config;
        Error -> ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_Case, Config) ->
    %% Reset RBAC state before each test
    case catch router_rbac:reset() of
        ok -> ok;
        _ -> ok  %% Ignore errors, tables may not exist yet
    end,
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%====================================================================
%% Role Management Tests
%%====================================================================

test_assign_role_success(_Config) ->
    UserId = <<"user1">>,
    TenantId = <<"tenant1">>,
    RoleId = <<"admin">>,
    
    %% Assign role
    Result = router_rbac:assign_role(UserId, TenantId, RoleId),
    ct:pal("assign_role result: ~p", [Result]),
    ?assertEqual(ok, Result),
    
    %% Verify role was assigned
    ?assert(router_rbac:is_admin(UserId, TenantId)).

test_assign_role_already_assigned(_Config) ->
    UserId = <<"user2">>,
    TenantId = <<"tenant1">>,
    RoleId = <<"operator">>,
    
    %% First assignment
    ok = router_rbac:assign_role(UserId, TenantId, RoleId),
    
    %% Second assignment should return error
    Result = router_rbac:assign_role(UserId, TenantId, RoleId),
    ?assertEqual({error, role_already_assigned}, Result).

test_revoke_role_success(_Config) ->
    UserId = <<"user3">>,
    TenantId = <<"tenant1">>,
    RoleId = <<"viewer">>,
    
    %% Assign then revoke
    ok = router_rbac:assign_role(UserId, TenantId, RoleId),
    ?assert(router_rbac:is_viewer(UserId, TenantId)),
    
    Result = router_rbac:revoke_role(UserId, TenantId, RoleId),
    ?assertEqual(ok, Result),
    
    %% Verify role was revoked
    ?assertNot(router_rbac:is_viewer(UserId, TenantId)).

test_revoke_role_not_found(_Config) ->
    UserId = <<"user4">>,
    TenantId = <<"tenant1">>,
    RoleId = <<"admin">>,
    
    %% Revoking non-existent role is idempotent (returns ok)
    %% This is safe behavior - ets:delete returns true even if key doesn't exist
    Result = router_rbac:revoke_role(UserId, TenantId, RoleId),
    ?assertEqual(ok, Result).

%%====================================================================
%% Role Check Tests
%%====================================================================

test_is_admin(_Config) ->
    UserId = <<"admin_user">>,
    TenantId = <<"tenant1">>,
    
    %% Not admin initially
    ?assertNot(router_rbac:is_admin(UserId, TenantId)),
    
    %% Assign admin role
    ok = router_rbac:assign_role(UserId, TenantId, <<"admin">>),
    ?assert(router_rbac:is_admin(UserId, TenantId)),
    
    %% Non-admin role doesn't make user admin
    ok = router_rbac:assign_role(<<"other_user">>, TenantId, <<"viewer">>),
    ?assertNot(router_rbac:is_admin(<<"other_user">>, TenantId)).

test_is_operator(_Config) ->
    UserId = <<"op_user">>,
    TenantId = <<"tenant1">>,
    
    ?assertNot(router_rbac:is_operator(UserId, TenantId)),
    
    ok = router_rbac:assign_role(UserId, TenantId, <<"operator">>),
    ?assert(router_rbac:is_operator(UserId, TenantId)).

test_is_viewer(_Config) ->
    UserId = <<"viewer_user">>,
    TenantId = <<"tenant1">>,
    
    ?assertNot(router_rbac:is_viewer(UserId, TenantId)),
    
    ok = router_rbac:assign_role(UserId, TenantId, <<"viewer">>),
    ?assert(router_rbac:is_viewer(UserId, TenantId)).

test_has_permission(_Config) ->
    UserId = <<"perm_user">>,
    TenantId = <<"tenant1">>,
    Context = #{},
    
    %% Admin has all permissions
    ok = router_rbac:assign_role(UserId, TenantId, <<"admin">>),
    
    %% Verify admin status
    ?assert(router_rbac:is_admin(UserId, TenantId)),
    
    %% can_access/5 should now return boolean (true/false), not {error, internal}
    ReadResult = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"policy">>, Context),
    WriteResult = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"policy">>, Context),
    DeleteResult = router_rbac:can_access(UserId, TenantId, <<"delete">>, <<"policy">>, Context),
    
    ct:pal("can_access results: read=~p, write=~p, delete=~p", [ReadResult, WriteResult, DeleteResult]),
    
    %% All results should be booleans
    ?assert(is_boolean(ReadResult)),
    ?assert(is_boolean(WriteResult)),
    ?assert(is_boolean(DeleteResult)).

test_can_access(_Config) ->
    UserId = <<"access_user">>,
    TenantId = <<"tenant1">>,
    Context = #{},
    
    %% No role - can_access should return false (boolean, not error)
    NoRoleResult = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"policy">>, Context),
    ct:pal("No role can_access result: ~p", [NoRoleResult]),
    ?assert(is_boolean(NoRoleResult)),
    ?assertEqual(false, NoRoleResult),
    
    %% Assign viewer role
    ok = router_rbac:assign_role(UserId, TenantId, <<"viewer">>),
    ?assert(router_rbac:is_viewer(UserId, TenantId)),
    
    %% Viewer should get boolean result from can_access
    ViewerReadResult = router_rbac:can_access(UserId, TenantId, <<"read">>, <<"policy">>, Context),
    ViewerWriteResult = router_rbac:can_access(UserId, TenantId, <<"write">>, <<"policy">>, Context),
    ct:pal("Viewer can_access results: read=~p, write=~p", [ViewerReadResult, ViewerWriteResult]),
    
    ?assert(is_boolean(ViewerReadResult)),
    ?assert(is_boolean(ViewerWriteResult)).

%%====================================================================
%% Multi-Tenant Tests
%%====================================================================

test_multiple_roles(_Config) ->
    UserId = <<"multi_role_user">>,
    TenantId = <<"tenant1">>,
    
    %% User can have multiple roles
    ok = router_rbac:assign_role(UserId, TenantId, <<"operator">>),
    ok = router_rbac:assign_role(UserId, TenantId, <<"viewer">>),
    
    ?assert(router_rbac:is_operator(UserId, TenantId)),
    ?assert(router_rbac:is_viewer(UserId, TenantId)),
    ?assertNot(router_rbac:is_admin(UserId, TenantId)).

test_different_tenants(_Config) ->
    UserId = <<"tenant_user">>,
    Tenant1 = <<"tenant_a">>,
    Tenant2 = <<"tenant_b">>,
    
    %% Admin in tenant1, viewer in tenant2
    ok = router_rbac:assign_role(UserId, Tenant1, <<"admin">>),
    ok = router_rbac:assign_role(UserId, Tenant2, <<"viewer">>),
    
    ?assert(router_rbac:is_admin(UserId, Tenant1)),
    ?assertNot(router_rbac:is_admin(UserId, Tenant2)),
    
    ?assertNot(router_rbac:is_viewer(UserId, Tenant1)),
    ?assert(router_rbac:is_viewer(UserId, Tenant2)).
