%% @doc Common Test Suite for RouterAdmin gRPC Integration Tests
%% Tests RouterAdmin gRPC service integration with policy management, dry-run evaluation, and status queries
-module(router_admin_grpc_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Include necessary header files
-include("../include/beamline_router.hrl").  %% Contains policy record definition
-include("../include/flow_pb.hrl").
%% Note: grpcbox.hrl is not needed here - router_admin_grpc module handles grpcbox includes internally

%% Suppress warnings for Common Test callbacks (called automatically by CT framework)
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
-export([
    test_upsert_policy_success/1,
    test_upsert_policy_unauthorized/1,
    test_delete_policy_success/1,
    test_delete_policy_not_found/1,
    test_get_policy_success/1,
    test_get_policy_not_found/1,
    test_list_policies_success/1,
    test_get_checkpoint_status/1,
    create_context_with_user/2
]).

all() ->
    [
        {group, integration_tests}
    ].

groups() ->
    [
        {integration_tests, [sequence], [
            test_upsert_policy_success,
            test_upsert_policy_unauthorized,
            test_get_policy_success,
            test_list_policies_success,
            test_delete_policy_success,
            test_delete_policy_not_found,
            test_get_policy_not_found,
            test_get_checkpoint_status
        ]}
    ].

init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(beamline_router, grpc_port, 0),
    ok = application:set_env(beamline_router, grpc_enabled, true),
    ok = application:set_env(beamline_router, admin_grpc_enabled, true),
    ok = application:set_env(beamline_router, cp2_plus_allowed, true),
    ok = application:set_env(beamline_router, nats_mode, mock),
    ok = application:set_env(beamline_router, rbac_test_mode, true),
    AdminKey = <<"test-admin-key">>,
    ok = application:set_env(beamline_router, admin_api_key, AdminKey),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Wait for gRPC server to start
            timer:sleep(500),
            [{admin_api_key, AdminKey} | Config];
        Error ->
            ct:fail("Failed to start beamline_router: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(beamline_router),
    ok.

init_per_testcase(_Case, Config) ->
    %% Reset policy store for clean test state
    case whereis(router_policy_store) of
        undefined -> ok;
        _ -> gen_server:call(router_policy_store, reset_all)
    end,
    %% Ensure router_rbac is running (don't reset - it clears default roles)
    case whereis(router_rbac) of
        undefined ->
            case router_rbac:start_link() of
                {ok, _} -> 
                    timer:sleep(500);  %% Wait for initialization
                _ -> ok
            end;
        _ -> 
            %% RBAC already running, ensure default roles are initialized
            timer:sleep(100)
    end,
    %% Reset RBAC tables to avoid stale permissions between tests
    case catch router_rbac:reset() of
        ok -> ok;
        {error, ResetError} ->
            ct:log("Warning: Failed to reset RBAC state: ~p", [ResetError]);
        {'EXIT', ResetExit} ->
            ct:log("Warning: RBAC reset exited: ~p", [ResetExit])
    end,
    %% Verify default roles are initialized by checking if admin role exists
    case ensure_default_roles_initialized() of
        ok -> ok;
        {error, InitError} ->
            ct:log("Warning: Failed to ensure default roles initialized: ~p", [InitError])
    end,
    %% Setup test user with admin role for all tenants used in tests
    TestUserId = <<"test-user">>,
    TestTenants = [<<"test_tenant">>],
    %% Assign admin role to test user for all tenants
    lists:foreach(
        fun(TenantId) ->
            case router_rbac:assign_role(TestUserId, TenantId, <<"admin">>) of
                ok -> 
                    %% Clear permission cache after role assignment to ensure new permissions are used
                    clear_permission_cache(TestUserId, TenantId),
                    %% Verify role was actually assigned
                    case router_rbac:get_user_roles(TestUserId, TenantId) of
                        Roles when is_list(Roles) ->
                            case lists:member(<<"admin">>, Roles) of
                                true -> ok;
                                false ->
                                    ct:log("Warning: Admin role not found in user roles after assignment for tenant ~p", [TenantId])
                            end;
                        _ ->
                            ct:log("Warning: Failed to get user roles after assignment for tenant ~p", [TenantId])
                    end;
                {error, role_already_assigned} -> 
                    %% Role already assigned, clear cache anyway to ensure fresh permissions
                    clear_permission_cache(TestUserId, TenantId);
                {error, AssignError} ->
                    ct:log("Warning: Failed to assign admin role to ~p for tenant ~p: ~p", 
                           [TestUserId, TenantId, AssignError])
            end
        end,
        TestTenants
    ),
    %% Small delay to ensure role assignment is processed
    timer:sleep(200),
    %% Verify role assignment by checking permissions (with retries)
    lists:foreach(
        fun(TenantId) ->
            %% Try multiple times to verify permissions (in case of timing issues)
            VerifyPermissions = fun VerifyPermissionsFun(Retries) ->
                case router_permissions:check_policy_write(TestUserId, TenantId, undefined, #{}) of
                    true -> 
                        ok;
                    false when Retries > 0 ->
                        %% Clear cache and retry
                        clear_permission_cache(TestUserId, TenantId),
                        timer:sleep(100),
                        VerifyPermissionsFun(Retries - 1);
                    false ->
                        %% Last attempt failed - log but don't fail test
                        ct:log("Warning: User ~p does not have write permission for tenant ~p after role assignment. " ++
                               "This may indicate RBAC configuration issue, but continuing test.", 
                               [TestUserId, TenantId])
                end
            end,
            VerifyPermissions(3)
        end,
        TestTenants
    ),
    [{test_user_id, TestUserId} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

%% @doc Ensure default roles are initialized in RBAC
ensure_default_roles_initialized() ->
    try
        case ets:whereis(rbac_roles) of
            undefined -> 
                {error, roles_table_not_found};
            RolesTable ->
                %% Check if admin role exists
                case ets:lookup(RolesTable, <<"admin">>) of
                    [] ->
                        %% Admin role not found - manually initialize default roles
                        ct:log("Warning: Admin role not found, initializing default roles manually"),
                        try
                            AdminPermissions = [
                                {<<"read">>, <<"policy">>},
                                {<<"write">>, <<"policy">>},
                                {<<"delete">>, <<"policy">>},
                                {<<"admin">>, <<"config">>},
                                {<<"read">>, <<"metrics">>}
                            ],
                            ets:insert(RolesTable, {<<"admin">>, AdminPermissions}),
                            
                            OperatorPermissions = [
                                {<<"read">>, <<"policy">>},
                                {<<"write">>, <<"policy">>},
                                {<<"read">>, <<"metrics">>},
                                {<<"read">>, <<"extension_registry">>},
                                {<<"write">>, <<"extension_registry">>}
                            ],
                            ets:insert(RolesTable, {<<"operator">>, OperatorPermissions}),
                            
                            ViewerPermissions = [
                                {<<"read">>, <<"policy">>},
                                {<<"read">>, <<"metrics">>},
                                {<<"read">>, <<"extension_registry">>}
                            ],
                            ets:insert(RolesTable, {<<"viewer">>, ViewerPermissions}),
                            
                            ct:log("Default roles initialized successfully"),
                            ok
                        catch
                            _:InitError ->
                                ct:log("Error initializing default roles: ~p", [InitError]),
                                {error, InitError}
                        end;
                    [_] ->
                        ok
                end
        end
    catch
        _:Reason -> 
            {error, Reason}
    end.

%% @doc Clear permission cache for user/tenant to ensure fresh permissions after role assignment
clear_permission_cache(UserId, TenantId) ->
    try
        case ets:whereis(permission_cache) of
            undefined -> ok;
            CacheTable ->
                %% Clear specific cache entry
                CacheKey = {UserId, TenantId},
                ets:delete(CacheTable, CacheKey),
                %% Also clear all cache entries for this user (in case of different tenant variations)
                Pattern = {{UserId, '_'}, '_', '_'},
                ets:match_delete(CacheTable, Pattern),
                ok
        end
    catch
        _:_ -> ok  %% Ignore errors (cache may not exist or be accessible)
    end.

%% @doc Create context with authentication and user ID
create_context_with_user(AdminKey, UserId) ->
    Metadata = [
        {<<"x-api-key">>, AdminKey},
        {<<"x-user-id">>, UserId},
        {<<"x-tenant-id">>, <<"test_tenant">>}
    ],
    #{metadata => Metadata}.

%% @doc Test: UpsertPolicy with valid authentication
test_upsert_policy_success(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy_1">>,
    
    %% Create AdminPolicy
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [
            #'AdminProvider'{id = <<"openai:gpt-4">>, weight = 1.0}
        ],
        sticky = false,
        rules = []
    },
    
    %% Create UpsertPolicyRequest
    RequestPb = #'UpsertPolicyRequest'{
        tenant_id = TenantId,
        policy = AdminPolicyPb
    },
    Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),
    
    %% Create context with authentication and user ID
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    
    %% Call upsert_policy
    case router_admin_grpc:upsert_policy(Ctx, Request) of
        {ok, Response, _} when is_binary(Response) ->
            %% Decode response
            ResponsePb = flow_pb:decode_msg(Response, 'UpsertPolicyResponse'),
            ?assert(ResponsePb#'UpsertPolicyResponse'.ok);
        {ok, ErrorMap, _} when is_map(ErrorMap) ->
            %% Handle error response (should not happen in success test)
            ct:fail("UpsertPolicy returned error map instead of protobuf: ~p", [ErrorMap]);
        Error ->
            ct:fail("UpsertPolicy failed: ~p", [Error])
    end,
    
    %% Verify policy was created
    {ok, Policy} = router_policy_store:get_policy(TenantId, PolicyId, undefined),
    ?assertEqual(PolicyId, Policy#policy.policy_id),
    ?assertEqual(TenantId, Policy#policy.tenant_id),
    
    ok.

%% @doc Test: UpsertPolicy without authentication
test_upsert_policy_unauthorized(_) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy_2">>,
    
    %% Create AdminPolicy
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [
            #'AdminProvider'{id = <<"openai:gpt-4">>, weight = 1.0}
        ],
        sticky = false,
        rules = []
    },
    
    %% Create UpsertPolicyRequest
    RequestPb = #'UpsertPolicyRequest'{
        tenant_id = TenantId,
        policy = AdminPolicyPb
    },
    Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),
    
    %% Create context without authentication
    Ctx = router_grpc_test_helper:create_context_without_auth(),
    
    %% Call upsert_policy - should fail with UNAUTHENTICATED
    try
        router_admin_grpc:upsert_policy(Ctx, Request),
        ct:fail("Expected UNAUTHENTICATED error")
    catch
        {grpc_error, {Status, _Msg}} ->
            ?assertEqual(16, Status),  %% GRPC_STATUS_UNAUTHENTICATED = 16
            ok
    end.

%% @doc Test: GetPolicy with valid authentication
test_get_policy_success(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy_3">>,
    
    %% First, create a policy
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [
            #'AdminProvider'{id = <<"openai:gpt-4">>, weight = 1.0}
        ],
        sticky = false,
        rules = []
    },
    RequestPb = #'UpsertPolicyRequest'{
        tenant_id = TenantId,
        policy = AdminPolicyPb
    },
    Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    {ok, _, _} = router_admin_grpc:upsert_policy(Ctx, Request),
    
    %% Now get the policy
    GetRequestPb = #'GetPolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
    
    %% Call get_policy
    {ok, GetResponse, _} = router_admin_grpc:get_policy(Ctx, GetRequest),
    
    %% Decode response
    GetResponsePb = flow_pb:decode_msg(GetResponse, 'GetPolicyResponse'),
    RetrievedPolicy = GetResponsePb#'GetPolicyResponse'.policy,
    ?assertEqual(PolicyId, RetrievedPolicy#'AdminPolicy'.policy_id),
    
    ok.

%% @doc Test: ListPolicies with valid authentication
test_list_policies_success(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"test_tenant">>,
    
    %% Create multiple policies
    lists:foreach(
        fun(N) ->
            PolicyId = <<"test_policy_", (integer_to_binary(N))/binary>>,
            AdminPolicyPb = #'AdminPolicy'{
                policy_id = PolicyId,
                providers = [
                    #'AdminProvider'{id = <<"openai:gpt-4">>, weight = 1.0}
                ],
                sticky = false,
                rules = []
            },
            RequestPb = #'UpsertPolicyRequest'{
                tenant_id = TenantId,
                policy = AdminPolicyPb
            },
            Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),
            TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
            Ctx = create_context_with_user(AdminKey, TestUserId),
            {ok, _, _} = router_admin_grpc:upsert_policy(Ctx, Request)
        end,
        lists:seq(1, 3)
    ),
    
    %% List policies
    ListRequestPb = #'ListPoliciesRequest'{
        tenant_id = TenantId
    },
    ListRequest = flow_pb:encode_msg(ListRequestPb, 'ListPoliciesRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    
    %% Call list_policies
    {ok, ListResponse, _} = router_admin_grpc:list_policies(Ctx, ListRequest),
    
    %% Decode response
    ListResponsePb = flow_pb:decode_msg(ListResponse, 'ListPoliciesResponse'),
    Policies = ListResponsePb#'ListPoliciesResponse'.policies,
    ?assertEqual(3, length(Policies)),
    
    ok.

%% @doc Test: DeletePolicy with valid authentication
test_delete_policy_success(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"test_tenant">>,
    PolicyId = <<"test_policy_delete">>,
    
    %% First, create a policy
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [
            #'AdminProvider'{id = <<"openai:gpt-4">>, weight = 1.0}
        ],
        sticky = false,
        rules = []
    },
    RequestPb = #'UpsertPolicyRequest'{
        tenant_id = TenantId,
        policy = AdminPolicyPb
    },
    Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    {ok, _, _} = router_admin_grpc:upsert_policy(Ctx, Request),
    
    %% Verify policy exists
    {ok, _} = router_policy_store:get_policy(TenantId, PolicyId, undefined),
    
    %% Delete policy
    DeleteRequestPb = #'DeletePolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    DeleteRequest = flow_pb:encode_msg(DeleteRequestPb, 'DeletePolicyRequest'),
    
    %% Call delete_policy
    {ok, DeleteResponse, _} = router_admin_grpc:delete_policy(Ctx, DeleteRequest),
    
    %% Decode response
    DeleteResponsePb = flow_pb:decode_msg(DeleteResponse, 'DeletePolicyResponse'),
    ?assert(DeleteResponsePb#'DeletePolicyResponse'.ok),
    
    %% Verify policy was deleted
    ?assertMatch({error, not_found}, router_policy_store:get_policy(TenantId, PolicyId, undefined)),
    
    ok.

%% @doc Test: DeletePolicy with non-existent policy
test_delete_policy_not_found(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"test_tenant">>,
    PolicyId = <<"non_existent_policy">>,
    
    %% Delete non-existent policy
    DeleteRequestPb = #'DeletePolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    DeleteRequest = flow_pb:encode_msg(DeleteRequestPb, 'DeletePolicyRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    
    %% Call delete_policy - should fail with NOT_FOUND
    try
        router_admin_grpc:delete_policy(Ctx, DeleteRequest),
        ct:fail("Expected NOT_FOUND error")
    catch
        {grpc_error, {Status, _Msg}} ->
            ?assertEqual(5, Status),  %% GRPC_STATUS_NOT_FOUND = 5
            ok
    end.

%% @doc Test: GetPolicy with non-existent policy
test_get_policy_not_found(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"test_tenant">>,
    PolicyId = <<"non_existent_policy">>,
    
    %% Get non-existent policy
    GetRequestPb = #'GetPolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    
    %% Call get_policy - should fail with NOT_FOUND
    try
        router_admin_grpc:get_policy(Ctx, GetRequest),
        ct:fail("Expected NOT_FOUND error")
    catch
        {grpc_error, {Status, _Msg}} ->
            ?assertEqual(5, Status),  %% GRPC_STATUS_NOT_FOUND = 5
            ok
    end.

%% @doc Test: GetCheckpointStatus with valid authentication
test_get_checkpoint_status(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    
    %% Create empty request
    RequestPb = #'GetCheckpointStatusRequest'{},
    Request = flow_pb:encode_msg(RequestPb, 'GetCheckpointStatusRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    
    %% Call get_checkpoint_status
    {ok, Response, _} = router_admin_grpc:get_checkpoint_status(Ctx, Request),
    
    %% Decode response
    ResponsePb = flow_pb:decode_msg(Response, 'GetCheckpointStatusResponse'),
    Status = ResponsePb#'GetCheckpointStatusResponse'.status,
    _CurrentCp = Status#'CheckpointStatus'.current_cp,
    _Cp2PlusAllowed = Status#'CheckpointStatus'.cp2_plus_allowed,
    
    ok.