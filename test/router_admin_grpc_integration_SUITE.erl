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
-export([init_per_testcase/2, end_per_testcase/2, suite/0]).



-export([
    test_upsert_policy_success/1,

    test_upsert_policy_unauthorized/1,
    test_delete_policy_success/1,
    test_delete_policy_not_found/1,
    test_get_policy_success/1,
    test_get_policy_not_found/1,
    test_list_policies_success/1,
    test_get_checkpoint_status/1,
    create_context_with_user/1
]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    Level = case os:getenv("ROUTER_TEST_LEVEL") of
        "heavy" -> heavy;
        "full"  -> full;
        _       -> fast
    end,
    groups_for_level(Level).

groups_for_level(heavy) ->
    [{group, integration_tests}];
groups_for_level(full) ->
    [{group, integration_tests}];
groups_for_level(_) -> %% fast
    [{group, integration_tests}].

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
    router_test_bootstrap:init_per_suite(Config, #{
        start => ensure_all_started,
        app_env => #{
            grpc_port => 0,
            grpc_enabled => true,
            admin_grpc_enabled => true,
            cp2_plus_allowed => true,
            nats_mode => mock,
            rbac_test_mode => true,
            admin_api_key => <<"test-admin-key">>
        }
    }).

end_per_suite(Config) ->
    router_test_bootstrap:end_per_suite(Config, #{
        start => ensure_all_started,
        stop => stop_app
    }),
    ok.

init_per_testcase(_Case, Config) ->
    BaseConfig = router_test_bootstrap:init_per_testcase(_Case, Config, #{}),
    ok = router_policy_store:reset(),
    TestUserId = <<"test-user">>,
    [{test_user_id, TestUserId} | BaseConfig].

end_per_testcase(_Case, _Config) ->
    router_test_bootstrap:end_per_testcase(_Case, [], #{}),
    ok.

%% @doc Create context with authentication and user ID
create_context_with_user(UserId) ->
    AdminKey = router_admin_grpc:get_admin_key(),
    Metadata = [
        {<<"x-api-key">>, AdminKey},
        {<<"x-user-id">>, UserId},
        {<<"x-tenant-id">>, <<"test_tenant">>}
    ],
    #{metadata => Metadata}.

%% @doc Test: UpsertPolicy with valid authentication
test_upsert_policy_success(Config) ->
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
    Ctx = create_context_with_user(TestUserId),
    
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
            %% Some definitions might return binary, some integer
            StatusInt = case Status of
                Bin when is_binary(Bin) -> binary_to_integer(Bin);
                Int when is_integer(Int) -> Int
            end,
            ?assertEqual(16, StatusInt),  %% GRPC_STATUS_UNAUTHENTICATED = 16
            ok
    end.

%% @doc Test: GetPolicy with valid authentication
test_get_policy_success(Config) ->
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
    Ctx = create_context_with_user(TestUserId),
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
            Ctx = create_context_with_user(TestUserId),
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
    Ctx = create_context_with_user(TestUserId),
    
    %% Call list_policies
    {ok, ListResponse, _} = router_admin_grpc:list_policies(Ctx, ListRequest),
    
    %% Decode response
    ListResponsePb = flow_pb:decode_msg(ListResponse, 'ListPoliciesResponse'),
    Policies = ListResponsePb#'ListPoliciesResponse'.policies,
    ?assertEqual(3, length(Policies)),
    
    ok.

%% @doc Test: DeletePolicy with valid authentication
test_delete_policy_success(Config) ->
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
    Ctx = create_context_with_user(TestUserId),
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
    TenantId = <<"test_tenant">>,
    PolicyId = <<"non_existent_policy">>,
    
    %% Delete non-existent policy
    DeleteRequestPb = #'DeletePolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    DeleteRequest = flow_pb:encode_msg(DeleteRequestPb, 'DeletePolicyRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(TestUserId),
    
    %% Call delete_policy - should fail with NOT_FOUND
    try
        router_admin_grpc:delete_policy(Ctx, DeleteRequest),
        ct:fail("Expected NOT_FOUND error")
    catch
        {grpc_error, {Status, _Msg}} ->
            StatusInt = case Status of
                Bin when is_binary(Bin) -> binary_to_integer(Bin);
                Int when is_integer(Int) -> Int
            end,
            ?assertEqual(5, StatusInt),  %% GRPC_STATUS_NOT_FOUND = 5
            ok
    end.

%% @doc Test: GetPolicy with non-existent policy
test_get_policy_not_found(Config) ->
    TenantId = <<"test_tenant">>,
    PolicyId = <<"non_existent_policy">>,
    
    %% Get non-existent policy
    GetRequestPb = #'GetPolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(TestUserId),
    
    %% Call get_policy - should fail with NOT_FOUND
    try
        router_admin_grpc:get_policy(Ctx, GetRequest),
        ct:fail("Expected NOT_FOUND error")
    catch
        {grpc_error, {Status, _Msg}} ->
            StatusInt = case Status of
                Bin when is_binary(Bin) -> binary_to_integer(Bin);
                Int when is_integer(Int) -> Int
            end,
            ?assertEqual(5, StatusInt),  %% GRPC_STATUS_NOT_FOUND = 5
            ok
    end.

%% @doc Test: GetCheckpointStatus with valid authentication
test_get_checkpoint_status(Config) ->
    %% Create empty request
    RequestPb = #'GetCheckpointStatusRequest'{},
    Request = flow_pb:encode_msg(RequestPb, 'GetCheckpointStatusRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(TestUserId),
    
    %% Call get_checkpoint_status
    {ok, Response, _} = router_admin_grpc:get_checkpoint_status(Ctx, Request),
    
    %% Decode response
    ResponsePb = flow_pb:decode_msg(Response, 'GetCheckpointStatusResponse'),
    Status = ResponsePb#'GetCheckpointStatusResponse'.status,
    _CurrentCp = Status#'CheckpointStatus'.current_cp,
    _Cp2PlusAllowed = Status#'CheckpointStatus'.cp2_plus_allowed,
    
    ok.
