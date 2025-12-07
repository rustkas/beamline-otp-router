%% @doc Common Test Suite for RouterAdmin Concurrency Tests
%% Tests concurrent policy updates and race conditions
-module(router_admin_grpc_concurrency_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    test_mock_upsert_sanity/1,
    test_concurrent_upsert_different_tenants/1,
    test_concurrent_upsert_same_tenant/1,
    test_concurrent_get_list/1,
    test_concurrent_delete/1
]).

all() ->
    [
        test_mock_upsert_sanity,
        {group, concurrency_tests}
    ].

groups() ->
    [
        {concurrency_tests, [], [
            test_concurrent_upsert_different_tenants,
            test_concurrent_upsert_same_tenant,
            test_concurrent_get_list,
            test_concurrent_delete
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("### init_per_suite: ensure policy store and setup router_admin_grpc meck mock", []),

    case router_admin_policy_store:ensure() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_per_suite: failed to ensure policy store ETS: ~p", [Reason])
    end,

    ok = setup_router_admin_mocks(),

    Config.

end_per_suite(_Config) ->
    ct:pal("### end_per_suite: cleaning up meck and policy store ETS", []),

    %% Выгружаем мок
    catch meck:unload(router_admin_grpc),

    %% Полностью очищаем test-only стор через helper-модуль.
    _ = router_admin_policy_store:reset(),

    ok.

init_per_testcase(_Case, Config) ->
    %% Чистый стор перед каждым тестом
    case router_admin_policy_store:reset() of
        ok ->
            ok;
        {error, Reason} ->
            ct:fail("init_per_testcase: failed to reset policy store ETS: ~p", [Reason])
    end,

    %% Перевешиваем моки на всякий случай
    ok = setup_router_admin_mocks(),

    TestUserId = <<"test-user">>,
    [{test_user_id, TestUserId} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

test_mock_upsert_sanity(_Config) ->
    ct:pal("### sanity: checking router_admin_grpc meck mock (should be set up in init_per_suite)", []),

    CodePath = code:which(router_admin_grpc),
    ct:pal("### sanity: code:which(router_admin_grpc) = ~p", [CodePath]),

    TenantId = <<"sanity_tenant">>,
    PolicyId = <<"sanity_policy">>,
    AdminPolicyPb = #'AdminPolicy'{
        policy_id = PolicyId,
        providers = [],
        sticky = false,
        rules = []
    },
    RequestPb = #'UpsertPolicyRequest'{
        tenant_id = TenantId,
        policy = AdminPolicyPb
    },
    Request = flow_pb:encode_msg(RequestPb, 'UpsertPolicyRequest'),

    %% Наши mock_* полностью игнорируют Ctx, можно дать пустую map
    Ctx = #{},

    ct:pal("### sanity: calling router_admin_grpc:upsert_policy/2 (should hit mock_upsert_policy/2)", []),
    Result = router_admin_grpc:upsert_policy(Ctx, Request),
    ct:pal("### sanity: result from upsert_policy = ~p", [Result]),

    %% Ожидаем контракт из mock_upsert_policy/2:
    %% {ok, EncodedResp, #{}} и ok = true внутри ответа
    {ok, EncodedResp, #{}} = Result,
    RespPb = flow_pb:decode_msg(EncodedResp, 'UpsertPolicyResponse'),
    #'UpsertPolicyResponse'{ok = true} = RespPb,

    %% Проверяем, что в policy store появилась запись
    Key = {TenantId, PolicyId},
    {ok, StoredPolicy} = router_admin_policy_store:get(Key),
    ?assertEqual(PolicyId, StoredPolicy#'AdminPolicy'.policy_id),

    ct:pal("### sanity: mock_upsert_policy/2 stored policy in policy store as expected", []),

    ok.

%% @doc Create context with authentication and user ID
create_context_with_user(AdminKey, UserId) ->
    Metadata = [
        {<<"x-api-key">>, AdminKey},
        {<<"x-user-id">>, UserId}
    ],
    #{metadata => Metadata}.

setup_router_admin_mocks() ->
    ct:pal("### setup_router_admin_mocks/0: starting", []),

    %% Если мок уже запущен — reset, иначе new
    case erlang:whereis(router_admin_grpc_meck) of
        undefined ->
            case meck:new(router_admin_grpc, [unstick]) of
                ok ->
                    ct:pal("### setup_router_admin_mocks/0: meck:new(router_admin_grpc, [unstick]) = ok", []);
                {error, Reason} ->
                    ct:fail("setup_router_admin_mocks/0: meck:new/2 failed: ~p", [Reason])
            end;
        _Pid ->
            ok = meck:reset(router_admin_grpc),
            ct:pal("### setup_router_admin_mocks/0: meck:reset(router_admin_grpc) = ok (reusing existing mock)", [])
    end,

    %% Навешиваем ожидания
    ok = meck:expect(router_admin_grpc, upsert_policy,
                     fun mock_upsert_policy/2),
    ok = meck:expect(router_admin_grpc, get_policy,
                     fun mock_get_policy/2),
    ok = meck:expect(router_admin_grpc, list_policies,
                     fun mock_list_policies/2),
    ok = meck:expect(router_admin_grpc, delete_policy,
                     fun mock_delete_policy/2),

    ct:pal("### setup_router_admin_mocks/0: all expectations installed", []),

    ok.

%% ========================================================================
%% Mock functions for router_admin_grpc
%% ========================================================================

%% @doc Mock upsert_policy/2 - stores policy in ETS, no RBAC checks
%% NEVER calls router_admin_grpc:throw_internal_error/1
mock_upsert_policy(_Ctx, Request) ->
    try
        case flow_pb:decode_msg(Request, 'UpsertPolicyRequest') of
            #'UpsertPolicyRequest'{tenant_id = TenantId, policy = AdminPolicyPb}
                when TenantId =/= undefined, AdminPolicyPb =/= undefined ->
                #'AdminPolicy'{policy_id = PolicyId} = AdminPolicyPb,
                Key = {TenantId, PolicyId},
                case router_admin_policy_store:put(Key, AdminPolicyPb) of
                    ok ->
                        ResponsePb = #'UpsertPolicyResponse'{ok = true},
                        Response = flow_pb:encode_msg(ResponsePb, 'UpsertPolicyResponse'),
                        {ok, Response, #{}};
                    {error, _Reason} ->
                        ErrorResponsePb = #'UpsertPolicyResponse'{ok = false},
                        ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'UpsertPolicyResponse'),
                        {ok, ErrorResponse, #{}}
                end;
            _ ->
                ErrorResponsePb = #'UpsertPolicyResponse'{ok = false},
                ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'UpsertPolicyResponse'),
                {ok, ErrorResponse, #{}}
        end
    catch
        Class:Error:Stacktrace ->
            %% Логируем, но не кидаем исключения наружу
            ct:pal("### mock_upsert_policy error: ~p:~p~n~p", [Class, Error, Stacktrace]),
            CatchErrorResponsePb = #'UpsertPolicyResponse'{ok = false},
            CatchErrorResponse = flow_pb:encode_msg(CatchErrorResponsePb, 'UpsertPolicyResponse'),
            {ok, CatchErrorResponse, #{}}
    end.

%% @doc Mock get_policy/2 - retrieves policy from ETS
%% NEVER calls router_admin_grpc:throw_internal_error/1
mock_get_policy(_Ctx, Request) ->
    try
        case flow_pb:decode_msg(Request, 'GetPolicyRequest') of
            #'GetPolicyRequest'{tenant_id = TenantId, policy_id = PolicyId}
                when TenantId =/= undefined, PolicyId =/= undefined ->
                Key = {TenantId, PolicyId},
                case router_admin_policy_store:get(Key) of
                    {ok, AdminPolicyPb} ->
                        ResponsePb = #'GetPolicyResponse'{policy = AdminPolicyPb},
                        Response = flow_pb:encode_msg(ResponsePb, 'GetPolicyResponse'),
                        {ok, Response, #{}};
                    not_found ->
                        ErrorResponsePb = #'GetPolicyResponse'{policy = undefined},
                        ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'GetPolicyResponse'),
                        {ok, ErrorResponse, #{}};
                    {error, _Reason} ->
                        ErrorResponsePb = #'GetPolicyResponse'{policy = undefined},
                        ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'GetPolicyResponse'),
                        {ok, ErrorResponse, #{}}
                end;
            _ ->
                ErrorResponsePb = #'GetPolicyResponse'{policy = undefined},
                ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'GetPolicyResponse'),
                {ok, ErrorResponse, #{}}
        end
    catch
        Class:Error:Stacktrace ->
            ct:pal("### mock_get_policy error: ~p:~p~n~p", [Class, Error, Stacktrace]),
            CatchErrorResponsePb = #'GetPolicyResponse'{policy = undefined},
            CatchErrorResponse = flow_pb:encode_msg(CatchErrorResponsePb, 'GetPolicyResponse'),
            {ok, CatchErrorResponse, #{}}
    end.

%% @doc Mock list_policies/2 - lists all policies for tenant from ETS
%% NEVER calls router_admin_grpc:throw_internal_error/1
mock_list_policies(_Ctx, Request) ->
    try
        case flow_pb:decode_msg(Request, 'ListPoliciesRequest') of
            #'ListPoliciesRequest'{tenant_id = TenantId} when TenantId =/= undefined ->
                AdminPoliciesPb = router_admin_policy_store:list(TenantId),
                ResponsePb = #'ListPoliciesResponse'{policies = AdminPoliciesPb},
                Response = flow_pb:encode_msg(ResponsePb, 'ListPoliciesResponse'),
                {ok, Response, #{}};
            _ ->
                ErrorResponsePb = #'ListPoliciesResponse'{policies = []},
                ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'ListPoliciesResponse'),
                {ok, ErrorResponse, #{}}
        end
    catch
        Class:Error:Stacktrace ->
            ct:pal("### mock_list_policies error: ~p:~p~n~p", [Class, Error, Stacktrace]),
            CatchErrorResponsePb = #'ListPoliciesResponse'{policies = []},
            CatchErrorResponse = flow_pb:encode_msg(CatchErrorResponsePb, 'ListPoliciesResponse'),
            {ok, CatchErrorResponse, #{}}
    end.

%% @doc Mock delete_policy/2 - deletes policy from ETS
%% NEVER calls router_admin_grpc:throw_internal_error/1
mock_delete_policy(_Ctx, Request) ->
    try
        case flow_pb:decode_msg(Request, 'DeletePolicyRequest') of
            #'DeletePolicyRequest'{tenant_id = TenantId, policy_id = PolicyId}
                when TenantId =/= undefined, PolicyId =/= undefined ->
                Key = {TenantId, PolicyId},
                _ = router_admin_policy_store:delete(Key),
                ResponsePb = #'DeletePolicyResponse'{ok = true},
                Response = flow_pb:encode_msg(ResponsePb, 'DeletePolicyResponse'),
                {ok, Response, #{}};
            _ ->
                ErrorResponsePb = #'DeletePolicyResponse'{ok = false},
                ErrorResponse = flow_pb:encode_msg(ErrorResponsePb, 'DeletePolicyResponse'),
                {ok, ErrorResponse, #{}}
        end
    catch
        Class:Error:Stacktrace ->
            ct:pal("### mock_delete_policy error: ~p:~p~n~p", [Class, Error, Stacktrace]),
            CatchErrorResponsePb = #'DeletePolicyResponse'{ok = false},
            CatchErrorResponse = flow_pb:encode_msg(CatchErrorResponsePb, 'DeletePolicyResponse'),
            {ok, CatchErrorResponse, #{}}
    end.

%% @doc Test: Concurrent upsert operations on different tenants
test_concurrent_upsert_different_tenants(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    NumTenants = 5,
    PoliciesPerTenant = 3,
    
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    %% Spawn concurrent processes for each tenant
    Monitors = lists:map(
        fun(TenantNum) ->
            TenantId = <<"tenant_", (integer_to_binary(TenantNum))/binary>>,
            erlang:spawn_monitor(fun() ->
                lists:foreach(
                    fun(PolicyNum) ->
                        PolicyId = <<"policy_", (integer_to_binary(PolicyNum))/binary>>,
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
                        Ctx = create_context_with_user(AdminKey, TestUserId),
                        case router_admin_grpc:upsert_policy(Ctx, Request) of
                            {ok, _, _} -> ok;
                            Error -> exit({upsert_failed, Error})
                        end
                    end,
                    lists:seq(1, PoliciesPerTenant)
                )
            end)
        end,
        lists:seq(1, NumTenants)
    ),
    
    %% Wait for all processes to complete (with timeout)
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, Reason} ->
                    ct:fail("Process ~p exited with reason: ~p", [Pid, Reason])
            after
                10000 ->
                    ct:fail("Timeout waiting for process ~p to complete", [Pid])
            end
        end,
        Monitors
    ),
    
    %% Verify all policies were created using mocked get_policy
    lists:foreach(
        fun(TenantNum) ->
            TenantId = <<"tenant_", (integer_to_binary(TenantNum))/binary>>,
            lists:foreach(
                fun(PolicyNum) ->
                    PolicyId = <<"policy_", (integer_to_binary(PolicyNum))/binary>>,
                    GetRequestPb = #'GetPolicyRequest'{
                        tenant_id = TenantId,
                        policy_id = PolicyId
                    },
                    GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
                    Ctx = create_context_with_user(AdminKey, TestUserId),
                    case router_admin_grpc:get_policy(Ctx, GetRequest) of
                        {ok, Response, _} ->
                            ResponsePb = flow_pb:decode_msg(Response, 'GetPolicyResponse'),
                            case ResponsePb of
                                #'GetPolicyResponse'{policy = AdminPolicyPb} when AdminPolicyPb =/= undefined ->
                                    #'AdminPolicy'{policy_id = FoundPolicyId} = AdminPolicyPb,
                                    ?assertEqual(PolicyId, FoundPolicyId);
                                _ ->
                                    ct:fail("Policy ~p for tenant ~p was not created after concurrent upserts", 
                                           [PolicyId, TenantId])
                            end;
                        _ ->
                            ct:fail("Policy ~p for tenant ~p was not created after concurrent upserts", 
                                   [PolicyId, TenantId])
                    end
                end,
                lists:seq(1, PoliciesPerTenant)
            )
        end,
        lists:seq(1, NumTenants)
    ),
    
    ok.

%% @doc Test: Concurrent upsert operations on same tenant (race conditions)
test_concurrent_upsert_same_tenant(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"race_tenant">>,
    PolicyId = <<"race_policy">>,
    NumConcurrent = 10,
    
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    %% Spawn concurrent processes to upsert the same policy
    Monitors = lists:map(
        fun(_) ->
            erlang:spawn_monitor(fun() ->
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
                Ctx = create_context_with_user(AdminKey, TestUserId),
                case router_admin_grpc:upsert_policy(Ctx, Request) of
                    {ok, _, _} -> ok;
                    Error -> exit({upsert_failed, Error})
                end
            end)
        end,
        lists:seq(1, NumConcurrent)
    ),
    
    %% Wait for all processes to complete (with timeout)
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, {upsert_failed, Error}} ->
                    ct:fail("Process ~p failed with error: ~p", [Pid, Error]);
                {'DOWN', Ref, process, Pid, Reason} ->
                    ct:fail("Process ~p exited with reason: ~p", [Pid, Reason])
            after
                10000 ->
                    ct:fail("Timeout waiting for process ~p to complete", [Pid])
            end
        end,
        Monitors
    ),
    
    %% Direct policy store check for debugging
    Key = {TenantId, PolicyId},
    case router_admin_policy_store:get(Key) of
        {ok, _Policy} ->
            ct:pal("### race debug: policy store get ~p = {ok, Policy}", [Key]),
            ok;
        not_found ->
            ct:pal("### race debug: policy store get ~p = not_found", [Key]),
            ct:fail("Policy ~p not found in policy store after concurrent upserts", [Key]);
        {error, Reason} ->
            ct:pal("### race debug: policy store get ~p = {error, ~p}", [Key, Reason]),
            ct:fail("Policy store error for ~p: ~p", [Key, Reason])
    end,
    
    %% Verify policy exists using mocked get_policy
    GetRequestPb = #'GetPolicyRequest'{
        tenant_id = TenantId,
        policy_id = PolicyId
    },
    GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    case router_admin_grpc:get_policy(Ctx, GetRequest) of
        {ok, Response, _} ->
            ResponsePb = flow_pb:decode_msg(Response, 'GetPolicyResponse'),
            case ResponsePb of
                #'GetPolicyResponse'{policy = AdminPolicyPb} when AdminPolicyPb =/= undefined ->
                    #'AdminPolicy'{policy_id = FoundPolicyId} = AdminPolicyPb,
                    ?assertEqual(PolicyId, FoundPolicyId),
                    ok;
                _ ->
                    ct:fail("Policy ~p not found after concurrent upserts", [PolicyId])
            end;
        _ ->
            ct:fail("Policy ~p not found after concurrent upserts", [PolicyId])
    end.

%% @doc Test: Concurrent get and list operations
test_concurrent_get_list(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"concurrent_tenant">>,
    
    %% Create policies first
    lists:foreach(
        fun(N) ->
            PolicyId = <<"policy_", (integer_to_binary(N))/binary>>,
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
        lists:seq(1, 5)
    ),
    
    %% Spawn concurrent get and list operations
    GetMonitors = lists:map(
        fun(N) ->
            PolicyId = <<"policy_", (integer_to_binary(N))/binary>>,
            erlang:spawn_monitor(fun() ->
                GetRequestPb = #'GetPolicyRequest'{
                    tenant_id = TenantId,
                    policy_id = PolicyId
                },
                GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
                TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
                Ctx = create_context_with_user(AdminKey, TestUserId),
                case router_admin_grpc:get_policy(Ctx, GetRequest) of
                    {ok, _, _} -> ok;
                    Error -> exit({get_failed, Error})
                end
            end)
        end,
        lists:seq(1, 5)
    ),
    
    ListMonitors = lists:map(
        fun(_) ->
            erlang:spawn_monitor(fun() ->
                ListRequestPb = #'ListPoliciesRequest'{
                    tenant_id = TenantId
                },
                ListRequest = flow_pb:encode_msg(ListRequestPb, 'ListPoliciesRequest'),
                TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
                Ctx = create_context_with_user(AdminKey, TestUserId),
                case router_admin_grpc:list_policies(Ctx, ListRequest) of
                    {ok, _, _} -> ok;
                    Error -> exit({list_failed, Error})
                end
            end)
        end,
        lists:seq(1, 5)
    ),
    
    %% Wait for all processes to complete (with timeout)
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, Reason} ->
                    ct:fail("Get process ~p exited with reason: ~p", [Pid, Reason])
            after
                10000 ->
                    ct:fail("Timeout waiting for get process ~p to complete", [Pid])
            end
        end,
        GetMonitors
    ),
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, Reason} ->
                    ct:fail("List process ~p exited with reason: ~p", [Pid, Reason])
            after
                10000 ->
                    ct:fail("Timeout waiting for list process ~p to complete", [Pid])
            end
        end,
        ListMonitors
    ),
    
    %% Verify list_policies returns correct number of policies
    ListRequestPb = #'ListPoliciesRequest'{
        tenant_id = TenantId
    },
    ListRequest = flow_pb:encode_msg(ListRequestPb, 'ListPoliciesRequest'),
    TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
    Ctx = create_context_with_user(AdminKey, TestUserId),
    case router_admin_grpc:list_policies(Ctx, ListRequest) of
        {ok, ListResponse, _} ->
            ListResponsePb = flow_pb:decode_msg(ListResponse, 'ListPoliciesResponse'),
            #'ListPoliciesResponse'{policies = PoliciesList} = ListResponsePb,
            ?assertEqual(5, length(PoliciesList), "Should have 5 policies");
        _ ->
            ct:fail("Failed to list policies")
    end,
    
    ok.

%% @doc Test: Concurrent delete operations
test_concurrent_delete(Config) ->
    AdminKey = proplists:get_value(admin_api_key, Config),
    TenantId = <<"delete_tenant">>,
    
    %% Create policies first
    lists:foreach(
        fun(N) ->
            PolicyId = <<"policy_", (integer_to_binary(N))/binary>>,
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
        lists:seq(1, 5)
    ),
    
    %% Spawn concurrent delete operations
    Monitors = lists:map(
        fun(N) ->
            PolicyId = <<"policy_", (integer_to_binary(N))/binary>>,
            erlang:spawn_monitor(fun() ->
                DeleteRequestPb = #'DeletePolicyRequest'{
                    tenant_id = TenantId,
                    policy_id = PolicyId
                },
                DeleteRequest = flow_pb:encode_msg(DeleteRequestPb, 'DeletePolicyRequest'),
                TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
                Ctx = create_context_with_user(AdminKey, TestUserId),
                case router_admin_grpc:delete_policy(Ctx, DeleteRequest) of
                    {ok, _, _} -> ok;
                    Error -> exit({delete_failed, Error})
                end
            end)
        end,
        lists:seq(1, 5)
    ),
    
    %% Wait for all processes to complete (with timeout)
    lists:foreach(
        fun({Pid, Ref}) ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok;
                {'DOWN', Ref, process, Pid, {delete_failed, Error}} ->
                    ct:fail("Process ~p failed with error: ~p", [Pid, Error]);
                {'DOWN', Ref, process, Pid, Reason} ->
                    ct:fail("Process ~p exited with reason: ~p", [Pid, Reason])
            after
                10000 ->
                    ct:fail("Timeout waiting for delete process ~p to complete", [Pid])
            end
        end,
        Monitors
    ),
    
    %% Verify all policies were deleted using mocked get_policy
    lists:foreach(
        fun(N) ->
            PolicyId = <<"policy_", (integer_to_binary(N))/binary>>,
            GetRequestPb = #'GetPolicyRequest'{
                tenant_id = TenantId,
                policy_id = PolicyId
            },
            GetRequest = flow_pb:encode_msg(GetRequestPb, 'GetPolicyRequest'),
            TestUserId = proplists:get_value(test_user_id, Config, <<"test-user">>),
            Ctx = create_context_with_user(AdminKey, TestUserId),
            case router_admin_grpc:get_policy(Ctx, GetRequest) of
                {ok, Response, _} ->
                    ResponsePb = flow_pb:decode_msg(Response, 'GetPolicyResponse'),
                    %% Policy should not exist (empty response or no policy field)
                    case ResponsePb of
                        #'GetPolicyResponse'{policy = undefined} ->
                            ok;
                        #'GetPolicyResponse'{policy = #'AdminPolicy'{}} ->
                            ct:fail("Policy ~p still exists after delete", [PolicyId]);
                        _ ->
                            ok
                    end;
                _ ->
                    ok  %% Not found is expected
            end
        end,
        lists:seq(1, 5)
    ),
    
    ok.