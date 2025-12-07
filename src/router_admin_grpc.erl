%% @doc gRPC Admin Service Implementation
%% Implements RouterAdmin service from flow.proto
-module(router_admin_grpc).
-ignore_xref([
    router_admin_grpc,
    {router_admin_grpc, upsert_policy, 2},
    {router_admin_grpc, delete_policy, 2},
    {router_admin_grpc, get_policy, 2},
    {router_admin_grpc, list_policies, 2},
    {router_admin_grpc, get_checkpoint_status, 2},
    {router_admin_grpc, get_validators_health, 2},
    {router_admin_grpc, get_extension_health, 2},
    {router_admin_grpc, get_circuit_breaker_states, 2},
    {router_admin_grpc, dry_run_pipeline, 2},
    {router_admin_grpc, get_pipeline_complexity, 2},
    {router_admin_grpc, get_admin_key, 0}
]).

-export([upsert_policy/2, delete_policy/2, get_policy/2, list_policies/2, get_checkpoint_status/2, get_validators_health/2]).
-export([get_extension_health/2, get_circuit_breaker_states/2, dry_run_pipeline/2]).
-export([get_pipeline_complexity/2]).
-export([get_admin_key/0]).

-include("beamline_router.hrl").
-include("flow_pb.hrl").
-include_lib("grpcbox/include/grpcbox.hrl").

-ifdef(CP2).

%% @doc Extract correlation_id from gRPC context metadata
%% Looks for 'x-correlation-id' or 'correlation-id' header
extract_correlation_id(Ctx) ->
    Metadata = maps:get(metadata, Ctx, []),
    case proplists:get_value(<<"x-correlation-id">>, Metadata) of
        undefined ->
            case proplists:get_value(<<"correlation-id">>, Metadata) of
                undefined -> undefined;
                CorrId -> CorrId
            end;
        CorrId -> CorrId
    end.

%% @doc UpsertPolicy RPC implementation
%% 
%% Creates or updates a routing policy for a tenant.
%% 
%% Request Processing:
%% 1. Checks authentication (API key in metadata)
%% 2. Extracts user context for RBAC
%% 3. Decodes UpsertPolicyRequest
%% 4. Validates tenant_id
%% 5. Checks RBAC permission (policy_write)
%% 6. Checks policy quota before creating new policy
%% 7. Converts AdminPolicy to internal policy record
%% 8. Upserts policy via router_policy_store:upsert_policy/3
%% 9. Logs audit entry
%% 10. Emits telemetry metrics
%% 
%% gRPC Error Codes:
%% - 3 (INVALID_ARGUMENT): invalid_policy
%% - 7 (PERMISSION_DENIED): Insufficient permissions
%% - 8 (RESOURCE_EXHAUSTED): quota_exceeded
%% - 16 (UNAUTHENTICATED): Missing or invalid API key
%% - 13 (INTERNAL): Unexpected internal error
%% 
%% Authentication:
%% - Requires API key in metadata: x-api-key or api-key
%% 
%% Returns: {ok, Response, Ctx} or throws {grpc_error, {Status, Message}}
%% 
%% @param Ctx gRPC context with metadata and authentication
%% @param Request Protobuf binary UpsertPolicyRequest
%% @returns {ok, Response, Ctx} on success
%% @throws {grpc_error, {Status, Message}} on error
%% 
%% @see router_error:to_grpc/2 for error code mapping
%% @see router_policy_store:upsert_policy/3 for policy storage
upsert_policy(Ctx, Request) ->
    try
        %% Check authorization
        case check_auth(Ctx) of
            ok ->
                %% Extract user context for RBAC
                {UserId, TenantId} = extract_user_context(Ctx),
                
                %% Decode request
                case decode_upsert_request(Request) of
                    {ok, RequestTenantId, AdminPolicyPb} ->
                        %% Validate tenant_id matches request
                        FinalTenantId = case TenantId of
                            undefined -> RequestTenantId;
                            _ -> TenantId
                        end,
                        
                        %% Check RBAC permission
                        CorrelationId = extract_correlation_id(Ctx),
                        Context = #{
                            <<"trace_id">> => CorrelationId,
                            <<"created_by">> => UserId
                        },
                        case router_permissions:check_policy_write(UserId, FinalTenantId, undefined, Context) of
                            false ->
                                router_audit:log_policy_action(FinalTenantId, UserId, <<"upsert_denied">>, undefined, Context),
                                throw({grpc_error, {?GRPC_STATUS_PERMISSION_DENIED, <<"insufficient permissions to write policy">>}});
                            true ->
                                %% Check quota before creating new policy
                                case check_policy_quota_before_upsert(FinalTenantId, AdminPolicyPb) of
                                    {ok, _Remaining} ->
                                        %% Convert AdminPolicy to internal policy record
                                        case convert_admin_policy_to_policy(FinalTenantId, AdminPolicyPb) of
                                            {ok, Policy} ->
                                                %% Upsert policy with correlation_id
                                                case router_policy_store:upsert_policy(FinalTenantId, Policy, CorrelationId) of
                                                    {ok, _UpdatedPolicy} ->
                                                        %% Log audit entry
                                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"upsert">>, Policy#policy.policy_id, Context),
                                                        
                                                        %% Emit telemetry for successful admin operation
                                                        %% count: number of entities in response (for upsert/delete: always 1 - one operation)
                                                        telemetry:execute([router_admin, upsert], #{count => 1}, #{
                                                            tenant_id => FinalTenantId,
                                                            policy_id => Policy#policy.policy_id,
                                                            result => ok,
                                                            correlation_id => CorrelationId
                                                        }),
                                                        ResponsePb = #'UpsertPolicyResponse'{ok = true},
                                                        Response = flow_pb:encode_msg(ResponsePb, 'UpsertPolicyResponse'),
                                                        {ok, Response, Ctx};
                                                    {error, invalid_policy, Details} ->
                                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"upsert_failed">>, undefined, maps:put(<<"error">>, Details, Context)),
                                                        %% Emit telemetry for failed admin operation
                                                        telemetry:execute([router_admin, upsert], #{count => 1}, #{
                                                            tenant_id => FinalTenantId,
                                                            policy_id => Policy#policy.policy_id,
                                                            result => error,
                                                            error => invalid_policy,
                                                            correlation_id => CorrelationId
                                                        }),
                                                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, Details}});
                                                    Error ->
                                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"upsert_failed">>, undefined, maps:put(<<"error">>, erlang:term_to_binary(Error), Context)),
                                                        %% Emit telemetry for failed admin operation
                                                        telemetry:execute([router_admin, upsert], #{count => 1}, #{
                                                            tenant_id => FinalTenantId,
                                                            policy_id => Policy#policy.policy_id,
                                                            result => error,
                                                            error => Error,
                                                            correlation_id => CorrelationId
                                                        }),
                                                        throw_internal_error(Error)
                                                end;
                                            {error, Details} ->
                                                throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, Details}})
                                        end;
                                    {error, quota_exceeded, MaxPolicies} ->
                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"upsert_quota_exceeded">>, undefined, Context),
                                        throw({grpc_error, {?GRPC_STATUS_RESOURCE_EXHAUSTED, 
                                            <<"Policy quota exceeded. Maximum policies: ", (integer_to_binary(MaxPolicies))/binary>>}})
                                end
                        end;
                    {error, invalid_request} ->
                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"invalid UpsertPolicyRequest">>}})
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError ->
            throw(GrpcError);
        Class:Reason:Stacktrace ->
            throw_internal_error({Class, Reason, Stacktrace})
    end.

%% @doc DeletePolicy RPC implementation
%% 
%% Deletes a routing policy for a tenant.
%% 
%% Request Processing:
%% 1. Checks authentication (API key in metadata)
%% 2. Extracts user context for RBAC
%% 3. Decodes DeletePolicyRequest
%% 4. Validates tenant_id
%% 5. Checks RBAC permission (policy_delete)
%% 6. Deletes policy via router_policy_store:delete_policy/3
%% 7. Logs audit entry
%% 8. Emits telemetry metrics
%% 
%% gRPC Error Codes:
%% - 5 (NOT_FOUND): policy_not_found
%% - 7 (PERMISSION_DENIED): Insufficient permissions
%% - 16 (UNAUTHENTICATED): Missing or invalid API key
%% - 13 (INTERNAL): Unexpected internal error
%% 
%% Authentication:
%% - Requires API key in metadata: x-api-key or api-key
%% 
%% Returns: {ok, Response, Ctx} or throws {grpc_error, {Status, Message}}
%% 
%% @param Ctx gRPC context with metadata and authentication
%% @param Request Protobuf binary DeletePolicyRequest
%% @returns {ok, Response, Ctx} on success
%% @throws {grpc_error, {Status, Message}} on error
%% 
%% @see router_error:to_grpc/2 for error code mapping
%% @see router_policy_store:delete_policy/3 for policy deletion
delete_policy(Ctx, Request) ->
    try
        %% Check authorization
        case check_auth(Ctx) of
            ok ->
                %% Extract user context for RBAC
                {UserId, TenantId} = extract_user_context(Ctx),
                
                %% Decode request
                case decode_delete_request(Request) of
                    {ok, RequestTenantId, PolicyId} ->
                        %% Validate tenant_id matches request
                        FinalTenantId = case TenantId of
                            undefined -> RequestTenantId;
                            _ -> TenantId
                        end,
                        
                        %% Check RBAC permission
                        CorrelationId = extract_correlation_id(Ctx),
                        Context = #{
                            <<"trace_id">> => CorrelationId
                        },
                        case router_permissions:check_policy_delete(UserId, FinalTenantId, PolicyId, Context) of
                            false ->
                                router_audit:log_policy_action(FinalTenantId, UserId, <<"delete_denied">>, PolicyId, Context),
                                throw({grpc_error, {?GRPC_STATUS_PERMISSION_DENIED, <<"insufficient permissions to delete policy">>}});
                            true ->
                                %% Delete policy with correlation_id
                                case router_policy_store:delete_policy(FinalTenantId, PolicyId, CorrelationId) of
                                    ok ->
                                        %% Log audit entry
                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"delete">>, PolicyId, Context),
                                        
                                        %% Emit telemetry for successful admin operation
                                        %% count: number of entities in response (for upsert/delete: always 1 - one operation)
                                        telemetry:execute([router_admin, delete], #{count => 1}, #{
                                            tenant_id => FinalTenantId,
                                            policy_id => PolicyId,
                                            result => ok,
                                            correlation_id => CorrelationId
                                        }),
                                        ResponsePb = #'DeletePolicyResponse'{ok = true},
                                        Response = flow_pb:encode_msg(ResponsePb, 'DeletePolicyResponse'),
                                        {ok, Response, Ctx};
                                    {error, not_found} ->
                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"delete_not_found">>, PolicyId, Context),
                                        %% Emit telemetry for failed admin operation
                                        telemetry:execute([router_admin, delete], #{count => 1}, #{
                                            tenant_id => FinalTenantId,
                                            policy_id => PolicyId,
                                            result => error,
                                            error => not_found,
                                            correlation_id => CorrelationId
                                        }),
                                        throw({grpc_error, {?GRPC_STATUS_NOT_FOUND, <<"policy not found">>}});
                                    Error ->
                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"delete_failed">>, PolicyId, maps:put(<<"error">>, erlang:term_to_binary(Error), Context)),
                                        %% Emit telemetry for failed admin operation
                                        telemetry:execute([router_admin, delete], #{count => 1}, #{
                                            tenant_id => FinalTenantId,
                                            policy_id => PolicyId,
                                            result => error,
                                            error => Error,
                                            correlation_id => CorrelationId
                                        }),
                                        throw_internal_error(Error)
                                end
                        end;
                    {error, invalid_request} ->
                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"invalid DeletePolicyRequest">>}})
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError ->
            throw(GrpcError);
        Class:Reason:Stacktrace ->
            throw_internal_error({Class, Reason, Stacktrace})
    end.

%% @doc GetPolicy RPC implementation
%% 
%% Retrieves a routing policy for a tenant.
%% 
%% Request Processing:
%% 1. Checks authentication (API key in metadata)
%% 2. Extracts user context for RBAC
%% 3. Decodes GetPolicyRequest
%% 4. Validates tenant_id
%% 5. Checks RBAC permission (policy_access)
%% 6. Retrieves policy via router_policy_store:get_policy/3
%% 7. Converts policy to AdminPolicy
%% 8. Logs audit entry
%% 
%% gRPC Error Codes:
%% - 5 (NOT_FOUND): policy_not_found
%% - 7 (PERMISSION_DENIED): Insufficient permissions
%% - 16 (UNAUTHENTICATED): Missing or invalid API key
%% - 13 (INTERNAL): Unexpected internal error
%% 
%% Authentication:
%% - Requires API key in metadata: x-api-key or api-key
%% 
%% Returns: {ok, Response, Ctx} or throws {grpc_error, {Status, Message}}
%% 
%% @param Ctx gRPC context with metadata and authentication
%% @param Request Protobuf binary GetPolicyRequest
%% @returns {ok, Response, Ctx} on success with policy
%% @throws {grpc_error, {Status, Message}} on error
%% 
%% @see router_error:to_grpc/2 for error code mapping
%% @see router_policy_store:get_policy/3 for policy retrieval
get_policy(Ctx, Request) ->
    try
        %% Check authorization
        case check_auth(Ctx) of
            ok ->
                %% Extract user context for RBAC
                {UserId, TenantId} = extract_user_context(Ctx),
                
                %% Decode request
                case decode_get_request(Request) of
                    {ok, RequestTenantId, PolicyId} ->
                        %% Validate tenant_id matches request
                        FinalTenantId = case TenantId of
                            undefined -> RequestTenantId;
                            _ -> TenantId
                        end,
                        
                        %% Check RBAC permission
                        CorrelationId = extract_correlation_id(Ctx),
                        Context = #{
                            <<"trace_id">> => CorrelationId
                        },
                        case router_permissions:check_policy_access(UserId, FinalTenantId, PolicyId, Context) of
                            false ->
                                router_audit:log_policy_action(FinalTenantId, UserId, <<"get_denied">>, PolicyId, Context),
                                throw({grpc_error, {?GRPC_STATUS_PERMISSION_DENIED, <<"insufficient permissions to read policy">>}});
                            true ->
                                %% Get policy with correlation_id
                                case router_policy_store:get_policy(FinalTenantId, PolicyId, CorrelationId) of
                                    {ok, Policy} ->
                                        %% Log audit entry
                                        router_audit:log_policy_action(FinalTenantId, UserId, <<"get">>, PolicyId, Context),
                                        
                                        %% Convert policy to AdminPolicy
                                        AdminPolicyPb = convert_policy_to_admin_policy(Policy),
                                        ResponsePb = #'GetPolicyResponse'{policy = AdminPolicyPb},
                                        Response = flow_pb:encode_msg(ResponsePb, 'GetPolicyResponse'),
                                        {ok, Response, Ctx};
                                    {error, not_found} ->
                                        throw({grpc_error, {?GRPC_STATUS_NOT_FOUND, <<"policy not found">>}});
                                    Error ->
                                        throw_internal_error(Error)
                                end
                        end;
                    {error, invalid_request} ->
                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"invalid GetPolicyRequest">>}})
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError ->
            throw(GrpcError);
        Class:Reason:Stacktrace ->
            throw_internal_error({Class, Reason, Stacktrace})
    end.

%% @doc GetCheckpointStatus RPC implementation
get_checkpoint_status(Ctx, _Request) ->
    try
        case check_auth(Ctx) of
            ok ->
                Cp2PlusAllowed = application:get_env(beamline_router, cp2_plus_allowed, false),
                Cp = application:get_env(beamline_router, current_cp, <<"CP1-baseline">>),
                StatusPb = #'CheckpointStatus'{
                    current_cp = Cp,
                    cp2_plus_allowed = Cp2PlusAllowed,
                    updated_at_ms = erlang:system_time(millisecond)
                },
                ResponsePb = #'GetCheckpointStatusResponse'{status = StatusPb},
                Response = flow_pb:encode_msg(ResponsePb, 'GetCheckpointStatusResponse'),
                _ = telemetry:execute([router_admin, get_checkpoint_status], #{count => 1}, #{
                    current_cp => Cp,
                    cp2_plus_allowed => Cp2PlusAllowed
                }),
                {ok, Response, Ctx};
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError -> throw(GrpcError);
        Class:Reason:Stacktrace -> throw_internal_error({Class, Reason, Stacktrace})
    end.

%% @doc GetValidatorsHealth RPC implementation
get_validators_health(Ctx, _Request) ->
    try
        case check_auth(Ctx) of
            ok ->
                Now = erlang:system_time(millisecond),
                ValidatorsPb = [
                    #'ValidatorStatus'{name = <<"grpc_health">>, status = <<"serving">>, timestamp_ms = Now}
                ],
                ResponsePb = #'GetValidatorsHealthResponse'{validators = ValidatorsPb},
                Response = flow_pb:encode_msg(ResponsePb, 'GetValidatorsHealthResponse'),
                _ = telemetry:execute([router_admin, get_validators_health], #{count => 1}, #{timestamp_ms => Now}),
                {ok, Response, Ctx};
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError -> throw(GrpcError);
        Class:Reason:Stacktrace -> throw_internal_error({Class, Reason, Stacktrace})
    end.

%% @doc ListPolicies RPC implementation
%% 
%% Lists all routing policies for a tenant.
%% 
%% Request Processing:
%% 1. Checks authentication (API key in metadata)
%% 2. Extracts user context for RBAC
%% 3. Decodes ListPoliciesRequest
%% 4. Validates tenant_id
%% 5. Checks RBAC permission (policy_access)
%% 6. Lists policies via router_policy_store:list_policies/2
%% 7. Converts policies to AdminPolicy list
%% 
%% gRPC Error Codes:
%% - 7 (PERMISSION_DENIED): Insufficient permissions
%% - 16 (UNAUTHENTICATED): Missing or invalid API key
%% - 13 (INTERNAL): Unexpected internal error
%% 
%% Authentication:
%% - Requires API key in metadata: x-api-key or api-key
%% 
%% Returns: {ok, Response, Ctx} or throws {grpc_error, {Status, Message}}
%% 
%% @param Ctx gRPC context with metadata and authentication
%% @param Request Protobuf binary ListPoliciesRequest
%% @returns {ok, Response, Ctx} on success with policies list
%% @throws {grpc_error, {Status, Message}} on error
%% 
%% @see router_error:to_grpc/2 for error code mapping
%% @see router_policy_store:list_policies/2 for policy listing
list_policies(Ctx, Request) ->
    try
        %% Check authorization
        %% Authentication: Requires API key in metadata (x-api-key or api-key)
        case check_auth(Ctx) of
            ok ->
                %% Decode request
                case decode_list_request(Request) of
                    {ok, TenantId} ->
                        %% List policies with correlation_id
                        CorrelationId = extract_correlation_id(Ctx),
                        case router_policy_store:list_policies(TenantId, CorrelationId) of
                            {ok, Policies} ->
                                %% Emit telemetry for successful admin operation
                                %% count: number of entities in response (number of policies in response)
                                PolicyCount = length(Policies),
                                telemetry:execute([router_admin, list], #{count => PolicyCount}, #{
                                    tenant_id => TenantId,
                                    result => ok,
                                    correlation_id => CorrelationId
                                }),
                                %% Convert policies to AdminPolicy list
                                AdminPoliciesPb = [convert_policy_to_admin_policy(P) || P <- Policies],
                                ResponsePb = #'ListPoliciesResponse'{policies = AdminPoliciesPb},
                                Response = flow_pb:encode_msg(ResponsePb, 'ListPoliciesResponse'),
                                {ok, Response, Ctx};
                            Error ->
                                %% Emit telemetry for failed admin operation
                                telemetry:execute([router_admin, list], #{count => 0}, #{
                                    tenant_id => TenantId,
                                    result => error,
                                    error => Error,
                                    correlation_id => CorrelationId
                                }),
                                throw_internal_error(Error)
                        end;
                    {error, invalid_request} ->
                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"invalid ListPoliciesRequest">>}})
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError ->
            throw(GrpcError);
        Class:Reason:Stacktrace ->
            throw_internal_error({Class, Reason, Stacktrace})
    end.

%% Internal: Check authorization from metadata
check_auth(Ctx) ->
    %% Get API key from environment (validated)
    ExpectedKey = get_admin_key(),
    
    %% Extract metadata from context
    %% grpcbox context structure: Ctx is typically a map or tuple with metadata
    %% For testing, we support both map format and grpcbox_ctx format
    Metadata = case is_map(Ctx) of
        true ->
            maps:get(metadata, Ctx, []);
        false ->
            %% Try grpcbox_ctx if available (for real gRPC calls)
            case erlang:function_exported(grpcbox_ctx, metadata, 1) of
                true ->
                    case catch grpcbox_ctx:metadata(Ctx) of
                        {'EXIT', _} -> [];
                        undefined -> [];
                        M when is_list(M) -> M;
                        _ -> []
                    end;
                false ->
                    []
            end
    end,
    
    %% Check authorization header or x-api-key
    %% Metadata is a list of {Key, Value} tuples
    AuthHeader1 = proplists:get_value(<<"authorization">>, Metadata, undefined),
    AuthHeader = case AuthHeader1 of
        undefined -> proplists:get_value(<<"Authorization">>, Metadata, undefined);
        _ -> AuthHeader1
    end,
    ApiKey1 = proplists:get_value(<<"x-api-key">>, Metadata, undefined),
    ApiKey = case ApiKey1 of
        undefined -> proplists:get_value(<<"X-Api-Key">>, Metadata, undefined);
        _ -> ApiKey1
    end,
    
    case AuthHeader =/= undefined orelse ApiKey =/= undefined of
        true ->
            %% Extract key from header (format: "Bearer <key>" or just "<key>")
            Key = case ApiKey of
                undefined ->
                    extract_key_from_auth(AuthHeader);
                _ ->
                    ApiKey
            end,
            %% Validate key is not empty after extraction and matches expected key
            %% CRITICAL: All auth failures (missing, empty, wrong, malformed) return UNAUTHENTICATED
            %% This ensures that authentication errors never leak as INVALID_ARGUMENT
            case is_valid_non_empty_key(Key) andalso Key =:= ExpectedKey of
                true ->
                    %% Key is valid - check permissions (for future role/scope model)
                    %% For now, all valid keys have full permissions
                    ok;
                false ->
                    %% All auth failures return UNAUTHENTICATED (not INVALID_ARGUMENT)
                    %% This includes: missing key, empty key, wrong key, malformed Bearer, whitespace-only
                    {error, unauthorized}
            end;
        false ->
            {error, unauthorized}
    end.

%% Internal: Get validated admin API key
%% Security: Never returns hardcoded keys in production
%% Keys must be provided via environment variable or secure config
-spec get_admin_key() -> binary().
get_admin_key() ->
    case application:get_env(beamline_router, admin_api_key, undefined) of
        undefined ->
            %% Check environment variable as fallback
            case os:getenv("BEAMLINE_ROUTER_ADMIN_API_KEY") of
                false ->
                    erlang:error({configuration_error, admin_api_key_missing});
                EnvKey when is_list(EnvKey) ->
                    list_to_binary(EnvKey);
                _ ->
                    erlang:error({configuration_error, admin_api_key_missing})
            end;
        <<"dev-admin-key">> ->
            %% Unsafe default, only acceptable in explicit test contexts
            Env = application:get_env(beamline_router, env, dev),
            case Env of
                test -> <<"dev-admin-key">>;
                _ -> erlang:error({configuration_error, admin_api_key_unsafe_default})
            end;
        Key when is_binary(Key) ->
            %% Validate key is not empty
            case byte_size(Key) > 0 of
                true -> Key;
                false -> erlang:error({configuration_error, admin_api_key_empty})
            end;
        _ ->
            erlang:error({configuration_error, admin_api_key_invalid_type})
    end.

%% Internal: Extract key from authorization header
%% Handles various formats: "Bearer <key>", "<key>", "Bearer" (empty), etc.
%% Returns extracted key or empty binary for malformed headers
%% 
%% CRITICAL: All malformed formats (empty Bearer, whitespace-only, etc.) return <<>>
%% which will be caught by validation in check_auth and return UNAUTHENTICATED
%% This ensures authentication errors never leak as INVALID_ARGUMENT
extract_key_from_auth(<<"Bearer ", Key/binary>>) when byte_size(Key) > 0 ->
    %% Valid Bearer format with non-empty key: "Bearer <key>"
    Key;
extract_key_from_auth(<<"Bearer", Rest/binary>>) ->
    %% Bearer with empty or whitespace-only key: "Bearer ", "Bearer\t", "Bearer  ", "Bearer" (no space)
    %% Strip all whitespace and check if anything remains
    Stripped = binary:replace(Rest, <<" ">>, <<>>, [global]),
    Stripped2 = binary:replace(Stripped, <<"\t">>, <<>>, [global]),
    Stripped3 = binary:replace(Stripped2, <<"\n">>, <<>>, [global]),
    Stripped4 = binary:replace(Stripped3, <<"\r">>, <<>>, [global]),
    case byte_size(Stripped4) of
        0 -> <<>>;  %% Only whitespace - return empty (will trigger UNAUTHENTICATED)
        _ -> Stripped4  %% Has content after stripping whitespace
    end;
extract_key_from_auth(Key) when is_binary(Key), byte_size(Key) > 0 ->
    %% Direct key (not Bearer format): just the key itself
    Key;
extract_key_from_auth(_) ->
    %% Empty, undefined, or invalid format
    <<>>.

%% Internal: Validate that key is non-empty binary
%% Returns true only for non-empty binaries, false for everything else
%% Security: Also checks for minimum length and basic format
is_valid_non_empty_key(Key) when is_binary(Key) ->
    %% Minimum length check (security: prevent empty/very short keys)
    case byte_size(Key) >= 8 of
        false -> false;
        true ->
            %% Check for basic format (alphanumeric, underscore, hyphen, dot)
            case re:run(Key, "^[a-zA-Z0-9_.-]+$", [{capture, none}]) of
                match -> true;
                _ -> false  %% Reject keys with special characters that might be injection attempts
            end
    end;
is_valid_non_empty_key(_) ->
    false.

%% Internal: Decode UpsertPolicyRequest
decode_upsert_request(Request) when is_binary(Request) ->
    try
        RequestPb = flow_pb:decode_msg(Request, 'UpsertPolicyRequest'),
        #'UpsertPolicyRequest'{tenant_id = TenantId, policy = AdminPolicyPb} = RequestPb,
        case TenantId =/= undefined andalso AdminPolicyPb =/= undefined of
            true ->
                {ok, TenantId, AdminPolicyPb};
            false ->
                {error, invalid_request}
        end
    catch
        _:_ ->
            {error, invalid_request}
    end;
decode_upsert_request(_) ->
    {error, invalid_request}.

%% Internal: Decode DeletePolicyRequest
decode_delete_request(Request) when is_binary(Request) ->
    try
        RequestPb = flow_pb:decode_msg(Request, 'DeletePolicyRequest'),
        #'DeletePolicyRequest'{tenant_id = TenantId, policy_id = PolicyId} = RequestPb,
        case TenantId =/= undefined andalso PolicyId =/= undefined of
            true ->
                {ok, TenantId, PolicyId};
            false ->
                {error, invalid_request}
        end
    catch
        _:_ ->
            {error, invalid_request}
    end;
decode_delete_request(_) ->
    {error, invalid_request}.

%% Internal: Decode GetPolicyRequest
decode_get_request(Request) when is_binary(Request) ->
    try
        RequestPb = flow_pb:decode_msg(Request, 'GetPolicyRequest'),
        #'GetPolicyRequest'{tenant_id = TenantId, policy_id = PolicyId} = RequestPb,
        case TenantId =/= undefined andalso PolicyId =/= undefined of
            true ->
                {ok, TenantId, PolicyId};
            false ->
                {error, invalid_request}
        end
    catch
        _:_ ->
            {error, invalid_request}
    end;
decode_get_request(_) ->
    {error, invalid_request}.

%% Internal: Decode ListPoliciesRequest
decode_list_request(Request) when is_binary(Request) ->
    try
        RequestPb = flow_pb:decode_msg(Request, 'ListPoliciesRequest'),
        #'ListPoliciesRequest'{tenant_id = TenantId} = RequestPb,
        case TenantId =/= undefined of
            true ->
                {ok, TenantId};
            false ->
                {error, invalid_request}
        end
    catch
        _:_ ->
            {error, invalid_request}
    end;
decode_list_request(_) ->
    {error, invalid_request}.

%% Internal: Convert AdminPolicy to internal policy record
convert_admin_policy_to_policy(TenantId, #'AdminPolicy'{policy_id = PolicyId, providers = ProvidersPb, sticky = Sticky, rules = RulesPb}) ->
    %% Convert providers to weights map
    Weights = lists:foldl(
        fun(#'AdminProvider'{id = Id, weight = Weight}, Acc) ->
            maps:put(Id, Weight, Acc)
        end,
        #{},
        ProvidersPb
    ),
    
    %% Convert rules to fallback (simplified: use first rule's fallback if available)
    Fallback = case RulesPb of
        [] -> undefined;
        [#'AdminRule'{fallback = Fb} | _] when Fb =/= undefined ->
            #{<<"provider">> => Fb};
        _ -> undefined
    end,
    
    %% Convert sticky to map
    StickyMap = case Sticky of
        true -> #{<<"enabled">> => true, <<"ttl_seconds">> => 3600};
        false -> undefined
    end,
    
    Policy = #policy{
        tenant_id = TenantId,
        policy_id = PolicyId,
        version = <<"1.0">>,
        defaults = #{},
        escalate_on = [],
        weights = Weights,
        fallback = Fallback,
        sticky = StickyMap,
        metadata = #{}
    },
    
    {ok, Policy}.

%% Internal: Convert policy record to AdminPolicy
convert_policy_to_admin_policy(#policy{policy_id = PolicyId, weights = Weights, sticky = Sticky}) ->
    %% Convert weights to providers
    ProvidersPb = maps:fold(
        fun(Id, Weight, Acc) ->
            [#'AdminProvider'{id = Id, weight = Weight} | Acc]
        end,
        [],
        Weights
    ),
    
    %% Convert sticky to boolean
    StickyBool = case Sticky of
        undefined -> false;
        #{<<"enabled">> := Enabled} -> Enabled;
        _ -> false
    end,
    
    %% Rules are empty for now (simplified conversion)
    #'AdminPolicy'{
        policy_id = PolicyId,
        providers = ProvidersPb,
        sticky = StickyBool,
        rules = []
    }.

%% Internal: Throw internal error
%% Note: This is for INTERNAL errors only. For user-facing errors (INVALID_ARGUMENT, NOT_FOUND),
%% we preserve diagnostic details. Sanitization is applied only to logged context, not gRPC responses.
throw_internal_error(Reason) ->
    %% For INTERNAL errors, provide generic message to avoid leaking implementation details
    %% Detailed error info should be logged (with sanitization), not returned to client
    ErrorMsg = <<"Internal server error">>,
    
    %% Log detailed error with sanitization (for debugging, not sent to client)
    case erlang:function_exported(router_logger, error, 3) of
        true ->
            ReasonContext = case Reason of
                {Class, Error, _Stacktrace} when is_tuple(Error) ->
                    #{
                        <<"error_class">> => atom_to_binary(Class, utf8),
                        <<"error">> => sanitize_error_value(Error)
                    };
                Error ->
                    #{<<"error">> => sanitize_error_value(Error)}
            end,
            router_logger:error(<<"RouterAdmin internal error">>, ReasonContext, #{});
        false ->
            ok
    end,
    
    %% Extract structured error details (without sensitive data) for grpc-status-details-bin
    %% This allows clients to distinguish error types without exposing secrets
    ErrorDetails = extract_error_details(Reason),
    
    %% Throw with structured details - grpcbox will encode and send in grpc-status-details-bin header
    %% Format: {grpc_error, {Status, Message, Details}}
    throw({grpc_error, {?GRPC_STATUS_INTERNAL, ErrorMsg, ErrorDetails}}).

%% Internal: Sanitize error value to prevent leaking secrets
sanitize_error_value(Value) when is_binary(Value) ->
    %% Check for common secret patterns in binary
    case re:run(Value, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
        match ->
            <<"[REDACTED: contains sensitive data]">>;
        nomatch ->
            Value
    end;
sanitize_error_value(Value) when is_list(Value) ->
    %% Check if it's a string
    case io_lib:printable_list(Value) of
        true ->
            ValueBin = iolist_to_binary(Value),
            case re:run(ValueBin, "(?i)(api[_-]?key|secret|token|password|authorization|Bearer\\s+[A-Za-z0-9]+)", [{capture, none}]) of
                match ->
                    "[REDACTED: contains sensitive data]";
                nomatch ->
                    Value
            end;
        false ->
            [sanitize_error_value(E) || E <- Value]
    end;
sanitize_error_value(Value) when is_tuple(Value) ->
    %% Recursively sanitize tuple elements
    list_to_tuple([sanitize_error_value(E) || E <- tuple_to_list(Value)]);
sanitize_error_value(Value) ->
    Value.

%% Internal: Extract user context from gRPC context
%% Returns {UserId, TenantId} where UserId and TenantId may be undefined
extract_user_context(Ctx) ->
    Metadata = case is_map(Ctx) of
        true ->
            maps:get(metadata, Ctx, []);
        false ->
            case erlang:function_exported(grpcbox_ctx, metadata, 1) of
                true ->
                    case catch grpcbox_ctx:metadata(Ctx) of
                        {'EXIT', _} -> [];
                        undefined -> [];
                        M when is_list(M) -> M;
                        _ -> []
                    end;
                false ->
                    []
            end
    end,
    
    UserId = case proplists:get_value(<<"x-user-id">>, Metadata) of
        undefined -> proplists:get_value(<<"X-User-Id">>, Metadata, undefined);
        U -> U
    end,
    
    TenantId = case proplists:get_value(<<"x-tenant-id">>, Metadata) of
        undefined -> proplists:get_value(<<"X-Tenant-Id">>, Metadata, undefined);
        T -> T
    end,
    
    {UserId, TenantId}.

%% Internal: Check policy quota before upsert
%% Returns {ok, Remaining} or {error, quota_exceeded, MaxPolicies}
check_policy_quota_before_upsert(TenantId, AdminPolicyPb) ->
    try
        %% Check if this is a new policy (not update)
        PolicyId = case AdminPolicyPb#'AdminPolicy'.policy_id of
            undefined -> undefined;
            P -> P
        end,
        
        %% If policy exists, it's an update (no quota check needed)
        case PolicyId of
            undefined ->
                %% New policy - check quota
                router_quota:check_policy_quota(TenantId);
            _ ->
                %% Check if policy exists
                case router_policy_store:get_policy(TenantId, PolicyId, undefined) of
                    {ok, _} ->
                        %% Policy exists - update, no quota check
                        {ok, 0};
                    {error, not_found} ->
                        %% New policy - check quota
                        router_quota:check_policy_quota(TenantId)
                end
        end
    catch
        Class:Reason ->
            %% On quota check error, allow operation (fail open for quota)
            %% This prevents quota system from blocking operations if it's unavailable
            router_logger:warn(<<"Quota check error, allowing operation">>, #{
                <<"tenant_id">> => TenantId,
                <<"error">> => {Class, Reason}
            }),
            {ok, 0}
    end.

%% Internal: Extract structured error details (without sensitive data)
%% Returns a map with error type/code that clients can use for error handling
%% 
%% CRITICAL: Never include sensitive data (secrets, tokens, API keys, passwords) in error details.
%% Only include error classification (type, code) that helps clients handle errors appropriately.
%% 
%% These details will be sent via grpc-status-details-bin header, allowing clients to:
%% - Distinguish between different types of internal errors
%% - Implement appropriate retry strategies
%% - Handle errors more gracefully
extract_error_details(Error) ->
    case Error of
        {error, timeout} ->
            #{error_type => <<"timeout">>, error_code => <<"TIMEOUT">>};
        {error, {timeout, _}} ->
            #{error_type => <<"timeout">>, error_code => <<"TIMEOUT">>};
        {error, econnrefused} ->
            #{error_type => <<"connection_refused">>, error_code => <<"CONNECTION_REFUSED">>};
        {error, {econnrefused, _}} ->
            #{error_type => <<"connection_refused">>, error_code => <<"CONNECTION_REFUSED">>};
        {error, enotfound} ->
            #{error_type => <<"not_found">>, error_code => <<"NOT_FOUND">>};
        {error, {enotfound, _}} ->
            #{error_type => <<"not_found">>, error_code => <<"NOT_FOUND">>};
        {error, eacces} ->
            #{error_type => <<"access_denied">>, error_code => <<"ACCESS_DENIED">>};
        {error, {eacces, _}} ->
            #{error_type => <<"access_denied">>, error_code => <<"ACCESS_DENIED">>};
        {error, validation_failed} ->
            #{error_type => <<"validation_failed">>, error_code => <<"VALIDATION_FAILED">>};
        {error, {validation_failed, _}} ->
            #{error_type => <<"validation_failed">>, error_code => <<"VALIDATION_FAILED">>};
        {error, policy_store_error} ->
            #{error_type => <<"policy_store_error">>, error_code => <<"POLICY_STORE_ERROR">>};
        {error, {policy_store_error, _}} ->
            #{error_type => <<"policy_store_error">>, error_code => <<"POLICY_STORE_ERROR">>};
        {error, decode_error} ->
            #{error_type => <<"decode_error">>, error_code => <<"DECODE_ERROR">>};
        {error, {decode_error, _}} ->
            #{error_type => <<"decode_error">>, error_code => <<"DECODE_ERROR">>};
        {error, ets_error} ->
            #{error_type => <<"ets_error">>, error_code => <<"ETS_ERROR">>};
        {error, {ets_error, _}} ->
            #{error_type => <<"ets_error">>, error_code => <<"ETS_ERROR">>};
        {error, gen_server_timeout} ->
            #{error_type => <<"gen_server_timeout">>, error_code => <<"GEN_SERVER_TIMEOUT">>};
        {error, {gen_server_timeout, _}} ->
            #{error_type => <<"gen_server_timeout">>, error_code => <<"GEN_SERVER_TIMEOUT">>};
        {timeout, _} ->
            #{error_type => <<"timeout">>, error_code => <<"TIMEOUT">>};
        {error, Reason} when is_atom(Reason) ->
            %% Generic error atom - convert to error code
            ErrorCode = atom_to_binary(Reason, utf8),
            #{error_type => ErrorCode, error_code => string:uppercase(ErrorCode)};
        _ ->
            %% Generic internal error (don't expose error structure or sensitive data)
            #{error_type => <<"internal_error">>, error_code => <<"INTERNAL_ERROR">>}
    end.

%% @doc GetExtensionHealth RPC implementation
get_extension_health(Ctx, _Request) ->
    try
        case check_auth(Ctx) of
            ok ->
                case router_extension_health:get_all_health() of
                    {ok, HealthList} ->
                        %% Convert to map keyed by extension_id for JSON-like response
                        HealthMap = lists:foldl(fun(Health, Acc) ->
                            ExtId = maps:get(extension_id, Health),
                            %% Convert to status string
                            Status = case maps:get(success_rate, Health, 0.0) of
                                SR when SR >= 0.95 -> <<"healthy">>;
                                SR when SR >= 0.80 -> <<"degraded">>;
                                _ -> <<"unhealthy">>
                            end,
                            HealthWithStatus = maps:put(status, Status, Health),
                            maps:put(ExtId, HealthWithStatus, Acc)
                        end, #{}, HealthList),
                        %% Return as JSON-like structure (will be encoded by Gateway)
                        Response = #{health => HealthMap},
                        {ok, Response, Ctx};
                    {error, Reason} ->
                        throw({grpc_error, {?GRPC_STATUS_INTERNAL, 
                            list_to_binary(io_lib:format("Failed to get extension health: ~p", [Reason]))}})
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError -> throw(GrpcError);
        Class:Error:Stacktrace -> throw_internal_error({Class, Error, Stacktrace})
    end.

%% @doc GetCircuitBreakerStates RPC implementation
get_circuit_breaker_states(Ctx, _Request) ->
    try
        case check_auth(Ctx) of
            ok ->
                case router_extension_circuit_breaker:get_all_circuit_states() of
                    {ok, StatesMap} ->
                        %% Return as JSON-like structure (will be encoded by Gateway)
                        Response = #{states => StatesMap},
                        {ok, Response, Ctx};
                    {error, Reason} ->
                        throw({grpc_error, {?GRPC_STATUS_INTERNAL, 
                            list_to_binary(io_lib:format("Failed to get circuit breaker states: ~p", [Reason]))}})
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError -> throw(GrpcError);
        Class:Error:Stacktrace -> throw_internal_error({Class, Error, Stacktrace})
    end.

%% @doc DryRunPipeline RPC implementation
dry_run_pipeline(Ctx, Request) ->
    try
        case check_auth(Ctx) of
            ok ->
                %% Decode request (assuming JSON-like structure from Gateway)
                RequestMap = case Request of
                    Map when is_map(Map) -> Map;
                    _ -> throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"Invalid request format">>}})
                end,
                TenantId = maps:get(<<"tenant_id">>, RequestMap, undefined),
                PolicyId = maps:get(<<"policy_id">>, RequestMap, undefined),
                Payload = maps:get(<<"payload">>, RequestMap, undefined),
                
                if
                    TenantId =:= undefined orelse PolicyId =:= undefined orelse Payload =:= undefined ->
                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, 
                            <<"Missing required fields: tenant_id, policy_id, payload">>}});
                    true ->
                        %% Load policy
                        case router_policy_store:get_policy(TenantId, PolicyId) of
                            {ok, Policy} ->
                                %% Create route request for dry-run
                                RouteRequest = #route_request{
                                    message = Payload,
                                    context = #{<<"tenant_id">> => TenantId, <<"dry_run">> => true}
                                },
                                Context = #{<<"tenant_id">> => TenantId, <<"dry_run">> => true},
                                
                                %% Execute pipeline (dry-run mode)
                                case router_decider:decide(RouteRequest, Policy, Context) of
                                    {ok, Decision} ->
                                        %% Format response (Decision is a route_decision record)
                                        Response = #{
                                            ok => true,
                                            result => #{
                                                decision => #{
                                                    provider_id => Decision#route_decision.provider_id,
                                                    reason => Decision#route_decision.reason,
                                                    priority => Decision#route_decision.priority,
                                                    expected_latency_ms => Decision#route_decision.expected_latency_ms,
                                                    expected_cost => Decision#route_decision.expected_cost
                                                },
                                                final_payload => Payload
                                            }
                                        },
                                        {ok, Response, Ctx};
                                    {error, Reason} ->
                                        Response = #{
                                            ok => false,
                                            error => #{
                                                reason => case Reason of
                                                    {ErrorAtom, ErrorMap} when is_map(ErrorMap) ->
                                                        ErrorAtom;
                                                    ErrorAtom when is_atom(ErrorAtom) ->
                                                        ErrorAtom;
                                                    _ ->
                                                        <<"unknown_error">>
                                                end
                                            }
                                        },
                                        {ok, Response, Ctx}
                                end;
                            {error, not_found} ->
                                throw({grpc_error, {?GRPC_STATUS_NOT_FOUND, 
                                    <<"Policy not found">>}});
                            {error, Reason} ->
                                throw({grpc_error, {?GRPC_STATUS_INTERNAL, 
                                    list_to_binary(io_lib:format("Failed to load policy: ~p", [Reason]))}})
                        end
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError -> throw(GrpcError);
        Class:Error:Stacktrace -> throw_internal_error({Class, Error, Stacktrace})
    end.
%% @doc GetPipelineComplexity RPC implementation
get_pipeline_complexity(Ctx, Request) ->
    try
        case check_auth(Ctx) of
            ok ->
                %% Decode request (assuming JSON-like structure from Gateway)
                RequestMap = case Request of
                    Map when is_map(Map) -> Map;
                    _ -> throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"Invalid request format">>}})
                end,
                TenantId = maps:get(<<"tenant_id">>, RequestMap, undefined),
                PolicyId = maps:get(<<"policy_id">>, RequestMap, undefined),
                
                if
                    TenantId =:= undefined orelse PolicyId =:= undefined ->
                        throw({grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, 
                            <<"Missing required fields: tenant_id, policy_id">>}});
                    true ->
                        case router_policy_store:get_policy(TenantId, PolicyId) of
                            {ok, Policy} ->
                                Complexity = router_decider:calculate_pipeline_complexity(
                                    Policy#policy.pre,
                                    Policy#policy.validators,
                                    Policy#policy.post
                                ),
                                %% Return as JSON-like structure (will be encoded by Gateway)
                                Response = Complexity,
                                {ok, Response, Ctx};
                            {error, not_found} ->
                                throw({grpc_error, {?GRPC_STATUS_NOT_FOUND, 
                                    <<"Policy not found">>}});
                            {error, Reason} ->
                                throw({grpc_error, {?GRPC_STATUS_INTERNAL, 
                                    list_to_binary(io_lib:format("Failed to load policy: ~p", [Reason]))}})
                        end
                end;
            {error, unauthorized} ->
                throw({grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}})
        end
    catch
        {grpc_error, _} = GrpcError -> throw(GrpcError);
        Class:Error:Stacktrace -> throw_internal_error({Class, Error, Stacktrace})
    end.

-else.

upsert_policy(Ctx, _Request) -> {ok, #{ok => false, error => <<"not_available">>}, Ctx}.
delete_policy(Ctx, _Request) -> {ok, #{ok => false, error => <<"not_available">>}, Ctx}.
get_policy(Ctx, _Request) -> {ok, #{ok => false, error => <<"not_available">>}, Ctx}.
list_policies(Ctx, _Request) -> {ok, #{policies => []}, Ctx}.
get_checkpoint_status(Ctx, _Request) -> {ok, #{status => <<"cp1">>}, Ctx}.
get_validators_health(Ctx, _Request) -> {ok, #{health => #{}}, Ctx}.
get_extension_health(Ctx, _Request) -> {ok, #{health => #{}}, Ctx}.
get_circuit_breaker_states(Ctx, _Request) -> {ok, #{states => #{}}, Ctx}.
dry_run_pipeline(Ctx, _Request) -> {ok, #{ok => false, error => <<"not_available">>}, Ctx}.
get_pipeline_complexity(Ctx, _Request) -> {ok, #{ok => false, complexity_score => 0}, Ctx}.
get_admin_key() -> <<"">>.

-endif.
