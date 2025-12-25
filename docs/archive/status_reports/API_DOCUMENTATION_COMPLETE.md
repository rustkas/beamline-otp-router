# Complete gRPC API Documentation

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: Complete

## Overview

This document provides complete documentation for all gRPC endpoints in Router component, including request/response examples and error codes.

## Service: Router.Decide

### Endpoint: Decide

**RPC**: `Router.Decide`  
**Method**: Unary  
**Authorization**: Not required (public API)

#### Request: RouteRequest

**Protobuf Message**: `RouteRequest`

**Required Fields**:
- `message` - Message to route (required)
  - `tenant_id` - Tenant identifier (required)
  - `message_type` - Message type (required)
  - `payload` - Message payload (required)

**Optional Fields**:
- `policy_id` - Policy identifier (optional, uses default if not provided)
- `context` - Additional context (optional)
- `message.message_id` - Message identifier (optional)
- `message.trace_id` - Trace identifier (optional)
- `message.metadata` - Message metadata (optional)
- `message.timestamp_ms` - Timestamp in milliseconds (optional)

**Example Request**:
```erlang
RouteRequestPb = #'RouteRequest'{
    message = #'Message'{
        message_id = ~"msg_001",
        tenant_id = ~"tenant_123",
        message_type = ~"chat",
        payload = ~"Hello, world",
        metadata = [],
        timestamp_ms = erlang:system_time(millisecond)
    },
    policy_id = ~"default_policy",
    context = []
},
Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest').
```

#### Response: RouteDecision

**Protobuf Message**: `RouteDecision`

**Fields**:
- `provider_id` - Selected provider identifier (required)
- `reason` - Decision reason (required)
- `metadata` - Additional metadata (optional)

**Example Response**:
```erlang
RouteDecisionPb = #'RouteDecision'{
    provider_id = ~"openai",
    reason = ~"weighted_selection",
    metadata = []
},
Response = flow_pb:encode_msg(RouteDecisionPb, 'RouteDecision').
```

#### Error Codes

| Error Code | gRPC Status | Description | When Returned |
|------------|-------------|-------------|---------------|
| `invalid_request` | INVALID_ARGUMENT (3) | Invalid request format | Missing required fields, invalid format |
| `missing_tenant_id` | INVALID_ARGUMENT (3) | Missing tenant_id | tenant_id not provided in message |
| `missing_message` | INVALID_ARGUMENT (3) | Missing message | message field not provided |
| `policy_not_found` | NOT_FOUND (5) | Policy not found | Policy with specified policy_id does not exist |
| `no_provider_available` | INTERNAL (13) | No provider available | No providers configured or all providers unavailable |
| `rate_limit_exceeded` | RESOURCE_EXHAUSTED (8) | Rate limit exceeded | Request rate exceeds configured limits |

**Example Error Response**:
```erlang
{grpc_error, {?GRPC_STATUS_NOT_FOUND, ~"policy not found"}}
```

#### Usage Example

```erlang
%% Create context
Ctx = router_grpc_test_helper:create_context_without_auth(),

%% Create and encode request
RouteRequestPb = #'RouteRequest'{
    message = #'Message'{
        tenant_id = ~"tenant_123",
        message_type = ~"chat",
        payload = ~"Hello"
    },
    policy_id = ~"default_policy"
},
Request = flow_pb:encode_msg(RouteRequestPb, 'RouteRequest'),

%% Call decide
case router_grpc:decide(Ctx, Request) of
    {ok, Response, _} ->
        RouteDecisionPb = flow_pb:decode_msg(Response, 'RouteDecision'),
        io:format("Provider: ~p, Reason: ~p~n", [
            RouteDecisionPb#'RouteDecision'.provider_id,
            RouteDecisionPb#'RouteDecision'.reason
        ]);
    {grpc_error, {Status, Msg}} ->
        io:format("Error: ~p - ~p~n", [Status, Msg])
end.
```

## Service: RouterAdmin

### Endpoint: UpsertPolicy

**RPC**: `RouterAdmin.UpsertPolicy`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

#### Request: UpsertPolicyRequest

**Protobuf Message**: `UpsertPolicyRequest`

**Required Fields**:
- `policy` - Policy to create/update (required)
  - `policy_id` - Policy identifier (required)
  - `tenant_id` - Tenant identifier (required)

**Optional Fields**:
- `policy.version` - Policy version (optional)
- `policy.weights` - Provider weights map (optional)
- `policy.defaults` - Default provider settings (optional)
- `policy.escalate_on` - Escalation conditions (optional)
- `policy.fallback` - Fallback provider (optional)
- `policy.sticky` - Sticky session configuration (optional)
- `policy.metadata` - Additional metadata (optional)

**Example Request**:
```erlang
UpsertPolicyRequestPb = #'UpsertPolicyRequest'{
    policy = #'AdminPolicy'{
        policy_id = ~"test_policy",
        tenant_id = ~"tenant_123",
        version = ~"1.0",
        providers = [
            #'AdminProvider'{
                id = ~"openai",
                weight = 0.7
            },
            #'AdminProvider'{
                id = ~"anthropic",
                weight = 0.3
            }
        ],
        rules = []
    }
},
Request = flow_pb:encode_msg(UpsertPolicyRequestPb, 'UpsertPolicyRequest').
```

#### Response: UpsertPolicyResponse

**Protobuf Message**: `UpsertPolicyResponse`

**Fields**:
- `policy` - Created/updated policy (required)
- `created` - Whether policy was created (true) or updated (false)

**Example Response**:
```erlang
UpsertPolicyResponsePb = #'UpsertPolicyResponse'{
    policy = #'AdminPolicy'{
        policy_id = ~"test_policy",
        tenant_id = ~"tenant_123",
        version = ~"1.0"
    },
    created = true
},
Response = flow_pb:encode_msg(UpsertPolicyResponsePb, 'UpsertPolicyResponse').
```

#### Error Codes

| Error Code | gRPC Status | Description | When Returned |
|------------|-------------|-------------|---------------|
| `unauthenticated` | UNAUTHENTICATED (16) | Missing or invalid API key | API key missing, invalid, or empty |
| `invalid_policy` | INVALID_ARGUMENT (3) | Invalid policy structure | Invalid weights, duplicate providers, etc. |
| `permission_denied` | PERMISSION_DENIED (7) | Insufficient permissions | User lacks required permissions |

#### Usage Example

```erlang
%% Create context with API key
Ctx = router_grpc_test_helper:create_context_with_auth(~"admin_api_key"),

%% Create and encode request
UpsertPolicyRequestPb = #'UpsertPolicyRequest'{
    policy = #'AdminPolicy'{
        policy_id = ~"test_policy",
        tenant_id = ~"tenant_123",
        providers = [
            #'AdminProvider'{id = ~"openai", weight = 0.7}
        ]
    }
},
Request = flow_pb:encode_msg(UpsertPolicyRequestPb, 'UpsertPolicyRequest'),

%% Call upsert_policy
case router_admin_grpc:upsert_policy(Ctx, Request) of
    {ok, Response, _} ->
        UpsertPolicyResponsePb = flow_pb:decode_msg(Response, 'UpsertPolicyResponse'),
        io:format("Policy created: ~p~n", [UpsertPolicyResponsePb#'UpsertPolicyResponse'.created]);
    {grpc_error, {Status, Msg}} ->
        io:format("Error: ~p - ~p~n", [Status, Msg])
end.
```

### Endpoint: DeletePolicy

**RPC**: `RouterAdmin.DeletePolicy`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

#### Request: DeletePolicyRequest

**Protobuf Message**: `DeletePolicyRequest`

**Required Fields**:
- `policy_id` - Policy identifier to delete (required)
- `tenant_id` - Tenant identifier (required)

**Example Request**:
```erlang
DeletePolicyRequestPb = #'DeletePolicyRequest'{
    policy_id = ~"test_policy",
    tenant_id = ~"tenant_123"
},
Request = flow_pb:encode_msg(DeletePolicyRequestPb, 'DeletePolicyRequest').
```

#### Response: DeletePolicyResponse

**Protobuf Message**: `DeletePolicyResponse`

**Fields**:
- `deleted` - Whether policy was deleted (true) or not found (false)

**Example Response**:
```erlang
DeletePolicyResponsePb = #'DeletePolicyResponse'{
    deleted = true
},
Response = flow_pb:encode_msg(DeletePolicyResponsePb, 'DeletePolicyResponse').
```

#### Error Codes

| Error Code | gRPC Status | Description | When Returned |
|------------|-------------|-------------|---------------|
| `unauthenticated` | UNAUTHENTICATED (16) | Missing or invalid API key | API key missing, invalid, or empty |
| `permission_denied` | PERMISSION_DENIED (7) | Insufficient permissions | User lacks required permissions |

### Endpoint: GetPolicy

**RPC**: `RouterAdmin.GetPolicy`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

#### Request: GetPolicyRequest

**Protobuf Message**: `GetPolicyRequest`

**Required Fields**:
- `policy_id` - Policy identifier to retrieve (required)
- `tenant_id` - Tenant identifier (required)

**Example Request**:
```erlang
GetPolicyRequestPb = #'GetPolicyRequest'{
    policy_id = ~"test_policy",
    tenant_id = ~"tenant_123"
},
Request = flow_pb:encode_msg(GetPolicyRequestPb, 'GetPolicyRequest').
```

#### Response: GetPolicyResponse

**Protobuf Message**: `GetPolicyResponse`

**Fields**:
- `policy` - Retrieved policy (required)

**Example Response**:
```erlang
GetPolicyResponsePb = #'GetPolicyResponse'{
    policy = #'AdminPolicy'{
        policy_id = ~"test_policy",
        tenant_id = ~"tenant_123",
        version = ~"1.0"
    }
},
Response = flow_pb:encode_msg(GetPolicyResponsePb, 'GetPolicyResponse').
```

#### Error Codes

| Error Code | gRPC Status | Description | When Returned |
|------------|-------------|-------------|---------------|
| `unauthenticated` | UNAUTHENTICATED (16) | Missing or invalid API key | API key missing, invalid, or empty |
| `policy_not_found` | NOT_FOUND (5) | Policy not found | Policy with specified policy_id does not exist |
| `permission_denied` | PERMISSION_DENIED (7) | Insufficient permissions | User lacks required permissions |

### Endpoint: ListPolicies

**RPC**: `RouterAdmin.ListPolicies`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

#### Request: ListPoliciesRequest

**Protobuf Message**: `ListPoliciesRequest`

**Required Fields**:
- `tenant_id` - Tenant identifier (required)

**Optional Fields**:
- `limit` - Maximum number of policies to return (optional)
- `offset` - Offset for pagination (optional)

**Example Request**:
```erlang
ListPoliciesRequestPb = #'ListPoliciesRequest'{
    tenant_id = ~"tenant_123",
    limit = 100,
    offset = 0
},
Request = flow_pb:encode_msg(ListPoliciesRequestPb, 'ListPoliciesRequest').
```

#### Response: ListPoliciesResponse

**Protobuf Message**: `ListPoliciesResponse`

**Fields**:
- `policies` - List of policies (required)
- `total` - Total number of policies (optional)

**Example Response**:
```erlang
ListPoliciesResponsePb = #'ListPoliciesResponse'{
    policies = [
        #'AdminPolicy'{
            policy_id = ~"policy_1",
            tenant_id = ~"tenant_123"
        },
        #'AdminPolicy'{
            policy_id = ~"policy_2",
            tenant_id = ~"tenant_123"
        }
    ],
    total = 2
},
Response = flow_pb:encode_msg(ListPoliciesResponsePb, 'ListPoliciesResponse').
```

#### Error Codes

| Error Code | gRPC Status | Description | When Returned |
|------------|-------------|-------------|---------------|
| `unauthenticated` | UNAUTHENTICATED (16) | Missing or invalid API key | API key missing, invalid, or empty |
| `permission_denied` | PERMISSION_DENIED (7) | Insufficient permissions | User lacks required permissions |

## Additional Admin Endpoints (CP2)

### Endpoint: GetCheckpointStatus

**RPC**: `RouterAdmin.GetCheckpointStatus`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

Returns checkpoint status for the system.

### Endpoint: GetValidatorsHealth

**RPC**: `RouterAdmin.GetValidatorsHealth`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

Returns health status of all validators.

### Endpoint: GetExtensionHealth

**RPC**: `RouterAdmin.GetExtensionHealth`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

Returns health status of extensions.

### Endpoint: GetCircuitBreakerStates

**RPC**: `RouterAdmin.GetCircuitBreakerStates`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

Returns circuit breaker states for all tenant/provider pairs.

### Endpoint: DryRunPipeline

**RPC**: `RouterAdmin.DryRunPipeline`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

Runs a dry-run of the extension pipeline without executing.

### Endpoint: GetPipelineComplexity

**RPC**: `RouterAdmin.GetPipelineComplexity`  
**Method**: Unary  
**Authorization**: Required (API key in metadata)

Returns complexity metrics for the extension pipeline.

## Error Code Reference

See `docs/GRPC_ERROR_CODES.md` for complete error code reference.

See `docs/ERROR_REASONS_REFERENCE.md` for complete error reasons reference.

## Request/Response Examples

### Complete Examples

See `docs/GRPC_API.md` for detailed request/response examples with correlation_id, telemetry, and error handling.

## References

- `proto/beamline/flow/v1/flow.proto` - Protobuf definitions
- `src/router_grpc.erl` - Router.Decide implementation
- `src/router_admin_grpc.erl` - RouterAdmin implementation
- `docs/GRPC_API.md` - Detailed API documentation
- `docs/GRPC_ERROR_CODES.md` - Error codes reference
- `docs/ERROR_REASONS_REFERENCE.md` - Error reasons reference

