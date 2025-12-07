# Router gRPC API Documentation

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Service**: Router (beamline.router.v1)

## Overview

The Router gRPC API provides two main services:
- **Router**: Core routing decision service
- **RouterAdmin**: Administrative operations for policy management

## Service: Router

### Endpoint: Decide

**RPC**: `Router.Decide`  
**Method**: Unary  
**Request**: `RouteRequest`  
**Response**: `RouteDecision`

#### Description

Processes a routing request and returns a routing decision with provider selection.

#### Request Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tenant_id` | string | Yes | Tenant identifier |
| `message` | map | Yes | Message payload (JSON object) |
| `context` | map | Optional | Additional context (correlation fields, metadata) |

#### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `provider_id` | string | Selected provider identifier |
| `metadata` | map | Additional metadata (executed extensions, etc.) |

#### Request Example

```json
{
  "tenant_id": "tenant-123",
  "message": {
    "user_id": "user-456",
    "request_id": "req-789"
  },
  "context": {
    "correlation_id": "corr-abc",
    "run_id": "run-xyz"
  }
}
```

#### Response Example

```json
{
  "provider_id": "provider-1",
  "metadata": {
    "executed_extensions": ["ext-1", "ext-2"],
    "decision_reason": "weighted_selection"
  }
}
```

#### Error Codes

See [Error Code Reference](#error-code-reference) for complete list.

## Service: RouterAdmin

### Endpoint: UpsertPolicy

**RPC**: `RouterAdmin.UpsertPolicy`  
**Method**: Unary  
**Request**: `UpsertPolicyRequest`  
**Response**: `UpsertPolicyResponse`

#### Description

Creates or updates a routing policy for a tenant.

#### Request Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tenant_id` | string | Yes | Tenant identifier |
| `policy` | AdminPolicy | Yes | Policy definition |

#### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `ok` | bool | Success indicator |

#### Error Codes

See [Error Code Reference](#error-code-reference) for complete list.

### Endpoint: DeletePolicy

**RPC**: `RouterAdmin.DeletePolicy`  
**Method**: Unary  
**Request**: `DeletePolicyRequest`  
**Response**: `DeletePolicyResponse`

#### Description

Deletes a routing policy for a tenant.

#### Request Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tenant_id` | string | Yes | Tenant identifier |
| `policy_id` | string | Yes | Policy identifier to delete |

#### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `ok` | bool | Success indicator |

### Endpoint: GetPolicy

**RPC**: `RouterAdmin.GetPolicy`  
**Method**: Unary  
**Request**: `GetPolicyRequest`  
**Response**: `GetPolicyResponse`

#### Description

Retrieves a routing policy for a tenant.

#### Request Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tenant_id` | string | Yes | Tenant identifier |
| `policy_id` | string | Yes | Policy identifier |

#### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `policy` | AdminPolicy | Policy definition |

### Endpoint: ListPolicies

**RPC**: `RouterAdmin.ListPolicies`  
**Method**: Unary  
**Request**: `ListPoliciesRequest`  
**Response**: `ListPoliciesResponse`

#### Description

Lists all routing policies for a tenant.

#### Request Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tenant_id` | string | Yes | Tenant identifier |

#### Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `policies` | repeated AdminPolicy | List of policies |

## Error Code Reference

| gRPC Status | Code | Error Reason | Description |
|-------------|------|--------------|-------------|
| INVALID_ARGUMENT | 3 | `invalid_policy` | Policy structure is invalid |
| INVALID_ARGUMENT | 3 | `invalid_request` | Request structure is invalid |
| INVALID_ARGUMENT | 3 | `missing_message` | Required message field is missing |
| INVALID_ARGUMENT | 3 | `missing_tenant_id` | Tenant ID is required but not provided |
| NOT_FOUND | 5 | `policy_not_found` | Policy not found |
| PERMISSION_DENIED | 7 | `permission_denied` | Insufficient permissions |
| RESOURCE_EXHAUSTED | 8 | `rate_limit_exceeded` | Rate limit exceeded |
| RESOURCE_EXHAUSTED | 8 | `quota_exceeded` | Quota exceeded |
| INTERNAL | 13 | `no_provider_available` | No provider available |
| INTERNAL | 13 | `internal_error` | Internal server error |
| UNAVAILABLE | 14 | `timeout` | Request timeout |
| UNAVAILABLE | 14 | `nats_unavailable` | NATS unavailable |
| UNAUTHENTICATED | 16 | `unauthenticated` | Missing or invalid API key |

## Authentication

Admin endpoints require authentication via API key in metadata:
- Header: `x-api-key` or `api-key`
- Value: Admin API key

## Rate Limiting

The `Decide` endpoint is subject to rate limiting:
- Per-tenant rate limits
- Per-user rate limits (if user_id provided)
- Returns `RESOURCE_EXHAUSTED` (8) when limit exceeded

## See Also

- `src/router_grpc.erl` - Router service implementation
- `src/router_admin_grpc.erl` - Admin service implementation
- `src/router_error.erl` - Error code mapping

