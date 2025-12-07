# gRPC Error Codes Reference

## Overview

This documentation describes the mapping between gRPC Status Codes, HTTP status codes, and internal error codes for Router and RouterAdmin services.

## Mapping Table

| gRPC Code | gRPC Status | HTTP Equivalent | Description | When Used |
|-----------|-------------|-----------------|-------------|-----------|
| 3 | INVALID_ARGUMENT | 400 Bad Request | Invalid request | - Missing required field<br>- Invalid data format<br>- Validation failed |
| 5 | NOT_FOUND | 404 Not Found | Resource not found | - Policy does not exist<br>- Tenant not found |
| 13 | INTERNAL | 500 Internal Server Error | Internal error | - Exception in processing<br>- Database error<br>- Unexpected error |
| 16 | UNAUTHENTICATED | 401 Unauthorized | Not authenticated | - Missing API key<br>- Invalid API key<br>- Invalid key format |

## Router.Decide Service

### INVALID_ARGUMENT (3)

**When returned:**
- Missing `message` in `RouteRequest`
- Missing `tenant_id` in `Message`
- Invalid Protobuf format

**Example:**
```erlang
{grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"missing tenant_id">>}}
```

### NOT_FOUND (5)

**When returned:**
- Policy with specified `policy_id` does not exist
- Tenant not found

**Example:**
```erlang
{grpc_error, {?GRPC_STATUS_NOT_FOUND, <<"policy not found">>}}
```

### INTERNAL (13)

**When returned:**
- No available providers
- Routing error
- Exception in processing

**Example:**
```erlang
{grpc_error, {?GRPC_STATUS_INTERNAL, <<"Internal server error">>}}
```

## RouterAdmin Service

### UNAUTHENTICATED (16)

**When returned:**
- Missing `x-api-key` or `authorization` header
- Invalid API key (does not match `admin_api_key`)
- Empty API key (`<<>>`)

**Important:** All authorization errors return `UNAUTHENTICATED`, not `INVALID_ARGUMENT`.

**Examples:**

```erlang
%% Missing key
Ctx = #{metadata => []},
{grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}}

%% Invalid key
Ctx = #{metadata => [{<<"x-api-key">>, <<"wrong-key">>}]},
{grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}}

%% Empty key
Ctx = #{metadata => [{<<"x-api-key">>, <<>>}]},
{grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, <<"missing or invalid API key">>}}
```

### INVALID_ARGUMENT (3)

**When returned:**
- Invalid policy (invalid weights, empty `policy_id`)
- Duplicate `provider.id`
- Weights outside range [0.0, 1.0]

**Important:** These are data validation errors, not authorization errors.

**Examples:**

```erlang
%% Invalid weights
AdminPolicyPb = #'AdminPolicy'{
    policy_id = <<"test">>,
    providers = [
        #'AdminProvider'{id = <<"openai">>, weight = 1.5}  %% > 1.0
    ]
},
{grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"weights must be in [0.0, 1.0]">>}}

%% Empty policy_id
AdminPolicyPb = #'AdminPolicy'{
    policy_id = <<>>,  %% Empty
    providers = [...]
},
{grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"policy_id cannot be empty">>}}
```

### NOT_FOUND (5)

**When returned:**
- Policy not found in `GetPolicy` or `DeletePolicy`

**Example:**
```erlang
{grpc_error, {?GRPC_STATUS_NOT_FOUND, <<"policy not found">>}}
```

### INTERNAL (13)

**When returned:**
- Internal error when saving/deleting policy
- ETS/Mnesia error
- Unexpected exception
- Operation timeout
- Connection errors

**Important:** Error messages do not contain sensitive data (API keys, secrets).

**Structured details (grpc-status-details-bin):**

For `INTERNAL` errors, the server provides structured details via the `grpc-status-details-bin` header, allowing clients to distinguish between types of internal errors without exposing secrets.

**Details format:**
```erlang
#{error_type => <<"timeout">>, error_code => <<"TIMEOUT">>}
```

**Supported error types:**

| Error Code | Error Type | Description |
|------------|------------|-------------|
| `TIMEOUT` | `timeout` | Operation exceeded timeout |
| `CONNECTION_REFUSED` | `connection_refused` | Connection to external service refused |
| `NOT_FOUND` | `not_found` | Resource not found (internal error) |
| `ACCESS_DENIED` | `access_denied` | Access denied (internal error) |
| `VALIDATION_FAILED` | `validation_failed` | Validation failed (internal error) |
| `POLICY_STORE_ERROR` | `policy_store_error` | Policy store operation error |
| `DECODE_ERROR` | `decode_error` | Message decoding error |
| `ETS_ERROR` | `ets_error` | ETS table operation error |
| `GEN_SERVER_TIMEOUT` | `gen_server_timeout` | gen_server call timeout |
| `INTERNAL_ERROR` | `internal_error` | Generic internal error |

**Example:**
```erlang
{grpc_error, {?GRPC_STATUS_INTERNAL, <<"Internal server error">>, 
              #{error_type => <<"timeout">>, error_code => <<"TIMEOUT">>}}}
```

## Error Handling in Client

### Erlang/OTP

```erlang
try
    {ok, Response, Ctx} = router_admin_grpc:upsert_policy(Ctx, Request),
    %% Success
    ok
catch
    {grpc_error, {?GRPC_STATUS_UNAUTHENTICATED, Message}} ->
        %% Auth failure - check API key
        {error, {unauthorized, Message}};
    {grpc_error, {?GRPC_STATUS_INVALID_ARGUMENT, Message}} ->
        %% Validation failure - check policy data
        {error, {invalid_policy, Message}};
    {grpc_error, {?GRPC_STATUS_NOT_FOUND, Message}} ->
        %% Resource not found
        {error, {not_found, Message}};
    {grpc_error, {?GRPC_STATUS_INTERNAL, Message, Details}} ->
        %% Server error with structured details - extract error type/code
        ErrorType = maps:get(error_type, Details, <<"internal_error">>),
        ErrorCode = maps:get(error_code, Details, <<"INTERNAL_ERROR">>),
        
        %% Handle specific error types
        case ErrorCode of
            <<"TIMEOUT">> ->
                %% Retry with backoff
                {error, {timeout, Message}};
            <<"CONNECTION_REFUSED">> ->
                %% Check service availability
                {error, {connection_refused, Message}};
            _ ->
                %% Generic internal error
                {error, {internal, Message}}
        end;
    {grpc_error, {?GRPC_STATUS_INTERNAL, Message}} ->
        %% Server error without details (backward compatibility)
        {error, {internal, Message}}
end.
```

### gRPC Client (grpcurl)

```bash
# Success
grpcurl -plaintext -H "x-api-key: dev-admin-key" \
  localhost:9000 beamline.flow.v1.RouterAdmin/UpsertPolicy \
  -d '{"tenant_id": "test", "policy": {...}}'

# UNAUTHENTICATED
grpcurl -plaintext localhost:9000 \
  beamline.flow.v1.RouterAdmin/UpsertPolicy \
  -d '{"tenant_id": "test", "policy": {...}}'
# Error: rpc error: code = Unauthenticated desc = missing or invalid API key

# INVALID_ARGUMENT
grpcurl -plaintext -H "x-api-key: dev-admin-key" \
  localhost:9000 beamline.flow.v1.RouterAdmin/UpsertPolicy \
  -d '{"tenant_id": "test", "policy": {"policy_id": "", "providers": [...]}}'
# Error: rpc error: code = InvalidArgument desc = policy_id cannot be empty
```

## Differences: gRPC vs HTTP

### gRPC Status Codes
- Used only in gRPC protocol
- Codes: numeric (3, 5, 13, 16)
- Statuses: string (`INVALID_ARGUMENT`, `NOT_FOUND`, `INTERNAL`, `UNAUTHENTICATED`)

### HTTP Status Codes
- Used in REST API (Gateway/NestJS)
- Codes: numeric (400, 404, 500, 401)
- Statuses: string (`Bad Request`, `Not Found`, `Internal Server Error`, `Unauthorized`)

### Mapping

| gRPC | HTTP | Note |
|------|------|------|
| INVALID_ARGUMENT (3) | 400 Bad Request | Validation error |
| NOT_FOUND (5) | 404 Not Found | Resource not found |
| INTERNAL (13) | 500 Internal Server Error | Server error |
| UNAUTHENTICATED (16) | 401 Unauthorized | Authorization error |

**Important:** Gateway may convert gRPC codes to HTTP status codes when proxying requests.

## Security

### Log Sanitization

All sensitive data (API keys, secrets) is sanitized in logs via `router_logger:sanitize_context/1`:

```erlang
%% In logs
Context = #{<<"api_key">> => <<"secret-key">>},
Sanitized = router_logger:sanitize_context(Context),
%% Result: #{<<"api_key">> => <<"[REDACTED]">>}
```

### Error Messages

- **UNAUTHENTICATED**: Message does not contain the actual key
- **INVALID_ARGUMENT**: Message contains diagnostic information (without secrets)
- **INTERNAL**: Generic message, details only in logs (with sanitization)

## Future Status Codes (for planning)

### PERMISSION_DENIED (7)

**When it will be used:**
- Valid API key, but insufficient permissions for operation
- Role/scope does not allow the action
- Example: key with `read_only` permissions attempts to execute `UpsertPolicy`

**Difference from UNAUTHENTICATED:**
- `UNAUTHENTICATED` (16): client cannot be identified (missing/invalid key)
- `PERMISSION_DENIED` (7): client is identified, but lacks permissions for the operation

**Example (future implementation):**
```erlang
%% Valid key but insufficient scope
Ctx = #{metadata => [
    {<<"x-api-key">>, <<"valid-key">>},
    {<<"x-scope">>, <<"read_only">>}
]},
{grpc_error, {?GRPC_STATUS_PERMISSION_DENIED, <<"insufficient permissions: write access required">>}}
```

### RESOURCE_EXHAUSTED (8)

**When it will be used:**
- Rate limit exceeded
- Quotas exhausted
- Too many concurrent requests

**Example (future implementation):**
```erlang
%% Rate limit exceeded
{grpc_error, {?GRPC_STATUS_RESOURCE_EXHAUSTED, <<"rate limit exceeded: 100 requests/minute">>}}
```

**Retry strategy:**
- Clients should use exponential backoff
- Check `Retry-After` header (if added)

## See Also

- `docs/GRPC_API.md` - Complete API documentation
- `apps/otp/router/src/router_admin_grpc.erl` - Error handling implementation
- `apps/otp/router/src/router_logger.erl` - Log sanitization
