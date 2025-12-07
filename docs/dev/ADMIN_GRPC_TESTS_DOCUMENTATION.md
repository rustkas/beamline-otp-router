# Admin gRPC Tests Documentation

**Date**: 2025-01-27  
**Status**: ✅ **Tests Implemented**  
**Component**: Router (`apps/otp/router/`)

## Overview

Admin gRPC tests verify the RouterAdmin gRPC service implementation, including policy management operations, authentication, error handling, and concurrency behavior.

## Test Suites

### 1. router_admin_grpc_integration_SUITE.erl

**Purpose**: Integration tests for RouterAdmin gRPC service

**Test Coverage** (8 tests):

1. **test_upsert_policy_success**:
   - Creates a policy via `UpsertPolicy` RPC
   - Verifies policy is created in policy store
   - Verifies response indicates success

2. **test_upsert_policy_unauthorized**:
   - Attempts to create a policy without authentication
   - Verifies `UNAUTHENTICATED` (16) error is returned
   - Verifies policy is not created

3. **test_get_policy_success**:
   - Creates a policy, then retrieves it via `GetPolicy` RPC
   - Verifies policy data matches created policy
   - Verifies response contains correct policy information

4. **test_list_policies_success**:
   - Creates multiple policies (3 policies)
   - Lists all policies via `ListPolicies` RPC
   - Verifies all policies are returned

5. **test_delete_policy_success**:
   - Creates a policy, then deletes it via `DeletePolicy` RPC
   - Verifies policy is deleted from policy store
   - Verifies response indicates success

6. **test_delete_policy_not_found**:
   - Attempts to delete a non-existent policy
   - Verifies `NOT_FOUND` (5) error is returned

7. **test_get_policy_not_found**:
   - Attempts to retrieve a non-existent policy
   - Verifies `NOT_FOUND` (5) error is returned

8. **test_get_checkpoint_status**:
   - Queries checkpoint status via `GetCheckpointStatus` RPC
   - Verifies response contains current CP and CP2+ status

**Test Pattern**:
- All tests use `router_grpc_test_helper:create_context_with_auth/1` for authentication
- All tests use `flow_pb` for protobuf encoding/decoding
- All tests verify both gRPC responses and policy store state
- Proper cleanup in `init_per_testcase/2` and `end_per_testcase/2`

**Running Tests**:
```bash
cd apps/otp/router
rebar3 as test ct --suite test/router_admin_grpc_integration_SUITE
```

### 2. router_admin_grpc_concurrency_SUITE.erl

**Purpose**: Concurrency tests for RouterAdmin gRPC service

**Test Coverage** (4 tests):

1. **test_concurrent_upsert_different_tenants**:
   - Spawns 5 concurrent processes, each creating 3 policies for different tenants
   - Verifies all policies are created correctly
   - Verifies no race conditions between different tenants

2. **test_concurrent_upsert_same_tenant**:
   - Spawns 10 concurrent processes, all upserting the same policy
   - Verifies all concurrent upserts succeed (idempotent operation)
   - Verifies final policy state is consistent

3. **test_concurrent_get_list**:
   - Creates 5 policies, then spawns concurrent get and list operations
   - Verifies all get operations succeed
   - Verifies all list operations succeed
   - Verifies no race conditions in read operations

4. **test_concurrent_delete**:
   - Creates 5 policies, then spawns concurrent delete operations
   - Verifies all delete operations succeed
   - Verifies all policies are deleted
   - Verifies no race conditions in delete operations

**Test Pattern**:
- All tests use `spawn` for true concurrency
- All tests verify final state consistency after concurrent operations
- All tests use proper cleanup in `init_per_testcase/2` and `end_per_testcase/2`
- Tests verify no data corruption or race conditions

**Running Tests**:
```bash
cd apps/otp/router
rebar3 as test ct --suite test/router_admin_grpc_concurrency_SUITE
```

## Test Helpers

### router_grpc_test_helper.erl

Provides helper functions for gRPC tests:

- `create_context_with_auth/1`: Creates gRPC context with authentication (x-api-key header)
- `create_context_without_auth/0`: Creates gRPC context without authentication
- `start_grpc_server/0` / `start_grpc_server/1`: Starts gRPC server with ephemeral port
- `stop_grpc_server/0`: Stops gRPC server
- `get_grpc_port/1`: Gets actual gRPC port (for ephemeral ports)

**Usage**:
```erlang
%% Create authenticated context
AdminKey = <<"test-admin-key">>,
Ctx = router_grpc_test_helper:create_context_with_auth(AdminKey),

%% Create unauthenticated context
Ctx = router_grpc_test_helper:create_context_without_auth(),
```

## Authentication

All RouterAdmin RPC methods require authentication via API key in gRPC metadata:

**Header**: `x-api-key` (lowercase)

**Example**:
```erlang
Ctx = #{
    metadata => [
        {<<"x-api-key">>, <<"test-admin-key">>}
    ]
},
{ok, Response, _} = router_admin_grpc:upsert_policy(Ctx, Request).
```

**Error Handling**:
- Missing API key → `UNAUTHENTICATED` (16)
- Invalid API key → `UNAUTHENTICATED` (16)
- Empty API key → `UNAUTHENTICATED` (16)

## Error Codes

**gRPC Status Codes Used**:
- `UNAUTHENTICATED` (16): Missing or invalid API key
- `PERMISSION_DENIED` (7): Insufficient permissions (RBAC)
- `NOT_FOUND` (5): Policy not found
- `INVALID_ARGUMENT` (3): Invalid request format
- `RESOURCE_EXHAUSTED` (8): Quota exceeded
- `INTERNAL` (13): Internal server error

**Error Handling Pattern**:
```erlang
try
    router_admin_grpc:upsert_policy(Ctx, Request)
catch
    {grpc_error, {Status, _Msg}} ->
        ?assertEqual(16, Status),  %% UNAUTHENTICATED
        ok
end.
```

## Test Configuration

**Required Environment Variables**:
- `admin_grpc_enabled`: Must be `true` (default: `true` in CP2+)
- `cp2_plus_allowed`: Must be `true` (default: `false`, set to `true` for CP2+ tests)
- `admin_api_key`: Admin API key for authentication (default: `<<"test-admin-key">>` in tests)

**Test Setup**:
```erlang
init_per_suite(Config) ->
    ok = application:set_env(beamline_router, admin_grpc_enabled, true),
    ok = application:set_env(beamline_router, cp2_plus_allowed, true),
    ok = application:set_env(beamline_router, admin_api_key, <<"test-admin-key">>),
    {ok, _} = application:ensure_all_started(beamline_router),
    Config.
```

## Best Practices

1. **Always use test helpers**: Use `router_grpc_test_helper` for context creation and server setup
2. **Verify both responses and state**: Check both gRPC responses and policy store state
3. **Clean up properly**: Reset RBAC and policy store in `init_per_testcase/2`
4. **Use proper error handling**: Catch `{grpc_error, {Status, _Msg}}` for error verification
5. **Test authentication**: Always test both authenticated and unauthenticated scenarios
6. **Test concurrency**: Use `spawn` for true concurrency testing
7. **Verify final state**: After concurrent operations, verify final state consistency

## References

- `apps/otp/router/src/router_admin_grpc.erl` - RouterAdmin implementation
- `apps/otp/router/docs/GRPC_API.md` - gRPC API documentation
- `apps/otp/router/docs/GRPC_ERROR_CODES.md` - Error codes reference
- `apps/otp/router/test/router_grpc_test_helper.erl` - Test helpers

