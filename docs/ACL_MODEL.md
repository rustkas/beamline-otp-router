# Router ACL Model

**Date**: 2025-11-30  
**Status**: 📋 **Formal Specification**  
**Purpose**: Define unambiguous ACL model for Router (CP2+)  
**Related**: `router_tenant_validator.erl`, `router_policy_store.erl`, `router_acl.erl`

## Executive Summary

This document formally defines the ACL (Access Control List) model for Router, eliminating ambiguity between `router_acl.erl` and `router_tenant_validator.erl`.

**Key Decision**: `router_tenant_validator.erl` is the **single source of truth** for tenant validation and ACL decisions. `router_acl.erl` is **deprecated** and should not be used in hot paths.

## Source of Truth

### Primary: Policy Registry (`router_policy_store`)

**Module**: `router_policy_store.erl`

**Purpose**: Centralized policy storage and retrieval

**Storage**:
- ETS tables with heir process for persistence
- Policies indexed by `{TenantId, PolicyId}`
- Default policy: `PolicyId = <<"default">>`

**API**:
- `load_policy(TenantId, PolicyId)`: Load policy for tenant
- `get_policy(TenantId, PolicyId)`: Get policy (with correlation ID)
- `upsert_policy(TenantId, Policy, CorrelationId)`: Create/update policy
- `delete_policy(TenantId, PolicyId, CorrelationId)`: Delete policy
- `list_policies(TenantId)`: List all policies for tenant

**Policy Format**:
- Policy is a map with routing rules, quotas, and permissions
- Policy existence indicates tenant is valid
- Policy content determines routing decisions (not ACL allow/deny)

### Secondary: Allowlist (Static Configuration)

**Module**: `router_tenant_validator.erl`

**Purpose**: Static allowlist for tenant validation (optional)

**Storage**:
- Application environment variables:
  - `result_ack_allowed_tenants`: For result/ACK messages
  - `caf_push_assignment_allowed_tenants`: For CAF assignments (fallback)

**Format**:
- List of tenant IDs (binary or string)
- Map of tenant IDs (keys are tenant IDs)

**Behavior**:
- If allowlist configured: tenant must be in allowlist
- If allowlist not configured: allowlist check passes (allow all)

## ACL Decision Flow

### Router Decision: Allow/Deny

**Module**: `router_tenant_validator.erl`

**Function**: `validate_tenant/2`

**Flow**:
1. **Format Validation**: Check if `tenant_id` is present and valid (binary, not empty)
2. **Allowlist Check**: If allowlist configured, check tenant against allowlist
3. **Policy Registry Check**: Check if policy exists for tenant (using `router_policy_store:load_policy/2`)
4. **Decision**:
   - **Allow**: If format valid, allowlist passes (if configured), and policy exists (or policy check not required)
   - **Deny**: If format invalid, allowlist fails, or policy required but not found

**Return Values**:
- `{ok, TenantId}`: Tenant is valid, allow request
- `{error, Reason, Context}`: Tenant is invalid, deny request

**Error Reasons**:
- `tenant_missing`: `tenant_id` is `undefined`
- `tenant_empty`: `tenant_id` is empty binary
- `tenant_not_allowed`: Tenant not in allowlist (if allowlist configured)
- `tenant_invalid_format`: `tenant_id` is not a binary
- `tenant_policy_not_found`: Policy not found (if policy required)

### Current Usage

**Used In**:
- `router_result_consumer.erl`: Validates tenant before processing `ExecResult`
- `router_ack_consumer.erl`: Validates tenant before processing `ExecAssignmentAck`
- `router_intake_validator.erl`: Validates tenant during intake validation

**Not Used**:
- `router_acl.erl`: **Deprecated**, not used in hot paths

## Deprecated: `router_acl.erl`

### Status: ⚠️ **DEPRECATED / TECH DEBT**

**Reason**: Not used in hot paths, redundant with `router_tenant_validator`

**Current State**:
- Module exists but is **not called** from any production code
- Uses ETS table for policy storage (separate from `router_policy_store`)
- Has own policy format and evaluation logic
- Provides `allow/2`, `allow/3`, `deny/2`, `deny/3` functions

**Migration Path**:
- **Option 1**: Remove `router_acl.erl` entirely (recommended)
- **Option 2**: Make `router_acl.erl` a facade over `router_tenant_validator` (if backward compatibility needed)

**Recommendation**: **Remove** `router_acl.erl` as it's not used and adds confusion.

## ACL Model Summary

### Source of Truth Hierarchy

1. **Policy Registry** (`router_policy_store`):
   - Primary source for tenant validation
   - Policy existence = tenant is valid
   - Policy content = routing rules (not ACL allow/deny)

2. **Allowlist** (static configuration):
   - Secondary source for tenant validation
   - Optional: if not configured, check passes
   - Used for static tenant filtering

3. **`router_tenant_validator`**:
   - **Single entry point** for tenant validation
   - Combines allowlist and policy registry checks
   - Returns `{ok, TenantId}` or `{error, Reason, Context}`

### Decision Flow

```
Request with tenant_id
    ↓
router_tenant_validator:validate_tenant/2
    ↓
Format Validation (tenant_id present, binary, not empty)
    ↓
Allowlist Check (if configured)
    ↓
Policy Registry Check (router_policy_store:load_policy/2)
    ↓
Decision: {ok, TenantId} | {error, Reason, Context}
```

### Audit Events

**Module**: `router_tenant_validator.erl`

**Function**: `emit_audit_event/3`

**Event Types**:
- `tenant_missing`: `tenant_id` is `undefined`
- `tenant_empty`: `tenant_id` is empty
- `tenant_not_allowed`: Tenant not in allowlist
- `tenant_policy_not_found`: Policy not found
- `tenant_invalid_format`: `tenant_id` format invalid

**Telemetry**:
- `[router_tenant_validator, audit]`: Audit event
- `router_tenant_audit_total`: Counter for audit events

**Metrics**:
- `router_tenant_audit_total`: Total audit events
- `router_results_tenant_rejected_total`: Rejected results (in `router_result_consumer`)
- `router_acks_tenant_rejected_total`: Rejected ACKs (in `router_ack_consumer`)

## Implementation Guidelines

### For New Code

**DO**:
- ✅ Use `router_tenant_validator:validate_tenant/2` for tenant validation
- ✅ Use `router_policy_store` for policy operations
- ✅ Emit audit events via `router_tenant_validator:emit_audit_event/3`

**DON'T**:
- ❌ Use `router_acl:allow/2` or `router_acl:deny/2` (deprecated)
- ❌ Use `router_acl:load_policies/1` (deprecated)
- ❌ Create new ACL modules (use `router_tenant_validator`)

### Migration from `router_acl`

**If you have code using `router_acl`**:

1. **Replace** `router_acl:allow(Tenant, Action)` with:
   ```erlang
   case router_tenant_validator:validate_tenant(Tenant, Context) of
       {ok, TenantId} -> {ok, allowed};
       {error, Reason, Context} -> {error, denied}
   end
   ```

2. **Replace** `router_acl:load_policies(PoliciesMap)` with:
   ```erlang
   %% Use router_policy_store:upsert_policy/3 for each policy
   lists:foreach(fun({TenantId, Policy}) ->
       router_policy_store:upsert_policy(TenantId, Policy, CorrelationId)
   end, PoliciesMap)
   ```

3. **Replace** `router_acl:audit/3` with:
   ```erlang
   router_tenant_validator:emit_audit_event(EventType, TenantId, Context)
   ```

## Testing

### Test Coverage

**Required Tests**:
- ✅ Tenant format validation (missing, empty, invalid format)
- ✅ Allowlist validation (configured, not configured, tenant in/out)
- ✅ Policy registry validation (policy exists, not found)
- ✅ Audit event emission (all event types)
- ✅ Integration with consumers (result, ack, intake)

**Test Files**:
- `apps/otp/router/test/router_tenant_validator_SUITE.erl`: Tenant validation tests
- `apps/otp/router/test/router_result_consumer_SUITE.erl`: Result consumer with tenant validation
- `apps/otp/router/test/router_ack_consumer_SUITE.erl`: ACK consumer with tenant validation
- `apps/otp/router/test/router_intake_validator_SUITE.erl`: Intake validator with tenant validation

**Deprecated Tests**:
- `apps/otp/router/test/router_acl_SUITE.erl`: **Mark as deprecated** (tests deprecated module)

## References

- `apps/otp/router/src/router_tenant_validator.erl`: Primary ACL implementation
- `apps/otp/router/src/router_policy_store.erl`: Policy registry
- `apps/otp/router/src/router_acl.erl`: **Deprecated** (not used)
- `apps/otp/router/src/router_result_consumer.erl`: Usage example
- `apps/otp/router/src/router_ack_consumer.erl`: Usage example
- `apps/otp/router/src/router_intake_validator.erl`: Usage example

