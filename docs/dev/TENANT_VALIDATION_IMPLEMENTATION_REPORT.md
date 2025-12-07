# Tenant ID Validation/ACL Implementation Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

Implemented tenant ID validation and ACL (Access Control List) for incoming `ExecResult` and `ExecAssignmentAck` messages. The implementation includes:
- Tenant validation against allowlist
- Tenant validation against policy registry
- Audit event emission for validation failures
- Message rejection on validation failure (no acknowledgment)

## Implementation Summary

### New Module: `router_tenant_validator.erl`

**Functions**:
- `validate_tenant/2`: Validates tenant_id against allowlist and policy registry
- `validate_tenant_allowlist/1`: Checks tenant against allowlist
- `validate_tenant_policy/1`: Checks if policy exists for tenant
- `emit_audit_event/3`: Emits audit events for validation failures

**Validation Flow**:
1. Check if `tenant_id` is present and not empty
2. Check against allowlist (if configured via `result_ack_allowed_tenants`)
3. Check against policy registry (if policy exists for tenant)
4. Emit audit events on validation failures
5. Return `{ok, TenantId}` or `{error, Reason, Context}`

**Audit Events**:
- `router_tenant_audit_total`: Counter for all tenant validation events
- Event types: `tenant_missing`, `tenant_empty`, `tenant_not_allowed`, `tenant_policy_not_found`, `tenant_invalid_format`

### Updated Modules

1. **`router_result_consumer.erl`**:
   - Added tenant validation in `process_exec_result/1`
   - Calls `router_tenant_validator:validate_tenant/2` before processing
   - Rejects messages on validation failure (no ACK)
   - Emits `router_results_tenant_rejected_total` counter
   - Added `process_validated_result/10` function for validated processing
   - Added `log_tenant_validation_error/4` for logging

2. **`router_ack_consumer.erl`**:
   - Added tenant validation in `process_ack/1`
   - Calls `router_tenant_validator:validate_tenant/2` before processing
   - Rejects messages on validation failure (no ACK)
   - Emits `router_acks_tenant_rejected_total` counter
   - Added `process_validated_ack/4` function for validated processing
   - Added `log_tenant_validation_error/3` for logging

3. **`beamline_router.app.src`**:
   - Added `result_ack_allowed_tenants` configuration parameter
   - Default: `undefined` (allow all tenants)

## Configuration

### `result_ack_allowed_tenants`

**Type**: `list()` | `map()` | `undefined`  
**Default**: `undefined`  
**Description**: Allowlist for tenant validation in ExecResult and ExecAssignmentAck messages.

**Examples**:
```erlang
%% List of allowed tenant IDs
{result_ack_allowed_tenants, [<<"tenant1">>, <<"tenant2">>]}

%% Map for additional metadata
{result_ack_allowed_tenants, #{
    <<"tenant1">> => #{},
    <<"tenant2">> => #{}
}}

%% Allow all tenants (default)
{result_ack_allowed_tenants, undefined}
```

## Validation Logic

### Allowlist Check

1. If `result_ack_allowed_tenants` is `undefined`: Allow all tenants
2. If list: Check if tenant_id is in the list (supports binary and string)
3. If map: Check if tenant_id is a key in the map
4. If tenant not in allowlist: Reject with `tenant_not_allowed` error

### Policy Registry Check

1. Attempt to load policy for tenant (using default policy)
2. If policy exists: Tenant is valid
3. If policy not found: Emit audit event but allow (for flexibility)
4. If error loading policy: Treat as not found

### Validation Results

- **Success**: `{ok, ValidatedTenantId}` - Continue processing
- **Failure**: `{error, Reason, Context}` - Reject message, emit audit event, do not ACK

## Audit Events

### Telemetry Events

**`[router_tenant_validator, audit]`**:
- Metadata: `event_type`, `tenant_id`, `timestamp`, `check_type`, `reason`, `source`, `assignment_id`, `request_id`, etc.

**Counters**:
- `router_tenant_audit_total`: All tenant validation events
- `router_results_tenant_rejected_total`: Rejected ExecResult messages
- `router_acks_tenant_rejected_total`: Rejected ACK messages

### Logging

- **Warn level**: Tenant validation failures
- **Context**: Assignment ID, Request ID, Provider ID, Job Type, Trace ID, Source, Reason

## Message Handling

### ExecResult Processing

1. Parse `ExecResult` JSON
2. Validate required fields (correlation IDs, status)
3. **Validate tenant_id** (NEW)
4. If validation fails: Reject, emit audit, do not ACK
5. If validation succeeds: Process result, emit usage event, ACK message

### ExecAssignmentAck Processing

1. Parse `ExecAssignmentAck` JSON
2. Validate required fields (assignment_id, status)
3. **Validate tenant_id** (NEW)
4. If validation fails: Reject, emit audit, do not ACK
5. If validation succeeds: Process ACK, log status, ACK message

## Security Considerations

### Message Rejection

- Messages with invalid tenant_id are **not acknowledged**
- JetStream will redeliver messages after ack wait timeout
- Prevents unauthorized usage events from being emitted
- Prevents unauthorized ACK processing

### Audit Trail

- All validation failures are logged with full context
- Telemetry events include correlation IDs for tracing
- Audit events can be used for security monitoring and alerting

### Policy Registry Integration

- Validation checks if policy exists for tenant
- Allows flexibility: tenants without policies are allowed (with audit)
- Can be enhanced to require policy existence for strict validation

## Testing

**Compilation**: ✅ Successful
- All modules compile without errors
- Minor warnings about unused variables (expected in error handlers)

**Integration**:
- `router_result_consumer` validates tenant_id before processing
- `router_ack_consumer` validates tenant_id before processing
- Audit events are emitted on validation failures

## Known Limitations

1. **Policy Registry Check**: Currently allows tenants without policies (with audit). Can be made strict if needed.
2. **Allowlist Configuration**: Single allowlist for both ExecResult and ACK. Separate allowlists can be added if needed.
3. **Message Redelivery**: Rejected messages are redelivered by JetStream. Consider implementing DLQ for persistent failures.

## Next Steps

1. **Strict Policy Validation**: Require policy existence for tenant validation
2. **Separate Allowlists**: Separate allowlists for ExecResult and ACK
3. **DLQ Integration**: Dead letter queue for persistently failing messages
4. **Integration Tests**: E2E tests with tenant validation scenarios
5. **Performance Testing**: Load test with tenant validation enabled

## References

- `src/router_tenant_validator.erl`: Tenant validation implementation
- `src/router_result_consumer.erl`: Result consumer with tenant validation
- `src/router_ack_consumer.erl`: ACK consumer with tenant validation
- `docs/CONFIG.md`: Configuration documentation

