# Router Multi-Tenant Smoke Tests

## Purpose

This document describes the multi-tenant smoke test suite for Router, which verifies tenant isolation, validation, and correct tenant labeling in metrics and logs.

**Test Suite**: `router_tenant_multitenant_smoke_SUITE.erl`

**Tags**: `@test_category fast, cp1_smoke`

## Goals

- Verify tenant validation works correctly (valid tenants succeed, invalid tenants are rejected)
- Verify tenant isolation (tenants operate independently)
- Verify metrics contain correct tenant labels and do not mix tenants
- Verify logs contain correct tenant labels and do not mix tenants

## Test Tenants

The test suite uses the following tenant IDs:

- **tenant-a** (`<<"tenant-a">>`): Valid tenant in allowlist
- **tenant-b** (`<<"tenant-b">>`): Valid tenant in allowlist
- **tenant-invalid** (`<<"tenant-invalid">>`): Invalid tenant (not in allowlist)

## Test Scenarios

### 1. Valid Tenant A → Success

**Test**: `test_valid_tenant_a_success/1`

**Purpose**: Verify that a valid tenant (tenant-a) is correctly validated and allowed.

**Steps**:
1. Configure tenant allowlist with `tenant-a` and `tenant-b`
2. Validate `tenant-a` using `router_tenant_validator:validate_tenant/2`
3. Verify validation succeeds with `{ok, TenantId}`
4. Verify allowlist checks pass for `tenant-a`

**Expected Results**:
- ✅ Validation returns `{ok, <<"tenant-a">>}`
- ✅ `router_nats_subscriber:check_tenant_allowed/1` returns `true`
- ✅ `router_caf_adapter:check_tenant_allowed/1` returns `true`

### 2. Invalid Tenant → Expected Rejection

**Test**: `test_invalid_tenant_rejection/1`

**Purpose**: Verify that an invalid tenant (not in allowlist) is correctly rejected.

**Steps**:
1. Configure tenant allowlist with `tenant-a` and `tenant-b` (not `tenant-invalid`)
2. Validate `tenant-invalid` using `router_tenant_validator:validate_tenant/2`
3. Verify validation fails with `{error, tenant_not_allowed, Context}`
4. Verify allowlist checks fail for `tenant-invalid`

**Expected Results**:
- ✅ Validation returns `{error, tenant_not_allowed, Context}`
- ✅ Error context contains `tenant_id => <<"tenant-invalid">>`
- ✅ Error context contains `reason => <<"tenant_id not in allowlist">>`
- ✅ `router_nats_subscriber:check_tenant_allowed/1` returns `false`
- ✅ `router_caf_adapter:check_tenant_allowed/1` returns `false`

### 3. Valid Tenant B → Independent Success

**Test**: `test_valid_tenant_b_independent/1`

**Purpose**: Verify that a second valid tenant (tenant-b) operates independently from tenant-a.

**Steps**:
1. Configure tenant allowlist with `tenant-a` and `tenant-b`
2. Validate `tenant-b` using `router_tenant_validator:validate_tenant/2`
3. Verify validation succeeds with `{ok, TenantId}`
4. Verify allowlist checks pass for `tenant-b`
5. Verify `tenant-a` still works (isolation check)

**Expected Results**:
- ✅ Validation returns `{ok, <<"tenant-b">>}`
- ✅ `router_nats_subscriber:check_tenant_allowed/1` returns `true` for `tenant-b`
- ✅ `router_caf_adapter:check_tenant_allowed/1` returns `true` for `tenant-b`
- ✅ `tenant-a` still works (no cross-tenant interference)

### 4. Tenant Metrics Isolation

**Test**: `test_tenant_metrics_isolation/1`

**Purpose**: Verify that telemetry metrics contain correct tenant labels and do not mix tenants.

**Steps**:
1. Setup telemetry handler to capture events
2. Generate validation events for `tenant-a`, `tenant-b`, and `tenant-invalid`
3. Collect all telemetry events
4. Filter events by `tenant_id` from metadata
5. Verify each event has the correct `tenant_id` label
6. Verify no cross-tenant mixing (tenant-a events don't contain tenant-b)

**Expected Results**:
- ✅ All `tenant-a` events have `tenant_id => <<"tenant-a">>` in metadata
- ✅ All `tenant-b` events have `tenant_id => <<"tenant-b">>` in metadata
- ✅ All `tenant-invalid` events have `tenant_id => <<"tenant-invalid">>` in metadata
- ✅ No `tenant-a` events contain `tenant-b` in metadata
- ✅ No `tenant-b` events contain `tenant-a` in metadata

**Telemetry Events Captured**:
- `[router_tenant_validator, audit]` - Audit events for tenant validation
- `[router_tenant_validator, router_tenant_audit_total]` - Counter for audit events
- `[router_core, route]` - Routing decisions (if triggered)
- `[router_nats_subscriber, decide_requests_total]` - Decide request counter (if triggered)

### 5. Tenant Logs Isolation

**Test**: `test_tenant_logs_isolation/1`

**Purpose**: Verify that logs contain correct tenant labels and do not mix tenants.

**Steps**:
1. Create test contexts with different tenants (`tenant-a`, `tenant-b`, `tenant-invalid`)
2. Validate tenants (triggers logging via `router_tenant_validator:emit_audit_event`)
3. Verify `tenant_id` is correctly passed in contexts
4. Verify no cross-tenant mixing in contexts
5. Verify `router_logger:filter_pii/1` preserves `tenant_id` structure

**Expected Results**:
- ✅ Contexts contain correct `tenant_id` values
- ✅ No cross-tenant mixing in contexts
- ✅ `tenant_id` is preserved after PII filtering (not filtered as PII)

**Note**: This test verifies the structure of log contexts rather than capturing actual log output, as Common Test does not easily capture log output. The test ensures that `tenant_id` is correctly structured in contexts that will be logged.

## Running the Tests

### Run All Multi-Tenant Smoke Tests

```bash
cd apps/otp/router
rebar3 ct --suite router_tenant_multitenant_smoke_SUITE
```

### Run Specific Test

```bash
cd apps/otp/router
rebar3 ct --suite router_tenant_multitenant_smoke_SUITE --case test_valid_tenant_a_success
```

### Run with Verbose Output

```bash
cd apps/otp/router
rebar3 ct --suite router_tenant_multitenant_smoke_SUITE --verbose
```

## Configuration

The test suite configures the following application environment variables:

- `caf_push_assignment_allowed_tenants`: List of allowed tenant IDs
  - Default in tests: `[<<"tenant-a">>, <<"tenant-b">>]`
- `nats_mode`: Set to `mock` for testing
- `grpc_enabled`: Set to `false` for testing
- `grpc_port`: Set to `0` for testing

## Interpreting Results

### All Tests Pass

✅ **Success**: All tenant validation, isolation, and labeling tests pass.

**Meaning**:
- Tenant validation works correctly
- Tenants operate independently
- Metrics and logs contain correct tenant labels
- No cross-tenant data leakage

### Test Failures

**`test_valid_tenant_a_success` fails**:
- **Possible causes**: Tenant allowlist not configured, validation logic broken
- **Check**: Verify `caf_push_assignment_allowed_tenants` is set correctly

**`test_invalid_tenant_rejection` fails**:
- **Possible causes**: Validation logic allows invalid tenants, allowlist check broken
- **Check**: Verify `router_tenant_validator:validate_tenant_allowlist/1` works correctly

**`test_valid_tenant_b_independent` fails**:
- **Possible causes**: Tenant isolation broken, shared state between tenants
- **Check**: Verify no shared state in tenant validation logic

**`test_tenant_metrics_isolation` fails**:
- **Possible causes**: Telemetry events missing `tenant_id` label, cross-tenant mixing
- **Check**: Verify telemetry metadata includes `tenant_id` for all events

**`test_tenant_logs_isolation` fails**:
- **Possible causes**: Log contexts missing `tenant_id`, PII filtering removes `tenant_id`
- **Check**: Verify `router_logger:filter_pii/1` preserves `tenant_id`

## Integration with CI

### Fast CI (PR Checks)

The multi-tenant smoke tests are included in fast CI runs:

```bash
cd apps/otp/router
make test-fast
```

This runs all fast test suites, including `router_tenant_multitenant_smoke_SUITE`.

### CP1 Smoke Tests

The multi-tenant smoke tests are included in CP1 smoke test runs:

```bash
cd apps/otp/router
make test-cp1-smoke
```

## Related Documentation

- `apps/otp/router/test/router_tenant_allowlist_SUITE.erl` - Unit tests for tenant allowlist parsing
- `apps/otp/router/src/router_tenant_validator.erl` - Tenant validation implementation
- `apps/otp/router/src/router_nats_subscriber.erl` - NATS subscriber with tenant checks
- `apps/otp/router/src/router_caf_adapter.erl` - CAF adapter with tenant checks
- `apps/otp/router/src/router_logger.erl` - Structured logging with tenant_id support

## Troubleshooting

### Telemetry Events Not Captured

**Issue**: `test_tenant_metrics_isolation` fails because no events are captured.

**Solution**:
1. Verify telemetry handler is attached in `init_per_suite/1`
2. Check that events are actually emitted (add debug logging)
3. Increase wait time after validation calls

### Tenant Validation Always Succeeds

**Issue**: Invalid tenants are not rejected.

**Solution**:
1. Verify `caf_push_assignment_allowed_tenants` is configured correctly
2. Check that `router_tenant_validator:validate_tenant_allowlist/1` returns `false` for invalid tenants
3. Verify allowlist is not empty (empty allowlist = allow all)

### Cross-Tenant Mixing Detected

**Issue**: Metrics or logs show tenant-a data in tenant-b events.

**Solution**:
1. Check for shared state in tenant validation logic
2. Verify telemetry metadata is correctly scoped per request
3. Check that `tenant_id` is correctly extracted from request context

## Negative Security Scenarios

This section describes security-focused negative test scenarios that verify Router's protection against tenant spoofing, cross-tenant data leakage, and incorrect header/payload handling.

### 6. Tenant Spoofing Rejection

**Test**: `test_tenant_spoofing_rejection/1`

**Purpose**: Verify that Router prevents tenant spoofing attempts where a request has `tenant_id=tenant-a` in headers but a different tenant in payload.

**Steps**:
1. Create headers with `tenant_id=tenant-a`
2. Create payload with `tenant_id=tenant-b` (spoofing attempt)
3. Extract `tenant_id` using Router's logic (headers priority)
4. Verify that header `tenant_id` is used, not payload
5. Validate the tenant from headers

**Expected Results**:
- ✅ Header `tenant_id` is used (tenant-a)
- ✅ Payload `tenant_id` is ignored (tenant-b)
- ✅ Tenant validation succeeds for header tenant (tenant-a is valid)
- ✅ Spoofing attempt is prevented (payload tenant not used)

**Security Implication**: Router prioritizes headers over payload, preventing attackers from spoofing tenant identity by manipulating payload.

### 7. Mixed Tenant Payload Ignored in Metrics/Logs

**Test**: `test_mixed_tenant_payload_ignored/1`

**Purpose**: Verify that if payload contains a different `tenant_id`, it does not appear in metrics/logs labels.

**Steps**:
1. Setup telemetry handler to capture events
2. Create headers with `tenant_id=tenant-a`
3. Create payload with `tenant_id=tenant-b` (different tenant)
4. Extract `tenant_id` (headers priority)
5. Validate tenant (emits telemetry events)
6. Collect telemetry events
7. Filter events by `tenant_id` in metadata
8. Verify only tenant-a events exist (payload tenant-b ignored)

**Expected Results**:
- ✅ Only tenant-a events exist in telemetry
- ✅ No tenant-b events exist (payload tenant ignored)
- ✅ All events have `tenant_id=tenant-a` in metadata
- ✅ No cross-tenant mixing in metrics/logs

**Security Implication**: Metrics and logs use header `tenant_id`, preventing cross-tenant data leakage through payload manipulation.

### 8. Tenant Header Priority Over Payload

**Test**: `test_tenant_header_priority_over_payload/1`

**Purpose**: Verify that Router correctly prioritizes headers over payload for `tenant_id` extraction, and uses payload as fallback when header is missing.

**Steps**:
1. **Scenario 1**: Header has `tenant_id`, payload has different `tenant_id`
   - Verify header `tenant_id` is used
2. **Scenario 2**: Header missing `tenant_id`, payload has `tenant_id`
   - Verify payload `tenant_id` is used (fallback)
3. **Scenario 3**: Both missing `tenant_id`
   - Verify `tenant_id` is `undefined`
4. Verify validation behavior for each scenario

**Expected Results**:
- ✅ **Scenario 1**: Header `tenant_id` is used when present
- ✅ **Scenario 2**: Payload `tenant_id` is used when header missing
- ✅ **Scenario 3**: `tenant_id` is `undefined` when both missing
- ✅ Validation succeeds for valid tenants from both sources

**Security Implication**: Router follows a clear priority: headers first, payload as fallback. This prevents spoofing while maintaining backward compatibility.

## Security Test Group

The security tests are grouped separately from basic multi-tenant tests:

```bash
# Run only security tests
cd apps/otp/router
rebar3 ct --suite router_tenant_multitenant_smoke_SUITE --group security_tests

# Run all tests (multitenant + security)
rebar3 ct --suite router_tenant_multitenant_smoke_SUITE
```

## Interpreting Security Test Results

### All Security Tests Pass

✅ **Success**: Router correctly prevents tenant spoofing and cross-tenant data leakage.

**Meaning**:
- Tenant spoofing attempts are prevented (headers priority)
- Cross-tenant data does not leak into metrics/logs
- Header/payload priority is correctly enforced

### Security Test Failures

**`test_tenant_spoofing_rejection` fails**:
- **Possible causes**: Router uses payload `tenant_id` when header is present, spoofing prevention broken
- **Check**: Verify `extract_header_or_payload` logic prioritizes headers

**`test_mixed_tenant_payload_ignored` fails**:
- **Possible causes**: Payload `tenant_id` appears in metrics/logs, cross-tenant leakage
- **Check**: Verify telemetry metadata uses header `tenant_id`, not payload

**`test_tenant_header_priority_over_payload` fails**:
- **Possible causes**: Incorrect priority logic, payload used when header present
- **Check**: Verify `extract_header_or_payload` implementation matches contract

## Runbook: Tenant Rejection Alert

### Alert: RouterHighTenantRejectionRate

**When this alert fires**: Router tenant rejection rate > 10% for 5 minutes.

**Steps**:
1. **Check tenant rejection metrics**:
   ```bash
   rate(router_results_tenant_rejected_total[5m]) / rate(router_results_total[5m])
   ```

2. **Identify rejected tenants**:
   - Check Router logs for tenant validation failures
   - Review tenant allowlist configuration
   - Check if tenants are in allowlist

3. **Verify allowlist configuration**:
   ```bash
   # Check Router configuration
   application:get_env(beamline_router, caf_push_assignment_allowed_tenants)
   ```

4. **Fix tenant issues**:
   - Add missing tenants to allowlist if legitimate
   - Remove invalid tenants from requests if incorrect
   - Verify tenant validation logic is correct

5. **If many tenants rejected**: Review tenant validation policy and allowlist configuration.

**Related Documentation**:
- `apps/otp/router/docs/dev/ROUTER_TENANT_MULTITENANT_SMOKE.md#test-invalid-tenant-rejection` - Invalid tenant rejection test
- `apps/otp/router/test/router_tenant_multitenant_smoke_SUITE.erl` - Multi-tenant smoke tests

## Future Enhancements

- **E2E Multi-Tenant Tests**: Add E2E tests that verify tenant isolation across full request flow
- **Load Testing**: Add load tests with multiple tenants to verify isolation under load
- **Log Capture**: Enhance log isolation test to capture actual log output (if possible)
- **Property-Based Testing**: Add property-based tests for tenant validation edge cases
- **Contract Violation Detection**: Add tests that verify contract violations are logged when tenant spoofing is detected

