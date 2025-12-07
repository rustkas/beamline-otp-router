# CP1 Baseline Testing Guide

This document describes how to run CP1 baseline tests separately from CP2+ heavy test suites.

## CP1 Baseline Test Suites

The following test suites are part of the **CP1 baseline smoke set**:

### Core Functionality
- **router_core_SUITE.erl**: Basic routing decisions, policy parsing, error handling
- **router_decider_SUITE.erl**: Decision engine (weighted routing, fallback)
- **router_policy_store_SUITE.erl**: Policy store operations (load, get, list)

### Policy Enforcement (CP1 Phase 3)
- **router_rbac_SUITE.erl**: Role-Based Access Control (admin, operator, viewer)
- **router_policy_enforcement_SUITE.erl**: RBAC + Quota + Rate Limiting integration
- **router_e2e_smoke_SUITE.erl**: End-to-end smoke test (RBAC → Quota → Rate Limiting → Audit)

### Error Handling
- **router_error_SUITE.erl**: Error mapping and gRPC error codes

## CP2+ Test Suites (Excluded from CP1 Smoke)

The following test suites are **excluded** from CP1 smoke tests (CP2+ features):

- **router_jetstream_e2e_SUITE.erl**: Heavy JetStream E2E tests (durable subscriptions, ACK/NAK, redelivery)
- **router_delivery_count_tracking_SUITE.erl**: JetStream delivery count tracking
- **router_idempotency_SUITE.erl**: Idempotency layer (CP2+ feature)
- **router_tenant_allowlist_SUITE.erl**: Tenant validation (CP2+ feature)
- **router_result_consumer_SUITE.erl**: Result consumer (may use JetStream features)
- **router_caf_adapter_SUITE.erl**: CAF adapter (may use JetStream features)
- **router_caf_adapter_enhanced_SUITE.erl**: Enhanced CAF adapter features
- **router_nats_subscriber_caf_SUITE.erl**: NATS/JetStream integration

## Running CP1 Smoke Tests

### Using Test Scripts

#### Bash (Linux/macOS/WSL)
```bash
cd apps/otp/router
./scripts/test_cp1_smoke.sh
```

#### PowerShell (Windows)
```powershell
cd apps/otp/router
.\scripts\test_cp1_smoke.ps1
```

#### Verbose Output
```bash
./scripts/test_cp1_smoke.sh --verbose
```

### Using rebar3 Directly

```bash
cd apps/otp/router
rebar3 ct --suite test/router_core_SUITE \
          --suite test/router_e2e_smoke_SUITE \
          --suite test/router_rbac_SUITE \
          --suite test/router_policy_enforcement_SUITE \
          --suite test/router_decider_SUITE \
          --suite test/router_policy_store_SUITE \
          --suite test/router_error_SUITE
```

### Using Makefile (if available)

If a Makefile target exists:
```bash
make test-cp1-smoke
```

## Test Configuration

CP1 smoke tests use the following configuration:

- **Test Profile**: `test` profile from `rebar.config`
- **Telemetry**: Disabled (`telemetry_enabled = false`) for faster execution
- **HEIR**: Disabled (`disable_heir = true`) for faster execution
- **gRPC Port**: Uses different ports (9001, 9003) to avoid conflicts

## Expected Test Duration

CP1 smoke tests should complete in **< 30 seconds** (excluding compilation time).

Heavy JetStream tests (excluded) may take **> 5 minutes** due to:
- NATS server startup/shutdown
- JetStream durable subscription creation
- Message redelivery scenarios
- Idempotency checks with TTL

## CI/CD Integration

### GitHub Actions Example

```yaml
- name: Run CP1 Smoke Tests
  run: |
    cd apps/otp/router
    ./scripts/test_cp1_smoke.sh
```

### GitLab CI Example

```yaml
test:cp1-smoke:
  script:
    - cd apps/otp/router
    - ./scripts/test_cp1_smoke.sh
```

## Troubleshooting

### Tests Fail to Start

1. **Check application dependencies**:
   ```bash
   rebar3 deps
   ```

2. **Verify test configuration**:
   ```bash
   cat config/test.config
   ```

3. **Check for port conflicts**:
   - CP1 tests use ports 9001, 9003
   - Ensure these ports are available

### Tests Timeout

1. **Increase timetrap** in test suite:
   ```erlang
   suite() ->
       [{timetrap, {seconds, 60}}].
   ```

2. **Check for hanging processes**:
   ```bash
   ps aux | grep beam
   ```

### ETS Table Errors

CP1 tests may skip gracefully if ETS tables are unavailable (known non-blocking issue):
- Tests will log: `"Skipping test: ETS table not accessible"`
- This is expected behavior and does not block CP1 acceptance

## References

- `docs/CP1_BASELINE.md`: CP1 baseline components and feature flags
- `docs/CP1_CHECKLIST.md`: CP1 acceptance criteria
- `apps/otp/router/test/router_e2e_smoke_SUITE.erl`: E2E smoke test implementation
- `apps/otp/router/scripts/test_cp1_smoke.sh`: Bash test runner script
- `apps/otp/router/scripts/test_cp1_smoke.ps1`: PowerShell test runner script

