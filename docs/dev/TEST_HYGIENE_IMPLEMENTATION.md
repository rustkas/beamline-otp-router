# Test Hygiene Implementation Report

**Date**: 2025-11-30  
**Status**: ✅ **Test Classification and Marking Implemented**

## Summary

Test suites have been classified into fast (smoke/contract) and slow (load/JetStream E2E/property) categories. Test execution scripts and CI configuration have been updated to run only fast tests in CP1 CI.

## Test Classification

### Fast Tests (12 suites) - CP1 CI

**Duration**: < 30 seconds  
**Category**: Smoke/Contract tests

1. `router_core_SUITE.erl` - Core routing decisions
2. `router_e2e_smoke_SUITE.erl` - E2E smoke test
3. `router_rbac_SUITE.erl` - RBAC tests
4. `router_policy_enforcement_SUITE.erl` - Policy enforcement
5. `router_decider_SUITE.erl` - Decision engine
6. `router_policy_store_SUITE.erl` - Policy store (basic)
7. `router_error_SUITE.erl` - Error handling
8. `router_grpc_SUITE.erl` - gRPC service (basic)
9. `router_grpc_integration_SUITE.erl` - gRPC integration (basic)
10. `router_caf_adapter_unit_SUITE.erl` - CAF adapter unit tests
11. `router_core_telemetry_contract_SUITE.erl` - Telemetry contract
12. `router_secrets_logging_SUITE.erl` - Secrets masking

### Slow Tests (22+ suites) - Full CI

**Duration**: > 5 minutes  
**Categories**: Load, JetStream E2E, Property-based, CP2+ features

#### JetStream E2E (6 suites)
- `router_jetstream_e2e_SUITE.erl`
- `router_delivery_count_tracking_SUITE.erl`
- `router_result_consumer_SUITE.erl`
- `router_caf_adapter_SUITE.erl`
- `router_caf_adapter_enhanced_SUITE.erl`
- `router_nats_subscriber_caf_SUITE.erl`

#### Property-Based (4 suites)
- `router_decider_prop_SUITE.erl`
- `router_policy_store_prop_SUITE.erl`
- `router_normalize_boolean_prop_SUITE.erl`
- `router_options_merge_prop_SUITE.erl`

#### Load (1 suite)
- `router_policy_store_load_SUITE.erl`

#### CP2+ Features (2 suites)
- `router_idempotency_SUITE.erl`
- `router_tenant_allowlist_SUITE.erl`

#### Advanced Integration (9+ suites)
- `router_policy_store_fault_tolerance_SUITE.erl`
- `router_admin_grpc_integration_SUITE.erl`
- `router_admin_grpc_concurrency_SUITE.erl`
- `router_assignment_SUITE.erl`
- `router_sticky_store_SUITE.erl`
- `router_policy_SUITE.erl`
- `router_policy_validator_SUITE.erl`
- `router_ets_guard_SUITE.erl`
- `router_error_status_SUITE.erl`

## Test Marking Strategy

### By Naming Convention

**Fast Tests**: No special suffix (default)  
**Slow Tests**: Suffixes indicate test type:
- `*_prop_SUITE.erl` - Property-based tests
- `*_load_SUITE.erl` - Load tests
- `*_e2e_SUITE.erl` - End-to-end tests (if heavy)
- `*_integration_SUITE.erl` - Integration tests (if heavy)

### Explicit Classification

Test suites are explicitly classified in test execution scripts:
- `scripts/test_fast.sh` / `scripts/test_fast.ps1` - Fast test suites list
- `scripts/test_slow.sh` / `scripts/test_slow.ps1` - Slow test suites list

## Scripts Created

### Fast Test Runner

**Files**:
- `scripts/test_fast.sh` (Bash)
- `scripts/test_fast.ps1` (PowerShell)

**Usage**:
```bash
make test-fast
# or
./scripts/test_fast.sh
./scripts/test_fast.sh --verbose
```

**Runs**: 12 fast test suites (smoke/contract)

### Slow Test Runner

**Files**:
- `scripts/test_slow.sh` (Bash)
- `scripts/test_slow.ps1` (PowerShell)

**Usage**:
```bash
make test-slow
# or
./scripts/test_slow.sh
./scripts/test_slow.sh --verbose
```

**Runs**: 22+ slow test suites (load/JetStream E2E/property)

### All Tests Runner

**Usage**:
```bash
make test-all
```

**Runs**: Fast + slow tests sequentially

## Makefile Targets

### New Targets

- `make test-fast` - Run fast tests only (smoke/contract)
- `make test-slow` - Run slow tests only (load/JetStream E2E/property)
- `make test-all` - Run all tests (fast + slow)

### Existing Targets

- `make test-cp1-smoke` - Run CP1 baseline smoke tests (subset of fast tests)
- `make test` - Run all tests (unchanged)
- `make test-parallel` - Run all tests in parallel (unchanged)

## CI/CD Updates

### CP1 CI (Fast Tests Only)

**Updated Files**:
- `.github/workflows/ci.yml` - Changed from `rebar3 ct` to `make test-fast`
- `scripts/run_checks.sh` - Changed from `rebar3 ct` to `make test-fast`
- `scripts/run_checks.ps1` - Changed from `rebar3 ct` to `make test-fast`

**Before**:
```yaml
- name: Common Test (router) - Parallel execution
  run: rebar3 as test ct -j 4 --logdir ct_logs
```

**After**:
```yaml
- name: Common Test (router) - Fast tests only (CP1 CI)
  run: make test-fast
```

### Full CI (All Tests)

Full CI can run all tests using:
```bash
make test-all
```

Or slow tests separately:
```bash
make test-slow
```

## Test Execution Time

### Fast Tests (CP1 CI)
- **Expected**: < 30 seconds (excluding compilation)
- **Suites**: 12 test suites
- **Purpose**: Quick validation of core functionality

### Slow Tests (Full CI)
- **Expected**: > 5 minutes (some suites)
- **Suites**: 22+ test suites
- **Purpose**: Comprehensive validation including performance and JetStream features

### All Tests
- **Expected**: > 5 minutes (fast + slow)
- **Suites**: 34+ test suites
- **Purpose**: Complete test coverage

## Test Profiling and Timing

### Slow Test Timing

The `test_slow.sh` script provides detailed timing information for each test suite:

**Features**:
- Individual suite execution time
- Total duration for all slow tests
- Top 5 slowest suites summary
- Pass/fail status per suite

**Example Output**:
```
=== Slow Test Execution Summary ===

Suite Execution Times:
  ✓ router_jetstream_e2e_SUITE                           5m 23s
  ✓ router_policy_store_load_SUITE                       3m 12s
  ✗ router_decider_prop_SUITE                            2m 45s (FAILED)
  ...

Total Duration: 15m 32s

Top 5 Slowest Suites:
  1. router_jetstream_e2e_SUITE                          5m 23s
  2. router_policy_store_load_SUITE                       3m 12s
  3. router_policy_store_fault_tolerance_SUITE            2m 58s
  4. router_decider_prop_SUITE                            2m 45s
  5. router_admin_grpc_concurrency_SUITE                  1m 34s
```

### CI Integration

**Benefits for CI/CD**:
1. **Identify bottlenecks**: See which suites take the longest
2. **Optimize test runs**: Focus optimization efforts on slowest suites
3. **Schedule decisions**: Understand how often to run slow tests
4. **Resource planning**: Estimate CI runner time requirements

**Usage in CI**:
```yaml
- name: Run Slow Tests (with timing)
  working-directory: apps/otp/router
  run: make test-slow
  continue-on-error: true
```

### Test Frequency Recommendations

Based on timing data:

- **Fast tests**: Run on every commit (CP1 CI)
- **Slow tests**: 
  - Run on PR merge to main
  - Run nightly for comprehensive coverage
  - Run on-demand for debugging

**Typical slow test duration**: 10-20 minutes (varies by suite count and system load)

### Profiling Best Practices

1. **Monitor trends**: Track timing over time to detect regressions
2. **Optimize slowest**: Focus on top 5 slowest suites for optimization
3. **Parallel execution**: Consider parallel runs for independent suites
4. **Selective runs**: Run only relevant slow suites during development

## Benefits

1. **Faster CI**: CP1 CI runs only fast tests (< 30s vs > 5min)
2. **Selective Execution**: Can run fast or slow tests separately
3. **Clear Classification**: Test suites are explicitly categorized
4. **Backward Compatible**: Existing `make test` and `make test-cp1-smoke` still work

## Migration Notes

### From `rebar3 ct` to `make test-fast`

**Before** (in CI):
```bash
rebar3 as test ct -j 4
```

**After** (in CI):
```bash
make test-fast
```

### CP1 Smoke Tests

CP1 smoke tests (`make test-cp1-smoke`) remain available and are a subset of fast tests:
- CP1 smoke: 7 suites (minimal CP1 baseline)
- Fast tests: 12 suites (includes additional contract/unit tests)

## References

- `docs/TEST_CLASSIFICATION.md`: Complete test classification guide
- `scripts/test_fast.sh`: Fast test runner (Bash)
- `scripts/test_slow.sh`: Slow test runner (Bash)
- `scripts/test_fast.ps1`: Fast test runner (PowerShell)
- `scripts/test_slow.ps1`: Slow test runner (PowerShell)
- `.github/workflows/ci.yml`: CI workflow (updated)
- `scripts/run_checks.sh`: Local checks script (updated)

