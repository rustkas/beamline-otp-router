# Concurrent Faults Test CI Stability

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Document CI stability verification process for concurrent faults tests.

## Overview

This document describes the process for verifying non-flakiness of concurrent faults tests in CI/CD environments. It provides commands, success criteria, and troubleshooting guidance.

## CI Stability Verification Process

### Step 1: Multiple Test Runs

**Purpose**: Verify tests pass consistently across multiple runs

**Commands**:
```bash
# Run concurrent fault tests multiple times (5 runs)
for i in {1..5}; do
    echo "Run $i/5"
    rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
      --case test_ack_error_with_tenant_validation_fail_concurrent \
      --case test_batch_nak_publish_failure_mixed \
      --case test_tenant_isolation_during_concurrent_faults
done

# Run decide consumer concurrent fault tests
for i in {1..5}; do
    echo "Run $i/5"
    rebar3 ct --suite apps/otp/router/test/router_decide_consumer_SUITE \
      --case test_decide_ack_error_with_tenant_validation_fail_concurrent \
      --case test_decide_batch_nak_publish_failure_mixed \
      --case test_decide_tenant_isolation_during_concurrent_faults
done
```

**Success Criteria**:
- All 5 runs pass consistently
- No timeout-related failures
- No race condition failures
- Execution time remains stable (±20% variance acceptable)

### Step 2: Parallel Execution Verification

**Purpose**: Verify parallelism and synchronization work correctly

**Commands**:
```bash
# Run tests with verbose output to check parallelism
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
  --case test_ack_error_with_tenant_validation_fail_concurrent \
  --case test_batch_nak_publish_failure_mixed \
  --verbose
```

**Success Criteria**:
- Messages processed in parallel (verified via ETS synchronization)
- No deadlocks or hangs
- All spawned processes complete successfully
- ETS synchronization works correctly

### Step 3: CI Environment Verification

**Purpose**: Verify tests work in CI environment (may be slower than local)

**Commands**:
```bash
# Run in CI environment (GitHub Actions, Drone CI, GitLab CI)
# Tests should be part of regular CI pipeline
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
  --suite apps/otp/router/test/router_decide_consumer_SUITE
```

**Success Criteria**:
- Tests pass in CI environment
- No environment-specific failures
- Timeout values sufficient for CI delays
- No resource constraints (memory, CPU)

## Known Stability Considerations

### Timeout Values

**Current Default**: 2000ms (2 seconds) for `wait_for_metric/4`

**Rationale**:
- Sufficient for most telemetry events (typically fast)
- Accounts for CI/CD environment delays
- Balance between responsiveness and stability

**Adjustment Guidelines**:
- If tests are flaky in CI: Increase timeout to 3000-5000ms
- If tests are too slow: Verify telemetry events are emitted promptly
- Document timeout rationale in test comments

### Parallelism and Synchronization

**Approach**: Real parallelism using `spawn` with ETS synchronization

**Stability Measures**:
- ETS operations are atomic (thread-safe)
- Bounded waits using `test_helpers:wait_for_condition/2,3`
- Explicit cleanup of spawned processes via `exit(Pid, normal)`

**Edge Cases**:
- Telemetry events may have slight propagation delays
- ETS synchronization ensures all parallel operations complete
- Order-independent verification (metrics/logs may arrive in any order)

### Test Isolation

**Measures**:
- Each test uses unique message IDs
- ETS tables are test-specific (private tables)
- Mocks are cleaned up after each test
- No shared state between tests

## Troubleshooting Flaky Tests

### Symptom: Timeout Failures

**Possible Causes**:
- CI environment slower than local
- Telemetry events delayed
- Network delays in CI

**Fixes**:
1. Increase timeout values in `wait_for_metric/4` calls
2. Add retry logic for critical assertions
3. Verify telemetry handler setup is correct

### Symptom: Race Condition Failures

**Possible Causes**:
- Parallel operations not properly synchronized
- ETS synchronization incomplete
- Spawned processes not cleaned up

**Fixes**:
1. Add additional synchronization points
2. Verify ETS synchronization logic
3. Ensure all spawned processes are cleaned up

### Symptom: Order-Dependent Failures

**Possible Causes**:
- Tests depend on specific order of telemetry events
- Metrics/logs arrive in unexpected order

**Fixes**:
1. Make verification order-independent
2. Use `wait_for_condition` instead of fixed waits
3. Verify all expected events, not specific order

## CI Integration Recommendations

### Regular CI Runs

**Frequency**: Every commit/PR

**Tests**: All concurrent fault tests

**Configuration**:
- Timeout: 2000ms (default)
- Retries: 0 (tests should be stable)
- Parallel execution: Enabled (if supported)

### Periodic Stability Verification

**Frequency**: Weekly

**Tests**: Run all concurrent fault tests 5 times

**Purpose**: Catch regressions in test stability

**Commands**:
```bash
# Weekly stability check script
#!/bin/bash
for i in {1..5}; do
    echo "Stability run $i/5"
    rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
      --suite apps/otp/router/test/router_decide_consumer_SUITE || exit 1
done
echo "All stability runs passed"
```

### Stress Test Runs

**Frequency**: Nightly (optional)

**Tests**: `router_concurrent_faults_stress_SUITE`

**Purpose**: Verify long-running stability

**Configuration**:
- Duration: Configurable via `STRESS_TEST_DURATION_MS` (default: 30 seconds)
- Can be excluded from regular CI if too slow

## Success Metrics

### Target Metrics

- **Test Pass Rate**: > 99% (allows for rare infrastructure issues)
- **Flakiness Rate**: < 1% (intermittent failures)
- **Average Execution Time**: Stable (±20% variance)
- **CI Failure Rate**: < 5% (environment-specific issues)

### Monitoring

**Track**:
- Test pass/fail rate per test
- Execution time trends
- Flakiness indicators
- CI environment-specific failures

**Alert Thresholds**:
- Test failure rate > 10% → High priority
- Test flakiness > 5% → Medium priority
- Execution time increase > 50% → Investigate

## References

- **Test Documentation**: `CONCURRENT_FAULTS_TEST_DOCUMENTATION.md`
- **Stability Tracking**: `FAULT_INJECTION_TEST_STABILITY.md`
- **Test Scenarios**: `FAULT_INJECTION_TEST_SCENARIOS.md`
- **Test Helpers**: `apps/otp/router/test/test_helpers.erl`

## Change History

**v1.0 (2025-11-30)**:
- Initial CI stability documentation
- Verification process defined
- Troubleshooting guidance added
- CI integration recommendations added

