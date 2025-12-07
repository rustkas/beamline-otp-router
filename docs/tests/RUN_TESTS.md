# Running NATS Connection Resilience Tests

## Quick Start

```bash
cd apps/otp/router

# Run all NATS connection tests
rebar3 ct --suite test/router_nats_connection_failure_SUITE
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE
rebar3 ct --suite test/router_nats_integration_SUITE
rebar3 ct --suite test/router_nats_performance_SUITE
rebar3 ct --suite test/router_recovery_state_integrity_SUITE

# Run all NATS tests together
rebar3 ct --suite test/router_nats_connection_failure_SUITE test/router_jetstream_fault_injection_SUITE test/router_nats_integration_SUITE test/router_nats_performance_SUITE test/router_recovery_state_integrity_SUITE
```

## Test Suites

### router_nats_connection_failure_SUITE

**Purpose**: Tests router_nats behavior during NATS/JetStream failures.

**Tests**: 22 tests covering:
- Basic failure handling (6 tests)
- Short-term connection drops (2 tests)
- Long-term connection issues (2 tests)
- JetStream-specific errors (5 tests)
- Router workflows during failure (4 tests)
- Recovery behavior (3 tests)

**Status**: ✅ **Stable** (recommended for PR pipeline)

**Run**:
```bash
rebar3 ct --suite test/router_nats_connection_failure_SUITE
```

**Expected Duration**: ~30-60 seconds

**CI Recommendation**: Include in standard PR pipeline

### router_nats_publish_failure_SUITE

**Purpose**: Tests router_nats behavior when publish and publish_with_ack operations fail.

**Tests**: 23 tests covering:
- Publish failure scenarios (8 tests: all error types × 2 modes)
- Publish_with_ack failure scenarios (8 tests: all error types × 2 modes)
- msg_id behavior (3 tests: stub IDs, no duplicates, uniqueness)
- Metrics behavior (4 tests: increments, queue, retry)

**Status**: ✅ **Stable** (recommended for PR pipeline)

**Run**:
```bash
rebar3 ct --suite test/router_nats_publish_failure_SUITE
```

**Expected Duration**: ~20-40 seconds

**CI Recommendation**: Include in standard PR pipeline

**Stability Validation**:
```bash
# Run suite multiple times to verify stability (burn-in before merge)
bash scripts/validate_publish_failure_tests.sh 10
```

**See**: `test/router_nats_publish_failure_SUITE.md` for detailed documentation

### router_jetstream_fault_injection_SUITE

**Purpose**: Tests router behavior under controlled fault injection scenarios.

**Tests**: 15 tests covering:
- Connection fault injection (3 tests)
- Publish fault injection (4 tests)
- ACK/NAK fault injection (3 tests)
- Subscribe fault injection (2 tests)
- Combined fault scenarios (3 tests)

**Status**: ✅ **Stable** (recommended for PR pipeline)

**Run**:
```bash
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE
```

**Expected Duration**: ~20-40 seconds

**CI Recommendation**: Include in standard PR pipeline

### router_nats_integration_SUITE

**Purpose**: Verifies router_nats doesn't break existing use-cases.

**Tests**: 10 tests covering:
- Normal operation (3 tests)
- Fail-open mode (3 tests)
- Long failures and recovery (3 tests)
- Supervisor compatibility (1 test)

**Status**: ✅ **Stable** (recommended for PR pipeline)

**Run**:
```bash
rebar3 ct --suite test/router_nats_integration_SUITE
```

**Expected Duration**: ~15-30 seconds

**CI Recommendation**: Include in standard PR pipeline

### router_nats_performance_SUITE

**Purpose**: Evaluates queue and backoff performance.

**Tests**: 7 tests covering:
- Queue performance (3 tests)
- Backoff performance (2 tests)
- Retry performance (2 tests)

**Status**: ⚠️ **Slow** (recommended for nightly/extended pipeline)

**Run**:
```bash
rebar3 ct --suite test/router_nats_performance_SUITE
```

**Expected Duration**: ~10-20 seconds (may vary with system load)

**CI Recommendation**: Include in nightly/extended pipeline only (not in standard PR pipeline)

## Test Status Summary

| Suite | Status | CI Pipeline | Duration | Notes |
|-------|--------|-------------|----------|-------|
| `router_nats_connection_failure_SUITE` | ✅ Stable | PR (standard) | ~30-60s | Recommended for all PRs |
| `router_jetstream_fault_injection_SUITE` | ✅ Stable | PR (standard) | ~20-40s | Recommended for all PRs |
| `router_nats_integration_SUITE` | ✅ Stable | PR (standard) | ~15-30s | Recommended for all PRs |
| `router_recovery_state_integrity_SUITE` | ✅ Stable | PR (standard) | ~30-60s | Recommended for all PRs |
| `router_nats_publish_failure_SUITE` | ✅ Stable | PR (standard) | ~20-40s | Recommended for all PRs |
| `router_nats_performance_SUITE` | ⚠️ Slow | Nightly/Extended | ~10-20s | May vary with system load |

### router_recovery_state_integrity_SUITE

**Purpose**: Tests router recovery state integrity after NATS/JetStream failures.

**Tests**: 11 tests covering:
- Connection loss recovery (3 tests)
- ACK/NAK errors recovery (3 tests)
- NATS/JetStream restart recovery (3 tests)
- Multiple fault cycles (2 tests)
- Publish errors recovery (2 tests)

**Status**: ✅ **Stable** (recommended for PR pipeline)

**Run**:
```bash
rebar3 ct --suite test/router_recovery_state_integrity_SUITE
```

**Expected Duration**: ~30-60 seconds

**CI Recommendation**: Include in standard PR pipeline

**Total Tests**: 88 tests (22 + 15 + 10 + 7 + 11 + 23)

**Note**: `router_nats_publish_failure_SUITE` (23 tests) is included in the total above.

**Standard PR Pipeline**: 81 tests (~115-230 seconds)
**Extended/Nightly Pipeline**: 88 tests (~125-250 seconds)

## CI/CD Integration

### GitLab CI

**Standard PR Pipeline** (`.gitlab-ci.yml`):
- `router_nats_connection_failure_SUITE` ✅
- `router_jetstream_fault_injection_SUITE` ✅
- `router_nats_integration_SUITE` ✅
- `router_recovery_state_integrity_SUITE` ✅

**Nightly/Extended Pipeline**:
- All standard suites ✅
- `router_nats_performance_SUITE` ⚠️

### Drone CI

**Standard PR Pipeline** (`.drone.yml`):
- `router_nats_connection_failure_SUITE` ✅
- `router_jetstream_fault_injection_SUITE` ✅
- `router_nats_integration_SUITE` ✅
- `router_recovery_state_integrity_SUITE` ✅

**Nightly/Extended Pipeline**:
- All standard suites ✅
- `router_nats_performance_SUITE` ⚠️

### GitHub Actions

Tests are automatically run in CI:

```yaml
# .github/workflows/validate.yml
- name: Run NATS connection tests (standard)
  run: |
    cd apps/otp/router
    rebar3 ct --suite test/router_nats_connection_failure_SUITE
    rebar3 ct --suite test/router_jetstream_fault_injection_SUITE
    rebar3 ct --suite test/router_nats_integration_SUITE
    rebar3 ct --suite test/router_recovery_state_integrity_SUITE

- name: Run NATS performance tests (nightly)
  if: github.event_name == 'schedule' || github.event_name == 'workflow_dispatch'
  run: |
    cd apps/otp/router
    rebar3 ct --suite test/router_nats_performance_SUITE
```

### Local Pre-Commit

Run tests before committing:

```bash
# Quick test (connection failure only)
cd apps/otp/router && rebar3 ct --suite test/router_nats_connection_failure_SUITE

# Standard test (recommended for PR)
cd apps/otp/router && rebar3 ct --suite test/router_nats_connection_failure_SUITE test/router_jetstream_fault_injection_SUITE test/router_nats_integration_SUITE test/router_recovery_state_integrity_SUITE

# Full test (all suites including performance)
cd apps/otp/router && rebar3 ct --suite test/router_nats_connection_failure_SUITE test/router_jetstream_fault_injection_SUITE test/router_nats_integration_SUITE test/router_recovery_state_integrity_SUITE test/router_nats_performance_SUITE
```

## Troubleshooting

### Flaky Tests

If tests are flaky (especially timeout/backoff tests):

1. **Check timing**: Increase timeouts in test if needed
2. **Check logs**: Review test logs for timing issues
3. **Run multiple times**: `for i in {1..5}; do rebar3 ct --suite test/router_nats_connection_failure_SUITE; done`

**Stability Validation Scripts**:

For `router_nats_publish_failure_SUITE`, use dedicated stability validation scripts:

```bash
# Bash (Linux/macOS/WSL)
bash scripts/validate_publish_failure_tests.sh 10

# PowerShell (Windows)
.\scripts\validate_publish_failure_tests.ps1 -Iterations 10
```

**When to run**:
- Before merging large changes to `router_nats`
- After modifying test suite
- When investigating flaky behavior

**Expected result**: All iterations pass (0 failures)

### Test Failures

**Common Issues**:

1. **Process not alive**: Check supervisor restart policy
2. **Metrics not incremented**: Check meck expectations
3. **Logs not found**: Check log filtering patterns

**Debug**:
```bash
# Run with verbose output
rebar3 ct --suite test/router_nats_connection_failure_SUITE --verbose

# Run specific test
rebar3 ct --suite test/router_nats_connection_failure_SUITE --case test_router_nats_no_crash_on_connection_loss
```

### Performance Issues

If tests are slow:

1. **Check queue size**: Reduce `nats_max_pending_operations` in test config
2. **Check backoff delays**: Reduce `nats_reconnect_delay_ms` in test config
3. **Check timeouts**: Reduce timeout values in tests

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete documentation
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Publish failure behavior specification
- `apps/otp/router/test/FAULT_INJECTION_TEST_CRITERIA.md` - Test criteria
- `apps/otp/router/test/router_nats_connection_failure_SUITE.md` - Connection failure tests
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.md` - Fault injection tests
- `apps/otp/router/test/router_nats_publish_failure_SUITE.md` - Publish failure tests
- `apps/otp/router/test/router_recovery_state_integrity_SUITE.md` - Recovery state integrity tests

