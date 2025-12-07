# Fault Injection Test Stability Tracking

## Purpose

This document tracks stability metrics for fault injection tests to ensure they remain reliable signals in CI/CD pipelines. Flaky or consistently failing tests are tracked here with root causes and fix priorities.

## Stability Metrics

### Test Suites

| Suite | Total Tests | Flaky Tests | Failed Tests | Last Review |
|-------|-------------|-------------|--------------|-------------|
| `router_result_consumer_SUITE` | 23 | 0 | 0 | 2025-11-30 |
| `router_decide_consumer_SUITE` | 23 | 0 | 0 | 2025-11-30 |
| `router_jetstream_fault_injection_SUITE` | 6 | 0 | 0 | 2025-11-30 |
| `router_concurrent_faults_stress_SUITE` | 3 | 0 | 0 | 2025-11-30 |

### Overall Statistics

- **Total fault injection tests**: 55 (52 regular + 3 stress)
- **Flaky tests**: 0 (0%)
- **Consistently failing tests**: 0 (0%)
- **Average execution time**: ~2-3 minutes (regular suites), ~30-60 seconds (stress suite)
- **Last comprehensive review**: 2025-11-30

## Flaky Test Tracking

### High Priority (Fix within 1 week)

**None currently identified.**

### Medium Priority (Fix within 1 month)

**None currently identified.**

### Low Priority (Fix when convenient)

**None currently identified.**

## Test Failure Tracking

### Recent Failures

**None in last 30 days.**

### Historical Failures

**None recorded.**

## Stability Issues

### Issue #1: [Issue Title]

**Status**: [Open/Investigating/Fixed]  
**Priority**: [High/Medium/Low]  
**Test**: `[test_name]` in `[suite_name]`  
**Failure Rate**: [percentage]  
**Flakiness**: [percentage]  
**First Observed**: [date]  
**Last Observed**: [date]  
**Root Cause**: [description or "TBD"]  
**Fix**: [description or "TBD"]  
**Notes**: [additional context]

## Observability-Linked Test Stability

### Purpose

Track stability of tests that are linked to observability metrics/alerts via `test_coverage` annotations. These tests form a critical bridge between alerts and fault injection scenarios, so their stability is especially important.

### Monitored Tests

Tests linked to observability (from `docs/PROMETHEUS_ALERTS.md` `test_coverage` annotations):

| Alert | Linked Tests | Suite | Last Check | Status |
|-------|--------------|-------|------------|--------|
| `RouterJetStreamRedeliveryHigh` | `test_nak_with_publish_failure_recovery` | `router_result_consumer_SUITE` | 2025-11-30 | ✅ Stable |
| `RouterJetStreamMaxDeliverExhausted` | `test_max_delivery_count_exhaustion` | `router_result_consumer_SUITE` | 2025-11-30 | ✅ Stable |

### Monitoring Notes

**When to check**:
- After changes to observability metrics/contracts
- After changes to alert definitions
- If alert fires in production and test doesn't catch the scenario
- During regular stability reviews

**What to watch for**:
- Tests start flaking after metric contract changes
- Tests fail after alert threshold changes
- Tests become outdated when observability implementation changes
- Missing test coverage when new alerts are added

### Recent Observations

**2025-11-30 - Initial Setup**:
- All observability-linked tests are stable (0 flaky, 0 failures)
- `test_coverage` annotations verified and accurate
- No issues identified

**Next review**: After next observability/metrics change or alert-related incident

## Monitoring Process

### Data Collection

**Sources**:
- CI/CD logs (GitHub Actions, Drone CI, GitLab CI)
- Common Test HTML reports
- Manual test runs

**Metrics collected**:
- Test pass/fail rate per test
- Test execution time
- Flakiness indicator (intermittent failures)
- Failure patterns

### Review Schedule

- **Weekly**: Review CI/CD logs for failures
- **Monthly**: Aggregate statistics and identify trends
- **Quarterly**: Comprehensive review and prioritization

### Action Thresholds

**High priority** (fix within 1 week):
- Test failure rate > 10%
- Test flakiness > 5%
- Test blocking CI pipeline

**Medium priority** (fix within 1 month):
- Test failure rate 5-10%
- Test flakiness 2-5%
- Test not blocking but causing noise

**Low priority** (fix when convenient):
- Test failure rate < 5%
- Test flakiness < 2%
- Test not blocking CI

## Fix Process

### When Test Becomes Flaky

1. **Identify root cause**:
   - Review test logs
   - Check for timing dependencies
   - Verify test isolation
   - Check for race conditions

2. **Fix test**:
   - Replace fixed sleeps with bounded waits
   - Improve test isolation
   - Fix race conditions
   - Update assertions if behavior changed

3. **Verify fix**:
   - Run test multiple times (10+ runs)
   - Verify no flakiness
   - Update this document

### Common Fix Patterns

**Timing issues**:
- Replace `timer:sleep(N)` with `test_helpers:wait_for_condition(Fun, Timeout)`
- Use explicit state checks instead of fixed delays

**Race conditions**:
- Add synchronization points
- Use ETS tables for coordination
- Wait for specific events instead of time

**Test isolation**:
- Ensure each test cleans up mocks
- Use unique message IDs per test
- Clear ETS tables between tests

## CI/CD Integration

### Monitoring Scripts

**Script**: `scripts/monitor_fault_test_stability.sh` (to be created)

**Functionality**:
- Parse CI/CD logs for fault injection test results
- Calculate failure rates and flakiness
- Generate stability report
- Alert on high-priority issues

### Reporting

**Weekly report** (automated):
- Test failure summary
- Flaky test list
- Priority issues

**Monthly report** (manual):
- Trend analysis
- Comprehensive statistics
- Action items

## Historical Data

### Q1 2025

- **Total tests**: 55 (52 regular + 3 stress)
- **Flaky tests**: 0
- **Failed tests**: 0
- **Average execution time**: ~2-3 minutes (regular), ~30-60 seconds (stress)
- **Issues fixed**: 0

## References

- `FAULT_INJECTION_MAINTENANCE_PROCESS.md`: Maintenance process documentation
- `FAULT_INJECTION_TEST_SCENARIOS.md`: Test scenarios documentation
- `FAULT_INJECTION_SMOKE_TESTS.md`: Smoke test set
- CI/CD logs: [links to CI/CD systems]

