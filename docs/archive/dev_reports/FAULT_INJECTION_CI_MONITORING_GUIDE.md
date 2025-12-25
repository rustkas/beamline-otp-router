# Fault Injection Tests - CI Monitoring Guide

**Date**: 2025-11-30  
**Purpose**: Guide for monitoring fault injection test stability and performance in CI

## Overview

This guide provides instructions for monitoring fault injection test stability, execution time, and failure rates in CI/CD pipelines.

## Monitoring Script

### Usage

```bash
cd apps/otp/router
./scripts/monitor_fault_injection_ci.sh
```

### Output

The script generates:
- `reports/fault-injection-ci/stability_summary.json` - Summary statistics
- `reports/fault-injection-ci/parsed_results.json` - Detailed results per run
- Console output with stability status

### Example Output

```
=== Fault Injection CI Monitoring ===
Suite: router_jetstream_fault_injection_SUITE
Report directory: reports/fault-injection-ci

Found 5 log directories
  Run 1: 8 passed, 0 failed, 0s
  Run 2: 8 passed, 0 failed, 0s
  Run 3: 8 passed, 0 failed, 0s
  Run 4: 8 passed, 0 failed, 0s
  Run 5: 8 passed, 0 failed, 0s

=== Stability Summary ===
Total runs analyzed: 5
Total passed: 40
Total failed: 0
Pass rate: 100.00%
Average duration: 45.2s
Status: STABLE

=== CI Duration Check ===
Average duration: 0.75 minutes
Threshold: 15 minutes
Duration within acceptable range
```

## Manual Monitoring

### Step 1: Check CI Job Status

1. Go to GitHub Actions tab
2. Select latest workflow run
3. Check "Erlang Router" job status
4. Look for "Common Test (router)" step

### Step 2: Review Test Logs

1. Download CT logs artifact
2. Open `_build/test/logs/index.html`
3. Find `router_jetstream_fault_injection_SUITE`
4. Review test results

### Step 3: Check Execution Time

1. In GitHub Actions, check job duration
2. Compare with previous runs
3. Note if duration increases significantly

### Step 4: Analyze Failures

If tests fail:
1. Check error messages in logs
2. Identify failing test cases
3. Check for timeout errors
4. Review test stability trends

## Success Criteria

### Stability Metrics

- **Pass Rate**: > 99% (allows for rare infrastructure issues)
- **Flakiness Rate**: < 1% (intermittent failures)
- **Average Execution Time**: Stable (±20% variance acceptable)

### Duration Thresholds

- **Acceptable**: < 5 minutes
- **Warning**: 5-15 minutes (consider separate job)
- **Critical**: > 15 minutes (create separate job)

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

## Weekly Monitoring Checklist

- [ ] Run monitoring script
- [ ] Check pass rate (should be > 99%)
- [ ] Check average duration (should be < 5 minutes)
- [ ] Review any failures
- [ ] Update stability report if needed

## Monthly Review

1. **Trend Analysis**:
   - Compare pass rates over time
   - Check duration trends
   - Identify patterns

2. **Action Items**:
   - Fix any high-priority issues
   - Adjust timeouts if needed
   - Consider separate CI job if duration > 15 minutes

3. **Documentation**:
   - Update stability report
   - Document any changes
   - Update thresholds if needed

## Troubleshooting

### High Failure Rate

**Symptoms**: Pass rate < 95%

**Possible Causes**:
- Timing issues (increase timeouts)
- Race conditions (fix synchronization)
- Test isolation problems (fix cleanup)

**Actions**:
1. Review test logs
2. Identify failing test cases
3. Check for timeout errors
4. Increase timeouts if needed
5. Fix race conditions

### Increasing Duration

**Symptoms**: Duration increases over time

**Possible Causes**:
- More test cases added
- Timeouts increased
- CI environment slower

**Actions**:
1. Check if duration exceeds 15 minutes
2. Consider separate CI job
3. Optimize test cases if possible

### Flaky Tests

**Symptoms**: Tests pass/fail intermittently

**Possible Causes**:
- Timing dependencies
- Race conditions
- Test isolation issues

**Actions**:
1. Run tests multiple times locally
2. Identify flaky test cases
3. Fix timing/race conditions
4. Improve test isolation

## Automated Monitoring

### GitHub Actions Integration

Add to `.github/workflows/ci.yml`:

```yaml
- name: Monitor Fault Injection Stability
  if: always()
  working-directory: apps/otp/router
  run: |
    ./scripts/monitor_fault_injection_ci.sh
    # Upload report as artifact
    # Check pass rate and alert if < 99%
```

### Weekly Report

Create scheduled workflow:

```yaml
on:
  schedule:
    - cron: '0 9 * * 1'  # Every Monday at 9 AM UTC

jobs:
  weekly_stability_report:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Generate Stability Report
        run: |
          cd apps/otp/router
          ./scripts/monitor_fault_injection_ci.sh
      - name: Upload Report
        uses: actions/upload-artifact@v4
        with:
          name: weekly_stability_report
          path: apps/otp/router/reports/fault-injection-ci
```

## References

- Monitoring script: `scripts/monitor_fault_injection_ci.sh`
- CI job guide: `docs/archive/dev/FAULT_INJECTION_CI_JOB_GUIDE.md`
- Test suite: `test/router_jetstream_fault_injection_SUITE.erl`
- Stability documentation: `test/FAULT_INJECTION_TEST_STABILITY.md`

