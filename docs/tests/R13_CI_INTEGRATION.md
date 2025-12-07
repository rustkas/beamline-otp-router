# R13: CI/CD Integration Guide

**Date**: 2025-11-30  
**Purpose**: Guide for integrating R13 metrics tests into CI/CD pipelines

## Overview

R13 test suite (`router_metrics_under_faults_SUITE.erl`) should be integrated into CI/CD pipelines to ensure metrics correctness under faults and load.

## GitHub Actions Integration

### Option 1: Add to Existing Test Workflow

Add R13 tests to `.github/workflows/test.yml.template`:

```yaml
- name: Run R13 Metrics Under Faults Tests
  working-directory: apps/otp/router
  run: |
    rebar3 as test ct --suite router_metrics_under_faults_SUITE --logdir ct_logs/r13
  continue-on-error: false
  timeout-minutes: 30

- name: Upload R13 Test Logs
  uses: actions/upload-artifact@v4
  if: always()
  with:
    name: r13-metrics-tests-logs
    path: apps/otp/router/ct_logs/r13
    retention-days: 7
```

### Option 2: Separate Nightly Workflow

Create `.github/workflows/r13-metrics-nightly.yml` for nightly runs:

```yaml
name: R13 Metrics Under Faults (Nightly)

on:
  schedule:
    - cron: '0 2 * * *' # Daily at 2 AM UTC
  workflow_dispatch:

jobs:
  r13-metrics-tests:
    name: R13 Metrics Under Faults Tests
    runs-on: ubuntu-latest
    
    steps:
      - name: Checkout code
        uses: actions/checkout@v4
      
      - name: Set up Erlang/OTP
        uses: erlang/otp@v1
        with:
          otp-version: '26'
      
      - name: Install rebar3
        run: |
          curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3
          chmod +x /usr/local/bin/rebar3
      
      - name: Get dependencies
        working-directory: apps/otp
        run: rebar3 deps
      
      - name: Compile
        working-directory: apps/otp
        run: rebar3 compile
      
      - name: Run R13 Metrics Under Faults Tests
        working-directory: apps/otp/router
        run: |
          rebar3 as test ct --suite router_metrics_under_faults_SUITE --logdir ct_logs/r13
        timeout-minutes: 30
      
      - name: Upload Test Logs
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: r13-metrics-tests-logs
          path: apps/otp/router/ct_logs/r13
          retention-days: 7
      
      - name: Upload Test Results
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: r13-metrics-test-results
          path: apps/otp/router/ct_logs/r13/*.xml
          retention-days: 7
```

## Test Execution

### Run All R13 Tests

```bash
# From apps/otp/router directory
rebar3 as test ct --suite router_metrics_under_faults_SUITE
```

### Run Specific Test Group

```bash
# Aggregation tests only
rebar3 as test ct --suite router_metrics_under_faults_SUITE --group aggregation_tests

# Rate tests only
rebar3 as test ct --suite router_metrics_under_faults_SUITE --group rate_tests

# Cardinality tests only
rebar3 as test ct --suite router_metrics_under_faults_SUITE --group cardinality_tests

# Combined tests only
rebar3 as test ct --suite router_metrics_under_faults_SUITE --group combined_tests
```

### Run Individual Test

```bash
# Example: Test aggregation under partial failures
rebar3 as test ct --suite router_metrics_under_faults_SUITE --case test_aggregation_under_partial_failures
```

## Test Duration

### Expected Durations

- **Aggregation tests**: ~60-90 seconds total
- **Rate tests**: ~120-180 seconds total
- **Cardinality tests**: ~80-120 seconds total
- **Combined tests**: ~300-600 seconds total (5-10 minutes)

**Total suite duration**: ~10-15 minutes

### Timeout Configuration

Set appropriate timeouts in CI/CD:

- **Individual test**: 5 minutes
- **Test group**: 10 minutes
- **Full suite**: 30 minutes

## CI/CD Best Practices

### 1. Parallel Execution

Run test groups in parallel when possible:

```yaml
strategy:
  matrix:
    test-group: [aggregation_tests, rate_tests, cardinality_tests, combined_tests]
  
jobs:
  r13-metrics-${{ matrix.test-group }}:
    name: R13 ${{ matrix.test-group }}
    runs-on: ubuntu-latest
    steps:
      - name: Run R13 ${{ matrix.test-group }}
        run: |
          rebar3 as test ct --suite router_metrics_under_faults_SUITE --group ${{ matrix.test-group }}
```

### 2. Artifact Collection

Always collect test logs and results:

```yaml
- name: Upload Test Logs
  uses: actions/upload-artifact@v4
  if: always()
  with:
    name: r13-${{ matrix.test-group }}-logs
    path: apps/otp/router/ct_logs/r13
    retention-days: 7
```

### 3. Failure Notifications

Configure notifications for test failures:

```yaml
- name: Notify on Failure
  if: failure()
  uses: actions/github-script@v7
  with:
    script: |
      github.rest.issues.createComment({
        issue_number: context.issue.number,
        owner: context.repo.owner,
        repo: context.repo.repo,
        body: '❌ R13 Metrics Under Faults tests failed. Check logs for details.'
      })
```

### 4. Test Result Reporting

Generate JUnit XML reports for better CI integration:

```yaml
- name: Generate Test Report
  run: |
    # Convert CT logs to JUnit XML (if available)
    # Or use rebar3 ct hooks for XML generation
```

## Local Testing Before CI

### Pre-CI Checklist

Before pushing to CI, test locally:

```bash
# 1. Compile
cd apps/otp && rebar3 compile

# 2. Run R13 tests
cd router && rebar3 as test ct --suite router_metrics_under_faults_SUITE

# 3. Check logs
ls -la ct_logs/r13/

# 4. Verify all tests pass
grep -r "TEST COMPLETE" ct_logs/r13/
```

### Common Issues

1. **Application not starting**: Check `init_per_suite` configuration
2. **Metrics not collected**: Verify `router_metrics:ensure()` is called
3. **Fault injection not working**: Check `router_nats_fault_injection` module
4. **Timeout errors**: Increase wait times or reduce load

## Monitoring and Alerts

### Test Success Rate

Monitor test success rate over time:
- Track failures per test group
- Alert on increasing failure rate
- Track flaky tests

### Performance Trends

Monitor test execution time:
- Alert on significant increases
- Track resource usage
- Optimize slow tests

## References

- **Test Suite**: `router_metrics_under_faults_SUITE.erl`
- **Specification**: `R13_METRICS_UNDER_FAULTS_SPEC.md`
- **README**: `R13_METRICS_UNDER_FAULTS_README.md`
- **Implementation Plan**: `R13_IMPLEMENTATION_PLAN.md`

