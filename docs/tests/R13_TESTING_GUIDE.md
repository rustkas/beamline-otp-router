# R13: Testing Guide

**Date**: 2025-11-30  
**Purpose**: Step-by-step guide for testing R13 implementation

## Quick Start

### Prerequisites

1. **Erlang/OTP 26** installed
2. **rebar3** installed and in PATH
3. **Dependencies** installed: `cd apps/otp && rebar3 deps`

### Run All Tests

```bash
# Using helper script (recommended)
cd apps/otp/router/test
./run_r13_tests.sh

# Or directly with rebar3
cd apps/otp/router
rebar3 as test ct --suite router_metrics_under_faults_SUITE
```

### Run Specific Test Group

```bash
# Aggregation tests only
./run_r13_tests.sh aggregation

# Rate tests only
./run_r13_tests.sh rate

# Cardinality tests only
./run_r13_tests.sh cardinality

# Combined tests only
./run_r13_tests.sh combined
```

## Step-by-Step Testing

### Step 1: Compile

```bash
cd apps/otp
rebar3 compile
```

**Expected**: Compilation succeeds without errors.

### Step 2: Run Quick Test

Start with a single test to verify setup:

```bash
cd apps/otp/router
rebar3 as test ct --suite router_metrics_under_faults_SUITE --case test_aggregation_under_partial_failures
```

**Expected**: Test passes or shows specific errors (not compilation errors).

### Step 3: Run Test Group

Run one test group to verify functionality:

```bash
rebar3 as test ct --suite router_metrics_under_faults_SUITE --group aggregation_tests
```

**Expected**: All 5 aggregation tests pass.

### Step 4: Run Full Suite

Run all tests:

```bash
rebar3 as test ct --suite router_metrics_under_faults_SUITE --logdir ct_logs/r13
```

**Expected Duration**: ~10-15 minutes

## Troubleshooting

### Issue: Application Not Starting

**Symptoms**: Tests fail with "Failed to start beamline_router"

**Solutions**:
1. Check Erlang/OTP version: `erl -version`
2. Verify dependencies: `rebar3 deps`
3. Check for port conflicts
4. Review application configuration

### Issue: Metrics Not Collected

**Symptoms**: All metrics are zero or missing

**Solutions**:
1. Verify `router_metrics:ensure()` is called
2. Check ETS table: `ets:info(router_metrics)`
3. Verify messages are being sent
4. Check fault injection is working

### Issue: Fault Injection Not Working

**Symptoms**: Tests pass but no failures are injected

**Solutions**:
1. Verify `router_nats_fault_injection:clear_all_faults()` is called
2. Check fault injection API usage
3. Verify NATS mock mode is enabled
4. Review fault injection module

### Issue: Timeout Errors

**Symptoms**: Tests timeout before completion

**Solutions**:
1. Increase wait times in tests
2. Reduce load in test scenarios
3. Check system resources
4. Review test duration expectations

### Issue: Tolerance Failures

**Symptoms**: Tests fail with tolerance exceeded

**Solutions**:
1. Run tests multiple times to establish baseline
2. Adjust tolerance values in test code
3. Check for timing issues
4. Verify load generation is correct

## Test Results Analysis

### Check Test Logs

```bash
# View test logs
ls -la ct_logs/r13/

# View specific test log
cat ct_logs/r13/router_metrics_under_faults_SUITE.log
```

### Check Metrics

During test execution, you can check metrics:

```erlang
%% In Erlang shell
1> router_metrics:ensure().
2> ets:tab2list(router_metrics).
```

### Verify Aggregation

Check that metrics match expected values:

```bash
# In test logs, look for:
# "Aggregation: Initial=X, Final=Y, Delta=Z, Expected=W, Tolerance=T"
```

### Verify Rate

Check rate calculations:

```bash
# In test logs, look for:
# "Rate: Observed=X.XX RPS, Expected=Y RPS, Min=Z.ZZ, Max=W.WW"
```

### Verify Cardinality

Check label cardinality:

```bash
# In test logs, look for:
# "Cardinality: Observed=X, MaxExpected=Y, MaxAllowed=Z"
```

## Tuning Tolerance Values

### Process

1. **Run tests 5-10 times** to establish baseline
2. **Collect actual values** from test logs
3. **Calculate variance** (min, max, average)
4. **Set tolerance** to 2-3x standard deviation
5. **Update test code** with new tolerance values

### Example

If aggregation test shows:
- Expected: 1000 requests
- Actual deltas: 980, 995, 1005, 990, 1002
- Average: 994.4
- Std dev: ~9.5
- Tolerance: 30 (3x std dev)

Update test:
```erlang
verify_aggregation(InitialMetrics, FinalMetrics, #{
    total_requests => TotalRequests,
    tolerance => 30  %% Updated from 50
}),
```

## Performance Optimization

### If Tests Are Too Slow

1. **Reduce load** in test scenarios
2. **Reduce wait times** (if stable)
3. **Run tests in parallel** (if possible)
4. **Optimize helper functions**

### If Tests Are Too Fast

1. **Increase load** for better coverage
2. **Add more scenarios**
3. **Extend duration** for prolonged tests

## CI/CD Integration

### Pre-CI Checklist

- [ ] All tests pass locally
- [ ] Tolerance values tuned
- [ ] No compilation errors
- [ ] No runtime errors
- [ ] Test logs reviewed

### Activate CI/CD

Follow instructions in `R13_CI_INTEGRATION.md`

## Next Steps After Testing

1. **Document Results**: Update README with actual test results
2. **Fix Issues**: Address any failures or warnings
3. **Tune Tolerances**: Adjust based on real results
4. **Activate CI/CD**: Integrate into GitHub Actions
5. **Monitor**: Track test success rate over time

## References

- **Test Suite**: `router_metrics_under_faults_SUITE.erl`
- **Specification**: `R13_METRICS_UNDER_FAULTS_SPEC.md`
- **README**: `R13_METRICS_UNDER_FAULTS_README.md`
- **CI/CD Guide**: `R13_CI_INTEGRATION.md`
- **Completion Summary**: `R13_COMPLETION_SUMMARY.md`

