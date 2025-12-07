# Stress Run Guide for Concurrent Faults Tests

## Overview

Stress-run scripts are used to detect flaky tests by running test suites multiple times. This guide explains how to use stress-run scripts for `router_concurrent_faults_SUITE`.

## Purpose

**Flaky Test Detection**: Run test suites multiple times to identify non-deterministic behavior:
- Timing-dependent failures
- Race conditions
- Resource leaks that manifest over time
- Non-deterministic fault injection behavior

**Backoff Timing Verification**: Verify that backoff delays are correctly applied during retry attempts.

## Usage

### Bash (Linux/macOS/WSL)

```bash
cd apps/otp/router
./scripts/stress_run_concurrent_faults.sh [iterations] [suite_name]
```

**Examples**:
```bash
# Run 10 iterations (default)
./scripts/stress_run_concurrent_faults.sh

# Run 20 iterations
./scripts/stress_run_concurrent_faults.sh 20

# Run specific suite 50 times
./scripts/stress_run_concurrent_faults.sh 50 router_concurrent_faults_SUITE
```

### PowerShell (Windows)

```powershell
cd apps/otp/router
.\scripts\stress_run_concurrent_faults.ps1 [iterations] [suite_name]
```

**Examples**:
```powershell
# Run 10 iterations (default)
.\scripts\stress_run_concurrent_faults.ps1

# Run 20 iterations
.\scripts\stress_run_concurrent_faults.ps1 -Iterations 20

# Run specific suite 50 times
.\scripts\stress_run_concurrent_faults.ps1 -Iterations 50 -SuiteName router_concurrent_faults_SUITE
```

## Output

### During Execution

```
=== Stress Run: router_concurrent_faults_SUITE ===
Iterations: 10
Suite: router_concurrent_faults_SUITE

[1/10] Running router_concurrent_faults_SUITE...
✓ Passed (45s)
[2/10] Running router_concurrent_faults_SUITE...
✓ Passed (43s)
...
```

### Final Summary

```
=== Stress Run Results ===
Total iterations: 10
Passed: 10
Failed: 0
Success rate: 100%
Total time: 450s
Average time per run: 45s
Results directory: stress_run_results_20250127_143022

✓ All iterations passed - no flaky tests detected
```

## Results Directory

Each stress run creates a results directory with:
- `run_N/` - Common Test logs for each iteration
- `run_N.log` - Full output log for each iteration

**Example structure**:
```
stress_run_results_20250127_143022/
├── run_1/
│   ├── index.html
│   └── ...
├── run_1.log
├── run_2/
│   └── ...
├── run_2.log
└── ...
```

## Interpreting Results

### All Passed (100% success rate)

✅ **No flaky tests detected** - Suite is stable and deterministic.

### Some Failed (< 100% success rate)

⚠️ **Potential flaky tests** - Review failed iterations:
1. Check logs in `stress_run_results_*/run_N.log`
2. Look for timing-dependent failures
3. Check for race conditions
4. Verify resource cleanup between iterations

### Common Flaky Patterns

**Timing-dependent failures**:
- Tests that rely on fixed `timer:sleep` values
- Race conditions between processes
- Network timeout assumptions

**Resource leaks**:
- ETS tables not cleaned up
- Processes not properly terminated
- Metrics accumulating across iterations

**Non-deterministic behavior**:
- Random number generation without seed
- Unordered map/list operations
- Concurrent operations without synchronization

## Backoff Timing Tests

### Purpose

Verify that backoff delays are correctly applied during retry attempts:
- Exponential backoff progression
- Linear backoff progression
- Jitter bounds
- Actual timing during retry attempts

### Test Coverage

1. **`test_backoff_exponential_timing`**: Verifies exponential backoff formula
2. **`test_backoff_linear_timing`**: Verifies linear backoff formula
3. **`test_backoff_jitter_bounds`**: Verifies jitter is within bounds
4. **`test_backoff_applied_during_retry`**: Integration test for actual retry timing

### Expected Behavior

**Exponential Backoff**:
- Attempt 1: ~100ms (base)
- Attempt 2: ~200ms (base * 2)
- Attempt 3: ~400ms (base * 4)
- Attempt 4: ~800ms (base * 8)
- Jitter: ±10% of base delay

**Linear Backoff**:
- Attempt 1: ~100ms (base * 1)
- Attempt 2: ~200ms (base * 2)
- Attempt 3: ~300ms (base * 3)
- Attempt 4: ~400ms (base * 4)
- Jitter: ±10% of base delay

## Best Practices

### For CI/CD

1. **Run stress tests periodically** (not on every commit):
   ```bash
   # Weekly stress run
   ./scripts/stress_run_concurrent_faults.sh 50
   ```

2. **Use reasonable iteration count**:
   - Development: 10-20 iterations
   - CI/CD: 50-100 iterations
   - Release validation: 100+ iterations

3. **Monitor success rate trends**:
   - Track success rate over time
   - Investigate if success rate drops below 95%

### For Development

1. **Run stress tests before committing**:
   ```bash
   ./scripts/stress_run_concurrent_faults.sh 10
   ```

2. **Investigate failures immediately**:
   - Don't ignore flaky tests
   - Fix root cause, not symptoms

3. **Document known flaky tests**:
   - Add comments in test code
   - Document in test README
   - Create issues for tracking

## Troubleshooting

### Tests Fail Intermittently

**Possible causes**:
- Timing assumptions too strict
- Race conditions
- Resource leaks
- Non-deterministic fault injection

**Solutions**:
- Use bounded waits instead of fixed sleeps
- Add synchronization for concurrent operations
- Ensure proper cleanup in `end_per_testcase/2`
- Use deterministic fault injection (no random behavior)

### Stress Run Takes Too Long

**Optimization strategies**:
- Reduce iteration count for quick checks
- Run specific test groups instead of full suite
- Use parallel execution (`rebar3 ct -j 4`)

### Results Directory Grows Large

**Cleanup**:
```bash
# Remove old stress run results
rm -rf stress_run_results_*
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
- name: Stress Run Concurrent Faults Tests
  run: |
    cd apps/otp/router
    ./scripts/stress_run_concurrent_faults.sh 20
  continue-on-error: true  # Don't fail CI on flaky tests, but report them
```

### Scheduled Runs

```yaml
on:
  schedule:
    - cron: '0 2 * * 0'  # Weekly on Sunday at 2 AM
```

## Related Documentation

- `apps/otp/router/test/FAULT_INJECTION_TEST_CRITERIA.md` - Test pass criteria
- `apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md` - Test scenarios
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - NATS resilience documentation

## Exit Codes

- `0` - All iterations passed (no flaky tests)
- `1` - At least one iteration failed (potential flaky tests)

