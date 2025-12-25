# Fault Injection Tests - Separate CI Job Guide

**Date**: 2025-11-30  
**Purpose**: Guide for creating a separate CI job for fault injection tests if they become too slow

## When to Create Separate CI Job

Create a separate CI job when:
- Test execution time exceeds **15 minutes** consistently
- Tests are blocking regular CI pipeline
- Tests need different environment (e.g., NATS server)
- Tests should run on schedule (nightly) vs every PR

## Implementation

### Option 1: Separate Job in Same Workflow

Add to `.github/workflows/ci.yml`:

```yaml
jobs:
  # ... existing jobs ...

  fault_injection_tests:
    name: Fault Injection Tests
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: [25.3, 26.2]
    steps:
      - uses: actions/checkout@v4

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: 'latest'

      - name: Cache rebar3
        uses: actions/cache@v4
        with:
          path: ~/.cache/rebar3
          key: rebar3-${{ runner.os }}-${{ matrix.otp }}-${{ hashFiles('**/rebar.lock') }}

      - name: Get deps
        working-directory: apps/otp/router
        run: rebar3 get-deps

      - name: Compile
        working-directory: apps/otp/router
        run: rebar3 compile

      - name: Fault Injection Tests
        working-directory: apps/otp/router
        run: |
          rebar3 ct --dir test --suite test/router_jetstream_fault_injection_SUITE
        timeout-minutes: 30

      - name: Upload CT logs
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: fault_injection_logs_${{ matrix.otp }}
          path: apps/otp/router/_build/test/logs
          retention-days: 7
```

### Option 2: Scheduled Job (Nightly)

Add to `.github/workflows/ci.yml`:

```yaml
on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  schedule:
    # Run nightly at 2 AM UTC
    - cron: '0 2 * * *'

jobs:
  fault_injection_nightly:
    name: Fault Injection Tests (Nightly)
    runs-on: ubuntu-latest
    if: github.event_name == 'schedule' || contains(github.event.pull_request.labels.*.name, 'test-fault-injection')
    # ... same steps as Option 1 ...
```

### Option 3: Conditional Job (Label-Based)

Add to `.github/workflows/ci.yml`:

```yaml
jobs:
  fault_injection_tests:
    name: Fault Injection Tests
    runs-on: ubuntu-latest
    if: contains(github.event.pull_request.labels.*.name, 'test-fault-injection') || github.event_name == 'push'
    # ... same steps as Option 1 ...
```

## Configuration Options

### Timeout

Set appropriate timeout based on test duration:

```yaml
- name: Fault Injection Tests
  timeout-minutes: 30  # Adjust based on actual test duration
```

### Parallel Execution

Run tests in parallel if supported:

```yaml
- name: Fault Injection Tests
  run: |
    rebar3 ct --dir test --suite test/router_jetstream_fault_injection_SUITE \
      --group integration_tests --parallel
```

### Environment Variables

Add environment variables if needed:

```yaml
env:
  NATS_URL: "nats://127.0.0.1:4222"
  NATS_MODE: "mock"  # or "real" for actual NATS server
```

## Monitoring

### Use Monitoring Script

After creating separate job, use monitoring script:

```bash
cd apps/otp/router
./scripts/monitor_fault_injection_ci.sh
```

### Check Job Duration

Monitor job duration in GitHub Actions:
1. Go to Actions tab
2. Select workflow run
3. Check "Fault Injection Tests" job duration
4. Compare with threshold (15 minutes)

### Stability Tracking

Track test stability over time:
- Weekly: Run monitoring script
- Monthly: Review stability trends
- Quarterly: Adjust timeouts if needed

## Rollback Plan

If separate job causes issues:

1. **Disable job temporarily**:
   ```yaml
   fault_injection_tests:
     if: false  # Disable temporarily
   ```

2. **Revert to main job**:
   - Remove separate job
   - Tests will run in main `erlang_router` job

3. **Adjust timeouts**:
   - Increase timeouts in test suite
   - Reduce test cases if needed

## Best Practices

1. **Start with Option 1** (separate job in same workflow)
   - Easiest to implement
   - Runs on every PR/commit
   - Easy to monitor

2. **Move to Option 2** (scheduled) if:
   - Tests are too slow for every PR
   - Tests don't need to block merges
   - Team prefers nightly runs

3. **Use Option 3** (label-based) if:
   - Tests are optional
   - Only run on specific PRs
   - Want manual control

## References

- CI workflow: `.github/workflows/ci.yml`
- Monitoring script: `scripts/monitor_fault_injection_ci.sh`
- Test suite: `test/router_jetstream_fault_injection_SUITE.erl`
- Activation report: `docs/archive/dev/FAULT_INJECTION_ACTIVATION_SUMMARY.md`

