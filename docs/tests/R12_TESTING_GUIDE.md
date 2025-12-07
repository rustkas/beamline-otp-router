# R12: Network Partition Scenarios - Testing Guide

**Date**: 2025-11-30  
**Purpose**: Step-by-step guide for running R12 network partition tests and interpreting results

## Quick Start

### Prerequisites

1. **Erlang/OTP 26** installed
2. **rebar3** installed and in PATH
3. **Dependencies** installed: `cd apps/otp && rebar3 deps`
4. **Network tools** (optional, for real mode): `iptables`, `tc` (Linux), or use mock mode

### Run All R12 Tests

```bash
# From application directory (recommended)
cd apps/otp/router

# Run all network partition tests
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12

# With verbose output
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12 --verbose
```

**Expected Duration**: ~30-45 minutes (all 21 tests)

## Test Execution Commands

### Run All Test Groups

```bash
cd apps/otp/router

# All tests (4 groups, 21 tests total)
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12
```

### Run Specific Test Group

```bash
cd apps/otp/router

# Single-instance tests (9 tests)
rebar3 as test ct --suite router_network_partition_SUITE --group single_instance_tests --logdir ct_logs/r12/single_instance

# Multi-instance tests (11 tests)
rebar3 as test ct --suite router_network_partition_SUITE --group multi_instance_tests --logdir ct_logs/r12/multi_instance

# Service-broker tests (3 tests)
rebar3 as test ct --suite router_network_partition_SUITE --group service_broker_tests --logdir ct_logs/r12/service_broker

# Flapping network tests (3 tests)
rebar3 as test ct --suite router_network_partition_SUITE --group flapping_network_tests --logdir ct_logs/r12/flapping
```

### Run Specific Test Case

```bash
cd apps/otp/router

# Single-instance JetStream partition (short)
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_short --logdir ct_logs/r12

# Single-instance JetStream partition (long)
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_long --logdir ct_logs/r12

# Multi-instance split-brain
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_split_brain --logdir ct_logs/r12

# Service-broker partition
rebar3 as test ct --suite router_network_partition_SUITE --case test_service_broker_partition --logdir ct_logs/r12

# Flapping network stability
rebar3 as test ct --suite router_network_partition_SUITE --case test_flapping_network_stability --logdir ct_logs/r12
```

### Run with Parallel Execution

```bash
cd apps/otp/router

# Run with 4 parallel workers (faster execution)
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12 -j 4
```

**Note**: Some tests may require sequential execution due to shared state. If tests fail with parallel execution, run without `-j` flag.

## Fault Injection Scripts

### Bash Script (Linux, macOS, WSL)

**Location**: `apps/otp/router/test/scripts/r12_network_partition_fault_injection.sh`

#### Mock Mode (Default, No Root Required)

```bash
cd apps/otp/router/test/scripts

# Create single-instance partition (Router -> NATS)
./r12_network_partition_fault_injection.sh create single_instance router nats --action drop

# List active partitions
./r12_network_partition_fault_injection.sh list

# Check partition status
./r12_network_partition_fault_injection.sh status <partition_id>

# Remove partition
./r12_network_partition_fault_injection.sh remove <partition_id>

# Heal partition (gradual recovery)
./r12_network_partition_fault_injection.sh heal <partition_id>

# Simulate flapping network
./r12_network_partition_fault_injection.sh flapping router nats 2000 10000
# (2000ms interval, 10000ms duration)
```

#### Real Mode (Requires Root, Uses iptables/tc)

```bash
# Enable real mode
export NETWORK_PARTITION_REAL=true

# Run with sudo
sudo ./r12_network_partition_fault_injection.sh create single_instance router nats --action drop

# Create partition with delay
sudo ./r12_network_partition_fault_injection.sh create single_instance router nats --action delay --delay_ms 5000

# Create partition with packet loss
sudo ./r12_network_partition_fault_injection.sh create single_instance router nats --action loss --loss_percent 50
```

### PowerShell Script (Windows)

**Location**: `apps/otp/router/test/scripts/r12_network_partition_fault_injection.ps1`

```powershell
cd apps/otp/router/test/scripts

# Create single-instance partition
.\r12_network_partition_fault_injection.ps1 create single_instance router nats -Action drop

# List active partitions
.\r12_network_partition_fault_injection.ps1 list

# Remove partition
.\r12_network_partition_fault_injection.ps1 remove <partition_id>

# Simulate flapping network
.\r12_network_partition_fault_injection.ps1 flapping router nats -IntervalMs 2000 -DurationMs 10000
```

## Test Output Interpretation

### Successful Test Run

```
=== Test Results ===
router_network_partition_SUITE:test_single_instance_jetstream_partition_short...ok
router_network_partition_SUITE:test_single_instance_jetstream_partition_long...ok
...
=== Summary ===
Total: 21
Passed: 21
Failed: 0
Skipped: 0
```

### Failed Test Run

```
=== Test Results ===
router_network_partition_SUITE:test_single_instance_jetstream_partition_short...ok
router_network_partition_SUITE:test_single_instance_jetstream_partition_long...FAILED
...
=== Summary ===
Total: 21
Passed: 20
Failed: 1
Skipped: 0
```

**Check logs**: `ct_logs/r12/test_server.html` or `ct_logs/r12/latest_run/index.html`

### Common Test Log Locations

- **HTML Report**: `ct_logs/r12/test_server.html` or `ct_logs/r12/latest_run/index.html`
- **Text Logs**: `ct_logs/r12/latest_run/*.log`
- **SASL Logs**: `ct_logs/r12/latest_run/sasl/*.log`
- **Coverage**: `ct_logs/r12/cover/index.html` (if coverage enabled)

## Interpreting Test Results

### Metrics Verification

All tests verify metrics at three stages:
1. **Initial Metrics**: Before partition (baseline)
2. **Partition Metrics**: During partition (fault state)
3. **Final Metrics**: After recovery (post-recovery state)

**Key Metrics to Check**:
- `router_nats_connection_failures_total` - Should increase during partition
- `router_nats_reconnect_attempts_total` - Should show retry attempts
- `router_circuit_breaker_state` - Should change from `closed` to `open` during partition
- `router_network_partition_duration_seconds` - Should match partition duration
- `router_network_partition_recovery_time_seconds` - Should show recovery time

### Contract Invariants Verification

All tests verify:
1. **Fail-Open Behavior**: Process remains alive (no crashes)
2. **MaxDeliver Semantics**: Messages either deliver or exhaust MaxDeliver
3. **Redelivery Limits**: Redelivery count within bounds
4. **Metrics Correctness**: Metrics reflect actual system behavior
5. **Data Guarantees**: No duplicates, losses, or inconsistencies
6. **Recovery Behavior**: Correct recovery after partition resolution

### Expected Log Messages

**During Partition**:
```
[WARN] Network partition detected: Router -> NATS JetStream
[ERROR] Connection lost to NATS JetStream broker
[INFO] Retry attempt 1/3 with exponential backoff (100ms)
[WARN] Circuit breaker state changed: closed -> open
```

**During Recovery**:
```
[INFO] Network partition resolved: Router -> NATS JetStream
[INFO] Reconnecting to NATS JetStream broker
[INFO] Connection restored successfully
[INFO] Circuit breaker closed, normal operation resumed
```

## Troubleshooting

### Issue: Tests Fail with "Connection Refused"

**Symptoms**: Tests fail immediately with connection errors

**Solutions**:
1. Verify NATS is running (if using real NATS): `docker ps | grep nats`
2. Check mock mode is enabled: Tests use `router_nats_fault_injection` (mock)
3. Verify application configuration: `apps/otp/router/config/test.config`

### Issue: Tests Timeout

**Symptoms**: Tests hang and eventually timeout

**Solutions**:
1. Check for stuck processes: `ps aux | grep beam`
2. Increase timeout in test configuration
3. Run tests sequentially (remove `-j` flag)
4. Check logs for deadlock indicators

### Issue: Metrics Not Collected

**Symptoms**: All metrics are zero or missing

**Solutions**:
1. Verify `router_metrics:ensure()` is called in test setup
2. Check ETS table: `ets:info(router_metrics)`
3. Verify telemetry handlers are attached
4. Check test configuration for metrics collection

### Issue: Resource Leaks Detected

**Symptoms**: Tests fail with "Resource leak detected" errors

**Solutions**:
1. Check memory growth: Should be < 10MB per test
2. Check process count: Should be stable
3. Verify ETS table cleanup: Check for orphaned entries
4. Review test cleanup code in `end_per_testcase/2`

## Generating Test Report

### Using Template

1. Copy template: `cp R12_RESULTS_REPORT_TEMPLATE.md R12_RESULTS_REPORT_$(date +%Y%m%d).md`
2. Fill in test results from logs
3. Extract metrics from test output
4. Document any deviations or improvements

### Extracting Metrics from Logs

```bash
# Extract metrics from Common Test logs
grep -r "router_nats_connection_failures_total" ct_logs/r12/ | head -20

# Extract log messages
grep -r "Network partition detected" ct_logs/r12/ | head -20

# Extract test durations
grep -r "test.*\.\.\..*ok\|FAILED" ct_logs/r12/ | grep -E "\.(ok|FAILED)"
```

## CI/CD Integration

### GitHub Actions Example

```yaml
- name: Run R12 Network Partition Tests
  working-directory: apps/otp/router
  run: |
    rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12 -j 4
  continue-on-error: true

- name: Upload R12 Test Logs
  uses: actions/upload-artifact@v4
  if: always()
  with:
    name: r12_test_logs
    path: apps/otp/router/ct_logs/r12
    retention-days: 7
```

## References

- **Specification**: `R12_NETWORK_PARTITION_SCENARIOS.md`
- **Report Template**: `R12_RESULTS_REPORT_TEMPLATE.md`
- **Example Report**: `R12_RESULTS_REPORT_EXAMPLE.md`
- **Logs and Metrics**: `R12_LOGS_AND_METRICS.md`
- **Test Suite**: `router_network_partition_SUITE.erl`

