# R12: Network Partition Scenarios - Test Execution Report

**Date**: 2025-11-30  
**Test Run ID**: r12-execution-20250127-001  
**Environment**: local  
**Test Mode**: mock (router_nats_fault_injection)

## Executive Summary

**Test Execution Status**: ✅ **Ready for Execution**

**Commands Prepared**:
- All test execution commands documented
- Fault injection scripts documented
- Log extraction commands prepared
- Report generation workflow defined

**Note**: Actual test execution requires:
1. Fix compilation errors in other modules (if any)
2. Ensure all dependencies are installed
3. Run tests and collect results

## Test Execution Commands

### Quick Start

```bash
# Navigate to router directory
cd /home/rustkas/aigroup/apps/otp/router

# Compile first
rebar3 compile

# Run all R12 tests
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12

# Run with verbose output
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12 --verbose
```

### Run Specific Test Groups

```bash
# Single-instance tests (9 tests, ~15-20 minutes)
rebar3 as test ct --suite router_network_partition_SUITE --group single_instance_tests --logdir ct_logs/r12/single_instance

# Multi-instance tests (11 tests, ~25-30 minutes)
rebar3 as test ct --suite router_network_partition_SUITE --group multi_instance_tests --logdir ct_logs/r12/multi_instance

# Service-broker tests (3 tests, ~10-15 minutes)
rebar3 as test ct --suite router_network_partition_SUITE --group service_broker_tests --logdir ct_logs/r12/service_broker

# Flapping network tests (3 tests, ~15-20 minutes)
rebar3 as test ct --suite router_network_partition_SUITE --group flapping_network_tests --logdir ct_logs/r12/flapping
```

### Run Individual Test Cases

#### Single-Instance Tests

```bash
# Short JetStream partition (5-10 seconds)
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_short --logdir ct_logs/r12

# Long JetStream partition (2-5 minutes)
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_long --logdir ct_logs/r12

# JetStream partition recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_recovery --logdir ct_logs/r12

# External service partition (short)
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_external_service_partition_short --logdir ct_logs/r12

# External service partition (long)
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_external_service_partition_long --logdir ct_logs/r12

# External service partition recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_external_service_partition_recovery --logdir ct_logs/r12

# Partial partition
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_partial_partition --logdir ct_logs/r12

# Full partition
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_full_partition --logdir ct_logs/r12

# Partition healing
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_partition_healing --logdir ct_logs/r12
```

#### Multi-Instance Tests

```bash
# Split-brain scenario
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_split_brain --logdir ct_logs/r12

# Split-brain leader election
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_split_brain_leader_election --logdir ct_logs/r12

# Split-brain no duplicate processing
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_split_brain_no_duplicate_processing --logdir ct_logs/r12

# Split-brain recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_split_brain_recovery --logdir ct_logs/r12

# Partial partition
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_partial_partition --logdir ct_logs/r12

# Leader election after healing
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_leader_election_after_healing --logdir ct_logs/r12

# JetStream partition (instance A isolated)
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_jetstream_partition_instance_a_isolated --logdir ct_logs/r12

# JetStream partition (cluster split)
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_jetstream_partition_jetstream_cluster_split --logdir ct_logs/r12

# JetStream partition recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_jetstream_partition_recovery --logdir ct_logs/r12

# Distributed locks partition
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_distributed_locks_partition --logdir ct_logs/r12

# Distributed locks recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_multi_instance_distributed_locks_recovery --logdir ct_logs/r12
```

#### Service-Broker Tests

```bash
# Service-broker partition
rebar3 as test ct --suite router_network_partition_SUITE --case test_service_broker_partition --logdir ct_logs/r12

# Service-broker partition retry behavior
rebar3 as test ct --suite router_network_partition_SUITE --case test_service_broker_partition_retry_behavior --logdir ct_logs/r12

# Service-broker partition recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_service_broker_partition_recovery --logdir ct_logs/r12
```

#### Flapping Network Tests

```bash
# Flapping network stability
rebar3 as test ct --suite router_network_partition_SUITE --case test_flapping_network_stability --logdir ct_logs/r12

# Flapping network no resource leaks
rebar3 as test ct --suite router_network_partition_SUITE --case test_flapping_network_no_resource_leaks --logdir ct_logs/r12

# Flapping network recovery
rebar3 as test ct --suite router_network_partition_SUITE --case test_flapping_network_recovery --logdir ct_logs/r12
```

## Fault Injection Scripts

### Bash Script Usage

**Location**: `apps/otp/router/test/scripts/r12_network_partition_fault_injection.sh`

#### Mock Mode (Default, No Root Required)

```bash
cd apps/otp/router/test/scripts

# Create single-instance partition (Router -> NATS, drop packets)
./r12_network_partition_fault_injection.sh create single_instance router nats --action drop

# Create partition with delay (5000ms)
./r12_network_partition_fault_injection.sh create single_instance router nats --action delay --delay_ms 5000

# Create partition with packet loss (50%)
./r12_network_partition_fault_injection.sh create single_instance router nats --action loss --loss_percent 50

# List active partitions
./r12_network_partition_fault_injection.sh list

# Check partition status
./r12_network_partition_fault_injection.sh status <partition_id>

# Remove partition
./r12_network_partition_fault_injection.sh remove <partition_id>

# Heal partition (gradual recovery)
./r12_network_partition_fault_injection.sh heal <partition_id>

# Simulate flapping network (2000ms interval, 10000ms duration)
./r12_network_partition_fault_injection.sh flapping router nats 2000 10000
```

#### Real Mode (Requires Root, Uses iptables/tc)

```bash
# Enable real mode
export NETWORK_PARTITION_REAL=true

# Run with sudo
sudo ./r12_network_partition_fault_injection.sh create single_instance router nats --action drop
sudo ./r12_network_partition_fault_injection.sh create single_instance router nats --action delay --delay_ms 5000
sudo ./r12_network_partition_fault_injection.sh remove <partition_id>
```

### PowerShell Script Usage

**Location**: `apps/otp/router/test/scripts/r12_network_partition_fault_injection.ps1`

```powershell
cd apps/otp/router/test/scripts

# Create single-instance partition
.\r12_network_partition_fault_injection.ps1 create single_instance router nats -Action drop

# Create partition with delay
.\r12_network_partition_fault_injection.ps1 create single_instance router nats -Action delay -DelayMs 5000

# List active partitions
.\r12_network_partition_fault_injection.ps1 list

# Remove partition
.\r12_network_partition_fault_injection.ps1 remove <partition_id>

# Simulate flapping network
.\r12_network_partition_fault_injection.ps1 flapping router nats -IntervalMs 2000 -DurationMs 10000
```

## Interpreting Test Results

### Expected Test Output

#### Successful Test

```
=== Test Results ===
router_network_partition_SUITE:test_single_instance_jetstream_partition_short...ok
  Duration: 8.5 seconds
  Metrics verified: ✅
  Contract invariants: ✅

=== Summary ===
Total: 21
Passed: 21
Failed: 0
Skipped: 0
```

#### Failed Test

```
=== Test Results ===
router_network_partition_SUITE:test_single_instance_jetstream_partition_long...FAILED
  Reason: Resource leak detected (memory growth: 15MB, threshold: 10MB)
  Duration: 125.3 seconds
  Metrics verified: ✅
  Contract invariants: ⚠️ (resource leak)

=== Summary ===
Total: 21
Passed: 20
Failed: 1
Skipped: 0
```

### Log File Locations

After running tests, logs are available in:

- **HTML Report**: `ct_logs/r12/test_server.html` or `ct_logs/r12/latest_run/index.html`
- **Text Logs**: `ct_logs/r12/latest_run/*.log`
- **SASL Logs**: `ct_logs/r12/latest_run/sasl/*.log`
- **Coverage Report**: `ct_logs/r12/cover/index.html` (if coverage enabled)

### Extracting Information from Logs

#### Extract Test Results Summary

```bash
# Extract test results summary
grep -E "\.(ok|FAILED|SKIPPED)" ct_logs/r12/latest_run/*.log | head -30

# Extract test durations
grep -E "Duration:" ct_logs/r12/latest_run/*.log

# Extract failed tests
grep -E "FAILED" ct_logs/r12/latest_run/*.log
```

#### Extract Metrics

```bash
# Extract connection failure metrics
grep -r "router_nats_connection_failures_total" ct_logs/r12/ | head -20

# Extract circuit breaker state changes
grep -r "circuit_breaker_state" ct_logs/r12/ | head -20

# Extract partition duration metrics
grep -r "network_partition_duration_seconds" ct_logs/r12/ | head -20

# Extract recovery time metrics
grep -r "network_partition_recovery_time_seconds" ct_logs/r12/ | head -20
```

#### Extract Log Messages

```bash
# Extract partition detection logs
grep -r "Network partition detected" ct_logs/r12/ | head -20

# Extract connection loss logs
grep -r "Connection lost" ct_logs/r12/ | head -20

# Extract retry attempt logs
grep -r "Retry attempt" ct_logs/r12/ | head -20

# Extract recovery logs
grep -r "Connection restored\|Network partition resolved" ct_logs/r12/ | head -20

# Extract circuit breaker logs
grep -r "Circuit breaker" ct_logs/r12/ | head -20
```

#### Extract Error Messages

```bash
# Extract all ERROR level logs
grep -r "\[ERROR\]" ct_logs/r12/ | head -30

# Extract all WARN level logs
grep -r "\[WARN\]" ct_logs/r12/ | head -30

# Extract crash reports
grep -r "CRASH REPORT" ct_logs/r12/ | head -20
```

## Metrics Verification

All tests verify metrics at three stages:

1. **Initial Metrics** (before partition):
   - Normal operation metrics
   - Baseline connection counts
   - Baseline error rates

2. **Partition Metrics** (during partition):
   - Connection failures increase
   - Circuit breaker opens
   - Retry attempts logged

3. **Final Metrics** (after recovery):
   - Recovery metrics
   - Circuit breaker closes
   - Normal operation resumed

### Key Metrics to Verify

- `router_nats_connection_failures_total{service="nats-jetstream", error_type="connection_refused"}` - Should increase during partition
- `router_nats_reconnect_attempts_total{service="nats-jetstream"}` - Should show retry attempts
- `router_circuit_breaker_state{service="nats-jetstream", state="open"}` - Should be 1 during partition
- `router_circuit_breaker_state{service="nats-jetstream", state="closed"}` - Should be 1 after recovery
- `router_network_partition_duration_seconds{partition_type="single_instance"}` - Should match partition duration
- `router_network_partition_recovery_time_seconds{partition_type="single_instance"}` - Should show recovery time

## Contract Invariants Verification

All tests verify:

1. **I1: Fail-Open Behavior** - Process remains alive (no crashes)
2. **I2: MaxDeliver Semantics** - Messages either deliver or exhaust MaxDeliver
3. **I3: Redelivery Limits** - Redelivery count within bounds (≤ 50)
4. **I4: Data Guarantees** - No duplicates, losses, or inconsistencies
5. **I5: Metrics Correctness** - Metrics reflect actual system behavior
6. **I6: Recovery Behavior** - Correct recovery after partition resolution

## Generating Test Report

### Step 1: Run Tests

```bash
cd apps/otp/router
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12
```

### Step 2: Extract Results

```bash
# Create report directory
mkdir -p reports/r12

# Extract test summary
grep -E "Total:|Passed:|Failed:" ct_logs/r12/latest_run/*.log > reports/r12/test_summary.txt

# Extract metrics
grep -r "router_nats_connection_failures_total\|router_circuit_breaker_state\|router_network_partition" ct_logs/r12/ > reports/r12/metrics.txt

# Extract logs
grep -r "Network partition\|Connection lost\|Connection restored" ct_logs/r12/ > reports/r12/logs.txt
```

### Step 3: Fill Report Template

1. Copy template:
   ```bash
   cp apps/otp/router/test/R12_RESULTS_REPORT_TEMPLATE.md reports/r12/R12_RESULTS_REPORT_$(date +%Y%m%d).md
   ```

2. Fill in test results from extracted files
3. Document any deviations and improvements

## Example Test Execution Workflow

### Complete Test Run

```bash
# 1. Compile
cd /home/rustkas/aigroup/apps/otp/router
rebar3 compile

# 2. Run all R12 tests
rebar3 as test ct --suite router_network_partition_SUITE --logdir ct_logs/r12

# 3. Extract results
mkdir -p reports/r12
grep -E "Total:|Passed:|Failed:" ct_logs/r12/latest_run/*.log > reports/r12/test_summary.txt
grep -r "router_nats_connection_failures_total" ct_logs/r12/ > reports/r12/metrics.txt
grep -r "Network partition\|Connection lost\|Connection restored" ct_logs/r12/ > reports/r12/logs.txt

# 4. Generate report
cp test/R12_RESULTS_REPORT_TEMPLATE.md reports/r12/R12_RESULTS_REPORT_$(date +%Y%m%d).md
# Then manually fill in the report with extracted data
```

### Single Test Case Execution

```bash
# Run single test
rebar3 as test ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_short --logdir ct_logs/r12

# Check results
cat ct_logs/r12/latest_run/*.log | grep -E "ok|FAILED"

# View HTML report
# Open: ct_logs/r12/test_server.html
```

## Troubleshooting

### Issue: Tests Fail to Compile

**Symptoms**: Compilation errors before tests run

**Solutions**:
1. Check Erlang/OTP version: `erl -version` (should be 26+)
2. Verify dependencies: `rebar3 deps`
3. Clean and rebuild: `rebar3 clean && rebar3 compile`
4. Check for syntax errors in test suite

### Issue: Tests Timeout

**Symptoms**: Tests hang and eventually timeout

**Solutions**:
1. Check for stuck processes: `ps aux | grep beam`
2. Increase timeout in test configuration
3. Run tests sequentially (remove `-j` flag)
4. Check logs for deadlock indicators
5. Verify mock mode is working correctly

### Issue: Metrics Not Collected

**Symptoms**: All metrics are zero or missing

**Solutions**:
1. Verify `router_metrics:ensure()` is called in test setup
2. Check ETS table: `ets:info(router_metrics)`
3. Verify telemetry handlers are attached
4. Check test configuration for metrics collection
5. Review `get_metrics_snapshot/0` implementation

### Issue: Resource Leaks Detected

**Symptoms**: Tests fail with "Resource leak detected" errors

**Solutions**:
1. Check memory growth: Should be < 10MB per test
2. Check process count: Should be stable
3. Verify ETS table cleanup: Check for orphaned entries
4. Review test cleanup code in `end_per_testcase/2`
5. Check for unclosed connections or file handles

## References

- **Testing Guide**: `R12_TESTING_GUIDE.md`
- **Report Template**: `R12_RESULTS_REPORT_TEMPLATE.md`
- **Example Report**: `R12_RESULTS_REPORT_EXAMPLE.md`
- **Logs and Metrics**: `R12_LOGS_AND_METRICS.md`
- **Test Suite**: `router_network_partition_SUITE.erl`

