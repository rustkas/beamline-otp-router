# R13: Metrics Under Faults and Load - README

**Date**: 2025-11-30  
**Status**: Implementation in Progress  
**Purpose**: Developer guide for R13 metrics validation tests

## Overview

R13 test suite validates metrics behavior under faults and load conditions:
- **Aggregation correctness**: Metrics accurately reflect actual operations
- **Rate calculations**: Rate metrics correctly calculate speeds
- **Label cardinality**: Label combinations stay within bounds

## Test Suite

**File**: `router_metrics_under_faults_SUITE.erl`

**Test Groups**:
1. **Aggregation Tests** - Verify metrics aggregation under various failure scenarios
2. **Rate Tests** - Verify rate calculation accuracy under different load patterns
3. **Cardinality Tests** - Verify label cardinality limits under load
4. **Combined Tests** - Verify all aspects under combined scenarios

## Running Tests

### Quick Start (Recommended)

```bash
# Using helper script
cd apps/otp/router/test
./run_r13_tests.sh

# Or with PowerShell
.\run_r13_tests.ps1
```

### Run All R13 Tests

```bash
# From router directory
cd apps/otp/router
rebar3 as test ct --suite router_metrics_under_faults_SUITE

# Or from project root
make test TESTSUITE=router_metrics_under_faults_SUITE
```

### Run Specific Test Group

```bash
# Aggregation tests only
make test TESTSUITE=router_metrics_under_faults_SUITE TESTGROUP=aggregation_tests

# Rate tests only
make test TESTSUITE=router_metrics_under_faults_SUITE TESTGROUP=rate_tests

# Cardinality tests only
make test TESTSUITE=router_metrics_under_faults_SUITE TESTGROUP=cardinality_tests

# Combined tests only
make test TESTSUITE=router_metrics_under_faults_SUITE TESTGROUP=combined_tests
```

### Run Individual Test

```bash
# Example: Test aggregation under partial failures
make test TESTSUITE=router_metrics_under_faults_SUITE TESTCASE=test_aggregation_under_partial_failures
```

## Test Scenarios

### Aggregation Tests

#### `test_aggregation_under_partial_failures`
- **Purpose**: Verify metrics aggregation when some requests fail
- **Scenario**: 1000 requests, 20% failure rate
- **Checks**: Total counters match expected values (± tolerance)
- **Duration**: ~10 seconds

#### `test_aggregation_under_retries`
- **Purpose**: Verify metrics aggregation with retries
- **Scenario**: 500 requests with intermittent failures causing retries
- **Checks**: Total includes retries, no duplicates
- **Duration**: ~15 seconds

#### `test_aggregation_under_node_failures`
- **Purpose**: Verify metrics aggregation during node failures and recovery
- **Scenario**: Load → node failure → recovery → continued load
- **Checks**: No gaps or duplicates after recovery
- **Duration**: ~15 seconds

#### `test_aggregation_under_flapping_connections`
- **Purpose**: Verify metrics aggregation with flapping connections
- **Scenario**: Connect/disconnect cycles during load
- **Checks**: No duplicates from reconnections
- **Duration**: ~12 seconds

#### `test_aggregation_after_recovery`
- **Purpose**: Verify metrics aggregation after recovery from failure
- **Scenario**: Normal → failure → recovery → normal
- **Checks**: No gaps after recovery
- **Duration**: ~12 seconds

### Rate Tests

#### `test_rate_under_spike_load`
- **Purpose**: Verify rate calculation under spike load
- **Scenario**: 500 RPS for 10 seconds
- **Checks**: Observed rate matches expected (± 10%)
- **Duration**: ~15 seconds

#### `test_rate_under_plateau_load`
- **Purpose**: Verify rate calculation under steady plateau load
- **Scenario**: 200 RPS for 30 seconds
- **Checks**: Observed rate matches expected (± 5%)
- **Duration**: ~40 seconds

#### `test_rate_under_sudden_stop`
- **Purpose**: Verify rate calculation when load suddenly stops
- **Scenario**: 300 RPS for 15 seconds, then sudden stop
- **Checks**: Rate drops to zero after stop
- **Duration**: ~20 seconds

#### `test_rate_under_failures`
- **Purpose**: Verify rate calculation with failures
- **Scenario**: 250 RPS for 20 seconds with 30% failure rate
- **Checks**: Rate accounts for failures (± 15%)
- **Duration**: ~25 seconds

#### `test_rate_after_restart`
- **Purpose**: Verify rate calculation after restart
- **Scenario**: Load → restart → continued load
- **Checks**: No false spikes after restart
- **Duration**: ~20 seconds

### Cardinality Tests

#### `test_cardinality_under_mass_load`
- **Purpose**: Verify label cardinality under mass load
- **Scenario**: 5000 requests with 100 tenants, 50 streams
- **Checks**: Cardinality stays within bounds
- **Duration**: ~10 seconds

#### `test_cardinality_with_many_tenants`
- **Purpose**: Verify label cardinality with many tenants
- **Scenario**: 3000 requests with 200 tenants, 5 streams
- **Checks**: Cardinality scales with tenants, not requests
- **Duration**: ~8 seconds

#### `test_cardinality_with_many_streams`
- **Purpose**: Verify label cardinality with many streams
- **Scenario**: 3000 requests with 10 tenants, 100 streams
- **Checks**: Cardinality scales with streams, not requests
- **Duration**: ~8 seconds

#### `test_cardinality_with_high_cardinality_leak`
- **Purpose**: Verify high-cardinality leak prevention
- **Scenario**: 2000 requests with unique request IDs
- **Checks**: Cardinality doesn't explode with unique IDs
- **Duration**: ~8 seconds

#### `test_cardinality_limits_enforced`
- **Purpose**: Verify cardinality limits are enforced
- **Scenario**: 10000 requests with 500 tenants, 100 streams
- **Checks**: Cardinality stays within enforced limits
- **Duration**: ~15 seconds

### Combined Tests

#### `test_metrics_under_combined_faults`
- **Purpose**: Verify all metrics aspects under combined faults
- **Scenario**: Multiple fault types simultaneously
- **Checks**: Aggregation, rate, cardinality all correct
- **Duration**: ~10 seconds

#### `test_metrics_under_chaos_scenarios`
- **Purpose**: Verify metrics under chaos (random faults/recoveries)
- **Scenario**: Random fault injection and recovery cycles
- **Checks**: Metrics remain accurate under chaos
- **Duration**: ~20 seconds

#### `test_metrics_under_prolonged_load`
- **Purpose**: Verify metrics under prolonged load
- **Scenario**: 10000 requests over 5 minutes (50 RPS)
- **Checks**: Aggregation, rate, cardinality all correct, no unbounded growth
- **Duration**: ~5 minutes

## Metrics Checked

### Aggregation Metrics

- `router_jetstream_ack_total` - Total acknowledgements
- `router_nats_publish_failures_total` - Total publish failures
- `router_jetstream_redelivery_total` - Total redeliveries
- `router_dlq_total` - Total DLQ messages

### Rate Metrics

- Calculated from `router_jetstream_ack_total` deltas over time
- Expected RPS vs observed RPS comparison

### Cardinality Metrics

- `router_jetstream_redelivery_total` - Label combinations counted
- Other labeled metrics as applicable

## Failure Types Modeled

### Connection Faults

- **Connect failures**: Connection attempts fail
- **Reconnect failures**: Reconnection attempts fail
- **Connection lost**: Active connections dropped

### Publish Faults

- **Publish failures**: Message publish operations fail
- **Publish timeouts**: Publish operations timeout
- **Intermittent publish**: Random publish failures

### Combined Faults

- **Multiple fault types**: Simultaneous connection and publish faults
- **Chaos scenarios**: Random fault injection and recovery
- **Flapping connections**: Repeated connect/disconnect cycles

## Success Criteria

### Aggregation

- ✅ Total counters match expected values (± tolerance)
- ✅ No event loss or duplication
- ✅ No gaps or jumps after recoveries
- ✅ Metrics consistent with logs

### Rate

- ✅ Observed rate matches expected rate (± tolerance %)
- ✅ No anomalous spikes or drops
- ✅ Rate stable after restarts
- ✅ Rate windows handled correctly

### Cardinality

- ✅ Unique label combinations within bounds
- ✅ No label explosion under load
- ✅ High-cardinality leaks prevented
- ✅ Cardinality matches expected model

## Troubleshooting

### Tests Failing with "Aggregation mismatch"

**Possible causes**:
- Metrics not being emitted correctly
- Fault injection not working as expected
- Timing issues (not waiting long enough)

**Solutions**:
- Check metrics ETS table: `ets:tab2list(router_metrics)`
- Verify fault injection: `router_nats_fault_injection:get_all_faults()`
- Increase wait times in test

### Tests Failing with "Rate mismatch"

**Possible causes**:
- Load generation not matching expected RPS
- Time measurement issues
- Metrics collection timing

**Solutions**:
- Verify load generation parameters
- Check time measurement accuracy
- Adjust tolerance if needed

### Tests Failing with "Cardinality exceeded"

**Possible causes**:
- Label cardinality limits not enforced
- High-cardinality values leaking into labels
- Too many unique label combinations

**Solutions**:
- Check label normalization in `router_metrics:normalize_labels/1`
- Verify label sanitization logic
- Review cardinality limits configuration

## CI/CD Integration

### GitHub Actions

Add to `.github/workflows/test.yml`:

```yaml
- name: Run R13 Metrics Under Faults Tests
  run: |
    make test TESTSUITE=router_metrics_under_faults_SUITE
```

### Nightly Tests

R13 tests can be run as nightly tests (longer duration):

```yaml
- name: Run R13 Nightly Tests
  if: github.event_name == 'schedule'
  run: |
    make test TESTSUITE=router_metrics_under_faults_SUITE TESTGROUP=combined_tests
```

## References

- **Specification**: `R13_METRICS_UNDER_FAULTS_SPEC.md`
- **Test Suite**: `router_metrics_under_faults_SUITE.erl`
- **Metrics Contracts**: `docs/dev/METRICS_CONTRACT_SPECIFICATION.md`
- **Fault Injection**: `router_nats_fault_injection` module
- **Metrics Storage**: `router_metrics.erl`
- **Prometheus Export**: `router_prometheus.erl`

