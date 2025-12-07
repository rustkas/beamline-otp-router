# Extended Stress and Soak Tests - Usage Guide

## Overview

This document describes how to use the extended stress and soak test infrastructure for multi-hour testing under fault injection scenarios.

## Test Suite

**File**: `apps/otp/router/test/router_stress_soak_SUITE.erl`

**Test Categories**:
- `single_fault_soak`: Single-fault scenarios (2-4 hours)
- `multi_fault_soak`: Multi-fault scenarios (4-6 hours)
- `baseline_soak`: Baseline scenarios without faults (2-4 hours)

## Environment Variables

### Test Duration

```bash
# Set test duration in hours (default: 0.5 for local, 2.0 for CI)
export STRESS_SOAK_DURATION_HOURS=4
```

### Load Profile

```bash
# Set base message rate (messages per second, default: 25)
export STRESS_SOAK_BASE_RATE=50
```

### CI Detection

Tests automatically detect CI environment and adjust defaults:
- **Local**: 30 minutes default duration
- **CI**: 2 hours default duration

## Running Tests

### Local Development (Short Run)

```bash
# Run single-fault connect soak test (30 minutes)
export STRESS_SOAK_DURATION_HOURS=0.5
rebar3 ct --suite router_stress_soak_SUITE --case test_single_fault_connect_soak
```

### Medium Validation Run

```bash
# Run multi-fault triple soak test (4 hours)
export STRESS_SOAK_DURATION_HOURS=4
rebar3 ct --suite router_stress_soak_SUITE --case test_multi_fault_triple_soak
```

### Long Overnight Run

```bash
# Run baseline normal soak test (8 hours)
export STRESS_SOAK_DURATION_HOURS=8
rebar3 ct --suite router_stress_soak_SUITE --case test_baseline_normal_soak
```

### Run All Tests in Category

```bash
# Run all single-fault soak tests
rebar3 ct --suite router_stress_soak_SUITE --group single_fault_soak
```

## Test Scenarios

### Single-Fault Soak Tests

#### `test_single_fault_connect_soak`

**Purpose**: Test system stability under cyclic connect failures

**Fault Pattern**: Connect fault for 5 minutes, normal for 5 minutes, repeat

**Load**: Steady medium load (configurable via `STRESS_SOAK_BASE_RATE`)

**Duration**: Configurable (default: 0.5-2 hours)

**What it tests**:
- Memory stability during repeated connection failures
- Process count stability
- Recovery after connection restoration
- No resource leaks over extended period

#### `test_single_fault_publish_soak`

**Purpose**: Test system stability under cyclic publish errors

**Fault Pattern**: Publish timeout for 5 minutes, normal for 5 minutes, repeat

**What it tests**:
- Message queue handling during publish failures
- Retry logic stability
- No message loss beyond acceptable SLA

#### `test_single_fault_ack_soak`

**Purpose**: Test system stability under cyclic ACK failures

**Fault Pattern**: ACK timeout for 5 minutes, normal for 5 minutes, repeat

**What it tests**:
- Redelivery mechanism stability
- No infinite redelivery loops
- Message semantics preservation

#### `test_single_fault_network_partition_soak`

**Purpose**: Test system stability under periodic network partitions

**Fault Pattern**: Connection closed for 5 minutes, normal for 5 minutes, repeat

**What it tests**:
- Network partition recovery
- State consistency after partition
- No resource leaks during partitions

### Multi-Fault Soak Tests

#### `test_multi_fault_triple_soak`

**Purpose**: Test system stability under simultaneous connect + publish + ACK faults

**Fault Pattern**: All three faults enabled for 5 minutes, disabled for 5 minutes, repeat

**Load**: Steady medium load with periodic bursts (2x base rate)

**What it tests**:
- System behavior under multiple simultaneous faults
- Resource stability under complex fault scenarios
- Recovery after all faults cleared

#### `test_multi_fault_mixed_pattern_soak`

**Purpose**: Test system stability under mixed intermittent + persistent faults

**Fault Pattern**:
- Intermittent connect failures (50% probability)
- Persistent publish failures (always fails)

**What it tests**:
- Handling of mixed fault patterns
- No resource accumulation from persistent failures
- Correct retry behavior

#### `test_multi_fault_cascading_soak`

**Purpose**: Test system stability under cascading fault chains

**Fault Pattern**: Sequential faults (connect → publish → ACK) in 10-minute cycles

**What it tests**:
- Cascading fault recovery
- State consistency through fault chains
- No resource leaks from cascading failures

### Baseline Soak Tests

#### `test_baseline_normal_soak`

**Purpose**: Establish baseline performance without faults

**Fault Pattern**: None

**Load**: Steady medium load

**What it tests**:
- Baseline resource usage
- Baseline performance metrics
- System stability under normal operation

#### `test_baseline_high_load_soak`

**Purpose**: Test system stability under high load without faults

**Fault Pattern**: None

**Load**: 3x base rate (high load)

**What it tests**:
- System behavior under sustained high load
- Resource usage under high load
- Performance metrics under high load

#### `test_baseline_burst_load_soak`

**Purpose**: Test system stability under burst load patterns

**Fault Pattern**: None

**Load**: Base rate with periodic bursts (2x base rate for 30% of time)

**What it tests**:
- System behavior under burst patterns
- Queue handling during bursts
- Recovery after bursts

## Monitoring and Metrics

### Resource Monitoring

Tests use `router_stress_monitor` to collect:
- **Memory**: Total, per-process, growth rate
- **Processes**: Count, queue sizes, critical process health
- **ETS Tables**: Count, sizes, memory usage
- **File Descriptors**: Open count (if available)

**Collection Frequency**: Every 5 minutes

### Performance Monitoring

Tests use `router_stress_perf_monitor` to collect:
- **Latency**: P50, P95, P99 for key operations
- **Throughput**: Messages per second
- **Queue Sizes**: Per-queue sizes

**Collection Frequency**: Every 1 minute (rolling window)

## Pass/Fail Criteria

### Resource Leak Criteria

**FAIL** if:
- Memory grows >10MB/hour for >2 hours
- Process count grows >100/hour for >2 hours
- ETS table grows >1000 entries/hour
- File descriptors grow >10/hour for >2 hours
- Any critical process dies unexpectedly

**PASS** if:
- All resources stabilize around plateau (±5-10% variance)
- No critical processes die
- Resource usage remains within acceptable bounds

### Performance Degradation Criteria

**FAIL** if:
- P95 latency grows >2x baseline for >30 minutes
- P99 latency grows >3x baseline for >30 minutes
- Throughput drops >30% below baseline for >30 minutes
- Queue sizes grow >1000 and don't drain
- System becomes unresponsive (no progress for >5 minutes)

**PASS** if:
- Latency remains within ±20% of baseline
- Throughput remains within ±15% of baseline
- Queues drain and stabilize
- System remains responsive

## Test Reports

### Report Format

Tests generate comprehensive JSON reports with:
- Resource usage trends
- Performance metrics
- Pass/fail status
- Detailed failure information

### Report Location

Reports are logged via Common Test `ct:comment/1` and can be extracted from test logs.

## CI/CD Integration

### Nightly Runs

Recommended configuration:
```bash
# Run 4-hour multi-fault soak test
export STRESS_SOAK_DURATION_HOURS=4
rebar3 ct --suite router_stress_soak_SUITE --case test_multi_fault_triple_soak

# Run 2-hour baseline soak test
export STRESS_SOAK_DURATION_HOURS=2
rebar3 ct --suite router_stress_soak_SUITE --case test_baseline_normal_soak
```

### Pre-Release Runs

Recommended configuration:
```bash
# Run full suite (all scenarios, 6-8 hours each)
export STRESS_SOAK_DURATION_HOURS=8
rebar3 ct --suite router_stress_soak_SUITE
```

### Scheduled Runs

- **Weekly**: Full suite (all scenarios)
- **Daily**: Quick validation (2-hour single-fault)

## Troubleshooting

### Test Timeout

If tests timeout:
- Check CI/CD timeout settings (should be hours for full suite)
- Verify system resources (memory, CPU)
- Check NATS/JetStream availability

### Resource Leaks Detected

If resource leaks are detected:
- Check process count growth trends
- Review memory usage trends
- Check ETS table sizes
- Review connection count

### Performance Degradation

If performance degrades:
- Check throughput trends
- Review latency trends
- Check queue sizes
- Review error accumulation

## References

- `STRESS_SOAK_TESTS_SPEC.md`: Detailed specification
- `router_stress_monitor.erl`: Resource monitoring module
- `router_stress_perf_monitor.erl`: Performance monitoring module
- `router_nats_fault_injection.erl`: Fault injection mechanism

