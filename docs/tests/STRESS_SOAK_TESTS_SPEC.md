# Extended Stress and Soak Tests Specification

## Purpose

This document defines the infrastructure for multi-hour stress and soak tests under various fault injection scenarios. These tests are designed to:

- **Detect resource leaks**: Memory, file descriptors, sockets, processes, ETS tables
- **Identify performance degradation**: Latency growth, throughput decline, queue buildup
- **Validate long-term stability**: System behavior over extended periods (hours to overnight runs)

## Scope

### Current State

**Existing**:
- Short extended soak test: `test_concurrent_faults_extended_soak/1` (30 seconds default)
- Basic fault injection infrastructure (`router_nats_fault_injection`)
- Basic resource checks (process count, memory)

**Missing**:
- Multi-hour soak test scenarios
- Systematic resource leak detection
- Performance degradation tracking over time
- Formalized pass/fail criteria for long-running tests
- Automated metrics collection and analysis

### Target State

**After Implementation**:
- Multi-hour soak test suite (2-8+ hours)
- Automated resource monitoring (memory, processes, ETS, file descriptors)
- Performance trend tracking (latency, throughput, queue sizes)
- Formal pass/fail criteria with thresholds
- Automated reporting and comparison with previous runs

## Test Categories

### 1. Single-Fault Soak Tests

**Purpose**: Test system stability under one persistent fault type

**Scenarios**:
- **Connect fault soak**: Connection failures repeated cyclically
- **Publish fault soak**: Publish errors repeated cyclically
- **ACK fault soak**: ACK failures repeated cyclically
- **Network partition soak**: Periodic network partitions

**Duration**: 2-4 hours
**Load Profile**: Steady medium load (10-50 msg/s)
**Fault Pattern**: Cyclic enable/disable (e.g., fault for 5 min, normal for 5 min)

### 2. Multi-Fault Soak Tests

**Purpose**: Test system stability under multiple simultaneous faults

**Scenarios**:
- **Triple-fault soak**: Connect + Publish + ACK faults simultaneously
- **Mixed pattern soak**: Intermittent + persistent faults
- **Cascading fault soak**: Sequential fault chains

**Duration**: 4-6 hours
**Load Profile**: Steady medium load with occasional bursts
**Fault Pattern**: Complex sequences with recovery periods

### 3. Baseline Soak Tests

**Purpose**: Establish baseline performance without faults

**Scenarios**:
- **Normal operation soak**: No faults, steady load
- **High load soak**: Higher load without faults
- **Burst load soak**: Periodic bursts without faults

**Duration**: 2-4 hours
**Load Profile**: Varies by scenario
**Fault Pattern**: None

## Resource Monitoring

### Memory Monitoring

**Metrics Collected**:
- Total memory usage (VM-wide)
- Per-process memory (critical processes)
- Memory growth rate (bytes/hour)
- Memory stabilization (plateau detection)

**Collection Frequency**: Every 5 minutes
**Storage**: Time-series data in ETS table

**Leak Detection**:
- **Fail if**: Monotonic growth > 10MB/hour for >2 hours
- **Fail if**: Step-wise growth (sudden jumps >50MB)
- **Pass if**: Memory stabilizes around plateau (±5% variance)

### Process Monitoring

**Metrics Collected**:
- Total process count
- Process count by type (supervisors, workers, gen_servers)
- Process queue sizes
- Process memory per type

**Collection Frequency**: Every 5 minutes

**Leak Detection**:
- **Fail if**: Process count grows >100/hour for >2 hours
- **Fail if**: Any process queue size >1000
- **Pass if**: Process count stabilizes (±10 processes)

### ETS Table Monitoring

**Metrics Collected**:
- ETS table count
- ETS table sizes (number of entries)
- ETS memory usage

**Collection Frequency**: Every 10 minutes

**Leak Detection**:
- **Fail if**: Any ETS table grows >1000 entries/hour
- **Fail if**: Total ETS memory grows >20MB/hour
- **Pass if**: ETS sizes stabilize

### File Descriptor Monitoring

**Metrics Collected**:
- Open file descriptors (if available via `erlang:system_info/1`)
- Socket count
- Port count

**Collection Frequency**: Every 10 minutes

**Leak Detection**:
- **Fail if**: File descriptors grow >10/hour for >2 hours
- **Pass if**: File descriptors stabilize

## Performance Monitoring

### Latency Tracking

**Metrics Collected**:
- P50, P95, P99 latency for key operations:
  - Message processing latency
  - Publish latency
  - ACK latency
  - Policy lookup latency

**Collection Frequency**: Every 1 minute (rolling window)

**Degradation Detection**:
- **Fail if**: P95 latency grows >2x baseline for >30 minutes
- **Fail if**: P99 latency grows >3x baseline for >30 minutes
- **Fail if**: Average latency grows >50% baseline for >1 hour
- **Pass if**: Latency remains within ±20% of baseline

### Throughput Tracking

**Metrics Collected**:
- Messages processed per second
- Publish operations per second
- ACK operations per second

**Collection Frequency**: Every 1 minute

**Degradation Detection**:
- **Fail if**: Throughput drops >30% below baseline for >30 minutes
- **Fail if**: Throughput drops to zero for >5 minutes (during normal operation)
- **Pass if**: Throughput remains within ±15% of baseline

### Queue Size Tracking

**Metrics Collected**:
- Message queue sizes (per process)
- Retry queue sizes
- Pending ACK queue sizes

**Collection Frequency**: Every 1 minute

**Degradation Detection**:
- **Fail if**: Any queue grows >1000 messages and doesn't drain
- **Fail if**: Queue size grows monotonically for >1 hour
- **Pass if**: Queues drain and stabilize

## Fault Injection Patterns

### Cyclic Fault Pattern

**Pattern**: `{cyclic, Fault, OnDuration, OffDuration}`

**Example**:
```erlang
{cyclic, {connect, {error, connection_refused}}, 300000, 300000}
%% Fault enabled for 5 minutes, disabled for 5 minutes, repeat
```

### Intermittent Fault Pattern

**Pattern**: `{intermittent, Fault, Probability}`

**Example**:
```erlang
{intermittent, {publish, {error, timeout}}, 0.3}
%% 30% probability of failure
```

### Burst Fault Pattern

**Pattern**: `{burst, Fault, BurstDuration, Interval}`

**Example**:
```erlang
{burst, {ack, {error, timeout}}, 60000, 600000}
%% Fault enabled for 1 minute every 10 minutes
```

### Sequential Fault Pattern

**Pattern**: `{sequential, [Fault1, Fault2, ...], Duration}`

**Example**:
```erlang
{sequential, [
    {connect, {error, connection_refused}},
    {publish, {error, timeout}},
    {ack, {error, timeout}}
], 300000}
%% Each fault enabled for 5 minutes in sequence
```

## Pass/Fail Criteria

### Resource Leak Criteria

**FAIL** if any of the following:
1. Memory grows monotonically >10MB/hour for >2 hours
2. Process count grows >100/hour for >2 hours
3. ETS table grows >1000 entries/hour
4. File descriptors grow >10/hour for >2 hours
5. Any critical process dies unexpectedly

**PASS** if:
1. All resources stabilize around plateau (±5-10% variance)
2. No critical processes die
3. Resource usage remains within acceptable bounds

### Performance Degradation Criteria

**FAIL** if any of the following:
1. P95 latency grows >2x baseline for >30 minutes
2. P99 latency grows >3x baseline for >30 minutes
3. Throughput drops >30% below baseline for >30 minutes
4. Queue sizes grow >1000 and don't drain
5. System becomes unresponsive (no progress for >5 minutes)

**PASS** if:
1. Latency remains within ±20% of baseline
2. Throughput remains within ±15% of baseline
3. Queues drain and stabilize
4. System remains responsive

### Error Criteria

**FAIL** if:
1. Unplanned crashes or exceptions (not part of fault scenario)
2. Error rate exceeds expected fault rate by >50%
3. System enters unrecoverable state

**PASS** if:
1. Only expected errors occur (matching fault scenario)
2. System recovers correctly after faults cleared
3. Error rate matches expected fault rate

## Test Execution

### Local Execution

**Short Run** (for development):
```bash
export STRESS_SOAK_DURATION_HOURS=1
export STRESS_SOAK_SCENARIO=single_fault_connect
rebar3 ct --suite router_stress_soak_SUITE --case test_single_fault_connect_soak
```

**Medium Run** (for validation):
```bash
export STRESS_SOAK_DURATION_HOURS=4
export STRESS_SOAK_SCENARIO=multi_fault_triple
rebar3 ct --suite router_stress_soak_SUITE --case test_multi_fault_triple_soak
```

**Long Run** (overnight):
```bash
export STRESS_SOAK_DURATION_HOURS=8
export STRESS_SOAK_SCENARIO=baseline_normal
rebar3 ct --suite router_stress_soak_SUITE --case test_baseline_normal_soak
```

### CI/CD Integration

**Nightly Runs**:
- Run 4-hour multi-fault soak test
- Run 2-hour baseline soak test
- Store results and compare with previous runs

**Pre-Release Runs**:
- Run full suite (all scenarios, 6-8 hours each)
- Generate comprehensive report
- Block release if any test fails

**Scheduled Runs**:
- Weekly: Full suite (all scenarios)
- Daily: Quick validation (2-hour single-fault)

## Reporting

### Test Report Format

**Report Structure**:
```json
{
  "test_name": "test_single_fault_connect_soak",
  "start_time": "2025-11-30T10:00:00Z",
  "end_time": "2025-11-30T14:00:00Z",
  "duration_hours": 4.0,
  "status": "pass|fail",
  "resource_metrics": {
    "memory": {
      "initial_mb": 150.5,
      "final_mb": 152.3,
      "peak_mb": 180.2,
      "growth_rate_mb_per_hour": 0.45,
      "stabilized": true
    },
    "processes": {
      "initial_count": 245,
      "final_count": 248,
      "peak_count": 260,
      "growth_rate_per_hour": 0.75,
      "stabilized": true
    }
  },
  "performance_metrics": {
    "latency": {
      "p50_baseline_ms": 5.2,
      "p50_final_ms": 5.8,
      "p95_baseline_ms": 12.5,
      "p95_final_ms": 13.1,
      "p99_baseline_ms": 25.0,
      "p99_final_ms": 26.2,
      "degradation_detected": false
    },
    "throughput": {
      "baseline_msg_per_sec": 45.2,
      "final_msg_per_sec": 44.8,
      "degradation_detected": false
    }
  },
  "failures": [],
  "warnings": []
}
```

### Comparison with Previous Runs

**Comparison Report**:
- Resource usage trend (improving/degrading/stable)
- Performance trend (improving/degrading/stable)
- New failures or warnings
- Regression detection

## Implementation Plan

### Phase 1: Core Infrastructure

1. **Resource Monitor Module** (`router_stress_monitor.erl`):
   - Memory tracking
   - Process tracking
   - ETS tracking
   - File descriptor tracking (if available)

2. **Performance Monitor Module** (`router_stress_perf_monitor.erl`):
   - Latency tracking
   - Throughput tracking
   - Queue size tracking

3. **Test Suite** (`router_stress_soak_SUITE.erl`):
   - Basic test structure
   - Single-fault soak test
   - Baseline soak test

### Phase 2: Advanced Scenarios

4. **Multi-Fault Soak Tests**:
   - Triple-fault soak
   - Mixed pattern soak
   - Cascading fault soak

5. **Advanced Fault Patterns**:
   - Cyclic fault pattern
   - Burst fault pattern
   - Sequential fault pattern

### Phase 3: Automation and Reporting

6. **Automated Analysis**:
   - Pass/fail criteria evaluation
   - Trend detection
   - Regression detection

7. **Reporting Tools**:
   - JSON report generation
   - Comparison with previous runs
   - Visualization (optional)

8. **CI/CD Integration**:
   - Nightly test runs
   - Pre-release test runs
   - Automated notifications

## References

- `router_advanced_concurrent_faults_SUITE.erl`: Advanced concurrent fault tests
- `router_jetstream_extended_recovery_SUITE.erl`: Extended recovery scenarios
- `router_nats_fault_injection.erl`: Fault injection mechanism
- `ADVANCED_CONCURRENT_FAULTS_SPEC.md`: Concurrent fault test specification

