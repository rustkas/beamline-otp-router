# Extended Recovery Scenarios - Resource Limits

## Overview

This document defines **resource limits** for Extended Recovery Scenarios tests. These limits are used to:

- Detect resource leaks during extended test runs
- Verify system stability under sustained load
- Establish performance degradation thresholds
- Set CI/CD failure conditions

## Resource Categories

### Process Count

**Baseline**: Normal operation process count

**Limits**:
- **Warning threshold**: Baseline + 50 processes
- **Failure threshold**: Baseline + 100 processes
- **Growth rate**: < 10 processes per hour during extended runs

**Measurement**:
```erlang
erlang:system_info(process_count)
```

**Detection**:
- Track process count at start of test
- Monitor process count every 5 minutes
- Fail test if process count exceeds failure threshold
- Warn if process count exceeds warning threshold

### Memory Usage

**Baseline**: Normal operation memory usage

**Limits**:
- **Warning threshold**: Baseline + 100 MB
- **Failure threshold**: Baseline + 200 MB
- **Growth rate**: < 20 MB per hour during extended runs

**Measurement**:
```erlang
{memory, MemoryBytes} = erlang:process_info(self(), memory)
```

**Detection**:
- Track memory usage at start of test
- Monitor memory usage every 5 minutes
- Fail test if memory usage exceeds failure threshold
- Warn if memory usage exceeds warning threshold

### Connection Count

**Baseline**: Normal operation NATS connection count

**Limits**:
- **Warning threshold**: Baseline + 5 connections
- **Failure threshold**: Baseline + 10 connections
- **Growth rate**: < 2 connections per hour during extended runs

**Measurement**:
- Track NATS connection count via monitoring
- Count active connections to NATS server

**Detection**:
- Track connection count at start of test
- Monitor connection count every 5 minutes
- Fail test if connection count exceeds failure threshold
- Warn if connection count exceeds warning threshold

### ETS Table Sizes

**Baseline**: Normal operation ETS table sizes

**Limits**:
- **Warning threshold**: Baseline + 1000 entries per table
- **Failure threshold**: Baseline + 5000 entries per table
- **Growth rate**: < 100 entries per hour per table

**Measurement**:
```erlang
ets:info(TableName, size)
```

**Detection**:
- Track ETS table sizes at start of test
- Monitor ETS table sizes every 5 minutes
- Fail test if any table exceeds failure threshold
- Warn if any table exceeds warning threshold

### Throughput

**Baseline**: Normal operation throughput (messages/second)

**Limits**:
- **Warning threshold**: Baseline * 0.9 (10% degradation)
- **Failure threshold**: Baseline * 0.8 (20% degradation)
- **Recovery threshold**: Baseline * 0.95 (5% degradation) for recovery tests

**Measurement**:
- Measure throughput over 1-minute windows
- Calculate average throughput per phase

**Detection**:
- Track throughput at baseline phase
- Monitor throughput during fault and recovery phases
- Fail test if throughput drops below failure threshold
- Warn if throughput drops below warning threshold

### Latency

**Baseline**: Normal operation latency (p50, p95, p99)

**Limits**:
- **Warning threshold**: Baseline * 1.2 (20% increase)
- **Failure threshold**: Baseline * 1.5 (50% increase)
- **Recovery threshold**: Baseline * 1.1 (10% increase) for recovery tests

**Measurement**:
- Measure latency percentiles over 1-minute windows
- Calculate average latency per phase

**Detection**:
- Track latency at baseline phase
- Monitor latency during fault and recovery phases
- Fail test if latency exceeds failure threshold
- Warn if latency exceeds warning threshold

## Test-Specific Limits

### MaxDeliver Scenarios

**Additional Limits**:
- **MaxDeliver exhausted count**: Track but no hard limit (expected behavior)
- **DLQ message count**: Track but no hard limit (expected behavior)
- **Delivery count distribution**: Monitor for anomalies

### Restart Scenarios

**Additional Limits**:
- **Reconnection time**: < 30 seconds per restart
- **Recovery time**: < 5 minutes to return to baseline
- **State preservation**: Verify ETS state preserved across restarts

### Combined Scenarios

**Additional Limits**:
- **Cumulative resource growth**: < sum of individual scenario limits
- **Performance degradation**: No more than worst individual scenario
- **Error accumulation**: No unbounded error growth

### Performance Scenarios

**Additional Limits**:
- **Long-term stability**: All metrics stable over 4+ hours
- **No degradation trend**: Metrics should not show monotonic degradation
- **Recovery time**: < 5 minutes for all fault types

## Implementation

### Resource Tracking

Resource tracking is implemented in `router_jetstream_extended_recovery_SUITE.erl`:

```erlang
-spec track_resources() -> map().
track_resources() ->
    #{
        process_count => erlang:system_info(process_count),
        memory_usage => get_memory_usage(),
        timestamp => erlang:monotonic_time(millisecond)
    }.
```

### Limit Verification

Limit verification is implemented in helper functions:

```erlang
-spec verify_no_resource_leaks(map(), map()) -> ok.
verify_no_resource_leaks(Initial, Final) ->
    InitialProcessCount = maps:get(process_count, Initial, 0),
    FinalProcessCount = maps:get(process_count, Final, 0),
    ProcessLeak = FinalProcessCount - InitialProcessCount,
    
    %% Allow some process growth, but not excessive
    true = ProcessLeak < 100,
    
    ct:comment("Process leak: ~p (acceptable)", [ProcessLeak]),
    ok.
```

### Threshold Configuration

Thresholds can be configured via environment variables:

```bash
# Process count limits
EXTENDED_TEST_PROCESS_WARNING=50
EXTENDED_TEST_PROCESS_FAILURE=100

# Memory limits
EXTENDED_TEST_MEMORY_WARNING_MB=100
EXTENDED_TEST_MEMORY_FAILURE_MB=200

# Throughput limits
EXTENDED_TEST_THROUGHPUT_WARNING_PCT=10
EXTENDED_TEST_THROUGHPUT_FAILURE_PCT=20

# Latency limits
EXTENDED_TEST_LATENCY_WARNING_PCT=20
EXTENDED_TEST_LATENCY_FAILURE_PCT=50
```

## Baseline Establishment

Baselines are established using `scripts/establish_performance_baseline.sh`:

```bash
./scripts/establish_performance_baseline.sh \
  --iterations 10 \
  --duration-min 5 \
  --message-rate 100 \
  --output-dir reports/baseline
```

This generates:
- `baseline_metrics.json`: Machine-readable baseline metrics
- `baseline_report.md`: Human-readable baseline report
- `baseline_thresholds.json`: Performance thresholds

## CI/CD Integration

Resource limits are enforced in CI/CD:

1. **Baseline collection**: Run baseline establishment before extended tests
2. **Limit verification**: Check limits during test execution
3. **Failure reporting**: Report limit violations in test reports
4. **Trend analysis**: Track resource usage trends over time

## Monitoring

### Real-Time Monitoring

During test execution:
- Resource metrics collected every 5 minutes
- Warnings logged if thresholds exceeded
- Test fails if failure thresholds exceeded

### Post-Test Analysis

After test completion:
- Resource usage trends analyzed
- Baseline comparison performed
- Degradation patterns identified
- Recommendations provided

## Examples

### Example 1: Process Leak Detection

```
Initial process count: 500
After 4 hours: 650 processes
Leak: 150 processes
Status: FAILED (exceeds 100 process limit)
```

### Example 2: Memory Growth

```
Initial memory: 200 MB
After 4 hours: 350 MB
Growth: 150 MB
Status: WARNING (exceeds 100 MB warning threshold, but below 200 MB failure threshold)
```

### Example 3: Throughput Degradation

```
Baseline throughput: 100 msg/s
After recovery: 85 msg/s
Degradation: 15% (15 msg/s)
Status: WARNING (exceeds 10% warning threshold, but below 20% failure threshold)
```

## References

- `EXTENDED_RECOVERY_SCENARIOS_SPEC.md`: Scenario specifications
- `router_jetstream_extended_recovery_SUITE.erl`: Test implementation
- `scripts/establish_performance_baseline.sh`: Baseline establishment script

