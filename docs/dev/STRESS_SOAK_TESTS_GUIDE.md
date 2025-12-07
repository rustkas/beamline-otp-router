# Extended Stress and Soak Tests Guide

## Overview

Extended stress and soak tests are designed to detect resource leaks and performance degradation over extended periods (hours to overnight runs) under various fault injection scenarios.

## Quick Start

### Run a Short Test (Development)

```bash
# 1-hour single-fault connect soak test
export STRESS_SOAK_DURATION_HOURS=1
cd apps/otp/router
rebar3 ct --suite router_stress_soak_SUITE --case test_single_fault_connect_soak
```

### Run a Medium Test (Validation)

```bash
# 4-hour multi-fault soak test
./scripts/run_stress_soak_test.sh test_multi_fault_triple_soak 4
```

### Run a Long Test (Overnight)

```bash
# 8-hour baseline soak test
./scripts/run_stress_soak_test.sh test_baseline_normal_soak 8
```

## Test Scenarios

### Single-Fault Soak Tests

**Purpose**: Test system stability under one persistent fault type

**Available Tests**:
- `test_single_fault_connect_soak`: Cyclic connect faults (5 min on, 5 min off)
- `test_single_fault_publish_soak`: Cyclic publish faults
- `test_single_fault_ack_soak`: Cyclic ACK faults

**Load Profile**: Steady medium load (20 msg/s)
**Default Duration**: 1 hour (configurable)

### Multi-Fault Soak Tests

**Purpose**: Test system stability under multiple simultaneous faults

**Available Tests**:
- `test_multi_fault_triple_soak`: Connect + Publish + ACK faults simultaneously
- `test_multi_fault_mixed_pattern_soak`: Intermittent + persistent faults

**Load Profile**: Steady medium load with bursts (25-30 msg/s)
**Default Duration**: 1 hour (configurable)

### Baseline Soak Tests

**Purpose**: Establish baseline performance without faults

**Available Tests**:
- `test_baseline_normal_soak`: Normal operation, steady load (20 msg/s)
- `test_baseline_high_load_soak`: High load without faults (50 msg/s)

**Load Profile**: Varies by test
**Default Duration**: 1 hour (configurable)

## Configuration

### Environment Variables

**STRESS_SOAK_DURATION_HOURS**: Test duration in hours
```bash
export STRESS_SOAK_DURATION_HOURS=4  # 4-hour test
```

**STRESS_SOAK_OUTPUT_DIR**: Output directory for reports
```bash
export STRESS_SOAK_OUTPUT_DIR=./my_results
```

### Test Timeout

Common Test timeout is automatically calculated as `duration * 1.1` (10% buffer).

For manual override:
```bash
rebar3 ct --suite router_stress_soak_SUITE --case test_single_fault_connect_soak --timeout 14400  # 4 hours in seconds
```

## Resource Monitoring

### Metrics Collected

**Memory**:
- Total memory usage
- Per-process memory
- Memory growth rate (MB/hour)

**Processes**:
- Total process count
- Process count by type
- Process queue sizes

**ETS Tables**:
- ETS table count
- ETS table sizes
- ETS memory usage

**Performance**:
- Router metrics (from `router_metrics` ETS table)
- Run queue length
- Wall clock time

### Collection Frequency

- **Default**: Every 5 minutes (300000 ms)
- **Configurable**: Via `router_stress_monitor:start(#{collection_interval_ms => 600000})` for 10-minute intervals

## Pass/Fail Criteria

### Resource Leak Criteria

**FAIL** if:
- Memory grows >10MB/hour for >2 hours
- Process count grows >100/hour for >2 hours
- ETS table grows >1000 entries/hour
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

**PASS** if:
- Latency remains within ±20% of baseline
- Throughput remains within ±15% of baseline
- Queues drain and stabilize

## Interpreting Results

### Test Report

After test completion, check the report in the output directory:

```bash
cat stress_soak_results/test_single_fault_connect_soak_20250127_120000/summary.txt
```

### Resource Summary

The report includes a resource summary with:
- Initial and final memory usage
- Memory growth rate (MB/hour)
- Process count growth
- Stabilization status

**Example**:
```
Resource Summary:
  memory:
    initial_mb: 150.5
    final_mb: 152.3
    growth_mb: 1.8
    growth_rate_mb_per_hour: 0.45
  processes:
    initial_count: 245
    final_count: 248
    growth: 3
    growth_rate_per_hour: 0.75
```

### Performance Summary

Performance metrics are collected from `router_metrics` ETS table and include:
- Message processing metrics
- Publish/ACK metrics
- Error rates

## Troubleshooting

### Test Timeout

**Problem**: Test times out before completion

**Solution**:
- Increase Common Test timeout: `--timeout <seconds>`
- Check system resources (CPU, memory)
- Verify NATS/JetStream availability

### Resource Leaks Detected

**Problem**: Test fails with resource leak

**Investigation**:
1. Check detailed snapshots in report
2. Identify which resource is leaking (memory, processes, ETS)
3. Review resource growth rate
4. Check for specific processes/components causing leaks

**Common Causes**:
- Unclosed file descriptors
- Accumulating ETS entries
- Process spawn without cleanup
- Memory not garbage collected

### Performance Degradation Detected

**Problem**: Test fails with performance degradation

**Investigation**:
1. Check latency trends in snapshots
2. Identify which operations are degrading
3. Review throughput trends
4. Check queue sizes

**Common Causes**:
- Resource exhaustion
- Backpressure from downstream
- Fault injection causing retry storms
- Memory pressure causing GC pauses

## CI/CD Integration

### Nightly Runs

**Recommended**: Run 4-hour multi-fault soak test nightly

```yaml
# .github/workflows/nightly-stress-soak.yml
- name: Run Multi-Fault Soak Test
  run: |
    cd apps/otp/router
    ./scripts/run_stress_soak_test.sh test_multi_fault_triple_soak 4
```

### Pre-Release Runs

**Recommended**: Run full suite (all scenarios, 6-8 hours each)

```bash
# Run all scenarios
for test in test_single_fault_connect_soak test_multi_fault_triple_soak test_baseline_normal_soak; do
    ./scripts/run_stress_soak_test.sh "$test" 6
done
```

### Scheduled Runs

**Weekly**: Full suite (all scenarios)
**Daily**: Quick validation (2-hour single-fault)

## Best Practices

### Development

1. **Start with short tests**: Use 1-hour tests during development
2. **Monitor resource usage**: Watch memory and process count during development
3. **Fix leaks early**: Don't wait for long tests to detect leaks

### Validation

1. **Run medium tests**: Use 4-hour tests for validation
2. **Compare with baseline**: Always compare with baseline soak test
3. **Document results**: Keep track of resource usage trends

### Production Readiness

1. **Run long tests**: Use 8+ hour tests before releases
2. **Full suite**: Run all scenarios before major releases
3. **Compare runs**: Track trends across multiple runs

## Performance Monitoring

### Using Performance Monitor

The performance monitor provides detailed latency and throughput tracking:

```erlang
%% Start monitor with performance tracking
{ok, Monitor} = router_stress_monitor:start(#{
    collection_interval_ms => 300000,
    enable_perf_monitor => true
}),

%% Record latencies in your test code
PerfMonitor = %% Get from monitor state or start separately
router_stress_perf_monitor:record_latency(PerfMonitor, message_processing, 5.2),
router_stress_perf_monitor:record_throughput(PerfMonitor, publish, 100),

%% Get statistics
Stats = router_stress_perf_monitor:get_stats(PerfMonitor),
LatencyStats = router_stress_perf_monitor:get_latency_stats(PerfMonitor, message_processing),
```

### Performance Degradation Detection

```erlang
%% Get baseline
Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),

%% ... run test ...

%% Check for degradation
{ok, no_degradation} = router_stress_perf_monitor:check_performance_degradation(
    PerfMonitor, Baseline, #{
        p95_latency_multiplier => 2.0,  %% P95 > 2x baseline = fail
        throughput_drop_percent => 30.0  %% Throughput drop >30% = fail
    }
)
```

## Comparing Test Runs

### Using Comparison Script

Compare two test runs to detect regressions:

```bash
./scripts/compare_stress_soak_runs.sh \
  stress_soak_results/test_baseline_normal_soak_20250126_120000 \
  stress_soak_results/test_baseline_normal_soak_20250127_120000
```

**Output**:
- `comparison_report.md`: Main comparison report
- `summary_comparison.txt`: Summary comparison
- `resource_comparison.txt`: Resource usage comparison

**Features**:
- Automatic regression detection
- Improvement detection
- Detailed resource comparison

## CI/CD Integration

### Nightly Runs

Nightly stress/soak tests run automatically every night at 2 AM UTC.

**Workflow**: `.github/workflows/router-stress-soak-nightly.yml`

**Default**:
- Test: `test_multi_fault_triple_soak`
- Duration: 4 hours
- Timeout: 8 hours

**Manual Trigger**:
- Can be triggered manually via GitHub Actions
- Customizable test case and duration

**Artifacts**:
- Test results stored for 30 days
- Summary in GitHub Actions
- Failure notifications via GitHub Issues

### PR Runs

Quick stress/soak tests run automatically on PRs affecting router code.

**Workflow**: `.github/workflows/router-stress-soak-pr.yml`

**Default**:
- Test: `test_single_fault_connect_soak`
- Duration: 1 hour
- Timeout: 2 hours

**Artifacts**:
- Test results stored for 7 days
- Fast feedback for developers

## References

- `STRESS_SOAK_TESTS_SPEC.md`: Detailed specification
- `STRESS_SOAK_NEXT_STEPS_COMPLETE.md`: Next steps completion summary
- `router_stress_monitor.erl`: Resource monitoring module
- `router_stress_perf_monitor.erl`: Performance monitoring module
- `router_stress_soak_SUITE.erl`: Test suite implementation
- `ADVANCED_CONCURRENT_FAULTS_SPEC.md`: Concurrent fault test specification

