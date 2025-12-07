# Stress/Soak Tests API Migration Guide

## Overview

This document describes the API changes in `router_stress_perf_monitor` and how to update test code to use the new API.

## API Changes

### Removed Functions

- `get_baseline/1` - Removed. Use `get_stats/1` after initial warm-up period instead.
- `generate_report/1` - Removed. Use `get_stats/1` to get current statistics.
- `check_degradation/2` - Replaced with `check_performance_degradation/3`.

### Changed Functions

- `record_throughput/2` → `record_throughput/3`
  - **Old**: `record_throughput(Pid, MessagesPerSec)`
  - **New**: `record_throughput(Pid, Operation, Count)`
  - Now requires operation name and count of operations

- `get_metrics/1` → `get_stats/1`
  - Returns comprehensive statistics instead of just metrics

### New Functions

- `get_latency_stats/2` - Get latency statistics for specific operation
- `get_throughput_stats/2` - Get throughput statistics for specific operation
- `get_queue_stats/2` - Get queue size statistics for specific operation
- `check_performance_degradation/3` - Check degradation with configurable thresholds
- `reset/1` - Reset all statistics

### Configuration Changes

- `window_size_ms` → `collection_window_ms` in `start/1` options

## Migration Steps

### 1. Update Monitor Initialization

**Before**:
```erlang
{ok, PerfMonitor} = router_stress_perf_monitor:start(#{window_size_ms => 60000}),
```

**After**:
```erlang
{ok, PerfMonitor} = router_stress_perf_monitor:start(#{collection_window_ms => 60000}),
```

### 2. Update Baseline Collection

**Before**:
```erlang
Baseline = router_stress_perf_monitor:get_baseline(PerfMonitor),
```

**After**:
```erlang
%% Wait for initial samples to establish baseline
timer:sleep(5000),
Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
```

### 3. Update Throughput Recording

**Before**:
```erlang
router_stress_perf_monitor:record_throughput(PerfMonitor, MessagesPerSec),
```

**After**:
```erlang
router_stress_perf_monitor:record_throughput(PerfMonitor, message_processing, Count),
```

### 4. Update Performance Degradation Check

**Before**:
```erlang
Result = router_stress_perf_monitor:check_degradation(PerfMonitor, Baseline),
```

**After**:
```erlang
Thresholds = #{
    p95_latency_multiplier => 2.0,
    p99_latency_multiplier => 3.0,
    throughput_drop_percent => 30.0
},
Result = router_stress_perf_monitor:check_performance_degradation(
    PerfMonitor, Baseline, Thresholds
),
```

### 5. Update Report Generation

**Before**:
```erlang
Report = router_stress_perf_monitor:generate_report(PerfMonitor),
```

**After**:
```erlang
Stats = router_stress_perf_monitor:get_stats(PerfMonitor),
```

## Performance Thresholds

### Default Thresholds

The suite uses the following default thresholds:

```erlang
Thresholds = #{
    p95_latency_multiplier => 2.0,      %% P95 latency >2x baseline
    p99_latency_multiplier => 3.0,       %% P99 latency >3x baseline
    throughput_drop_percent => 30.0      %% Throughput drop >30%
},
```

### Custom Thresholds

You can customize thresholds per test:

```erlang
StrictThresholds = #{
    p95_latency_multiplier => 1.5,      %% Stricter: 1.5x
    p99_latency_multiplier => 2.0,       %% Stricter: 2.0x
    throughput_drop_percent => 20.0      %% Stricter: 20% drop
},
```

## Statistics Structure

### Latency Statistics

```erlang
#{
    count => 1000,
    p50_ms => 5.2,
    p95_ms => 12.5,
    p99_ms => 25.0,
    mean_ms => 6.8,
    min_ms => 1.0,
    max_ms => 50.0
}
```

### Throughput Statistics

```erlang
#{
    count => 100,
    total_operations => 5000,
    duration_ms => 120000,
    throughput_per_sec => 41.67
}
```

### Queue Statistics

```erlang
#{
    count => 100,
    mean => 25.5,
    min => 0,
    max => 100,
    p95 => 80.0,
    p99 => 95.0
}
```

## Integration with Resource Monitor

The resource monitor (`router_stress_monitor`) can optionally integrate with the performance monitor:

```erlang
{ok, ResourceMonitor} = router_stress_monitor:start(#{
    collection_interval_ms => 300000,
    enable_perf_monitor => true  %% Automatically starts perf monitor
}),
```

When enabled, resource snapshots will include performance metrics from the perf monitor.

## Best Practices

1. **Baseline Collection**: Always wait 5-10 seconds after starting monitors before collecting baseline to allow initial samples.

2. **Operation Names**: Use consistent operation names:
   - `message_processing` - Message processing latency
   - `publish` - Publish operation latency
   - `ack` - ACK operation latency
   - `policy_lookup` - Policy lookup latency

3. **Threshold Selection**: 
   - Use stricter thresholds (1.5x, 20%) for critical operations
   - Use default thresholds (2.0x, 30%) for general operations
   - Adjust based on SLA requirements

4. **Periodic Collection**: Collect statistics periodically (every 1-5 minutes) to track trends over time.

5. **Reset Between Tests**: Use `reset/1` to clear statistics between test runs if needed.

## Example: Complete Test Setup

```erlang
test_example_soak(_Config) ->
    %% Start monitors
    {ok, ResourceMonitor} = router_stress_monitor:start(#{
        collection_interval_ms => 300000
    }),
    {ok, PerfMonitor} = router_stress_perf_monitor:start(#{
        collection_window_ms => 60000
    }),
    
    %% Collect baseline
    router_stress_monitor:collect_snapshot(ResourceMonitor),
    timer:sleep(5000),
    Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    %% Run test...
    
    %% Evaluate results
    ResourceLeakResult = router_stress_monitor:check_resource_leaks(ResourceMonitor),
    CurrentStats = router_stress_perf_monitor:get_stats(PerfMonitor),
    
    Thresholds = #{
        p95_latency_multiplier => 2.0,
        throughput_drop_percent => 30.0
    },
    PerfResult = router_stress_perf_monitor:check_performance_degradation(
        PerfMonitor, Baseline, Thresholds
    ),
    
    %% Cleanup
    router_stress_monitor:stop(ResourceMonitor),
    router_stress_perf_monitor:stop(PerfMonitor),
    
    ok.
```

## References

- `router_stress_perf_monitor.erl`: Performance monitor implementation
- `router_stress_monitor.erl`: Resource monitor implementation
- `router_stress_soak_SUITE.erl`: Updated test suite examples
- `STRESS_SOAK_TESTS_USAGE.md`: Usage guide

