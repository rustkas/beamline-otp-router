# Extended Recovery Scenarios Test Suite

## Overview

The `router_jetstream_extended_recovery_SUITE` test suite provides **long-running recovery scenario tests** for the NATS JetStream-based router system. These tests focus on:

- **Extended fault/recovery cycles** (minutes to hours)
- **Performance degradation detection** over time
- **Resource leak detection** (memory, processes, connections)
- **Throughput/latency recovery measurement**
- **MaxDeliver exhaustion accumulation** scenarios

## Quick Start

### Run Single Scenario

```bash
cd apps/otp/router
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --case test_maxdeliver_gradual_accumulation
```

### Run All Scenarios

```bash
# Warning: May take several hours
rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

### Run by Category

```bash
# MaxDeliver scenarios only
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --group maxdeliver_scenarios

# Performance scenarios only
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --group performance_scenarios
```

## Test Categories

### MaxDeliver Scenarios

- **`test_maxdeliver_gradual_accumulation`** (~30 min): Gradual delivery count accumulation
- **`test_maxdeliver_mass_exhaustion`** (~18 min): Mass MaxDeliver exhaustion
- **`test_maxdeliver_periodic_consumer_hang`** (~45 min): Periodic consumer hang cycles

### Restart Scenarios

- **`test_repeated_jetstream_restarts`** (~75 min): Repeated JetStream node restarts
- **`test_repeated_router_restarts`** (~75 min): Repeated router process restarts
- **`test_network_partition_recovery`** (~75 min): Network partition recovery

### Combined Scenarios

- **`test_sequential_fault_chain`** (~110 min): Sequential fault chain
- **`test_repeating_fault_cycles`** (~135 min): Repeating fault cycles

### Performance Scenarios

- **`test_long_running_stability`** (~240 min / 4 hours): Long-running stability
- **`test_recovery_time_measurement`** (~17 min per fault type): Recovery time measurement

## Configuration

### Environment Variables

- `EXTENDED_TEST_DURATION_MIN`: Override default test duration
- `EXTENDED_TEST_MESSAGE_RATE`: Messages per second (default: 100)
- `EXTENDED_TEST_FAULT_INTERVAL_SEC`: Interval between faults (default: 600)

### Example

```bash
EXTENDED_TEST_DURATION_MIN=60 rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

## Test Results

### Success Criteria

- ✅ **No message loss** (except expected MaxDeliver exhaustion)
- ✅ **Recovery time** < 5 minutes
- ✅ **No resource leaks** (process count, memory stable)
- ✅ **Performance stability** (throughput/latency stable over time)

### Metrics Collected

- **Functional**: Message count, MaxDeliver exhausted, DLQ count
- **Performance**: Throughput (msg/s), Latency (p50, p95, p99)
- **Resources**: Process count, Memory usage, Connection count

## Relationship to Other Test Suites

### Existing Test Suites

- **`router_jetstream_fault_injection_SUITE`**: Short fault injection tests (< 5 min)
- **`router_concurrent_faults_stress_SUITE`**: Concurrent fault stress tests (30 sec - 5 min)

### Extended Recovery Suite

- **Focus**: Long-running scenarios (minutes to hours)
- **Purpose**: Performance tracking, resource leak detection, extended stability

## CI/CD Integration

### Pull Request Checks

**Use existing suites** for fast feedback:
```bash
rebar3 ct --suite router_jetstream_fault_injection_SUITE
```

### Nightly Jobs

**Use extended recovery suite** for comprehensive coverage:
```bash
rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

**Note**: Extended recovery tests may run for hours. Configure CI/CD with appropriate timeouts.

## Documentation

- **`EXTENDED_RECOVERY_SCENARIOS_SPEC.md`**: Detailed scenario specifications
- **`EXTENDED_RECOVERY_COVERAGE.md`**: Coverage analysis and gap analysis

## Troubleshooting

### Test Timeout

If tests timeout, check:
- CI/CD timeout settings (should be hours for full suite)
- System resources (memory, CPU)
- NATS/JetStream availability

### Resource Leaks Detected

If resource leaks are detected:
- Check process count growth
- Check memory usage trends
- Review ETS table sizes
- Check connection count

### Performance Degradation

If performance degrades:
- Check throughput trends
- Check latency trends
- Review metric snapshots
- Check for accumulated errors

## References

- `router_jetstream_extended_recovery_SUITE.erl`: Test implementation
- `EXTENDED_RECOVERY_SCENARIOS_SPEC.md`: Scenario specifications
- `EXTENDED_RECOVERY_COVERAGE.md`: Coverage analysis
- `router_jetstream_fault_injection_SUITE.erl`: Existing fault injection tests

