# Extended Recovery Scenarios Test Coverage

## Overview

This document provides a **coverage analysis** for Extended Recovery Scenarios tests and their relationship to existing fault injection tests.

## Test Suite Comparison

### Existing Test Suites

#### `router_jetstream_fault_injection_SUITE`

**Focus**: Short fault injection tests (seconds to minutes)

**Coverage**:
- ✅ Connection loss and recovery
- ✅ Consumer reconnection
- ✅ Stream availability after recovery
- ✅ ETS state preservation
- ✅ Redelivery metrics
- ✅ MaxDeliver exhausted metrics

**Duration**: < 5 minutes per test

**Gap**: No long-running scenarios, no performance degradation tracking

#### `router_concurrent_faults_stress_SUITE`

**Focus**: Concurrent fault stress tests

**Coverage**:
- ✅ Concurrent faults (connect + publish)
- ✅ Tenant isolation under faults
- ✅ Multiple recovery cycles (5 cycles)

**Duration**: 30 seconds to 5 minutes

**Gap**: Limited to 5 cycles, no extended performance tracking

#### `router_concurrent_faults_SUITE`

**Focus**: Multiple simultaneous faults

**Coverage**:
- ✅ Concurrent fault combinations
- ✅ Edge cases (restart storms, poison messages)
- ✅ Backoff timing verification

**Duration**: < 10 minutes per test

**Gap**: No long-running scenarios, no resource leak detection

### New Test Suite: `router_jetstream_extended_recovery_SUITE`

**Focus**: Long-running recovery scenarios (minutes to hours)

**Coverage**:
- ✅ MaxDeliver exhaustion accumulation (30-45 minutes)
- ✅ Repeated fault/recovery cycles (60-135 minutes)
- ✅ Performance degradation detection (240 minutes)
- ✅ Resource leak detection
- ✅ Throughput/latency recovery measurement

**Duration**: 17 minutes to 4 hours per test

**Fills Gap**: Long-running scenarios, performance tracking, resource leak detection

## Coverage Matrix

### MaxDeliver Scenarios

| Scenario | Existing Tests | Extended Recovery | Gap Filled |
|----------|---------------|-------------------|------------|
| Single message MaxDeliver exhaustion | ✅ `router_jetstream_fault_injection_SUITE` | ❌ | - |
| Gradual accumulation over time | ❌ | ✅ `test_maxdeliver_gradual_accumulation` | Long-running accumulation |
| Mass exhaustion (1000+ messages) | ❌ | ✅ `test_maxdeliver_mass_exhaustion` | Scale testing |
| Periodic consumer hang | ❌ | ✅ `test_maxdeliver_periodic_consumer_hang` | Repeated hang cycles |

### Restart Scenarios

| Scenario | Existing Tests | Extended Recovery | Gap Filled |
|----------|---------------|-------------------|------------|
| Single NATS restart | ✅ `router_jetstream_fault_injection_SUITE` | ❌ | - |
| Single router restart | ✅ `router_concurrent_faults_SUITE` | ❌ | - |
| Repeated JetStream restarts (12+ cycles) | ❌ | ✅ `test_repeated_jetstream_restarts` | Extended cycles |
| Repeated router restarts (12+ cycles) | ❌ | ✅ `test_repeated_router_restarts` | Extended cycles |
| Network partition recovery | ❌ | ✅ `test_network_partition_recovery` | Partition scenarios |

### Combined Fault Scenarios

| Scenario | Existing Tests | Extended Recovery | Gap Filled |
|----------|---------------|-------------------|------------|
| Concurrent faults | ✅ `router_concurrent_faults_SUITE` | ❌ | - |
| Sequential fault chain | ❌ | ✅ `test_sequential_fault_chain` | Long fault sequences |
| Repeating fault cycles (12+ cycles) | ⚠️ Limited (5 cycles) | ✅ `test_repeating_fault_cycles` | Extended cycles |

### Performance Scenarios

| Scenario | Existing Tests | Extended Recovery | Gap Filled |
|----------|---------------|-------------------|------------|
| Short-term performance | ✅ Various suites | ❌ | - |
| Long-running stability (4+ hours) | ❌ | ✅ `test_long_running_stability` | Extended stability |
| Recovery time measurement | ❌ | ✅ `test_recovery_time_measurement` | Recovery metrics |

## Gap Analysis

### Covered by Existing Tests

**Short Fault Scenarios** (✅ Covered):
- Single connection loss
- Single consumer reconnection
- Single stream unavailability
- Single MaxDeliver exhaustion
- Concurrent faults (short duration)
- 5 recovery cycles

**Functional Correctness** (✅ Covered):
- Message processing correctness
- Metric emission
- State preservation
- Error handling

### Covered by Extended Recovery Scenarios

**Long-Running Scenarios** (✅ New Coverage):
- Gradual MaxDeliver accumulation (30 minutes)
- Mass MaxDeliver exhaustion (18 minutes)
- Periodic consumer hang (45 minutes)
- Repeated restarts (75 minutes)
- Sequential fault chain (110 minutes)
- Repeating fault cycles (135 minutes)
- Long-running stability (240 minutes)

**Performance Tracking** (✅ New Coverage):
- Throughput measurement over time
- Latency measurement over time
- Recovery time measurement
- Performance degradation detection

**Resource Leak Detection** (✅ New Coverage):
- Process count tracking
- Memory usage tracking
- Connection count tracking
- ETS table size tracking

### Not Yet Covered

**Production-Scale Scenarios** (❌ Not Covered):
- Multi-node JetStream cluster failures
- Cross-region network partitions
- Database connection pool exhaustion
- Disk space exhaustion

**Advanced Scenarios** (❌ Not Covered):
- Rolling restarts with zero downtime
- Canary deployment failures
- Configuration change failures
- Schema migration failures

## Test Execution Strategy

### Local Development

**Quick Tests** (Use existing suites):
```bash
# Short fault injection tests
rebar3 ct --suite router_jetstream_fault_injection_SUITE

# Concurrent fault tests
rebar3 ct --suite router_concurrent_faults_SUITE
```

**Extended Tests** (Use new suite):
```bash
# Single extended scenario (30-60 minutes)
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --case test_maxdeliver_gradual_accumulation

# All extended scenarios (may take hours)
rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

### CI/CD Integration

**Pull Request Checks** (Use existing suites):
- `router_jetstream_fault_injection_SUITE`: Fast feedback
- `router_concurrent_faults_SUITE`: Concurrent fault coverage

**Nightly/Long-Running Jobs** (Use new suite):
- `router_jetstream_extended_recovery_SUITE`: Extended scenarios
- Run as separate CI job with extended timeout
- May run for hours (especially `test_long_running_stability`)

### Test Selection

**By Duration**:
- **< 5 minutes**: Existing suites
- **5-30 minutes**: Extended recovery (selected scenarios)
- **30+ minutes**: Extended recovery (full suite, nightly only)

**By Focus**:
- **Functional correctness**: Existing suites
- **Performance/stability**: Extended recovery suite
- **Resource leaks**: Extended recovery suite

## Recommendations

### For Developers

1. **Use existing suites** for quick validation during development
2. **Use extended suite** for:
   - Performance regression testing
   - Resource leak detection
   - Long-running stability verification

### For CI/CD

1. **PR checks**: Run existing suites (fast feedback)
2. **Nightly jobs**: Run extended recovery suite (comprehensive coverage)
3. **Release validation**: Run full extended suite before releases

### For Production

1. **Monitor metrics** similar to extended recovery tests:
   - Throughput trends
   - Latency trends
   - Resource usage trends
   - Recovery times

2. **Use extended recovery scenarios** as:
   - Performance baselines
   - Recovery time SLAs
   - Resource usage limits

## Summary

### Coverage Status

| Category | Existing Tests | Extended Recovery | Total Coverage |
|----------|---------------|-------------------|----------------|
| Short faults | ✅ Complete | ❌ Not needed | ✅ Complete |
| Long-running faults | ❌ Missing | ✅ Complete | ✅ Complete |
| Performance tracking | ❌ Missing | ✅ Complete | ✅ Complete |
| Resource leak detection | ❌ Missing | ✅ Complete | ✅ Complete |
| Recovery time measurement | ❌ Missing | ✅ Complete | ✅ Complete |

### Test Suite Roles

- **`router_jetstream_fault_injection_SUITE`**: Functional correctness, fast feedback
- **`router_concurrent_faults_stress_SUITE`**: Concurrent fault handling
- **`router_jetstream_extended_recovery_SUITE`**: Long-running stability, performance, resource leaks

### Next Steps

1. ✅ **Implemented**: Extended recovery scenarios specification
2. ✅ **Implemented**: Extended recovery test suite
3. ⏳ **Pending**: CI/CD integration for nightly jobs
4. ⏳ **Pending**: Performance baseline establishment
5. ⏳ **Pending**: Resource usage limit definition

## References

- `EXTENDED_RECOVERY_SCENARIOS_SPEC.md`: Detailed scenario specifications
- `router_jetstream_extended_recovery_SUITE.erl`: Test implementation
- `router_jetstream_fault_injection_SUITE.erl`: Existing fault injection tests
- `router_concurrent_faults_stress_SUITE.erl`: Existing stress tests

