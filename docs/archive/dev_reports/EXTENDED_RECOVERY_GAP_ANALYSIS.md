# Extended Recovery Scenarios - Gap Analysis

## Overview

This document provides a **gap analysis** comparing Extended Recovery Scenarios with existing JetStream fault injection tests documented in `JETSTREAM_FAULT_INJECTION_TESTS.md`. The analysis identifies what Extended Recovery explicitly covers, how it relates to existing scenarios (S1, S2, S3), and remaining gaps.

## Existing Test Coverage (from JETSTREAM_FAULT_INJECTION_TESTS.md)

### Scenario S1: Single Fault Injection / Localized Failures

**Focus**: Individual fault events with immediate recovery verification

**Typical Coverage**:
- Single connection loss and recovery
- Single consumer reconnection
- Single stream unavailability
- Single MaxDeliver exhaustion event
- ETS state preservation during single restart

**Duration**: Seconds to minutes per test

**Status**: ✅ **Implemented** in `router_jetstream_fault_injection_SUITE`

### Scenario S2: Concurrent Faults / Stress

**Focus**: Multiple simultaneous faults under stress conditions

**Typical Coverage**:
- Concurrent connect + publish faults
- Concurrent publish + ACK/NAK faults
- Concurrent validation + publish faults
- Tenant isolation under concurrent faults
- Multiple recovery cycles (limited to 5 cycles)

**Duration**: 30 seconds to 5 minutes per test

**Status**: ✅ **Implemented** in `router_concurrent_faults_stress_SUITE`

### Scenario S3: MaxDeliver / Delivery Guarantees

**Focus**: Correctness of MaxDeliver behavior and delivery guarantees

**Typical Coverage**:
- Delivery count tracking under ACK failures
- Processing delays with redelivery and delivery count
- MaxDeliver exhaustion metric emission
- Tracking cleanup after exhaustion
- Dead-letter queue handling

**Duration**: Minutes per test

**Status**: ✅ **Implemented** in `router_delivery_count_tracking_SUITE`

## Extended Recovery Coverage

### What Extended Recovery Explicitly Covers

#### 1. Long-Running and Performance-Oriented Scenarios

**Gap Closed**: Existing tests focus on correctness but lack long-running scenarios

**Extended Recovery Adds**:
- ✅ **Extended duration tests**: 17 minutes to 4 hours per scenario
- ✅ **Performance tracking**: Throughput and latency measurement over time
- ✅ **Resource leak detection**: Memory, process, connection tracking over extended periods
- ✅ **Stability verification**: System behavior consistency across multiple cycles

**Examples**:
- `test_long_running_stability`: 4-hour stability test
- `test_repeating_fault_cycles`: 12+ cycles over 135 minutes
- `test_sequential_fault_chain`: 110-minute sequential fault chain

#### 2. Production-Scale Scenarios

**Gap Closed**: Existing tests focus on single-component failures

**Extended Recovery Adds**:
- ✅ **Multi-node cluster failures**: `test_multi_node_jetstream_cluster_failures`
  - Simulates 3-node JetStream cluster
  - Verifies quorum maintenance
  - Tests graceful degradation
- ✅ **Cross-region partitions**: `test_cross_region_network_partitions`
  - Simulates network partitions between regions
  - Verifies partition handling
  - Tests automatic reconnection
- ✅ **Rolling restart zero downtime**: `test_rolling_restart_zero_downtime`
  - Simulates rolling restart of router instances
  - Verifies zero downtime requirement
  - Tests state preservation

#### 3. Formal Resource Limits and Baseline

**Gap Closed**: Existing tests verify correctness but lack formal performance thresholds

**Extended Recovery Adds**:
- ✅ **Performance baseline**: Script for establishing baseline metrics
- ✅ **Formal resource limits**: Documented thresholds (Warning/Failure) for all resource types
- ✅ **CI/CD integration**: Automated baseline comparison and limit enforcement
- ✅ **Statistical analysis**: 95% confidence intervals for thresholds

**Resources with Defined Limits**:
- Process count
- Memory usage
- Connection count
- ETS table sizes
- Throughput (percentage of baseline)
- Latency (percentage increase from baseline)

#### 4. Extended MaxDeliver Scenarios

**Gap Closed**: Existing S3 tests verify single MaxDeliver events

**Extended Recovery Adds**:
- ✅ **Gradual accumulation**: `test_maxdeliver_gradual_accumulation` (30 minutes)
  - Messages gradually accumulate delivery count
  - Tracks accumulation over extended period
  - Verifies DLQ handling and recovery
- ✅ **Mass exhaustion**: `test_maxdeliver_mass_exhaustion` (18 minutes)
  - Many messages exhaust MaxDeliver simultaneously
  - Verifies system stability during mass exhaustion
  - Tests recovery after mass exhaustion
- ✅ **Periodic consumer hang**: `test_maxdeliver_periodic_consumer_hang` (45 minutes)
  - Consumer periodically "hangs" causing accumulation
  - Verifies system handles periodic hangs gracefully
  - Tests recovery after hang cycles

## Mapping to Existing Scenarios

### Extended Recovery vs S1 (Single Fault Injection)

**S1 Coverage**:
- Single fault events
- Immediate recovery verification
- Short duration (seconds to minutes)

**Extended Recovery Extension**:
- **Scales S1 to extended duration**: Same fault types but over hours instead of minutes
- **Adds performance measurement**: Tracks throughput/latency during and after recovery
- **Adds resource tracking**: Monitors resource usage during extended runs
- **Example**: `test_repeated_jetstream_restarts` extends single restart to 12+ cycles over 75 minutes

**Relationship**: Extended Recovery **extends** S1 scenarios to long-running contexts with performance tracking

### Extended Recovery vs S2 (Concurrent Faults)

**S2 Coverage**:
- Multiple simultaneous faults
- Stress conditions
- Limited cycles (5 cycles)

**Extended Recovery Extension**:
- **Extends cycle count**: From 5 cycles to 12+ cycles
- **Adds stability measurement**: Verifies performance doesn't degrade across cycles
- **Adds resource leak detection**: Tracks resource usage across extended cycles
- **Example**: `test_repeating_fault_cycles` extends concurrent faults to 12 cycles over 135 minutes

**Relationship**: Extended Recovery **scales** S2 scenarios to extended cycles with stability verification

### Extended Recovery vs S3 (MaxDeliver)

**S3 Coverage**:
- Single MaxDeliver exhaustion events
- Delivery count tracking
- DLQ handling verification

**Extended Recovery Extension**:
- **Adds accumulation scenarios**: Gradual and mass MaxDeliver exhaustion
- **Adds periodic patterns**: Consumer hang cycles causing accumulation
- **Adds performance impact**: Measures throughput/latency during accumulation
- **Example**: `test_maxdeliver_gradual_accumulation` extends single exhaustion to 30-minute accumulation

**Relationship**: Extended Recovery **extends** S3 scenarios to accumulation patterns with performance tracking

## Remaining Gaps

### 1. Business-Critical Message Flow Coverage

**Gap**: Extended Recovery focuses on general Router/JetStream patterns

**Missing**:
- Long-running scenarios for specific business-critical message flows
- End-to-end scenarios with actual business logic
- Real-world message pattern replay

**Recommendation**: Extend Extended Recovery with business-flow-specific scenarios

### 2. Edge Case JetStream Configurations

**Gap**: Extended Recovery uses typical JetStream configurations

**Missing**:
- Different retention policies (WorkQueue, Limits, Interest)
- Different ack policies (Explicit, All, None)
- Different deliver policies (All, Last, New, ByStartTime)
- Edge case combinations that are important in production

**Recommendation**: Add configuration matrix to Extended Recovery scenarios

### 3. Version Upgrade/Downgrade Scenarios

**Gap**: Extended Recovery runs on fixed versions (OTP matrix, single NATS version)

**Missing**:
- NATS version upgrade scenarios
- Router version upgrade scenarios
- Downgrade scenarios
- Rolling upgrade with zero downtime

**Recommendation**: Add upgrade/downgrade scenarios to Extended Recovery

### 4. Production Data Replay

**Gap**: Extended Recovery uses synthetic workload

**Missing**:
- Scenarios with production data replay
- Real-world message patterns
- Production-scale message volumes
- Production-like message sizes and distributions

**Recommendation**: Add production data replay capability (if production data available)

### 5. Real Infrastructure Setup

**Gap**: Extended Recovery simulates multi-node, multi-region, multi-instance scenarios

**Missing**:
- Real multi-node JetStream cluster setup in tests
- Real multi-region NATS setup in tests
- Real multi-instance router deployment in tests
- Actual network partition injection (iptables, network namespaces)

**Recommendation**: Add real infrastructure setup for production-scale scenarios (future enhancement)

### 6. Extended Duration Tests

**Gap**: Longest Extended Recovery test is 4 hours

**Missing**:
- 24-hour stability tests
- Week-long soak tests
- Continuous fault injection over days
- Long-term resource leak detection (beyond 4 hours)

**Recommendation**: Add extended duration test profiles (24+ hours)

### 7. Operational Change Scenarios

**Gap**: Extended Recovery focuses on fault/recovery, not operational changes

**Missing**:
- Configuration change scenarios
- Schema migration scenarios
- Policy update scenarios
- Capacity scaling scenarios

**Recommendation**: Add operational change scenarios to Extended Recovery

## Coverage Matrix

| Scenario Type | S1 (Single Fault) | S2 (Concurrent) | S3 (MaxDeliver) | Extended Recovery | Gap Status |
|---------------|-------------------|-----------------|-----------------|-------------------|------------|
| Short faults | ✅ | ✅ | ✅ | ❌ Not needed | ✅ Complete |
| Long-running faults | ❌ | ⚠️ Limited | ❌ | ✅ | ✅ Complete |
| Performance tracking | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Resource leak detection | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Recovery time measurement | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Production-scale | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Multi-node cluster | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Cross-region partitions | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Rolling restart | ❌ | ❌ | ❌ | ✅ | ✅ Complete |
| Business-flow specific | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |
| Edge configurations | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |
| Version upgrades | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |
| Production data replay | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |
| Real infrastructure | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |
| 24+ hour tests | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |
| Operational changes | ❌ | ❌ | ❌ | ❌ | ⚠️ Gap |

## Recommendations

### Immediate Actions

1. ✅ **Completed**: Extended Recovery implementation closes major gaps in long-running and production-scale scenarios
2. ✅ **Completed**: Formal resource limits and baseline establishment
3. ✅ **Completed**: CI/CD integration for automated extended test execution

### Future Enhancements

1. **Business-flow coverage**: Add scenarios for specific business-critical message flows
2. **Configuration matrix**: Test edge case JetStream configurations
3. **Version upgrades**: Add upgrade/downgrade scenarios
4. **Real infrastructure**: Move from simulation to real multi-node/multi-region setup
5. **Extended duration**: Add 24+ hour test profiles
6. **Operational changes**: Add configuration change and migration scenarios

## Conclusion

Extended Recovery Scenarios **successfully close the major gaps** in long-running, performance-oriented, and production-scale testing. The system provides:

- ✅ Comprehensive long-running scenario coverage
- ✅ Formal performance baseline and resource limits
- ✅ Production-scale scenario simulation
- ✅ CI/CD integration for automated execution

**Remaining gaps** are primarily in:
- Business-flow-specific scenarios
- Edge case configurations
- Operational change scenarios
- Real infrastructure setup (vs simulation)

These gaps can be addressed in future iterations as needed.

