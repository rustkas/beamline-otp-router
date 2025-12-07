# Extended Recovery Scenarios Specification

## Overview

This document defines the **Extended Recovery Scenarios** test suite for NATS JetStream-based router system. These scenarios focus on **long-running, multi-minute/multi-hour tests** that verify system behavior under sustained fault conditions and recovery cycles.

**Purpose**: Close the gap between existing short fault injection tests and real-world production scenarios where systems experience repeated failures and recoveries over extended periods.

## Relationship to Existing Tests

### Existing Test Suites

- **`router_jetstream_fault_injection_SUITE`**: Short fault injection tests (seconds to minutes)
  - Focus: Correctness under single fault events
  - Duration: < 5 minutes per test
  - Coverage: Connection loss, consumer reconnection, stream availability

- **`router_concurrent_faults_stress_SUITE`**: Concurrent fault stress tests
  - Focus: Multiple simultaneous faults
  - Duration: 30 seconds to 5 minutes
  - Coverage: Concurrent faults, tenant isolation, multiple recovery cycles

### Gap Analysis

**Missing Coverage**:
- ❌ Long-running scenarios (hours, not minutes)
- ❌ Performance degradation tracking over time
- ❌ Resource leak detection in extended runs
- ❌ MaxDeliver exhaustion accumulation scenarios
- ❌ Repeated fault/recovery cycles (10+ cycles)
- ❌ Throughput/latency recovery metrics
- ❌ Memory/CPU growth tracking

**Extended Recovery Scenarios Address**:
- ✅ Long-running tests (minutes to hours)
- ✅ Performance metrics collection (throughput, latency, resource usage)
- ✅ Resource leak detection (processes, memory, connections)
- ✅ MaxDeliver exhaustion accumulation and recovery
- ✅ Repeated fault/recovery cycles with stability verification
- ✅ Throughput/latency recovery time measurement

## Scenario Categories

### Category 1: MaxDeliver Exhaustion Scenarios

#### Scenario 1.1: Gradual MaxDeliver Accumulation

**Description**: Messages gradually accumulate delivery count until MaxDeliver is exhausted.

**Phases**:
1. **Baseline Phase** (5 minutes):
   - Normal message processing
   - Measure baseline throughput/latency
   - Track delivery counts

2. **Accumulation Phase** (10 minutes):
   - Inject intermittent ACK failures (30% failure rate)
   - Messages accumulate delivery counts
   - Track messages approaching MaxDeliver

3. **Exhaustion Phase** (5 minutes):
   - Messages reach MaxDeliver limit
   - Verify DLQ handling
   - Verify metrics emission

4. **Recovery Phase** (10 minutes):
   - Remove ACK failures
   - Verify new messages process normally
   - Verify exhausted messages don't re-deliver

**Success Criteria**:
- ✅ No message loss (except expected MaxDeliver exhaustion)
- ✅ DLQ receives exhausted messages
- ✅ MaxDeliver exhausted metrics emitted correctly
- ✅ New messages process at baseline throughput after recovery
- ✅ No "stuck" messages in stream

**Metrics Collected**:
- Delivery count distribution
- MaxDeliver exhausted count
- DLQ message count
- Throughput (messages/second)
- Latency (p50, p95, p99)

#### Scenario 1.2: Mass MaxDeliver Exhaustion

**Description**: Many messages exhaust MaxDeliver simultaneously.

**Phases**:
1. **Baseline Phase** (3 minutes): Normal processing
2. **Mass Failure Phase** (5 minutes):
   - Inject 100% ACK failure rate
   - Process 1000+ messages
   - All messages exhaust MaxDeliver
3. **Recovery Phase** (10 minutes):
   - Remove failures
   - Verify system recovers
   - Verify no performance degradation

**Success Criteria**:
- ✅ All exhausted messages sent to DLQ
- ✅ System remains stable during mass exhaustion
- ✅ No performance degradation after recovery
- ✅ No resource leaks

#### Scenario 1.3: Periodic Consumer Hang

**Description**: Consumer periodically "hangs" (doesn't ACK), causing delivery count accumulation.

**Phases**:
1. **Baseline Phase** (5 minutes): Normal processing
2. **Hang Cycles** (30 minutes):
   - Every 2 minutes: Consumer hangs for 30 seconds
   - Messages accumulate delivery counts
   - After hang: Consumer resumes, messages redeliver
3. **Recovery Phase** (10 minutes):
   - No more hangs
   - Verify system recovers
   - Verify no accumulated issues

**Success Criteria**:
- ✅ System handles periodic hangs gracefully
- ✅ Messages eventually process or exhaust MaxDeliver
- ✅ No unbounded delivery count growth
- ✅ Performance returns to baseline after recovery

### Category 2: JetStream/Router Restart Scenarios

#### Scenario 2.1: Repeated JetStream Node Restarts

**Description**: JetStream node restarts repeatedly over extended period.

**Phases**:
1. **Baseline Phase** (5 minutes): Normal processing
2. **Restart Cycles** (60 minutes):
   - Every 5 minutes: Restart JetStream node
   - Process messages during restarts
   - Verify reconnection
3. **Recovery Phase** (10 minutes):
   - No more restarts
   - Verify stable operation
   - Verify no accumulated issues

**Success Criteria**:
- ✅ Router reconnects after each restart
- ✅ No message loss
- ✅ Consumer state preserved
- ✅ No performance degradation over cycles
- ✅ No resource leaks

**Metrics Collected**:
- Reconnection time
- Messages processed per cycle
- Throughput during/after restarts
- Memory usage over time

#### Scenario 2.2: Router Process Restarts

**Description**: Router processes restart repeatedly.

**Phases**:
1. **Baseline Phase** (5 minutes): Normal processing
2. **Restart Cycles** (60 minutes):
   - Every 3 minutes: Restart router process
   - Verify state recovery
   - Verify message processing resumes
3. **Recovery Phase** (10 minutes):
   - No more restarts
   - Verify stable operation

**Success Criteria**:
- ✅ Router recovers state after restart
- ✅ No message loss
- ✅ ETS state preserved
- ✅ Consumer subscriptions restored
- ✅ No performance degradation

#### Scenario 2.3: Network Partition Recovery

**Description**: Network partitions between router and JetStream.

**Phases**:
1. **Baseline Phase** (5 minutes): Normal processing
2. **Partition Cycles** (60 minutes):
   - Every 4 minutes: Network partition for 1 minute
   - Verify connection loss handling
   - Verify reconnection after partition ends
3. **Recovery Phase** (10 minutes):
   - No more partitions
   - Verify stable operation

**Success Criteria**:
- ✅ Router handles partitions gracefully
- ✅ Reconnects after partition ends
- ✅ No message loss
- ✅ Backlog processes correctly
- ✅ No performance degradation

### Category 3: Combined Fault Scenarios

#### Scenario 3.1: Sequential Fault Chain

**Description**: Multiple different faults occur sequentially in one long test.

**Phases**:
1. **Baseline Phase** (5 minutes): Normal processing
2. **Fault Chain** (90 minutes):
   - **Phase A** (15 min): Network partition
   - **Phase B** (15 min): JetStream node restart
   - **Phase C** (15 min): MaxDeliver exhaustion accumulation
   - **Phase D** (15 min): Router process restart
   - **Phase E** (15 min): Consumer hang cycles
   - **Phase F** (15 min): Combined faults (partition + restart)
3. **Recovery Phase** (15 minutes):
   - All faults removed
   - Verify complete recovery
   - Verify no accumulated issues

**Success Criteria**:
- ✅ System handles each fault phase correctly
- ✅ No accumulated issues from previous phases
- ✅ Performance returns to baseline after recovery
- ✅ No resource leaks

#### Scenario 3.2: Repeating Fault Cycles

**Description**: Same fault pattern repeats many times.

**Phases**:
1. **Baseline Phase** (5 minutes): Normal processing
2. **Repeating Cycles** (120 minutes):
   - Every 10 minutes:
     - Fault injected (2 minutes)
     - Recovery period (8 minutes)
     - Measure metrics
   - Repeat 12 times
3. **Recovery Phase** (10 minutes):
   - No more faults
   - Verify stable operation

**Success Criteria**:
- ✅ System behaves consistently across cycles
- ✅ Performance in cycle 12 >= performance in cycle 1
- ✅ No resource leaks
- ✅ No accumulated errors

**Metrics Collected**:
- Throughput per cycle
- Latency per cycle
- Memory usage per cycle
- Process count per cycle
- Error rate per cycle

### Category 4: Performance Degradation Detection

#### Scenario 4.1: Long-Running Stability

**Description**: System runs for extended period with normal load.

**Phases**:
1. **Extended Run** (240 minutes / 4 hours):
   - Normal message processing
   - Continuous load (100 msg/s)
   - Periodic metric collection
2. **Analysis Phase**:
   - Verify no performance degradation
   - Verify no resource leaks
   - Verify stable metrics

**Success Criteria**:
- ✅ Throughput stable over time
- ✅ Latency stable over time
- ✅ Memory usage stable (no unbounded growth)
- ✅ Process count stable
- ✅ No error accumulation

**Metrics Collected** (every 5 minutes):
- Throughput (messages/second)
- Latency (p50, p95, p99)
- Memory usage (MB)
- Process count
- ETS table sizes
- Connection count

#### Scenario 4.2: Recovery Time Measurement

**Description**: Measure time to recover throughput/latency after different fault types.

**Phases**:
1. **Baseline Phase** (5 minutes): Measure baseline
2. **Fault Injection** (2 minutes): Inject fault
3. **Recovery Measurement** (10 minutes):
   - Remove fault
   - Measure time to return to baseline
   - Track throughput/latency recovery curve

**Success Criteria**:
- ✅ Recovery time < 5 minutes for all fault types
- ✅ Throughput returns to baseline
- ✅ Latency returns to baseline

**Fault Types Tested**:
- Network partition
- JetStream restart
- Router restart
- ACK failures
- Consumer hang

## Metrics and Observability

### Required Metrics

**Functional Metrics**:
- Message processing count
- MaxDeliver exhausted count
- DLQ message count
- Redelivery count
- Connection state changes
- Consumer reconnection count

**Performance Metrics**:
- Throughput (messages/second)
- Latency (p50, p95, p99)
- Recovery time (time to return to baseline)

**Resource Metrics**:
- Memory usage (MB)
- Process count
- ETS table sizes
- Connection count
- CPU usage (if available)

### Metric Collection Points

**Phase Boundaries**:
- Start of baseline phase
- Start of fault phase
- End of fault phase
- Start of recovery phase
- End of recovery phase

**Periodic Collection**:
- Every 30 seconds during long phases
- Every 5 minutes during extended runs

### Diagnostic Information

**On Test Failure**:
- Phase where failure occurred
- Metrics at failure point
- Resource usage at failure
- Error logs
- ETS table dumps (if applicable)

## Test Configuration

### Environment Variables

- `EXTENDED_TEST_DURATION_MIN`: Override default test duration (default: varies by scenario)
- `EXTENDED_TEST_MESSAGE_RATE`: Messages per second (default: 100)
- `EXTENDED_TEST_FAULT_INTERVAL_SEC`: Interval between faults in repeating cycles (default: 600)
- `EXTENDED_TEST_ENABLE_RESOURCE_TRACKING`: Enable detailed resource tracking (default: true)

### Test Execution

**Local Development**:
```bash
cd apps/otp/router
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --case test_maxdeliver_gradual_accumulation
```

**CI/CD (Nightly)**:
```bash
# Run all extended scenarios (may take hours)
rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

**Specific Scenario**:
```bash
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --case test_sequential_fault_chain
```

## Success Criteria Summary

### Functional Criteria

- ✅ **No unexplained message loss** (except expected MaxDeliver exhaustion)
- ✅ **Predictable MaxDeliver behavior**: Messages either deliver or go to DLQ
- ✅ **Correct recovery**: New messages process normally after recovery
- ✅ **No stuck messages**: Backlog processes or exhausts correctly

### Performance Criteria

- ✅ **Recovery time**: < 5 minutes to return to baseline
- ✅ **Resource stability**: No unbounded growth (memory, processes, connections)
- ✅ **Performance stability**: Cycle N performance >= Cycle 1 performance

### Stability Criteria

- ✅ **No resource leaks**: Memory/process count stable over time
- ✅ **No error accumulation**: Error rate doesn't grow unbounded
- ✅ **Consistent behavior**: System behaves consistently across cycles

## Test Coverage Matrix

| Scenario | Duration | MaxDeliver | Restarts | Partitions | Combined | Performance |
|----------|----------|------------|----------|------------|----------|-------------|
| 1.1: Gradual Accumulation | 30 min | ✅ | ❌ | ❌ | ❌ | ✅ |
| 1.2: Mass Exhaustion | 18 min | ✅ | ❌ | ❌ | ❌ | ✅ |
| 1.3: Periodic Hang | 45 min | ✅ | ❌ | ❌ | ❌ | ✅ |
| 2.1: JS Restarts | 75 min | ❌ | ✅ | ❌ | ❌ | ✅ |
| 2.2: Router Restarts | 75 min | ❌ | ✅ | ❌ | ❌ | ✅ |
| 2.3: Network Partitions | 75 min | ❌ | ❌ | ✅ | ❌ | ✅ |
| 3.1: Sequential Chain | 110 min | ✅ | ✅ | ✅ | ✅ | ✅ |
| 3.2: Repeating Cycles | 135 min | ❌ | ✅ | ✅ | ❌ | ✅ |
| 4.1: Long Stability | 240 min | ❌ | ❌ | ❌ | ❌ | ✅ |
| 4.2: Recovery Time | 17 min | ❌ | ✅ | ✅ | ❌ | ✅ |

## Implementation Notes

### Test Structure

- **Separate test case per scenario**: Each scenario is a separate test case
- **Helper modules**: Shared helpers for metrics, resource tracking, fault injection
- **Configurable duration**: Environment variables for test duration
- **Graceful termination**: Tests can be stopped early with results

### Resource Tracking

- **ETS tables**: Track delivery counts, metrics, state
- **Process monitoring**: Track process count over time
- **Memory monitoring**: Track memory usage (if available)
- **Connection monitoring**: Track NATS connections

### Fault Injection

- **Reuse existing infrastructure**: Use `router_nats_fault_injection` module
- **Extended fault types**: Add long-duration faults (network partition simulation)
- **Fault scheduling**: Support scheduled faults (every N minutes)

## References

- `router_jetstream_fault_injection_SUITE.erl`: Existing fault injection tests
- `router_concurrent_faults_stress_SUITE.erl`: Concurrent fault stress tests
- `router_nats_fault_injection.erl`: Fault injection infrastructure
- `docs/NATS_CONNECTION_RESILIENCE.md`: NATS resilience documentation

