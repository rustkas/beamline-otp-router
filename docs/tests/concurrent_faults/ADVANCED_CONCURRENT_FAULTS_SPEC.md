# Advanced Concurrent Faults Test Specification

## Overview

This document describes the advanced concurrent fault test scenarios for JetStream router/client, covering complex simultaneous failure patterns that go beyond basic concurrent tests.

## Purpose

- **Verify system resilience** under simultaneous and multiple failures:
  - During `connect` (connection to cluster/JetStream/NATS)
  - During `publish` (message sending)
  - During `ack`/`nak` (message acknowledgment)

- **Ensure**:
  - No deadlocks, race conditions, resource leaks
  - No message loss beyond acceptable SLA
  - At-least-once delivery semantics preserved
  - System correctly recovers, retries, replays

## Test Suite

**File**: `apps/otp/router/test/router_advanced_concurrent_faults_SUITE.erl`

**Test Groups**:
- `triple_fault_tests`: Triple-fault scenarios (connect + publish + ack/nak simultaneously)
- `mixed_pattern_tests`: Mixed intermittent + persistent fault patterns
- `cascading_fault_tests`: Cascading fault chains

## Triple-Fault Scenarios

### Scenario Matrix

| Scenario | Connect Fault | Publish Fault | ACK/NAK Fault | Description |
|----------|---------------|--------------|---------------|-------------|
| **A** | Interruption during handshake | Part sent, part timeout | ACK doesn't reach client | Connection breaks during connect, publish partially succeeds, ACK lost |
| **B** | Network drops after few ms | Client continues trying | NAK fails (network down) | Connection established then drops, publish attempts continue, NAK fails |
| **C** | Flapping (up/down multiple times) | Parallel workers publish | ACKs lost/duplicated | Connection flaps, multiple publishers, ACK handling issues |

### Test Cases

#### `test_triple_fault_scenario_a_connect_publish_ack`

**Scenario**: Connect interruption + Publish partial success + ACK loss

**Faults**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`

**Expected Behavior**:
- Client performs correct retries
- No infinite hangs
- Queue state remains consistent
- No message loss beyond SLA
- System recovers after faults cleared

**Verification**:
- Process liveness (no deadlocks)
- Error metrics increase during fault
- Message semantics preserved (at-least-once)
- Recovery after faults cleared

#### `test_triple_fault_scenario_b_connect_publish_nak`

**Scenario**: Connection drops after establishment + Publish continues + NAK fails

**Faults**:
- `connect`: `close_connection`
- `publish`: `{error, timeout}`
- `nak`: `{error, connection_refused}`

**Expected Behavior**:
- No "hanging" messages stuck forever
- Correct retry logic
- No infinite retry loops
- Messages redelivered correctly

**Verification**:
- Redelivery metrics increase
- No message loss
- Process liveness maintained

#### `test_triple_fault_scenario_c_flapping_connect_publish_ack`

**Scenario**: Flapping connection + Parallel publish + ACK loss/duplication

**Faults**:
- `connect`: Flapping (enable/disable repeatedly)
- `publish`: `{error, timeout}` (during flapping)
- `ack`: `{error, timeout}` (during flapping)

**Expected Behavior**:
- No invariant violations (incorrect delivery count, wild duplicates)
- Correct handling of duplicate ACKs
- System stabilizes after flapping stops

**Verification**:
- Connection metrics reflect flapping
- No excessive redelivery
- Process liveness maintained

#### `test_triple_fault_connect_publish_ack_simultaneous`

**Scenario**: All three fault types occur simultaneously at the same moment

**Faults**: All enabled at once

**Expected Behavior**:
- System handles simultaneous faults correctly
- No race conditions
- Correct recovery after all faults cleared

#### `test_triple_fault_connect_publish_nak_simultaneous`

**Scenario**: Connect + Publish + NAK simultaneous faults

**Faults**: All enabled at once

**Expected Behavior**:
- System handles simultaneous faults correctly
- Messages redelivered correctly

## Mixed Pattern Scenarios (Intermittent + Persistent)

### Pattern Matrix

| Pattern | Intermittent Fault | Persistent Fault | Description |
|---------|-------------------|------------------|-------------|
| **1** | Connect (50% probability) | Publish (always fails) | Connection drops/recoveres, but publish always rejected |
| **2** | ACK (30% probability) | Connect (always fails) | Connection can't establish, but when it does, ACKs partially lost |
| **3** | Publish (40% probability) | ACK (always fails) | Publish sometimes succeeds, but ACK almost always fails |

### Test Cases

#### `test_mixed_intermittent_connect_persistent_publish`

**Scenario**: Intermittent connect + Persistent publish error

**Faults**:
- `connect`: Intermittent (50% probability) - `{intermittent, close_connection, 0.5}`
- `publish`: Persistent - `{error, quota_exceeded}`

**Expected Behavior**:
- Client doesn't waste resources on meaningless retries
- Correct logging/metrics
- System enters degraded mode gracefully

**Verification**:
- Publish failures increase significantly
- No excessive retry attempts
- Process liveness maintained

#### `test_mixed_persistent_connect_intermittent_ack`

**Scenario**: Persistent connect issue + Intermittent ACK loss

**Faults**:
- `connect`: Persistent - `{error, connection_refused}`
- `ack`: Intermittent (30% probability) - `{intermittent, {error, timeout}, 0.3}`

**Expected Behavior**:
- No infinite loops
- Correct redelivery without infinite duplication

**Verification**:
- Message semantics preserved
- Redelivery within acceptable limits

#### `test_mixed_intermittent_publish_persistent_ack`

**Scenario**: Intermittent publish timeout + Persistent ACK failure

**Faults**:
- `publish`: Intermittent (40% probability) - `{intermittent, {error, timeout}, 0.4}`
- `ack`: Persistent - `{error, timeout}`

**Expected Behavior**:
- Delivery guarantee not broken (acceptable duplicates or controlled loss)
- No infinite retry loops

**Verification**:
- ACK failures increase
- Redelivery occurs but within limits
- No infinite retries

#### `test_mixed_pattern_flapping_with_persistent_errors`

**Scenario**: Flapping connection with persistent errors

**Faults**:
- `connect`: Flapping (enable/disable repeatedly)
- `publish`: Persistent - `{error, quota_exceeded}`

**Expected Behavior**:
- System doesn't enter retry storm
- Correct backoff behavior

## Cascading Fault Scenarios

### Test Cases

#### `test_cascading_connect_publish_ack_chain`

**Scenario**: Cascading fault chain (connect → publish → ack)

**Fault Sequence**:
1. Connect fails
2. Some publish goes to void (during connect failure)
3. Connection recovers, but publish fails
4. Publish recovers, but ACK fails

**Expected Behavior**:
- System handles cascading faults correctly
- Correct retry logic through the chain

#### `test_cascading_reconnect_storm_publish_backlog_ack_loss`

**Scenario**: Reconnect storm + publish backlog + ACK loss

**Faults**:
- Multiple reconnections in short time
- Backlog of publish attempts
- ACKs lost during reconnections

**Expected Behavior**:
- System handles reconnect storm without crash
- Backlog processed correctly after storm
- No message loss

**Verification**:
- Connection metrics reflect storm
- Backlog processed after storm
- No excessive redelivery

#### `test_cascading_multiple_recovery_cycles`

**Scenario**: Multiple fault → recovery cycles

**Fault Cycles**: 3 cycles of fault → recovery

**Expected Behavior**:
- System recovers correctly after each cycle
- No state drift across cycles
- New messages after cycles have normal behavior

## Fault Injection Mechanism

### Extended Fault Types

The fault injection mechanism supports:

1. **Persistent Faults**: Always fail
   ```erlang
   router_nats_fault_injection:enable_fault(Operation, Fault)
   ```

2. **Intermittent Faults**: Fail with probability
   ```erlang
   router_nats_fault_injection:enable_fault(Operation, {intermittent, Fault, Probability})
   ```
   - `Probability`: 0.0 to 1.0 (0.0 = never fails, 1.0 = always fails)

### Supported Operations

- `connect`: Connection establishment
- `publish`: Message publishing
- `publish_with_ack`: Publish with acknowledgment
- `ack`: Message acknowledgment
- `nak`: Negative acknowledgment
- `subscribe`: Subscription operations

### Fault Types

- `{error, Reason}`: Return error with reason
- `timeout`: Operation timeout
- `close_connection`: Close connection
- `{delay, Milliseconds}`: Delay operation
- `{intermittent, Fault, Probability}`: Intermittent fault with probability

## Verification Criteria

### 1. No Deadlocks or Resource Leaks

**Checks**:
- Process liveness: `is_process_alive(Pid)` for critical processes
- Process count: Should not grow unbounded (< 10000)
- Memory usage: Should not grow unbounded

**Helper**: `verify_no_deadlocks_or_leaks/0`

### 2. Message Semantics (At-Least-Once)

**Checks**:
- Redelivery metrics: Should increase but within limits
- ACK metrics: Should reflect successful processing
- No excessive duplicates: Redelivery delta should be reasonable

**Helper**: `verify_message_semantics/3`

### 3. Recovery

**Checks**:
- System recovers after faults cleared
- New messages processed successfully
- Old messages reach expected final state
- Metrics normalize after recovery

**Helper**: `wait_for_stabilization/1`

### 4. Metrics Validation

**Metrics Checked**:
- `router_nats_connection_lost_total`: Connection losses
- `router_nats_connection_restored_total`: Connection restorations
- `router_nats_publish_failures_total`: Publish failures
- `router_nats_ack_failures_total`: ACK failures
- `router_jetstream_redelivery_total`: Message redeliveries
- `router_jetstream_ack_total`: Successful ACKs

## Test Execution

### Run All Advanced Concurrent Fault Tests

```bash
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE
```

### Run Specific Test Group

```bash
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE --group triple_fault_tests
```

### Run Specific Test Case

```bash
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE --case test_triple_fault_scenario_a_connect_publish_ack
```

### Run with Verbose Output

```bash
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE --verbose
```

## CI Integration

### Test Categories

- `fault_injection`: All fault injection tests
- `advanced_concurrent_faults`: Advanced concurrent fault tests
- `integration`: Integration tests
- `slow`: Long-running tests (may run in nightly CI)

### Recommended CI Strategy

- **Fast CI**: Run basic concurrent fault tests only
- **Nightly CI**: Run all advanced concurrent fault tests
- **Pre-release**: Run full test suite including advanced concurrent faults

## Maintenance

### Adding New Test Cases

1. Follow naming convention: `test_<scenario_name>`
2. Include comprehensive doc comments describing scenario and verification criteria
3. Use helper functions for verification (`verify_no_deadlocks_or_leaks/0`, `verify_message_semantics/3`)
4. Clean up all fault injections in `end_per_testcase/2`
5. Verify critical invariants (no deadlocks, message semantics, recovery)

### Updating Existing Tests

1. Maintain backward compatibility with existing test structure
2. Update documentation when scenarios change
3. Ensure all timing optimizations are applied
4. Verify test independence (no order dependencies)

## References

- `apps/otp/router/test/router_concurrent_faults_SUITE.erl`: Basic concurrent fault tests
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`: JetStream fault injection tests
- `apps/otp/router/src/router_nats_fault_injection.erl`: Fault injection mechanism
- `apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md`: General fault injection test scenarios

