# Advanced Concurrent Faults Tests

## Quick Start

### Run Tests

```bash
# Run all advanced concurrent fault tests
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE

# Run specific test group
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE --group triple_fault_tests

# Run specific test case
rebar3 ct --suite apps/otp/router/test/router_advanced_concurrent_faults_SUITE --case test_triple_fault_scenario_a_connect_publish_ack
```

## What's New

This test suite extends basic concurrent fault tests with:

1. **Triple-Fault Scenarios**: Simultaneous failures in connect + publish + ack/nak
2. **Mixed Patterns**: Intermittent + persistent fault combinations
3. **Cascading Faults**: Chain of faults (connect → publish → ack)

## Test Coverage

### Triple-Fault Tests

- **Scenario A**: Connect interruption + Publish partial success + ACK loss
- **Scenario B**: Connection drops + Publish continues + NAK fails
- **Scenario C**: Flapping connection + Parallel publish + ACK loss/duplication
- **Simultaneous**: All three fault types at once

### Mixed Pattern Tests

- Intermittent connect + Persistent publish error
- Persistent connect + Intermittent ACK loss
- Intermittent publish + Persistent ACK failure
- Flapping with persistent errors

### Cascading Fault Tests

- Connect → Publish → ACK chain
- Reconnect storm + Publish backlog + ACK loss
- Multiple recovery cycles

## Fault Injection Extensions

### Intermittent Faults

Faults that fail with a probability (0.0 to 1.0):

```erlang
router_nats_fault_injection:enable_fault(Operation, {intermittent, Fault, Probability})
```

Example:
```erlang
%% ACK fails 30% of the time
router_nats_fault_injection:enable_fault(ack, {intermittent, {error, timeout}, 0.3})
```

### Persistent Faults

Faults that always fail:

```erlang
router_nats_fault_injection:enable_fault(Operation, Fault)
```

Example:
```erlang
%% Publish always fails
router_nats_fault_injection:enable_fault(publish, {error, quota_exceeded})
```

## Verification

All tests verify:

1. **No Deadlocks or Leaks**: Process liveness, process count, memory
2. **Message Semantics**: At-least-once delivery, no excessive duplicates
3. **Recovery**: System recovers after faults cleared
4. **Metrics**: Error metrics increase during faults, normalize after recovery

## Documentation

- **Full Specification**: `ADVANCED_CONCURRENT_FAULTS_SPEC.md`
- **Basic Fault Tests**: `router_concurrent_faults_SUITE.erl`
- **JetStream Fault Tests**: `router_jetstream_fault_injection_SUITE.erl`

## Integration

These tests are integrated with:

- Fault injection mechanism: `router_nats_fault_injection.erl`
- Metrics system: `router_metrics`
- Router NATS client: `router_nats.erl`

## CI Strategy

- **Fast CI**: Basic concurrent fault tests only
- **Nightly CI**: All advanced concurrent fault tests
- **Pre-release**: Full test suite including advanced concurrent faults

