# router_recovery_state_integrity Test Suite

## Overview

Comprehensive recovery state integrity test suite for router after NATS/JetStream failures. Tests verify that router recovers without restart and maintains state integrity (ETS tables, delivery counts, idempotency).

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete documentation including metrics, logs, monitoring, and operational procedures.

## Test Philosophy

This test suite verifies three critical invariants:

1. **Liveness**: Router recovers from failures without requiring restart
2. **Consistency**: Internal state (ETS, delivery counts, idempotency) remains consistent after recovery
3. **No Degradation**: Router doesn't accumulate garbage or degrade performance after multiple fault cycles

## Test Scenarios

### Scenario A: Connection Loss + Re-connect

**Tests**:
- `test_recovery_connection_loss_ets_integrity` - ETS tables don't contain orphaned entries after connection loss and recovery
- `test_recovery_connection_loss_delivery_counts` - Delivery counts remain accurate after connection loss and recovery
- `test_recovery_connection_loss_idempotency` - Idempotency state remains consistent after connection loss and recovery

**Test Flow**:
1. **Normal Phase**: Send messages, track delivery counts, mark messages as processed
2. **Fault Phase**: Induce connection loss, continue operations (should queue or fail gracefully)
3. **Recovery Phase**: Restore connection, verify router recovers without restart
4. **Verification Phase**: Check ETS integrity, delivery count accuracy, idempotency consistency

**Invariants Verified**:
- Router recovers without restart
- ETS tables don't contain orphaned entries
- Delivery counts remain accurate
- Idempotency entries don't leak
- No unbounded growth in ETS tables

### Scenario B: ACK/NAK Errors Without Connection Loss

**Tests**:
- `test_recovery_ack_nak_errors_ets_integrity` - ETS integrity after ACK/NAK errors
- `test_recovery_ack_nak_errors_delivery_counts` - Delivery count accuracy after ACK/NAK errors
- `test_recovery_ack_nak_errors_idempotency` - Idempotency consistency after ACK/NAK errors

**Test Flow**:
1. **Normal Phase**: Normal operations with ACK/NAK
2. **Fault Phase**: Enable fault injection for ACK/NAK operations (errors without connection loss)
3. **Recovery Phase**: Disable fault injection, verify recovery
4. **Verification Phase**: Check state integrity

**Invariants Verified**:
- Router recovers without restart
- ETS tables remain consistent
- Delivery counts accurate
- Idempotency works correctly

### Scenario C: NATS/JetStream Restart

**Tests**:
- `test_recovery_nats_restart_ets_integrity` - ETS integrity after NATS/JetStream restart
- `test_recovery_nats_restart_delivery_counts` - Delivery count accuracy after NATS/JetStream restart
- `test_recovery_nats_restart_idempotency` - Idempotency consistency after NATS/JetStream restart

**Test Flow**:
1. **Normal Phase**: Normal operations
2. **Fault Phase**: Simulate NATS/JetStream restart (connection loss)
3. **Recovery Phase**: Simulate NATS/JetStream coming back online
4. **Verification Phase**: Check state integrity

**Invariants Verified**:
- Router recovers without restart
- ETS tables don't lose state
- Delivery counts preserved
- Idempotency entries preserved

### Scenario D: Multiple Fault Cycles

**Tests**:
- `test_recovery_multiple_fault_cycles_ets_integrity` - ETS integrity after multiple fault cycles
- `test_recovery_multiple_fault_cycles_no_degradation` - No performance degradation after multiple fault cycles

**Test Flow**:
1. **Multiple Cycles**: Induce fault → recovery → fault → recovery (3-5 cycles)
2. **Verification Phase**: Check ETS integrity, verify no unbounded growth

**Invariants Verified**:
- Router recovers after each cycle without restart
- ETS tables don't accumulate garbage
- No unbounded growth
- Performance doesn't degrade

### Scenario E: Publish Errors During Recovery

**Tests**:
- `test_recovery_publish_errors_ets_integrity` - ETS integrity after publish errors
- `test_recovery_publish_errors_message_processing` - Message processing after publish errors recovery

**Test Flow**:
1. **Fault Phase**: Enable publish error fault injection
2. **Recovery Phase**: Disable fault injection
3. **Verification Phase**: Check ETS integrity, verify new messages can be processed

**Invariants Verified**:
- Router recovers without restart
- ETS tables remain consistent
- New messages can be processed after recovery

## State Integrity Checks

### ETS Table Integrity

**Tables Checked**:
- `router_delivery_count` - Delivery count tracking
- `router_ack_delivery_count` - ACK delivery count tracking
- `router_idempotency` - Idempotency tracking

**Checks Performed**:
- No orphaned entries (entries that will never be processed)
- No unbounded growth (size < 1000 entries)
- No memory leaks (size doesn't grow significantly after recovery)

### Delivery Count Accuracy

**Checks Performed**:
- Delivery counts remain accurate after recovery
- Delivery counts don't exceed MaxDeliver (5)
- Delivery counts don't get corrupted (logical values)

### Idempotency Consistency

**Checks Performed**:
- Idempotency entries don't leak
- Messages marked as processed remain marked after recovery
- New messages can still be processed (idempotency works for new messages)
- No duplicate processing after recovery

## Test Criteria

Each test verifies three categories of criteria:

1. **Liveness**: Router recovers without restart
   - `router_nats` process remains alive
   - `beamline_router_sup` supervisor remains alive
   - Connection state returns to normal

2. **Consistency**: State remains consistent
   - ETS tables don't contain orphaned entries
   - Delivery counts remain accurate
   - Idempotency state remains consistent

3. **No Degradation**: No performance degradation
   - ETS size doesn't grow unbounded
   - No memory leaks
   - Performance doesn't degrade after multiple cycles

## Running Tests

### Run All Recovery State Integrity Tests

```bash
cd apps/otp/router
rebar3 ct --suite test/router_recovery_state_integrity_SUITE
```

### Run Specific Scenario

```bash
# Scenario A: Connection loss
rebar3 ct --suite test/router_recovery_state_integrity_SUITE --case test_recovery_connection_loss_ets_integrity

# Scenario B: ACK/NAK errors
rebar3 ct --suite test/router_recovery_state_integrity_SUITE --case test_recovery_ack_nak_errors_ets_integrity

# Scenario C: NATS restart
rebar3 ct --suite test/router_recovery_state_integrity_SUITE --case test_recovery_nats_restart_ets_integrity

# Scenario D: Multiple cycles
rebar3 ct --suite test/router_recovery_state_integrity_SUITE --case test_recovery_multiple_fault_cycles_ets_integrity

# Scenario E: Publish errors
rebar3 ct --suite test/router_recovery_state_integrity_SUITE --case test_recovery_publish_errors_ets_integrity
```

## Expected Duration

- **Individual tests**: ~2-5 seconds each
- **Full suite**: ~30-60 seconds

## CI Recommendation

✅ **Recommended for PR pipeline** - These tests verify critical state integrity after recovery scenarios.

## Dependencies

- `router_nats_fault_injection` module (for fault injection scenarios)
- `router_idempotency` module (for idempotency checks)
- ETS tables: `router_delivery_count`, `router_ack_delivery_count`, `router_idempotency`

## Notes

- Tests use fault injection where available, but also test scenarios without fault injection (connection loss simulation)
- ETS tables are cleared before and after each test to ensure clean state
- Tests verify both immediate recovery and long-term state integrity
- Tests verify no performance degradation after multiple fault cycles

