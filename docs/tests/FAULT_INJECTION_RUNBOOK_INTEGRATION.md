# Fault Injection Test Runbook Integration

## Purpose

This document provides integration points between fault injection tests and operational runbooks. It helps SRE/Ops teams understand what automated test coverage exists for fault scenarios they may encounter in production.

## Integration Points

### Scenario 1: NATS Connection Loss

**Runbook Section**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` → "Diagnosis: NATS Failures"

**Test Coverage**:
- **Test**: `test_nats_connection_loss_recovery` (`router_jetstream_fault_injection_SUITE`)
- **Smoke test**: Yes (in `FAULT_INJECTION_SMOKE_TESTS.md`)
- **Duration**: ~30-60 seconds

**What the test guarantees**:
- Router reconnects automatically after NATS connection loss
- Consumers continue functioning after reconnection
- New messages processed without anomalies
- ETS state preserved during reconnection

**If this scenario occurs in production**:
- Router should recover automatically (verified by test)
- If router doesn't recover, check logs for reconnection errors
- If ETS state is corrupted, check `test_ets_state_preservation_during_nats_restart` scenario

**Related tests**:
- `test_jetstream_consumer_reconnection` - Consumer reconnection behavior
- `test_stream_availability_after_recovery` - Stream availability after recovery
- `test_ets_state_preservation_during_nats_restart` - ETS state preservation

### Scenario 2: Max Delivery Count Exhaustion

**Runbook Section**: TBD (to be added to runbook)

**Test Coverage**:
- **Test**: `test_max_delivery_count_exhaustion` (`router_result_consumer_SUITE`)
- **Test**: `test_decide_max_delivery_count_exhaustion` (`router_decide_consumer_SUITE`)
- **Smoke test**: Yes (in `FAULT_INJECTION_SMOKE_TESTS.md`)
- **Duration**: ~30-60 seconds

**What the test guarantees**:
- Router/consumer does not enter infinite retry
- Transitions to expected final state (DLQ/drop)
- MaxDeliver exhaustion metric is emitted
- Warning log is written

**If this scenario occurs in production**:
- Messages exceeding MaxDeliver should transition to DLQ/drop (verified by test)
- If messages continue retrying indefinitely, this indicates a bug (tests should catch this)
- Check MaxDeliver configuration and DLQ routing

**Related tests**:
- `test_ets_delivery_count_consistency_during_faults` - Delivery count tracking
- `test_ets_cleanup_after_recovery` - ETS cleanup after exhaustion

### Scenario 3: Prolonged Fault Periods

**Runbook Section**: TBD (to be added to runbook)

**Test Coverage**:
- **Test**: `test_prolonged_fault_period_recovery_no_router_restart` (`router_result_consumer_SUITE`)
- **Test**: `test_decide_prolonged_fault_period_recovery_no_router_restart` (`router_decide_consumer_SUITE`)
- **Smoke test**: Yes (in `FAULT_INJECTION_SMOKE_TESTS.md`)
- **Duration**: ~60-90 seconds

**What the test guarantees**:
- Router continues living throughout fault period
- Processes new messages after recovery without restart
- Old messages reach expected final state
- No process crashes or hangs

**If this scenario occurs in production**:
- Router should continue running during prolonged faults (verified by test)
- Router should recover automatically when faults clear (verified by test)
- If router crashes or hangs, this indicates a bug (tests should catch this)

**Related tests**:
- `test_multiple_fault_recovery_cycles` - Multiple fault → recovery cycles
- `test_ets_delivery_count_consistency_during_faults` - State consistency during faults

### Scenario 4: ETS State Corruption

**Runbook Section**: TBD (to be added to runbook)

**Test Coverage**:
- **Test**: `test_ets_delivery_count_consistency_during_faults` (`router_result_consumer_SUITE`)
- **Test**: `test_decide_ets_delivery_count_consistency_during_faults` (`router_decide_consumer_SUITE`)
- **Test**: `test_ets_state_preservation_during_nats_restart` (`router_jetstream_fault_injection_SUITE`)
- **Duration**: ~60-90 seconds

**What the test guarantees**:
- ETS delivery counts remain consistent before, during, and after faults
- No "eternal" entries in ETS
- Expected delivery count increases
- No "carried over" artifacts for new messages after recovery

**If this scenario occurs in production**:
- ETS state should remain consistent (verified by test)
- If ETS state is corrupted, check for bugs in delivery count tracking
- Check for memory leaks or unbounded ETS growth

**Related tests**:
- `test_ets_cleanup_after_recovery` - ETS cleanup after recovery
- `test_ets_state_preservation_during_nats_restart` - ETS preservation during NATS restart

## Runbook Update Checklist

When updating runbooks for fault scenarios:

1. **Add test references**:
   - [ ] Link to relevant test case in `FAULT_INJECTION_TEST_SCENARIOS.md`
   - [ ] Link to smoke test if applicable
   - [ ] Describe what the test guarantees

2. **Add test guarantees**:
   - [ ] Document expected behavior verified by test
   - [ ] Document recovery behavior verified by test
   - [ ] Document state consistency verified by test

3. **Add troubleshooting hints**:
   - [ ] Reference test scenarios for similar symptoms
   - [ ] Link to test documentation for behavior details

## Test-to-Runbook Mapping

| Test | Runbook Section | Status |
|------|----------------|--------|
| `test_nats_connection_loss_recovery` | `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` → "Diagnosis: NATS Failures" | ✅ Integrated |
| `test_max_delivery_count_exhaustion` | TBD | ⏳ Pending |
| `test_prolonged_fault_period_recovery_no_router_restart` | TBD | ⏳ Pending |
| `test_ets_delivery_count_consistency_during_faults` | TBD | ⏳ Pending |
| `test_ets_state_preservation_during_nats_restart` | TBD | ⏳ Pending |

## Maintenance

### When Tests Change

1. **Update runbook references**:
   - [ ] Verify test still covers same scenario
   - [ ] Update test guarantees if behavior changed
   - [ ] Update links if test moved or renamed

2. **Update runbook procedures**:
   - [ ] Update expected behavior if test behavior changed
   - [ ] Update troubleshooting hints if test scenarios changed

### When Runbooks Change

1. **Verify test coverage**:
   - [ ] Check if new scenario is covered by tests
   - [ ] Add test if scenario is not covered
   - [ ] Update traceability if new requirement added

## References

- `FAULT_INJECTION_TEST_SCENARIOS.md`: Test scenarios documentation
- `FAULT_INJECTION_SMOKE_TESTS.md`: Smoke test set
- `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`: Requirements traceability
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`: Router intake runbook
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`: Gateway rate limiting runbook

