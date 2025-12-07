# R8 Edge-Case Scenarios Summary

## Overview

This document summarizes the edge-case scenarios added to R8 coverage, including partial recovery, AckPolicy/DeliverPolicy variations, and consumer group isolation.

## Edge-Case Scenarios

### 1. Triple-Fault with Partial Recovery

**Test**: `test_triple_fault_partial_recovery/1`

**Scenario**:
- Configure triple faults (connect + publish + ACK)
- Send messages during fault
- Clear connect fault (partial recovery)
- Keep publish and ACK faults active
- Verify system stability during partial recovery
- Clear remaining faults (full recovery)

**Contract Expectations**:
- ✅ System handles partial recovery correctly
- ✅ System remains stable during partial recovery
- ✅ Metrics reflect partial recovery state
- ✅ No message loss during partial recovery

**Key Verification**:
- Process liveness during partial recovery
- Metrics accuracy at different stages (initial, partial recovery, full recovery)
- No message loss during transitions

### 2. Triple-Fault with AckPolicy Variations

**Test**: `test_triple_fault_ackpolicy_variations/1`

**Scenario**:
- Configure triple faults (connect + publish + ACK)
- Test with different AckPolicy values:
  - `explicit`: Requires explicit ACK/NAK
  - `none`: No acknowledgment required
  - `all`: All messages in batch acknowledged
- Verify triple faults work correctly with each policy

**Contract Expectations**:
- ✅ Different AckPolicy values handled correctly
- ✅ Triple faults work correctly with different AckPolicy
- ✅ Metrics reflect AckPolicy-specific behavior

**Key Verification**:
- Explicit AckPolicy: ACK/NAK required, triple faults handled correctly
- None AckPolicy: No acknowledgment required, triple faults don't break behavior
- All AckPolicy: Batch acknowledgment, triple faults handled correctly

### 3. Triple-Fault with DeliverPolicy Variations

**Test**: `test_triple_fault_deliverpolicy_variations/1`

**Scenario**:
- Configure triple faults (connect + publish + ACK)
- Test with different DeliverPolicy values:
  - `all`: Deliver all messages
  - `new`: Deliver only new messages
  - `last`: Deliver last message per subject
- Verify message delivery respects DeliverPolicy

**Contract Expectations**:
- ✅ Different DeliverPolicy values handled correctly
- ✅ Message delivery respects DeliverPolicy
- ✅ Triple faults work correctly with different DeliverPolicy

**Key Verification**:
- All DeliverPolicy: All messages delivered, triple faults handled correctly
- New DeliverPolicy: Only new messages delivered, triple faults don't affect old messages
- Last DeliverPolicy: Last message per subject delivered, triple faults handled correctly

### 4. Triple-Fault with Consumer Group Isolation

**Test**: `test_triple_fault_consumer_group_isolation/1`

**Scenario**:
- Configure triple faults (connect + publish + ACK)
- Test with different consumer groups:
  - `router-results-group` (result consumer)
  - `router-acks-group` (ACK consumer)
  - `router-decide-group` (decide consumer)
- Verify faults in one group don't affect others

**Contract Expectations**:
- ✅ Consumer group isolation maintained
- ✅ Faults in one consumer group don't affect others
- ✅ Metrics correctly labeled per consumer group
- ✅ No cross-group state corruption

**Key Verification**:
- Faults in `router-results-group` don't affect `router-acks-group` or `router-decide-group`
- Metrics correctly labeled per consumer group
- No cross-group state corruption
- Each consumer group processes independently

## Coverage Matrix

| Edge-Case Pattern | Test Case | Category | Status |
|-------------------|-----------|----------|--------|
| Partial Recovery | `test_triple_fault_partial_recovery` | Recovery | ✅ Covered |
| AckPolicy Variations | `test_triple_fault_ackpolicy_variations` | Policy | ✅ Covered |
| DeliverPolicy Variations | `test_triple_fault_deliverpolicy_variations` | Policy | ✅ Covered |
| Consumer Group Isolation | `test_triple_fault_consumer_group_isolation` | Isolation | ✅ Covered |

## Test Execution

```bash
# Run all edge-case tests
rebar3 ct --suite router_triple_fault_contract_SUITE --case test_triple_fault_partial_recovery
rebar3 ct --suite router_triple_fault_contract_SUITE --case test_triple_fault_ackpolicy_variations
rebar3 ct --suite router_triple_fault_contract_SUITE --case test_triple_fault_deliverpolicy_variations
rebar3 ct --suite router_triple_fault_contract_SUITE --case test_triple_fault_consumer_group_isolation
```

## References

- `router_triple_fault_contract_SUITE.erl`: Edge-case test implementations
- `R8_COVERAGE_MATRIX.md`: Complete coverage matrix
- `TRIPLE_FAULT_PATTERNS_CATALOG.md`: Pattern catalog with edge-case details

