# Fault Injection Test Audit

## Purpose

This document provides an audit of fault injection tests to identify:
- Duplicate test scenarios (tests checking the same behavior)
- Test redundancy (overlapping coverage)
- Opportunities for test consolidation
- Clear differentiation between similar tests

## Audit Methodology

1. **Group tests by scenario type**: ACK errors, NAK errors, recovery, ETS integrity, delivery counts
2. **Compare test invariants**: What exactly does each test verify?
3. **Identify overlaps**: Do multiple tests verify the same invariant?
4. **Recommend actions**: Merge, differentiate, or keep separate

## Test Suite Overview

### `router_result_consumer_SUITE.erl`
- **Focus**: Result consumer fault injection and recovery
- **Test count**: 19 tests (including fault injection tests)
- **Fault injection tests**: 8 tests

### `router_decide_consumer_SUITE.erl`
- **Focus**: Decide consumer fault injection and recovery
- **Test count**: 19 tests (including fault injection tests)
- **Fault injection tests**: 8 tests

### `router_jetstream_fault_injection_SUITE.erl`
- **Focus**: NATS/JetStream integration fault scenarios
- **Test count**: 4 tests
- **Fault injection tests**: 4 tests

### `router_recovery_state_integrity_SUITE.erl`
- **Focus**: ETS integrity, delivery counts, idempotency during recovery
- **Test count**: 14 tests
- **Fault injection tests**: 14 tests (overlapping with new tests)

## Duplicate/Overlapping Test Analysis

### Category 1: ACK/NAK Error Tests

#### New Tests (router_result_consumer_SUITE, router_decide_consumer_SUITE)
- `test_ack_error_with_tenant_validation_fail_concurrent`
- `test_ack_error_with_tenant_validation_fail_same_message`
- `test_nak_with_publish_failure_recovery`
- `test_batch_nak_publish_failure_mixed`

#### Existing Tests (router_recovery_state_integrity_SUITE)
- `test_recovery_ack_nak_errors_ets_integrity`
- `test_recovery_ack_nak_errors_delivery_counts`
- `test_recovery_ack_nak_errors_idempotency`

#### Analysis
**Differentiation**:
- **New tests**: Focus on **concurrent faults** (ACK error + tenant validation fail simultaneously)
- **Existing tests**: Focus on **ETS integrity** and **delivery count accuracy** after ACK/NAK errors
- **New tests**: Verify **no router restart** and **recovery without manual intervention**
- **Existing tests**: Verify **state consistency** (ETS, delivery counts, idempotency)

**Verdict**: ✅ **NOT duplicates** - Different focus:
- New tests: Concurrent fault handling + recovery
- Existing tests: State integrity verification

**Recommendation**: Keep both sets, but clarify differentiation in documentation.

### Category 2: ETS Integrity Tests

#### New Tests
- `test_ets_delivery_count_consistency_during_faults`
- `test_ets_cleanup_after_recovery`

#### Existing Tests (router_recovery_state_integrity_SUITE)
- `test_recovery_connection_loss_ets_integrity`
- `test_recovery_connection_loss_delivery_counts`
- `test_recovery_ack_nak_errors_ets_integrity`
- `test_recovery_ack_nak_errors_delivery_counts`

#### Analysis
**Differentiation**:
- **New tests**: Focus on **ETS consistency during faults** (before/during/after)
- **Existing tests**: Focus on **ETS integrity after recovery** (post-recovery state)
- **New tests**: Verify **no "eternal" entries** and **no "carried over" artifacts**
- **Existing tests**: Verify **ETS size** and **delivery count accuracy**

**Verdict**: ⚠️ **Partial overlap** - Both verify ETS integrity, but:
- New tests: During-fault consistency
- Existing tests: Post-recovery integrity

**Recommendation**: 
- **Keep both sets** - Different phases (during vs after)
- **Clarify in documentation**: New tests = during-fault, Existing tests = post-recovery

### Category 3: NATS Connection Loss Tests

#### New Tests (router_jetstream_fault_injection_SUITE)
- `test_nats_connection_loss_recovery`
- `test_ets_state_preservation_during_nats_restart`

#### Existing Tests (router_recovery_state_integrity_SUITE)
- `test_recovery_connection_loss_ets_integrity`
- `test_recovery_connection_loss_delivery_counts`
- `test_recovery_connection_loss_idempotency`

#### Analysis
**Differentiation**:
- **New tests**: Focus on **router reconnection** and **consumer functionality** after NATS restart
- **Existing tests**: Focus on **state integrity** (ETS, delivery counts, idempotency) after connection loss
- **New tests**: Verify **new messages processed** after recovery
- **Existing tests**: Verify **state preserved** during connection loss

**Verdict**: ✅ **NOT duplicates** - Different focus:
- New tests: Reconnection and message processing
- Existing tests: State preservation

**Recommendation**: Keep both sets, but clarify differentiation in documentation.

### Category 4: Delivery Count Tests

#### New Tests
- `test_ets_delivery_count_consistency_during_faults`
- `test_max_delivery_count_exhaustion`

#### Existing Tests (router_recovery_state_integrity_SUITE)
- `test_recovery_connection_loss_delivery_counts`
- `test_recovery_ack_nak_errors_delivery_counts`

#### Analysis
**Differentiation**:
- **New tests**: Focus on **delivery count consistency during faults** and **max delivery count exhaustion**
- **Existing tests**: Focus on **delivery count accuracy after recovery**
- **New tests**: Verify **no "eternal" entries** and **max deliver limits**
- **Existing tests**: Verify **delivery count matches expected value**

**Verdict**: ✅ **NOT duplicates** - Different focus:
- New tests: During-fault consistency + exhaustion
- Existing tests: Post-recovery accuracy

**Recommendation**: Keep both sets, but clarify differentiation in documentation.

## Summary of Findings

### No True Duplicates Found
All test sets have **clear differentiation**:
- **New tests**: Focus on **concurrent faults**, **during-fault consistency**, **recovery without restart**
- **Existing tests**: Focus on **state integrity**, **post-recovery verification**, **idempotency**

### Partial Overlaps (Acceptable)
Some tests verify similar invariants but at **different phases**:
- **During faults** vs **After recovery**
- **Concurrent faults** vs **Sequential faults**
- **Recovery behavior** vs **State integrity**

### Recommendations

#### 1. Documentation Clarification
**Action**: Update test documentation to clearly differentiate:
- **New tests**: Concurrent fault handling, during-fault consistency, recovery without restart
- **Existing tests**: State integrity, post-recovery verification, idempotency

**Files to update**:
- `FAULT_INJECTION_TEST_SCENARIOS.md` - Add section on test differentiation
- `router_recovery_state_integrity_SUITE.erl` - Add doc comments clarifying focus

#### 2. Test Naming Consistency
**Action**: Ensure test names clearly indicate their focus:
- ✅ New tests: `test_*_during_faults`, `test_*_recovery_no_router_restart`
- ✅ Existing tests: `test_recovery_*_ets_integrity`, `test_recovery_*_delivery_counts`

**Status**: ✅ **Already consistent** - Test names clearly indicate focus

#### 3. Test Organization
**Action**: Consider organizing tests by phase:
- **During-fault tests**: New tests (concurrent faults, during-fault consistency)
- **Post-recovery tests**: Existing tests (state integrity, accuracy verification)

**Status**: ✅ **Already organized** - Tests are in appropriate suites

## Test Coverage Matrix

| Scenario | New Tests | Existing Tests | Overlap | Action |
|----------|-----------|----------------|---------|--------|
| ACK errors (concurrent) | ✅ | ❌ | None | Keep new |
| ACK errors (sequential) | ✅ | ✅ | Partial | Keep both (different phases) |
| NAK errors | ✅ | ✅ | Partial | Keep both (different focus) |
| ETS integrity (during) | ✅ | ❌ | None | Keep new |
| ETS integrity (after) | ✅ | ✅ | Partial | Keep both (different phases) |
| Delivery counts (during) | ✅ | ❌ | None | Keep new |
| Delivery counts (after) | ✅ | ✅ | Partial | Keep both (different phases) |
| Max delivery count | ✅ | ❌ | None | Keep new |
| NATS restart | ✅ | ✅ | Partial | Keep both (different focus) |
| Multiple fault cycles | ✅ | ❌ | None | Keep new |

## Conclusion

**No test consolidation needed**. All tests have clear differentiation:
- **New tests**: Focus on concurrent faults, during-fault consistency, recovery without restart
- **Existing tests**: Focus on state integrity, post-recovery verification, idempotency

**Action items**:
1. ✅ Update documentation to clarify test differentiation
2. ✅ Ensure test names clearly indicate focus
3. ✅ Maintain test organization by phase and focus

**No redundant tests identified** - All tests serve distinct purposes.

