# Fault Injection Requirements Traceability

## Purpose

This document provides traceability between fault injection requirements (from TZ/requirements) and test cases. It ensures:
- All key requirements have explicit test coverage
- No "orphaned" requirements without tests
- No tests without clear requirements/justification

## Requirements Categories

### R1: Concurrent Fault Handling

**Requirement**: System must handle concurrent faults (multiple simultaneous error conditions) without crashing or losing messages.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R1.1: ACK error + tenant validation fail (concurrent messages) | `test_ack_error_with_tenant_validation_fail_concurrent` | `router_result_consumer_SUITE` | ✅ Covered |
| R1.1: ACK error + tenant validation fail (concurrent messages) | `test_decide_ack_error_with_tenant_validation_fail_concurrent` | `router_decide_consumer_SUITE` | ✅ Covered |
| R1.2: ACK error + tenant validation fail (same message, sequential) | `test_ack_error_with_tenant_validation_fail_same_message` | `router_result_consumer_SUITE` | ✅ Covered |
| R1.2: ACK error + tenant validation fail (same message, sequential) | `test_decide_ack_error_with_tenant_validation_fail_same_message` | `router_decide_consumer_SUITE` | ✅ Covered |
| R1.3: NAK + publish failure with recovery | `test_nak_with_publish_failure_recovery` | `router_result_consumer_SUITE` | ✅ Covered |
| R1.3: NAK + publish failure with recovery | `test_decide_nak_with_publish_failure_recovery` | `router_decide_consumer_SUITE` | ✅ Covered |
| R1.4: Batch messages with mixed NAK + publish failure | `test_batch_nak_publish_failure_mixed` | `router_result_consumer_SUITE` | ✅ Covered |
| R1.4: Batch messages with mixed NAK + publish failure | `test_decide_batch_nak_publish_failure_mixed` | `router_decide_consumer_SUITE` | ✅ Covered |

### R2: Prolonged Fault Periods

**Requirement**: System must handle extended periods of faults and recover without requiring router restart.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R2.1: Extended fault period with recovery (no router restart) | `test_prolonged_fault_period_recovery_no_router_restart` | `router_result_consumer_SUITE` | ✅ Covered (Smoke) |
| R2.1: Extended fault period with recovery (no router restart) | `test_decide_prolonged_fault_period_recovery_no_router_restart` | `router_decide_consumer_SUITE` | ✅ Covered (Smoke) |

### R3: ETS and Delivery Count Integrity

**Requirement**: ETS tables and delivery counts must remain consistent before, during, and after faults.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R3.1: ETS delivery count consistency during faults | `test_ets_delivery_count_consistency_during_faults` | `router_result_consumer_SUITE` | ✅ Covered |
| R3.1: ETS delivery count consistency during faults | `test_decide_ets_delivery_count_consistency_during_faults` | `router_decide_consumer_SUITE` | ✅ Covered |
| R3.2: ETS cleanup after recovery | `test_ets_cleanup_after_recovery` | `router_result_consumer_SUITE` | ✅ Covered |
| R3.2: ETS cleanup after recovery | `test_decide_ets_cleanup_after_recovery` | `router_decide_consumer_SUITE` | ✅ Covered |
| R3.3: ETS state preservation during NATS restart | `test_ets_state_preservation_during_nats_restart` | `router_jetstream_fault_injection_SUITE` | ✅ Covered |

### R4: Max Delivery Count Exhaustion

**Requirement**: System must not enter infinite retry loops and must transition to expected final state (DLQ/drop) when max delivery count is exhausted.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R4.1: Max delivery count exhaustion | `test_max_delivery_count_exhaustion` | `router_result_consumer_SUITE` | ✅ Covered (Smoke) |
| R4.1: Max delivery count exhaustion | `test_decide_max_delivery_count_exhaustion` | `router_decide_consumer_SUITE` | ✅ Covered (Smoke) |

### R5: Multiple Fault → Recovery Cycles

**Requirement**: System must handle multiple fault → recovery cycles without state drift or corruption.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R5.1: Multiple fault → recovery cycles | `test_multiple_fault_recovery_cycles` | `router_result_consumer_SUITE` | ✅ Covered |
| R5.1: Multiple fault → recovery cycles | `test_decide_multiple_fault_recovery_cycles` | `router_decide_consumer_SUITE` | ✅ Covered |

### R6: NATS/JetStream Restart

**Requirement**: System must handle NATS/JetStream backend unavailability/restart and recover without requiring router restart.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R6.1: NATS connection loss and recovery | `test_nats_connection_loss_recovery` | `router_jetstream_fault_injection_SUITE` | ✅ Covered (Smoke) |
| R6.2: JetStream consumer reconnection | `test_jetstream_consumer_reconnection` | `router_jetstream_fault_injection_SUITE` | ✅ Covered |
| R6.3: Stream availability after recovery | `test_stream_availability_after_recovery` | `router_jetstream_fault_injection_SUITE` | ✅ Covered |

### R7: Extended Recovery Scenarios

**Requirement**: System must handle extended recovery scenarios including MaxDeliver accumulation, repeated fault/recovery cycles, and performance degradation detection.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R7.1: Gradual MaxDeliver accumulation | `test_maxdeliver_gradual_accumulation` | `router_jetstream_extended_recovery_SUITE` | ✅ Covered |
| R7.2: Repeated fault/recovery cycles | `test_cascading_multiple_recovery_cycles` | `router_advanced_concurrent_faults_SUITE` | ✅ Covered |
| R7.3: Performance degradation detection | `test_long_running_stability` | `router_jetstream_extended_recovery_SUITE` | ✅ Covered |

### R10: Publish Failure Under High Load with Explicit Retry Model and E2E Circuit Breaker

**Requirement**: System must handle publish failures under high load with explicit retry model and circuit breaker protection at E2E level.

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R10.1: Mass publish failure → circuit breaker activation | `test_mass_publish_failure_circuit_breaker_activation` | `router_publish_failure_e2e_SUITE` | ⏳ To be implemented |
| R10.2: Recovery after failure (half-open → closed) | `test_recovery_after_failure_half_open_closed` | `router_publish_failure_e2e_SUITE` | ⏳ To be implemented |
| R10.3: Latency-based circuit breaker trigger | `test_latency_based_circuit_breaker_trigger` | `router_publish_failure_e2e_SUITE` | ⏳ To be implemented |

**References**:
- [R10 Specification](./R10_PUBLISH_FAILURE_E2E_SPEC.md)
- [R10 Consistency Check](./R10_CONSISTENCY_CHECK.md)
- [R10 Metrics Review](./R10_METRICS_REVIEW.md)
- [R10 Quick Reference](./R10_QUICK_REFERENCE.md)

### R8: Triple-Fault Combinations and Extended Mixed Patterns

**Requirement**: System must handle triple-fault combinations (3+ simultaneous faults) and extended mixed-pattern scenarios (intermittent + persistent faults) with explicit contract verification:
- MaxDeliver semantics preservation
- Redelivery limits enforcement
- Delivery count tracking correctness
- Metrics and labels accuracy
- Cross-tenant isolation

| Requirement | Test Case | Suite | Status |
|------------|-----------|-------|--------|
| R8.1: Connect + Publish + ACK triple fault (contract) | `test_triple_connect_publish_ack_contract` | `router_triple_fault_contract_SUITE` | ✅ Covered |
| R8.1: Connect + Publish + ACK triple fault (soak) | `test_multi_fault_triple_soak` | `router_stress_soak_SUITE` | ✅ Covered |
| R8.2: Connect + Validation + NAK triple fault | `test_triple_connect_validation_nak_contract` | `router_triple_fault_contract_SUITE` | ✅ Covered |
| R8.3: Publish + MaxDeliver + Intermittent ACK | `test_triple_publish_maxdeliver_ack_contract` | `router_triple_fault_contract_SUITE` | ✅ Covered |
| R8.4: Connect + Publish + MaxDeliver exhaustion | `test_triple_connect_publish_maxdeliver_contract` | `router_triple_fault_contract_SUITE` | ✅ Covered |
| R8.5: ACK + NAK + Publish triple fault | `test_triple_ack_nak_publish_contract` | `router_triple_fault_contract_SUITE` | ✅ Covered |
| R8.6: Intermittent Connect + Persistent Publish (mixed pattern) | `test_multi_fault_mixed_pattern_soak` | `router_stress_soak_SUITE` | ✅ Covered |
| R8.6: Intermittent Connect + Persistent Publish (contract) | `test_mixed_intermittent_connect_persistent_publish` | `router_advanced_concurrent_faults_SUITE` | ✅ Covered |
| R8.7: Intermittent ACK + Persistent Validation (mixed pattern) | `test_multi_fault_mixed_pattern_soak` | `router_stress_soak_SUITE` | ✅ Covered |
| R8.7: Intermittent ACK + Persistent Validation (contract) | `test_mixed_persistent_connect_intermittent_ack` | `router_advanced_concurrent_faults_SUITE` | ✅ Covered |
| R8.8: Cascading fault chains | `test_multi_fault_cascading_soak` | `router_stress_soak_SUITE` | ✅ Covered |
| R8.8: Cascading fault chains (contract) | `test_cascading_connect_publish_ack_chain` | `router_advanced_concurrent_faults_SUITE` | ✅ Covered |

## Critical Invariants Coverage

### I1: No Router Restart (`no_router_restart`)

**Requirement**: Router/consumer processes must remain alive throughout all fault scenarios.

| Invariant | Test Cases | Coverage |
|-----------|------------|----------|
| I1.1: Consumer remains alive during concurrent faults | All concurrent fault tests (R1.1-R1.4) | ✅ Covered |
| I1.2: Consumer remains alive during prolonged faults | `test_prolonged_fault_period_recovery_no_router_restart` | ✅ Covered |
| I1.3: Consumer remains alive during NATS restart | `test_nats_connection_loss_recovery`, `test_ets_state_preservation_during_nats_restart` | ✅ Covered |
| I1.4: Consumer remains alive during max delivery count exhaustion | `test_max_delivery_count_exhaustion` | ✅ Covered |

### I2: Tracking State Consistency (`tracking_state_consistency`)

**Requirement**: ETS delivery counts and tracking state must remain consistent before, during, and after faults.

| Invariant | Test Cases | Coverage |
|-----------|------------|----------|
| I2.1: Delivery counts tracked correctly during faults | `test_ets_delivery_count_consistency_during_faults` | ✅ Covered |
| I2.2: No "eternal" entries in ETS | `test_ets_delivery_count_consistency_during_faults`, `test_ets_cleanup_after_recovery` | ✅ Covered |
| I2.3: Expected delivery count increases | `test_ets_delivery_count_consistency_during_faults` | ✅ Covered |
| I2.4: No "carried over" artifacts for new messages | `test_ets_delivery_count_consistency_during_faults`, `test_multiple_fault_recovery_cycles` | ✅ Covered |
| I2.5: ETS state preserved during NATS restart | `test_ets_state_preservation_during_nats_restart` | ✅ Covered |

### I3: Recovery (`recovery`)

**Requirement**: System must recover from faults without requiring manual intervention.

| Invariant | Test Cases | Coverage |
|-----------|------------|----------|
| I3.1: New messages processed successfully after recovery | `test_prolonged_fault_period_recovery_no_router_restart` | ✅ Covered |
| I3.2: Old messages reach expected final state | `test_prolonged_fault_period_recovery_no_router_restart` | ✅ Covered |
| I3.3: No infinite retries | `test_max_delivery_count_exhaustion` | ✅ Covered |
| I3.4: Router reconnects after NATS restart | `test_nats_connection_loss_recovery` | ✅ Covered |

### I4: Absence of Infinite Retries

**Requirement**: Messages should not be retried indefinitely.

| Invariant | Test Cases | Coverage |
|-----------|------------|----------|
| I4.1: Max delivery count limits respected | `test_max_delivery_count_exhaustion` | ✅ Covered |
| I4.2: Messages transition to final state after exhaustion | `test_max_delivery_count_exhaustion` | ✅ Covered |
| I4.3: MaxDeliver exhaustion metric emitted | `test_max_delivery_count_exhaustion` | ✅ Covered |
| I4.4: Warning log written on exhaustion | `test_max_delivery_count_exhaustion` | ✅ Covered |

## Metrics and Logging Coverage

### M1: Error Metrics

**Requirement**: Error metrics must be incremented during faults.

| Metric | Test Cases | Coverage |
|--------|------------|----------|
| M1.1: `router_jetstream_ack_error` | All ACK error tests (R1.1, R1.2) | ✅ Covered |
| M1.2: `router_jetstream_nak_error` | All NAK error tests (R1.3, R1.4) | ✅ Covered |
| M1.3: `router_jetstream_maxdeliver_exhausted_total` | `test_max_delivery_count_exhaustion` | ✅ Covered |

### M2: Recovery Metrics

**Requirement**: Recovery events/metrics must be recorded during recovery.

| Metric | Test Cases | Coverage |
|--------|------------|----------|
| M2.1: Reconnection events | `test_nats_connection_loss_recovery`, `test_jetstream_consumer_reconnection` | ✅ Covered |
| M2.2: Stream restoration events | `test_stream_availability_after_recovery` | ✅ Covered |

### M3: Logging

**Requirement**: Appropriate log entries must be written during faults and recovery.

| Log Type | Test Cases | Coverage |
|----------|------------|----------|
| M3.1: Error logs for ACK/NAK errors | All ACK/NAK error tests | ✅ Covered |
| M3.2: Warning logs for MaxDeliver exhaustion | `test_max_delivery_count_exhaustion` | ✅ Covered |
| M3.3: Info logs for recovery events | Recovery tests (R2.1, R6.1-R6.3) | ✅ Covered |

## Coverage Summary

### Requirements Coverage

| Category | Requirements | Covered | Coverage % |
|----------|--------------|---------|------------|
| R1: Concurrent Fault Handling | 4 | 4 | 100% |
| R2: Prolonged Fault Periods | 1 | 1 | 100% |
| R3: ETS and Delivery Count Integrity | 3 | 3 | 100% |
| R4: Max Delivery Count Exhaustion | 1 | 1 | 100% |
| R5: Multiple Fault → Recovery Cycles | 1 | 1 | 100% |
| R6: NATS/JetStream Restart | 3 | 3 | 100% |
| R7: Extended Recovery Scenarios | 3 | 3 | 100% |
| R8: Triple-Fault Combinations and Extended Mixed Patterns | 8 | 8 | 100% |
| R10: Publish Failure Under High Load with Explicit Retry Model and E2E Circuit Breaker | 3 | 0 | 0% (to be implemented) |
| **Total** | **27** | **24** | **89%** (R10 pending) |

### Invariants Coverage

| Invariant | Test Cases | Coverage % |
|-----------|------------|------------|
| I1: No Router Restart | 14 test cases | 100% |
| I2: Tracking State Consistency | 5 test cases | 100% |
| I3: Recovery | 4 test cases | 100% |
| I4: Absence of Infinite Retries | 4 test cases | 100% |

### Metrics and Logging Coverage

| Category | Metrics/Logs | Covered | Coverage % |
|----------|--------------|---------|------------|
| M1: Error Metrics | 3 | 3 | 100% |
| M2: Recovery Metrics | 2 | 2 | 100% |
| M3: Logging | 3 | 3 | 100% |
| **Total** | **8** | **8** | **100%** |

## Orphaned Requirements

**None identified**. All requirements have explicit test coverage.

## Orphaned Tests

**None identified**. All tests have clear requirements/justification:
- All tests map to specific requirements (R1-R6)
- All tests verify critical invariants (I1-I4)
- All tests check metrics/logging (M1-M3)

## Maintenance

### Adding New Requirements
1. Add requirement to appropriate category (R1-R6)
2. Create test case(s) to cover requirement
3. Update traceability table
4. Update coverage summary

### Adding New Tests
1. Identify requirement(s) covered by test
2. Add test to traceability table
3. Update coverage summary
4. Document in `FAULT_INJECTION_TEST_SCENARIOS.md`

### Removing Requirements/Tests
1. Document reason for removal
2. Update traceability table
3. Update coverage summary
4. Verify no other tests depend on removed requirement/test

## R8: Detailed Coverage

### Triple-Fault Combinations

**Coverage**: All 5 triple-fault patterns are covered by contract tests in `router_triple_fault_contract_SUITE`:

1. **Connect + Publish + ACK** (`test_triple_connect_publish_ack_contract`):
   - Verifies fail-open behavior
   - Verifies MaxDeliver semantics
   - Verifies redelivery limits
   - Verifies metrics correctness
   - Verifies cross-tenant isolation

2. **Connect + Validation + NAK** (`test_triple_connect_validation_nak_contract`):
   - Verifies validation failure handling
   - Verifies NAK/publish issue handling
   - Verifies no infinite loops

3. **Publish + MaxDeliver + Intermittent ACK** (`test_triple_publish_maxdeliver_ack_contract`):
   - Verifies MaxDeliver exhaustion handling
   - Verifies intermittent ACK failure handling
   - Verifies final state transition

4. **Connect + Publish + MaxDeliver** (`test_triple_connect_publish_maxdeliver_contract`):
   - Verifies MaxDeliver exhaustion under multiple faults
   - Verifies final state transition
   - Verifies recovery after exhaustion

5. **ACK + NAK + Publish** (`test_triple_ack_nak_publish_contract`):
   - Verifies ACK/NAK failure handling
   - Verifies no message loss
   - Verifies redelivery correctness

### Extended Mixed Patterns

**Coverage**: Extended mixed-pattern scenarios are covered by:

1. **Intermittent + Persistent Combinations**:
   - `test_multi_fault_mixed_pattern_soak` (stress/soak)
   - `test_mixed_intermittent_connect_persistent_publish` (contract)
   - `test_mixed_persistent_connect_intermittent_ack` (contract)

2. **Cascading Fault Chains**:
   - `test_multi_fault_cascading_soak` (stress/soak)
   - `test_cascading_connect_publish_ack_chain` (contract)

### Contract Invariants Verified

All R8 tests verify the following contract invariants:

- **I1: Fail-Open Behavior**: Router doesn't crash under any fault combination
- **I2: MaxDeliver Semantics**: Messages either deliver or exhaust MaxDeliver
- **I3: Redelivery Limits**: Redelivery count within reasonable bounds
  - **MaxRedelivery**: Router-level limit on redelivery attempts (default: 50 redeliveries)
    - Purpose: Prevents excessive redelivery loops at Router level (safety limit)
    - Metric: `router_jetstream_redelivery_total` (tracks redelivery count)
  - **MaxDeliver**: JetStream-level limit on delivery attempts (default: 3 attempts)
    - Purpose: Prevents infinite retries at JetStream level (hard limit)
    - Metric: `router_jetstream_maxdeliver_exhausted_total` (incremented when limit is reached)
  - **Relationship**: MaxRedelivery is a safety limit, MaxDeliver is the hard limit
    - If MaxDeliver = 3 and MaxRedelivery = 50, MaxDeliver will be reached first (hard limit)
    - MaxRedelivery prevents scenarios where MaxDeliver is high but Router creates excessive redelivery loops
- **I4: Delivery Count Tracking**: `delivery_count` correctly tracked
- **I5: Metrics Correctness**: Metrics accurately reflect system behavior
- **I6: Cross-Tenant Isolation**: Faults for one tenant don't affect other tenants

### Test Suites

- **Contract Tests**: `router_triple_fault_contract_SUITE.erl`
  - Focus: Explicit contract assertions
  - Duration: Short (seconds to minutes)
  - Verification: Detailed contract invariants

- **Stress/Soak Tests**: `router_stress_soak_SUITE.erl`
  - Focus: Long-running stability and resource leaks
  - Duration: Hours (2-8 hours)
  - Verification: Resource stability, performance degradation

- **Advanced Concurrent Faults**: `router_advanced_concurrent_faults_SUITE.erl`
  - Focus: Complex simultaneous fault scenarios
  - Duration: Medium (minutes to hours)
  - Verification: Message semantics, recovery

### Documentation

- **Pattern Catalog**: `TRIPLE_FAULT_PATTERNS_CATALOG.md`
  - Formal catalog of all triple-fault and mixed-pattern scenarios
  - Contract rules for each pattern
  - Expected behavior specifications

- **Usage Guide**: `STRESS_SOAK_TESTS_USAGE.md`
  - How to run stress/soak tests
  - Configuration options
  - Pass/fail criteria

## References

- `FAULT_INJECTION_TEST_SCENARIOS.md`: Detailed test scenarios documentation
- `FAULT_INJECTION_SMOKE_TESTS.md`: Minimal critical smoke test set
- `TRIPLE_FAULT_PATTERNS_CATALOG.md`: Triple-fault patterns catalog with contract rules
- `STRESS_SOAK_TESTS_USAGE.md`: Stress/soak tests usage guide
- `router_triple_fault_contract_SUITE.erl`: Contract tests for triple-fault combinations
- `router_stress_soak_SUITE.erl`: Stress/soak tests for long-running scenarios
- Original TZ/Requirements: [Reference to original requirements document]

