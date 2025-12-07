# Fault Injection Test Scenarios

## Overview

This document describes the fault injection and recovery test scenarios for router consumers (`router_result_consumer` and `router_decide_consumer`). These tests verify system resilience, state consistency, and recovery mechanisms under various fault conditions.

**See also**: `FAULT_INJECTION_SMOKE_TESTS.md` for minimal critical smoke test set for production incidents.

## Test Suites

### `router_result_consumer_SUITE.erl`
Tests for the result consumer that processes execution results from CAF and emits usage events.

**Key scenarios:**
- Concurrent fault handling (ACK errors + tenant validation failures)
- Prolonged fault periods with recovery
- ETS delivery count consistency
- Max delivery count exhaustion
- Multiple fault → recovery cycles

### `router_decide_consumer_SUITE.erl`
Tests for the decide consumer that processes routing decisions and publishes replies.

**Key scenarios:**
- Concurrent fault handling (ACK errors + tenant validation failures)
- Prolonged fault periods with recovery
- ETS delivery count consistency
- Max delivery count exhaustion
- Multiple fault → recovery cycles

### `router_jetstream_fault_injection_SUITE.erl`
Integration tests for NATS/JetStream fault scenarios.

**Key scenarios:**
- NATS connection loss and recovery
- JetStream consumer reconnection
- Stream availability after recovery
- ETS state preservation during NATS restart

## Test Scenarios

### 1. Concurrent Faults

**Scenario S1: ACK error + tenant validation fail (concurrent messages)**
- **Description**: Message A has successful tenant validation but ACK fails, Message B fails tenant validation in parallel
- **Verifies**: 
  - No infinite retries
  - No message loss
  - No process crash
  - Tracking state consistency: ETS delivery counts remain consistent
- **Tests**: 
  - `test_ack_error_with_tenant_validation_fail_concurrent`
  - `test_decide_ack_error_with_tenant_validation_fail_concurrent`

**Scenario S2: ACK error + tenant validation fail (same message, sequential)**
- **Description**: Message passes validation initially, but ACK fails, then on retry, tenant validation fails
- **Verifies**: 
  - Tracking state consistency: delivery counts increment correctly
  - No router restart: consumer continues processing without restart
- **Tests**: 
  - `test_ack_error_with_tenant_validation_fail_same_message`
  - `test_decide_ack_error_with_tenant_validation_fail_same_message`

**Scenario S3: NAK + publish failure with recovery**
- **Description**: Publish result/reply fails, consumer does NAK, then publish succeeds after recovery
- **Verifies**: 
  - Recovery: consumer recovers without router restart
  - Tracking state consistency: delivery counts and ETS state remain consistent
- **Tests**: 
  - `test_nak_with_publish_failure_recovery`
  - `test_decide_nak_with_publish_failure_recovery`
  - `test_redelivery_metric_labels` (metric contract validation)
  - `test_redelivery_tenant_validation_failed` (tenant validation scenario)
- **Observability Links**:
  - **Alert**: `RouterJetStreamRedeliveryHigh` in `docs/PROMETHEUS_ALERTS.md` (triggered when redelivery rate > 20/min)
  - **Metric**: `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}`
  - **Dashboard**: See `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` for redelivery monitoring queries

**Scenario S4: Batch messages with mixed NAK + publish failure**
- **Description**: Multiple messages, some with publish failures causing NAK, others succeed normally
- **Verifies**: 
  - No router restart: consumer processes all messages without restart
  - Tracking state consistency: ETS state remains consistent across mixed failures
  - No global blocking
- **Tests**: 
  - `test_batch_nak_publish_failure_mixed`
  - `test_decide_batch_nak_publish_failure_mixed`

### 2. Prolonged Fault Periods

**Scenario: Extended fault period with recovery (no router restart)**
- **Description**: Extended period of ACK/publish failures (first 10 calls fail), then recovery
- **Verifies**: 
  - Router continues living
  - Processes new messages after recovery without restart
  - Old messages reach expected final state
- **Tests**: 
  - `test_prolonged_fault_period_recovery_no_router_restart`
  - `test_decide_prolonged_fault_period_recovery_no_router_restart`

### 3. ETS and Delivery Count Integrity

**Scenario: ETS delivery count consistency during faults**
- **Description**: Delivery counts tracked correctly before/during/after faults
- **Verifies**: 
  - No "eternal" entries
  - Expected delivery count increases
  - No "carried over" artifacts for new messages after recovery
- **Tests**: 
  - `test_ets_delivery_count_consistency_during_faults`
  - `test_decide_ets_delivery_count_consistency_during_faults`

**Scenario: ETS cleanup after recovery**
- **Description**: ETS entries are properly cleaned up after successful processing
- **Verifies**: 
  - No stale entries remain that could affect future processing
- **Tests**: 
  - `test_ets_cleanup_after_recovery`
  - `test_decide_ets_cleanup_after_recovery`

### 4. Max Delivery Count Exhaustion

**Scenario: Max delivery count exhaustion**
- **Description**: Message goes through series of NAK/errors until max delivery count (default: 3)
- **Verifies**: 
  - Router/consumer does not enter infinite retry
  - Transitions to expected final state (dead-letter / drop / special metric)
  - MaxDeliver exhaustion metric is emitted
  - Warning log is written
- **Tests**: 
  - `test_max_delivery_count_exhaustion`
  - `test_decide_max_delivery_count_exhaustion`
  - `test_maxdeliver_exhausted_metric_labels` (metric contract validation)
  - `test_maxdeliver_exhausted_different_limits` (different MaxDeliver values)
- **Observability Links**:
  - **Alert**: `RouterJetStreamMaxDeliverExhausted` in `docs/PROMETHEUS_ALERTS.md`
  - **Metric**: `router_jetstream_maxdeliver_exhausted_total{assignment_id,request_id,msg_id,delivery_count,max_deliver,reason}`
  - **Dashboard**: See `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` for monitoring queries

### 5. Multiple Fault → Recovery Cycles

**Scenario: Multiple fault → recovery cycles**
- **Description**: Two short fault periods in a row
- **Verifies**: 
  - ETS tracking and delivery count don't "drift" from multiple recoveries
  - New messages after cycles have normal delivery counts (not "tainted")
- **Tests**: 
  - `test_multiple_fault_recovery_cycles`
  - `test_decide_multiple_fault_recovery_cycles`

### 6. Extended Concurrent Fault Scenarios

**Scenario: Prolonged faults with consumer restart**
- **Description**: During a fault period (ACK/NAK/publish errors), consumer restarts, then recovers
- **Verifies**: 
  - All subscriptions are restored after restart
  - Messages are not lost during restart
  - No double processing beyond expected retries
  - Consumer continues processing after restart
- **Tests**: 
  - `test_prolonged_faults_with_consumer_restart`
  - `test_decide_prolonged_faults_with_consumer_restart`

**Scenario: Tenant isolation during concurrent faults**
- **Description**: Tenant A generates combinations of failures (ACK error + validation fail, NAK + publish failure), Tenant B processes without failures in parallel
- **Verifies**: 
  - Tenant B is not slowed down/blocked due to Tenant A faults
  - Metrics/logs for Tenant A and Tenant B are not mixed
  - Tenant isolation is maintained under concurrent faults
- **Tests**: 
  - `test_tenant_isolation_during_concurrent_faults`
  - `test_decide_tenant_isolation_during_concurrent_faults`

**Scenario: Final state and idempotency with multiple retries**
- **Description**: Complex scenario with multiple retries, verifying final message states and idempotency
- **Verifies**: 
  - Each message reaches final status (success, DLQ, finally rejected)
  - Number of processing/publications does not exceed contract (idempotency at business result level)
  - No duplicate processing beyond designed retries
- **Tests**: 
  - `test_final_state_and_idempotency_multi_retry`
  - `test_decide_final_state_and_idempotency_multi_retry`

**Scenario: Comprehensive metrics and logging validation**
- **Description**: Simultaneous ACK errors, tenant validation fail, and publish failures
- **Verifies**: 
  - All necessary metrics are incremented with correct metadata (tenant, reason)
  - No contradictory metrics appear (e.g., success + tenant_fail simultaneously)
  - No duplicate metrics for the same event
  - Logs are correctly structured and contain expected information
- **Tests**: 
  - `test_comprehensive_metrics_and_logging_validation`
  - `test_decide_comprehensive_metrics_and_logging_validation`

### 7. NATS/JetStream Restart

**Scenario: NATS connection loss and recovery**
- **Description**: NATS connection is lost, then restored
- **Verifies**: 
  - Router reconnects
  - Consumers function
  - New messages processed without anomalies
- **Tests**: 
  - `test_nats_connection_loss_recovery`

**Scenario: JetStream consumer reconnection**
- **Description**: JetStream consumer subscription is lost, then re-established
- **Verifies**: 
  - Consumer reconnects
  - Continues processing messages
- **Tests**: 
  - `test_jetstream_consumer_reconnection`

**Scenario: Stream availability after recovery**
- **Description**: JetStream stream becomes unavailable, then restored
- **Verifies**: 
  - Router handles stream unavailability
  - Recovers when stream is restored
- **Tests**: 
  - `test_stream_availability_after_recovery`

**Scenario: ETS state preservation during NATS restart**
- **Description**: NATS/JetStream is explicitly stopped/started or fully simulated as unavailable
- **Verifies**: 
  - ETS tables (delivery counts, tracking state) are preserved during NATS restart
  - No state corruption
  - Processing continues correctly after recovery
- **Tests**: 
  - `test_ets_state_preservation_during_nats_restart`

## Critical Invariants

### 1. No Router Restart (`no_router_restart`)
- **Requirement**: Router/consumer processes must remain alive throughout all fault scenarios
- **Verification**: `is_process_alive(ConsumerPid)` checks in all tests
- **Rationale**: System should recover without requiring process restarts

### 2. Tracking State Consistency (`tracking_state_consistency`)
- **Requirement**: ETS delivery counts and tracking state must remain consistent before, during, and after faults
- **Verification**: 
  - Delivery count snapshots before/during/after faults
  - No "eternal" entries
  - Expected delivery count increases
  - No "carried over" artifacts for new messages
- **Rationale**: State corruption can lead to incorrect message processing

### 3. Recovery (`recovery`)
- **Requirement**: System must recover from faults without requiring manual intervention
- **Verification**: 
  - New messages processed successfully after recovery
  - Old messages reach expected final state
  - No infinite retries
- **Rationale**: System must be self-healing

### 4. Absence of Infinite Retries
- **Requirement**: Messages should not be retried indefinitely
- **Verification**: 
  - Max delivery count exhaustion tests
  - Delivery count limits respected
  - Messages transition to final state (DLQ/drop) after exhaustion
- **Rationale**: Infinite retries can cause resource exhaustion

## Test Stability and Performance

### Timing Optimizations
- **Replaced fixed `timer:sleep` with bounded waits**: Tests use `test_helpers:wait_for_condition` instead of fixed sleeps where possible
- **Reduced delays**: Processing delays reduced from 100ms to 50ms where appropriate
- **Bounded polling**: All waits use bounded polling with configurable timeouts (default: 10ms interval)

### Helper Functions
- `wait_for_condition/2,3`: Wait for condition to become true (bounded polling)
- `wait_for_meck_call/4,5`: Wait for meck function to be called
- `wait_for_metric/4`: Wait for metric to be emitted
- `wait_for_log/4`: Wait for log entry to be written
- `wait_for_ets_entry/3`: Wait for ETS entry to appear

### Test Independence
- **No order dependencies**: Tests can run in any order
- **Isolated state**: Each test sets up its own mocks and ETS tables
- **Cleanup**: All tests properly clean up mocks and ETS tables

## Metrics and Logging Verification

### Metrics Checked
- `router_jetstream_ack_total`: ACK operations
- `router_jetstream_ack_error`: ACK errors
- `router_jetstream_nak_error`: NAK errors
- `router_jetstream_maxdeliver_exhausted_total`: Max delivery count exhaustion
- `router_redelivery_total`: Message redeliveries

### Logging Checked
- Error logs: ACK/NAK errors, connection failures
- Warning logs: MaxDeliver exhaustion, tenant validation failures
- Info logs: Recovery events, reconnection events

### Verification Approach
- **ETS tracking**: Metrics and logs are captured in ETS tables during tests
- **Post-test verification**: Tests check ETS tables for expected metrics/logs
- **Helper functions**: `wait_for_metric` and `wait_for_log` helpers for bounded polling

## Running Tests

### Run all fault injection tests
```bash
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE
rebar3 ct --suite apps/otp/router/test/router_decide_consumer_SUITE
rebar3 ct --suite apps/otp/router/test/router_jetstream_fault_injection_SUITE
```

### Run specific test
```bash
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE --case test_max_delivery_count_exhaustion
```

### Run with verbose output
```bash
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE --verbose
```

## Maintenance Notes

### Adding New Tests
1. Follow naming convention: `test_<scenario_name>`
2. Include comprehensive doc comments describing scenario and verification criteria
3. Use bounded waits instead of fixed sleeps
4. Clean up all mocks and ETS tables
5. Verify critical invariants (no_router_restart, tracking_state_consistency, recovery)

### Updating Existing Tests
1. Maintain backward compatibility with existing test structure
2. Update documentation when scenarios change
3. Ensure all timing optimizations are applied
4. Verify test independence (no order dependencies)

## Test Differentiation

### New Fault Injection Tests vs Existing Recovery Tests

**New tests** (this document) focus on:
- **Concurrent fault handling**: Multiple simultaneous error conditions
- **During-fault consistency**: ETS/delivery count integrity during faults
- **Recovery without restart**: Router continues living and recovers automatically
- **Production incident scenarios**: NATS restart, max delivery count exhaustion

**Existing tests** (`router_recovery_state_integrity_SUITE`) focus on:
- **Post-recovery state integrity**: ETS/delivery count accuracy after recovery
- **Idempotency consistency**: Idempotency entries preserved during recovery
- **State preservation**: ETS size and content after connection loss

**Key difference**: New tests verify **behavior during faults**, existing tests verify **state after recovery**.

See `FAULT_INJECTION_TEST_AUDIT.md` for detailed analysis of test differentiation and overlap.

## Observability Integration

### Metrics and Alerts

Fault injection scenarios are linked to observability metrics and alerts:

| Scenario | Metric | Alert | Test Coverage |
|----------|--------|-------|---------------|
| **Redelivery (NAK)** | `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}` | `RouterJetStreamRedeliveryHigh` (> 20/min) | `test_redelivery_metric_labels`, `test_redelivery_tenant_validation_failed`, `test_nak_with_publish_failure_recovery` |
| **MaxDeliver Exhaustion** | `router_jetstream_maxdeliver_exhausted_total{assignment_id,request_id,msg_id,delivery_count,max_deliver,reason}` | `RouterJetStreamMaxDeliverExhausted` (> 0/min) | `test_maxdeliver_exhausted_metric_labels`, `test_maxdeliver_exhausted_different_limits`, `test_max_delivery_count_exhaustion` |

**See also**:
- `docs/PROMETHEUS_ALERTS.md` - Alert definitions and thresholds
- `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` - Dashboard queries and monitoring
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` - Metric contract validation tests

### From Alert to Test

**Workflow**: When an alert fires:
1. Check alert definition in `docs/PROMETHEUS_ALERTS.md` (see `test_coverage` annotation)
2. Find related test scenario in this document (search by scenario name or metric)
3. Run smoke test for quick validation (see `FAULT_INJECTION_SMOKE_TESTS.md`)
4. Review test guarantees to understand expected behavior
5. Check runbook for troubleshooting steps (see `FAULT_INJECTION_RUNBOOK_INTEGRATION.md`)

### Adding New Alerts/Metrics

**When adding a new alert or metric** (in `docs/PROMETHEUS_ALERTS.md` or observability docs):

1. **Check for corresponding fault injection scenario**:
   - Search `FAULT_INJECTION_TEST_SCENARIOS.md` for related scenarios
   - If scenario exists → proceed to step 2
   - If scenario doesn't exist → decide if fault injection test is needed:
     - **Yes**: Add scenario following `FAULT_INJECTION_MAINTENANCE_PROCESS.md` (Process 1)
     - **No**: Document why (e.g., metric is informational only, not fault-related)

2. **Add `test_coverage` annotation**:
   - In alert definition, add: `test_coverage: "Fault injection tests: [list tests]. See apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md#[scenario-link]"`
   - Include both metric contract validation tests (if any) and real scenario tests

3. **Update observability integration table**:
   - Add entry to "Observability Integration" table in this document
   - Link scenario to metric and alert

**See also**: `FAULT_INJECTION_MAINTENANCE_PROCESS.md` (Process 5: Metrics Contract Synchronization) for detailed metrics contract maintenance.

## References

- `apps/otp/router/test/test_helpers.erl`: Helper functions for bounded waits and polling
- `apps/otp/router/src/router_jetstream.erl`: JetStream interaction module
- `apps/otp/router/src/router_result_consumer.erl`: Result consumer implementation
- `apps/otp/router/src/router_decide_consumer.erl`: Decide consumer implementation
- `apps/otp/router/test/FAULT_INJECTION_SMOKE_TESTS.md`: Minimal critical smoke test set
- `apps/otp/router/test/FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`: Requirements traceability matrix
- `apps/otp/router/test/FAULT_INJECTION_TEST_AUDIT.md`: Test audit and differentiation analysis
- `apps/otp/router/test/FAULT_INJECTION_MAINTENANCE_PROCESS.md`: Maintenance process documentation
- `apps/otp/router/test/FAULT_INJECTION_TEST_STABILITY.md`: Test stability tracking
- `apps/otp/router/test/FAULT_INJECTION_BACKEND_EXTENSIBILITY.md`: Backend extensibility guide
- `apps/otp/router/test/FAULT_INJECTION_RUNBOOK_INTEGRATION.md`: Runbook integration guide

