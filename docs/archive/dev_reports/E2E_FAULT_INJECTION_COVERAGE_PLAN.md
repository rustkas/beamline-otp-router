# E2E Fault Injection Coverage Plan

**Version**: 1.0  
**Date**: 2025-11-30  
**Status**: 📋 Planning

## Purpose

This document defines the plan to ensure all fault injection scenarios from `JETSTREAM_FAULT_INJECTION_TESTS.md` are covered by E2E tests in:
- `router_jetstream_e2e_SUITE.erl`
- `router_delivery_count_tracking_SUITE.erl`

## Goal

Ensure that every fault injection scenario described in `JETSTREAM_FAULT_INJECTION_TESTS.md` has explicit E2E test coverage, with clear mapping between specification scenarios and test cases.

## Test Suite Responsibilities

**Clear separation of concerns**:

- **`router_jetstream_e2e_SUITE.erl`**: End-to-end behavior of Router under fault conditions:
  - Process resilience (process remains alive)
  - Metrics emission (redelivery, MaxDeliver exhaustion)
  - Message processing flow (ACK/NAK, redelivery)
  - Integration with JetStream/NATS

- **`router_delivery_count_tracking_SUITE.erl`**: Delivery count tracking logic and invariants under fault conditions:
  - Delivery count increment/decrement logic
  - MaxDeliver exhaustion detection
  - ETS table state consistency
  - Delivery count tracking under ACK/NAK failures

## Work Breakdown

### Phase 1: Analysis and Mapping

#### Task 1.1: Analyze Specification

**Input**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`

**Actions**:
1. Read and parse all fault injection scenarios
2. Extract scenario details:
   - Fault types (ACK/NAK errors, timeouts, delays, MaxDeliver exhaustion)
   - Expected system behavior (retries, redeliveries, metrics, state changes)
   - Preconditions and postconditions (stream state, subscriptions, messages)

**Output**: Structured list of scenarios:
- Scenario ID
- Scenario name
- Fault type
- Expected behavior
- Required SUITE (router_jetstream_e2e_SUITE | router_delivery_count_tracking_SUITE)

**Scenarios from Specification**:

| ID | Name | Fault Type | Expected Behavior | Required SUITE |
|----|------|------------|-------------------|----------------|
| S1 | Intermittent ACK/NAK Errors | ACK fails on first attempt | Router continues, retries ACK, processes message. **IMPORTANT**: Redelivery metric is **NOT** emitted (ACK failure doesn't trigger redelivery) | router_jetstream_e2e_SUITE |
| S2 | Processing Delays Causing Redelivery Growth | Tenant validation fails | Router NAKs message, emits redelivery metric, continues | router_jetstream_e2e_SUITE |
| S3 | MaxDeliver Exhaustion for Partial Messages | Message exceeds MaxDeliver limit | Router emits exhaustion metric, removes tracking, continues | router_delivery_count_tracking_SUITE |

#### Task 1.2: Review `router_jetstream_e2e_SUITE.erl`

**Input**: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl.skip`

**Actions**:
1. Review all tests related to:
   - JetStream/NATS error injection
   - Component restarts
   - Delivery guarantee violations
2. For each test:
   - Identify which scenario from specification it covers
   - Map: `E2E-test → fault-injection-scenario`
   - Note partial coverage (if expected behavior is not fully verified)

**Output**: Mapping table:
- Test name → Scenario ID(s)
- Coverage status (full | partial | none)

**Current Tests Analysis**:

| Test Name | Scenario Mapping | Coverage Status | Notes |
|-----------|------------------|-----------------|-------|
| `test_message_nak_redelivery` | S2 (partial) | Partial | Tests NAK, but doesn't verify redelivery metric |
| `test_message_redelivery_on_failure` | S2 (partial) | Partial | Tests NAK on validation failure, but doesn't verify metric |
| `test_nak_redelivery_on_validator_error` | S2 (partial) | Partial | Tests NAK and redelivery metric, but doesn't verify delivery count |
| `test_redelivery_until_ack_or_maxdeliver` | S3 (partial) | Partial | Tests MaxDeliver exhaustion, but doesn't verify partial message handling |
| `test_message_acknowledgment` | S1 (partial) | Partial | Tests ACK, but doesn't test intermittent failures |

**Gaps Identified**:
- S1: No test for intermittent ACK failures (first fails, subsequent succeeds)
- S2: Tests verify NAK and metrics, but don't verify delivery count tracking
- S3: Tests verify MaxDeliver exhaustion, but don't verify partial message handling (some messages succeed, others exhaust)

#### Task 1.3: Review `router_delivery_count_tracking_SUITE.erl`

**Input**: `apps/otp/router/test/router_delivery_count_tracking_SUITE.erl`

**Actions**:
1. Review tests that check delivery count tracking under fault conditions
2. Map tests to specification scenarios
3. Identify gaps (e.g., delivery count tracking under specific fault types)

**Output**: Mapping table with coverage status

**Current Tests Analysis**:

| Test Name | Scenario Mapping | Coverage Status | Notes |
|-----------|------------------|-----------------|-------|
| `test_delivery_count_tracking` | None | None | Basic tracking, no fault injection |
| `test_delivery_count_increment` | None | None | Increment logic, no fault injection |
| `test_maxdeliver_exhaustion_emits_metric` | S3 (partial) | Partial | Tests exhaustion metric, but no fault injection context |
| `test_maxdeliver_exhaustion_removes_tracking` | S3 (partial) | Partial | Tests tracking removal, but no fault injection context |
| `test_maxdeliver_not_exhausted` | None | None | Basic logic, no fault injection |
| `test_cleanup_after_ack` | None | None | Cleanup logic, no fault injection |
| `test_concurrent_delivery_count_tracking` | None | None | Concurrent tracking, no fault injection |

**Gaps Identified**:
- No tests verify delivery count tracking under ACK/NAK failures (S1)
- No tests verify delivery count tracking under processing delays (S2)
- No tests verify delivery count tracking for partial messages (S3)

### Phase 2: Gap Analysis and Requirements

#### Task 2.1: Create Coverage Matrix

**Output**: Matrix showing:
- Scenario → E2E test(s) or "NO COVERAGE"
- Partial coverage details (what's missing)

**Coverage Matrix** (Updated after implementation):

| Scenario | router_jetstream_e2e_SUITE | router_delivery_count_tracking_SUITE | Status |
|----------|----------------------------|--------------------------------------|--------|
| S1: Intermittent ACK/NAK Errors | ✅ `test_intermittent_ack_failure_recovery/1` | ✅ `test_delivery_count_tracking_under_ack_failures/1` | **COVERED** |
| S2: Processing Delays → Redelivery Growth | ✅ `test_processing_delays_redelivery_with_delivery_count/1` | ✅ `test_processing_delays_redelivery_with_delivery_count/1` | **COVERED** |
| S3: MaxDeliver Exhaustion (Partial Messages) | ✅ `test_maxdeliver_exhaustion_partial_messages_e2e/1` | ✅ Existing tests (`test_maxdeliver_exhaustion_*`) | **COVERED** |

**Legend**:
- ✅ Full coverage
- ⚠️ Partial coverage (missing some verifications)
- ❌ No coverage

#### Task 2.2: Formalize Requirements for New Tests

**For each uncovered/partially covered scenario**, document:

1. **Test location**: Which SUITE should contain the test
2. **Test name**: Proposed test function name
3. **Preconditions**: Stream state, subscriptions, fault injection setup
4. **Steps**: What to break, what operations to trigger
5. **Expected results**: Message state, delivery count, behavior, metrics

**New Test Requirements**:

##### Test 1: `test_intermittent_ack_failure_recovery` (router_jetstream_e2e_SUITE.erl)

**Scenario**: S1 - Intermittent ACK/NAK Errors

**Preconditions**:
- Router started with mock NATS
- Message ready for processing
- Fault injection enabled: first ACK fails, subsequent ACKs succeed

**Steps**:
1. Enable fault injection: `router_nats_fault_injection:enable_fault(ack, {error, connection_refused})`
2. Process message (ACK should fail)
3. Verify Router process remains alive
4. Disable fault injection
5. Process same message again (ACK should succeed)

**Expected Results**:
- ✅ Router process remains alive after ACK failure
- ✅ ACK errors are handled gracefully (no crash)
- ✅ Message processing continues after ACK failure
- ✅ Second attempt succeeds
- ✅ **Redelivery metric is NOT emitted** (ACK failure doesn't trigger redelivery) - **This is a critical negative assertion**

**Location**: `router_jetstream_e2e_SUITE.erl`

##### Test 2: `test_processing_delays_redelivery_with_delivery_count` (router_delivery_count_tracking_SUITE.erl)

**Scenario**: S2 - Processing Delays Causing Redelivery Growth

**Preconditions**:
- Router started with mock NATS
- Delivery count tracking enabled
- Tenant validation configured to fail for specific tenant

**Steps**:
1. Send message with invalid tenant (triggers NAK)
2. Verify NAK is called
3. Verify redelivery metric is emitted
4. Verify delivery count is incremented
5. Send same message again (redelivery)
6. Verify delivery count increments again

**Expected Results**:
- ✅ NAK is called when tenant validation fails
- ✅ Redelivery metric (`router_jetstream_redelivery_total`) is emitted with correct metadata
- ✅ **Metric includes required labels**: `assignment_id`, `request_id`, `reason`, `source`
- ✅ Delivery count is tracked and incremented on each redelivery
- ✅ Delivery count matches redelivery attempts

**Location**: `router_delivery_count_tracking_SUITE.erl`

##### Test 3: `test_maxdeliver_exhaustion_partial_messages_e2e` (router_jetstream_e2e_SUITE.erl)

**Scenario**: S3 - MaxDeliver Exhaustion for Partial Messages

**Preconditions**:
- Router started with mock NATS
- MaxDeliver = 3
- Two messages: Message 1 (invalid tenant), Message 2 (valid tenant)

**Steps**:
1. Send Message 1 with invalid tenant (triggers NAK, delivery count = 1)
2. Send Message 2 with valid tenant (should succeed)
3. Redeliver Message 1 (delivery count = 2)
4. Redeliver Message 1 again (delivery count = 3, MaxDeliver exhausted)
5. Verify Message 1 exhaustion metric is emitted
6. Verify Message 1 tracking entry is removed
7. Verify Message 2 is processed successfully (not affected)

**Expected Results**:
- ✅ MaxDeliver exhaustion metric (`router_jetstream_maxdeliver_exhausted_total`) is emitted for Message 1
- ✅ **Metric includes required labels**: `assignment_id`, `request_id`, `msg_id`, `delivery_count`, `max_deliver`, `reason`
- ✅ Message 1 tracking entry is removed after exhaustion
- ✅ Message 2 is processed successfully (not affected by Message 1 exhaustion)
- ✅ Idempotency is preserved (Message 2 can be processed multiple times safely)
- ✅ Router process remains alive

**Location**: `router_jetstream_e2e_SUITE.erl`

##### Test 4: `test_delivery_count_tracking_under_ack_failures` (router_delivery_count_tracking_SUITE.erl)

**Scenario**: S1 - Delivery count tracking under ACK failures

**Preconditions**:
- Router started with mock NATS
- Delivery count tracking enabled
- Fault injection enabled: ACK fails intermittently

**Steps**:
1. Enable fault injection: first ACK fails
2. Process message (ACK fails, but delivery count should be tracked)
3. Verify delivery count is tracked even if ACK fails
4. Disable fault injection
5. Process same message again (ACK succeeds)
6. Verify delivery count increments

**Expected Results**:
- ✅ Delivery count is tracked even when ACK fails
- ✅ Delivery count increments on each delivery attempt
- ✅ ACK failure doesn't prevent delivery count tracking

**Location**: `router_delivery_count_tracking_SUITE.erl`

### Phase 3: Implementation

#### Task 3.1: Add Tests to `router_jetstream_e2e_SUITE.erl`

**Actions**:
1. Remove `.skip` extension (if tests are ready to run)
2. Add new test: `test_intermittent_ack_failure_recovery`
3. Enhance existing test: `test_nak_redelivery_on_validator_error` (add delivery count verification)
4. Add new test: `test_maxdeliver_exhaustion_partial_messages_e2e`
5. Ensure all tests follow existing test style and patterns

**Requirements**:
- Use `router_nats_fault_injection` module for fault injection
- Verify process health (process remains alive)
- Verify metrics via telemetry handlers
- Use `test_helpers:wait_for_condition/2` for async verification

#### Task 3.2: Add Tests to `router_delivery_count_tracking_SUITE.erl`

**Actions**:
1. Add new test: `test_processing_delays_redelivery_with_delivery_count`
2. Add new test: `test_delivery_count_tracking_under_ack_failures`
3. Enhance existing tests to include fault injection context where applicable

**Requirements**:
- Use fault injection infrastructure
- Verify delivery count tracking under fault conditions
- Verify metrics and state consistency

### Phase 4: Validation and Documentation

#### Task 4.1: Run All E2E Tests

**Actions**:
1. Run `router_jetstream_e2e_SUITE`:
   ```bash
   cd apps/otp/router
   rebar3 ct --suite router_jetstream_e2e_SUITE
   ```
2. Run `router_delivery_count_tracking_SUITE`:
   ```bash
   rebar3 ct --suite router_delivery_count_tracking_SUITE
   ```
3. **Run tests multiple times** (at least 3 times) to ensure stability under fault injection:
   - Fault injection scenarios may introduce flakiness
   - Verify consistent pass rate
4. Verify all tests pass consistently
5. Document any flaky tests with details

**Output**: Test execution report with:
- Pass/fail status for each test
- Execution time
- Number of runs performed
- Any flaky test notes (with frequency if observed)

#### Task 4.2: Update Documentation

**Actions**:
1. Create/update coverage matrix document
2. Update `JETSTREAM_FAULT_INJECTION_TESTS.md` with E2E test references
3. Document any scenarios that remain uncovered (with justification)

**Output**: Updated documentation with:
- Matrix: `Scenario → E2E Test(s)`
- Test execution status
- Coverage gaps (if any) with justification

## Acceptance Criteria

✅ **Each scenario** from `JETSTREAM_FAULT_INJECTION_TESTS.md`:
- Has explicit E2E test coverage, OR
- Is documented as not covered with justification

✅ **Repository contains**:
- **Updated coverage matrix** with explicit test names and SUITE locations: `specification → E2E test(s)` (e.g., `S1 → router_jetstream_e2e_SUITE:test_intermittent_ack_failure_recovery/1`)
- All E2E tests in `router_jetstream_e2e_SUITE.erl` and `router_delivery_count_tracking_SUITE.erl` pass
- Coverage matrix is updated in this document and cross-referenced in `JETSTREAM_FAULT_INJECTION_TESTS.md`

✅ **Test Quality**:
- Tests follow existing patterns and style
- Tests are stable (no flaky tests)
- Tests verify all expected behaviors from specification

## References

- **Specification**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **E2E Suite**: `apps/otp/router/test/router_jetstream_e2e_SUITE.erl.skip`
- **Delivery Count Suite**: `apps/otp/router/test/router_delivery_count_tracking_SUITE.erl`
- **Fault Injection Infrastructure**: `apps/otp/router/src/router_nats_fault_injection.erl` (if exists)

## Implementation Status

1. ✅ Create this plan document
2. ✅ **Phase 1: Analysis and Mapping** - **COMPLETED** (scenarios analyzed, current tests reviewed, gaps identified)
3. ✅ **Phase 2: Gap Analysis and Requirements** - **COMPLETED** (coverage matrix created, test requirements formalized)
4. ✅ **Phase 3: Implementation** - **COMPLETED** (all scenarios implemented)
5. ⏳ **Phase 4: Validation and Documentation** - **IN PROGRESS** (tests implemented, need to run and verify)

**Current Status**: All test implementations completed. Ready for Phase 4: running tests and final documentation updates.

## Implemented Tests

### router_jetstream_e2e_SUITE.erl

1. ✅ **`test_intermittent_ack_failure_recovery/1`** (S1)
   - Verifies Router handles intermittent ACK failures gracefully
   - Checks process health, redelivery metric absence, recovery

2. ✅ **`test_processing_delays_redelivery_with_delivery_count/1`** (S2)
   - Verifies redelivery tracking with delivery count
   - Checks NAK calls, redelivery metrics with required labels, delivery count increments

3. ✅ **`test_maxdeliver_exhaustion_partial_messages_e2e/1`** (S3)
   - Verifies MaxDeliver exhaustion for partial messages
   - Checks exhaustion metrics with required labels, tracking removal, partial message handling

### router_delivery_count_tracking_SUITE.erl

1. ✅ **`test_delivery_count_tracking_under_ack_failures/1`** (S1)
   - Verifies delivery count tracking under ACK failures
   - Checks tracking persistence, increments, cleanup

2. ✅ **`test_processing_delays_redelivery_with_delivery_count/1`** (S2)
   - Verifies delivery count tracking under processing delays
   - Checks increments on redeliveries, count accuracy

