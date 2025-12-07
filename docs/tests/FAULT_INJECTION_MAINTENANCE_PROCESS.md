# Fault Injection Test Maintenance Process

## Purpose

This document defines processes for maintaining fault injection test suite quality, traceability, and relevance as the system evolves. It ensures that fault injection tests remain:
- **Accurate**: Tests reflect current system behavior and requirements
- **Complete**: All requirements have test coverage
- **Stable**: Tests don't become flaky or unreliable
- **Relevant**: Smoke tests reflect current production risks

## Process 1: Traceability Maintenance

### When to Update

**Trigger events**:
- Requirements change (new fault scenarios, changed recovery behavior)
- System changes (new backend, changed MaxDeliver policy, new DLQ mechanism)
- Architecture changes (new components, changed message flow)
- Production incidents (new failure patterns discovered)

### Update Checklist

When requirements or system behavior changes:

1. **Update Requirements Traceability** (`FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`):
   - [ ] Add new requirements to appropriate category (R1-R6)
   - [ ] Create test cases for new requirements
   - [ ] Update coverage summary
   - [ ] Verify no orphaned requirements

2. **Update Test Scenarios** (`FAULT_INJECTION_TEST_SCENARIOS.md`):
   - [ ] Document new scenarios
   - [ ] Update scenario descriptions if behavior changed
   - [ ] Add new test cases to appropriate suite
   - [ ] Update test differentiation section if needed

3. **Update Smoke Tests** (`FAULT_INJECTION_SMOKE_TESTS.md`):
   - [ ] Add new smoke tests if scenario becomes critical
   - [ ] Remove smoke tests if scenario is no longer critical
   - [ ] Update smoke test descriptions if behavior changed

4. **Verify Test Coverage**:
   - [ ] Run all fault injection tests
   - [ ] Verify new tests pass
   - [ ] Verify existing tests still pass (no regressions)
   - [ ] Check for orphaned tests (tests without requirements)

### Validation Steps

**Before committing changes**:

1. **Check for orphaned requirements**:
   ```bash
   # Review FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md
   # Verify all requirements have test cases
   # Check "Orphaned Requirements" section is empty
   ```

2. **Check for orphaned tests**:
   ```bash
   # Review FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md
   # Verify all tests have requirements
   # Check "Orphaned Tests" section is empty
   ```

3. **Verify traceability table accuracy**:
   - All test cases listed in traceability table exist in test suites
   - All test cases in test suites are listed in traceability table
   - Test case names match between documentation and code

4. **Run tests**:
   ```bash
   cd apps/otp/router
   rebar3 ct --suite test/router_result_consumer_SUITE \
            --suite test/router_decide_consumer_SUITE \
            --suite test/router_jetstream_fault_injection_SUITE
   ```

### Example: MaxDeliver Policy Change

**Scenario**: MaxDeliver policy changes from 3 to 5

**Steps**:
1. Update `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`:
   - Update R4.1 (Max Delivery Count Exhaustion) description
   - Update test cases to reflect new MaxDeliver value

2. Update test cases:
   - Update `test_max_delivery_count_exhaustion` to use MaxDeliver=5
   - Update assertions to check for exhaustion at 5, not 3

3. Update `FAULT_INJECTION_TEST_SCENARIOS.md`:
   - Update scenario descriptions mentioning MaxDeliver=3

4. Verify tests pass with new policy

## Process 2: Test Stability Monitoring

### Monitoring Strategy

**Track metrics**:
- Flaky test rate (tests that pass/fail intermittently)
- Test failure rate (tests that consistently fail)
- Test execution time (tests that become slow)

**Collection method**:
- CI/CD logs analysis (GitHub Actions, Drone CI, GitLab CI)
- Test execution reports (Common Test HTML reports)
- Manual tracking in issue tracker or documentation

### Monitoring Frequency

- **Weekly**: Review CI/CD logs for fault injection test failures
- **Monthly**: Aggregate statistics and identify trends
- **Quarterly**: Comprehensive review and prioritization

### Tracking Template

**Document**: `FAULT_INJECTION_TEST_STABILITY.md` (see separate document)

**Track per test**:
- Test name and suite
- Failure rate (percentage of runs that fail)
- Flakiness indicator (intermittent failures)
- Last failure date
- Root cause (if identified)
- Fix priority (high/medium/low)
- Status (open/investigating/fixed)

### Action Thresholds

**High priority** (fix within 1 week):
- Test failure rate > 10%
- Test flakiness > 5%
- Test blocking CI pipeline

**Medium priority** (fix within 1 month):
- Test failure rate 5-10%
- Test flakiness 2-5%
- Test not blocking but causing noise

**Low priority** (fix when convenient):
- Test failure rate < 5%
- Test flakiness < 2%
- Test not blocking CI

### Fix Process

1. **Identify root cause**:
   - Review test logs
   - Check for timing dependencies
   - Verify test isolation
   - Check for race conditions

2. **Fix test**:
   - Replace fixed sleeps with bounded waits
   - Improve test isolation
   - Fix race conditions
   - Update assertions if behavior changed

3. **Verify fix**:
   - Run test multiple times (10+ runs)
   - Verify no flakiness
   - Update stability tracking

## Process 3: Smoke Test Review

### Review Frequency

- **Quarterly**: Regular review of smoke test set
- **After major incidents**: Review if new failure patterns discovered
- **After major releases**: Review if system behavior changed significantly

### Review Checklist

1. **Review production incidents**:
   - [ ] Identify new failure patterns
   - [ ] Check if existing smoke tests cover these patterns
   - [ ] Add new smoke tests if needed

2. **Review smoke test relevance**:
   - [ ] Check if smoke tests still reflect critical scenarios
   - [ ] Remove smoke tests for scenarios no longer critical
   - [ ] Simplify smoke tests if they duplicate other checks

3. **Review smoke test stability**:
   - [ ] Check if smoke tests are flaky
   - [ ] Fix flaky smoke tests (high priority)
   - [ ] Remove smoke tests that can't be stabilized

4. **Update documentation**:
   - [ ] Update `FAULT_INJECTION_SMOKE_TESTS.md`
   - [ ] Update smoke test descriptions
   - [ ] Update CI integration if smoke set changed

### Decision Criteria

**Add smoke test** if:
- Scenario is common in production incidents
- Scenario can cause service unavailability
- Scenario requires manual intervention if not handled correctly
- Test runs quickly (< 60 seconds)

**Remove smoke test** if:
- Scenario is no longer relevant (system changed)
- Scenario is covered by other smoke tests (duplication)
- Test is consistently flaky and can't be fixed
- Test is too slow (> 60 seconds)

**Simplify smoke test** if:
- Test checks too many things (split into multiple tests)
- Test has unnecessary complexity
- Test can be made faster without losing coverage

### Review Template

**Quarterly Review** (YYYY-QX):

1. **Production incidents reviewed**: [list incidents]
2. **New smoke tests added**: [list tests]
3. **Smoke tests removed**: [list tests]
4. **Smoke tests simplified**: [list tests]
5. **Stability issues**: [list issues]
6. **Next review date**: [date]

## Process 4: Integration with Runbooks

### Runbook Integration Points

**Key fault scenarios** to link in runbooks:
1. **NATS connection loss** → `test_nats_connection_loss_recovery`
2. **Max delivery count exhaustion** → `test_max_delivery_count_exhaustion`
3. **Prolonged fault periods** → `test_prolonged_fault_period_recovery_no_router_restart`
4. **ETS state corruption** → `test_ets_delivery_count_consistency_during_faults`

### Runbook Update Checklist

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

### Example Runbook Entry

**Scenario**: NATS connection loss

**Test Coverage**:
- Test: `test_nats_connection_loss_recovery` (`router_jetstream_fault_injection_SUITE`)
- Smoke test: Yes (in `FAULT_INJECTION_SMOKE_TESTS.md`)
- Guarantees:
  - Router reconnects automatically after NATS connection loss
  - Consumers continue functioning after reconnection
  - New messages processed without anomalies
  - ETS state preserved during reconnection

**If this scenario occurs in production**:
- Router should recover automatically (verified by test)
- If router doesn't recover, check logs for reconnection errors
- If ETS state is corrupted, check `test_ets_state_preservation_during_nats_restart` scenario

## Process 5: Metrics Contract Synchronization

### Purpose

Ensure that metric contract changes (in `docs/PROMETHEUS_ALERTS.md` or code) are properly reflected in:
- Test contract constants (in test files)
- Fault injection documentation (if metrics are referenced)
- Runbooks (if metrics are used for troubleshooting)

### When to Update

**Trigger events**:
- Metric contract changes in `docs/PROMETHEUS_ALERTS.md` (new labels, removed labels, label type changes)
- Metric contract changes in code (new labels added to `emit_counter` calls)
- Metric name changes
- Label value format changes (e.g., `reason` values change)

### Update Checklist

When metric contract changes:

1. **Update Test Contract Constants**:
   - [ ] Update metric contract constants in test files:
     - `router_jetstream_fault_injection_SUITE.erl`: `?REDELIVERY_REQUIRED_LABELS`, `?REDELIVERY_OPTIONAL_LABELS`, `?MAXDELIVER_REQUIRED_LABELS`, `?MAXDELIVER_OPTIONAL_LABELS`
   - [ ] Update `assert_metric_labels_by_contract/3` if contract structure changes
   - [ ] Run metric validation tests to verify contract compliance

2. **Update Documentation**:
   - [ ] Update `docs/PROMETHEUS_ALERTS.md` if not already updated
   - [ ] Update fault injection documentation if metrics are referenced:
     - `FAULT_INJECTION_TEST_SCENARIOS.md` (if metric descriptions changed)
     - `FAULT_INJECTION_RUNBOOK_INTEGRATION.md` (if runbook references need updates)

3. **Update Runbooks** (if applicable):
   - [ ] Check `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` for metric references
   - [ ] Check other runbooks for metric references
   - [ ] Update metric names/labels if changed
   - [ ] Update metric descriptions if behavior changed

4. **Verify Test Coverage**:
   - [ ] Run metric validation tests: `test_redelivery_metric_labels`, `test_maxdeliver_exhausted_metric_labels`
   - [ ] Verify tests catch contract violations
   - [ ] Update tests if contract allows new optional labels

### Example: Adding New Label to Metric

**Scenario**: Add `tenant_id` label to `router_jetstream_redelivery_total`

**Steps**:
1. Update `docs/PROMETHEUS_ALERTS.md`:
   - Change: `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}` 
   - To: `router_jetstream_redelivery_total{assignment_id,request_id,reason,source,tenant_id}`

2. Update test contract constants in `router_jetstream_fault_injection_SUITE.erl`:
   - Add `tenant_id` to `?REDELIVERY_REQUIRED_LABELS` or `?REDELIVERY_OPTIONAL_LABELS` (depending on contract)

3. Update code to emit `tenant_id` label in `emit_counter` calls

4. Update tests to verify `tenant_id` label is present/valid

5. Update runbooks if `tenant_id` is used for troubleshooting

6. Run tests to verify contract compliance

### Validation Steps

**Before committing changes**:

1. **Run automated coverage check** (recommended first step):
   ```bash
   cd apps/otp/router
   bash scripts/check_fault_injection_coverage.sh
   ```
   This script automatically verifies:
   - `test_coverage` annotations reference existing tests
   - Metric definitions exist in `PROMETHEUS_ALERTS.md`
   - Metric contract constants are defined (if applicable)

2. **Verify contract consistency** (manual check if needed):
   ```bash
   # Check PROMETHEUS_ALERTS.md matches test constants
   grep -A 2 "router_jetstream_redelivery_total" docs/PROMETHEUS_ALERTS.md
   ```

3. **Run metric validation tests** (if applicable):
   ```bash
   cd apps/otp/router
   rebar3 ct --suite test/router_result_consumer_SUITE \
            --case test_max_delivery_count_exhaustion
   ```

4. **Check runbook references**:
   ```bash
   grep -r "router_jetstream_redelivery_total\|router_jetstream_maxdeliver_exhausted_total" docs/OPS_RUNBOOK*.md
   ```

## Process 6: Backend Extensibility

### Current Backend Dependencies

**NATS/JetStream specific**:
- `router_nats` module (NATS connection, publish, subscribe)
- `router_jetstream` module (JetStream ACK/NAK, delivery count tracking)
- JetStream consumer configuration (MaxDeliver, AckPolicy)
- NATS subject patterns (`beamline.router.v1.*`, `caf.exec.result.v1`)

**Reusable abstractions**:
- `test_helpers` module (bounded waits, polling)
- Fault injection patterns (mock-based error injection)
- ETS tracking patterns (delivery count, idempotency)
- Test scenario structure (fault period, recovery, verification)

### Extension Guide

**For new backend (e.g., Kafka, Redis Streams)**:

1. **Identify backend-specific components**:
   - Connection management (replace `router_nats`)
   - Message publishing (replace `router_nats:publish`)
   - Message acknowledgment (replace `router_jetstream:ack/nak`)
   - Consumer configuration (replace JetStream consumer config)

2. **Adapt test helpers**:
   - Update mocks for new backend modules
   - Adapt fault injection for new backend APIs
   - Update ETS tracking if backend has different message IDs

3. **Reuse test scenarios**:
   - Keep same fault scenarios (connection loss, ACK errors, etc.)
   - Adapt test cases to new backend APIs
   - Update assertions for backend-specific behavior

4. **Update documentation**:
   - Document backend-specific dependencies
   - Update traceability for backend-specific requirements
   - Add backend-specific smoke tests if needed

### Documentation Structure

**File**: `FAULT_INJECTION_BACKEND_EXTENSIBILITY.md` (see separate document)

**Sections**:
1. Current backend dependencies (NATS/JetStream)
2. Reusable abstractions
3. Extension guide for new backends
4. Backend-specific test patterns

## Maintenance Schedule

### Daily
- Monitor CI/CD for fault injection test failures

### Weekly
- Review test stability metrics
- Fix high-priority flaky tests

### Monthly
- Aggregate test stability statistics
- Review and prioritize medium-priority issues

### Quarterly
- Review smoke test set
- Review traceability completeness
- Review runbook integration
- Review metrics contract synchronization (verify test constants match `PROMETHEUS_ALERTS.md`)

### On-Demand
- Update traceability when requirements change
- Update smoke tests after major incidents
- Update runbooks when fault scenarios change
- **Process review triggers**: See `FAULT_INJECTION_MAINTENANCE_DRY_RUN.md` (Process Review Triggers section) for events that require revisiting maintenance processes
- **Automated checks**: Run `scripts/check_fault_injection_coverage.sh` before committing changes to metrics/observability docs

## References

- `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`: Requirements traceability matrix
- `FAULT_INJECTION_TEST_SCENARIOS.md`: Test scenarios documentation
- `FAULT_INJECTION_SMOKE_TESTS.md`: Smoke test set
- `FAULT_INJECTION_TEST_STABILITY.md`: Test stability tracking
- `FAULT_INJECTION_BACKEND_EXTENSIBILITY.md`: Backend extensibility guide
- `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`: Router intake runbook
- `docs/OPS_RUNBOOK_GATEWAY_RATE_LIMITING.md`: Gateway rate limiting runbook

