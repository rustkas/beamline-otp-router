# Regression Checklist for Router/JetStream Changes

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Ensure reliability and fault tolerance are maintained when making changes to Router or JetStream logic.

## When to Use This Checklist

**Use this checklist when making changes to**:
- Router consumer logic (`router_result_consumer`, `router_decide_consumer`)
- JetStream configuration (streams, max_deliver, policy)
- Metric contracts (metric names, labels, types)
- NATS/JetStream integration code
- Error handling or retry logic
- Tenant validation logic

## Pre-Change Checklist

### 1. Review Existing Documentation

- [ ] Read `docs/RELIABILITY_FAULT_TOLERANCE.md` to understand reliability guarantees
- [ ] Review `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for concurrent fault scenarios
- [ ] Check `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md` if changing metrics
- [ ] Review `test/FAULT_INJECTION_TEST_SCENARIOS.md` for general fault scenarios

### 2. Identify Affected Tests

- [ ] List all test suites that may be affected by your changes
- [ ] Identify specific test cases that verify the behavior you're changing
- [ ] Note any tests that may need updates due to your changes

**Key Test Suites**:
- `router_result_consumer_SUITE.erl` - Result consumer tests (23 tests)
- `router_decide_consumer_SUITE.erl` - Decide consumer tests (23 tests)
- `router_jetstream_fault_injection_SUITE.erl` - JetStream fault injection (6+ tests)
- `router_concurrent_faults_stress_SUITE.erl` - Stress tests (3 tests)
- `router_nats_connection_failure_SUITE.erl` - NATS connection resilience (22 tests)

## During Change Checklist

### 3. Maintain Test Compatibility

- [ ] Ensure existing tests still pass with your changes
- [ ] Update test expectations if behavior intentionally changed
- [ ] Add new tests if introducing new behavior or scenarios
- [ ] Verify test isolation (no shared state between tests)

### 4. Maintain Metric Contracts

**If changing metrics**:
- [ ] Update `router_metrics_contract_helpers.erl` FIRST (single source of truth)
- [ ] Update tests to use new contract
- [ ] Update `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md` LAST
- [ ] Verify metric labels match contract (required/optional, types, formats)

**See**: `docs/archive/dev/METRICS_CONTRACT_MAINTENANCE.md` for detailed process.

### 5. Maintain Reliability Guarantees

- [ ] Verify process resilience (consumers remain alive during faults)
- [ ] Verify message semantics (no message loss beyond contract)
- [ ] Verify tenant isolation (faults don't cross tenant boundaries)
- [ ] Verify recovery without restart (system recovers automatically)

## Post-Change Checklist

### 6. Run Test Suites

**Required (before PR merge)**:
- [ ] Run `router_result_consumer_SUITE` - All tests pass
- [ ] Run `router_decide_consumer_SUITE` - All tests pass
- [ ] Run `router_jetstream_fault_injection_SUITE` - All tests pass

**Recommended (before PR merge)**:
- [ ] Run `router_nats_connection_failure_SUITE` - All tests pass
- [ ] Run concurrent fault tests multiple times (5 runs) to verify stability

**Optional (nightly/weekly)**:
- [ ] Run `router_concurrent_faults_stress_SUITE` - Stress tests pass
- [ ] Run full fault injection test suite (all suites)

**Commands**:
```bash
# Required tests
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE
rebar3 ct --suite apps/otp/router/test/router_decide_consumer_SUITE
rebar3 ct --suite apps/otp/router/test/router_jetstream_fault_injection_SUITE

# Stability verification (5 runs)
for i in {1..5}; do
    rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
      --case test_ack_error_with_tenant_validation_fail_concurrent \
      --case test_batch_nak_publish_failure_mixed
done

# Stress tests (optional)
rebar3 ct --suite apps/otp/router/test/router_concurrent_faults_stress_SUITE
```

### 7. Verify Documentation Consistency

- [ ] Update `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` if adding new concurrent fault scenarios
- [ ] Update `test/FAULT_INJECTION_TEST_SCENARIOS.md` if adding new fault scenarios
- [ ] Update `docs/RELIABILITY_FAULT_TOLERANCE.md` if changing reliability guarantees
- [ ] Update `docs/PROMETHEUS_ALERTS.md` if changing metrics or alerts
- [ ] Update `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md` if changing metric contracts

### 8. Verify Observability

**If changing metrics**:
- [ ] Verify metrics are exported to Prometheus (if applicable)
- [ ] Verify metric labels match contract
- [ ] Verify alerts are configured (if applicable)
- [ ] Update dashboard queries if metric structure changed

**If changing logs**:
- [ ] Verify log format matches structured JSON format
- [ ] Verify log levels are appropriate (ERROR/WARN/INFO/DEBUG)
- [ ] Verify error codes and tags are present

### 9. CI/CD Verification

- [ ] Verify tests pass in CI environment
- [ ] Check for flakiness (tests should pass consistently)
- [ ] Verify test execution time is acceptable (not too slow)
- [ ] Check CI logs for any warnings or errors

**See**: `test/CONCURRENT_FAULTS_CI_STABILITY.md` for CI stability verification process.

## Specific Change Scenarios

### Scenario A: Changing Consumer Logic

**Example**: Modifying retry logic, error handling, or message processing

**Checklist**:
- [ ] Run all consumer test suites
- [ ] Verify concurrent fault tests still pass
- [ ] Verify tenant isolation tests still pass
- [ ] Verify final state/idempotency tests still pass
- [ ] Check metrics/logs are still emitted correctly

### Scenario B: Changing JetStream Configuration

**Example**: Modifying MaxDeliver, stream policy, or consumer configuration

**Checklist**:
- [ ] Run `router_jetstream_fault_injection_SUITE`
- [ ] Verify MaxDeliver exhaustion tests still pass
- [ ] Verify redelivery tests still pass
- [ ] Update configuration documentation if needed
- [ ] Verify metrics reflect new configuration

### Scenario C: Changing Metric Contracts

**Example**: Adding/removing metric labels, changing metric names, or changing label types

**Checklist**:
- [ ] Follow 3-step process: Code → Tests → Documentation
- [ ] Update `router_metrics_contract_helpers.erl` FIRST
- [ ] Update all tests using affected metrics
- [ ] Update `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md` LAST
- [ ] Verify Prometheus exports (if applicable)
- [ ] Update alerts/dashboards if needed

**See**: `docs/archive/dev/METRICS_CONTRACT_MAINTENANCE.md` for detailed process.

### Scenario D: Changing Error Handling

**Example**: Modifying error recovery, retry logic, or failure modes

**Checklist**:
- [ ] Run all fault injection test suites
- [ ] Verify recovery scenarios still work
- [ ] Verify error metrics/logs are still emitted
- [ ] Verify no new error paths are introduced
- [ ] Check error handling doesn't break reliability guarantees

## Failure Scenarios

### If Tests Fail

1. **Identify root cause**:
   - Review test logs
   - Check if behavior intentionally changed
   - Verify test expectations are correct

2. **Fix or update**:
   - Fix code if bug introduced
   - Update tests if behavior intentionally changed
   - Update documentation if contracts changed

3. **Re-run tests**:
   - Verify all tests pass
   - Run stability verification if needed

### If Documentation Inconsistency Detected

1. **Identify inconsistency**:
   - Compare code vs. documentation
   - Check test expectations vs. documentation

2. **Update documentation**:
   - Update affected documentation files
   - Ensure consistency across all documents

3. **Verify**:
   - Re-read updated documentation
   - Verify links and cross-references work

## Periodic Review

### Weekly Review

- [ ] Review CI test results for flakiness
- [ ] Check for new test failures
- [ ] Review test execution times

### Monthly Review

- [ ] Review test coverage statistics
- [ ] Identify gaps in test coverage
- [ ] Update regression checklist if needed

### Quarterly Review

- [ ] Comprehensive test suite review
- [ ] Documentation consistency audit
- [ ] Process improvement recommendations

## References

- **Reliability Documentation**: `docs/RELIABILITY_FAULT_TOLERANCE.md`
- **Test Documentation**: 
  - `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md`
  - `test/FAULT_INJECTION_TEST_SCENARIOS.md`
- **Metrics Contract**: `docs/archive/dev/METRICS_CONTRACT_SPECIFICATION.md`
- **Metrics Maintenance**: `docs/archive/dev/METRICS_CONTRACT_MAINTENANCE.md`
- **CI Stability**: `test/CONCURRENT_FAULTS_CI_STABILITY.md`
- **Test Stability**: `test/FAULT_INJECTION_TEST_STABILITY.md`

## Change History

**v1.0 (2025-11-30)**:
- Initial regression checklist
- Pre-change, during-change, and post-change checklists
- Specific change scenarios
- Failure scenarios and periodic review

