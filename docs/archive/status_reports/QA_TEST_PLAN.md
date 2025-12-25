# QA Test Plan - Router Component

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: Active

## Purpose

This document provides comprehensive test plan for Router component, including test execution procedures, test data requirements, and validation criteria.

## Test Categories

### 1. Unit Tests

**Scope**: Individual module functionality  
**Location**: `test/router_*_SUITE.erl`  
**Execution**: `rebar3 ct --suite test/<suite_name>`

**Key Test Suites**:
- `router_circuit_breaker_SUITE.erl` - Circuit breaker state machine tests
- `router_policy_SUITE.erl` - Policy loading and parsing tests
- `router_decider_SUITE.erl` - Decision algorithm tests
- `router_rbac_SUITE.erl` - RBAC system tests
- `router_quota_SUITE.erl` - Quota management tests

**Validation Criteria**:
- All test cases pass
- No compilation errors
- No runtime errors
- Proper cleanup between tests

### 2. Integration Tests

**Scope**: Component integration and E2E scenarios  
**Location**: `test/router_*_e2e_SUITE.erl`, `test/router_*_integration_SUITE.erl`  
**Execution**: `rebar3 ct --suite test/<suite_name>`

**Key Test Suites**:
- `router_publish_failure_e2e_SUITE.erl` - E2E publish failure scenarios
- `router_extensions_e2e_SUITE.erl` - Extension pipeline E2E tests
- `router_headers_propagation_e2e_SUITE.erl` - Headers propagation E2E tests
- `router_intake_e2e_SUITE.erl` - Intake processing E2E tests

**Validation Criteria**:
- All E2E scenarios complete successfully
- Proper state transitions verified
- Metrics emitted correctly
- No resource leaks

### 3. Load Tests

**Scope**: Performance under load  
**Location**: `test/router_*_load_SUITE.erl`  
**Execution**: `rebar3 ct --suite test/<suite_name> --profile load`

**Key Test Suites**:
- `router_policy_applier_load_SUITE.erl` - Policy applier load tests
- `router_extensions_pipeline_load_SUITE.erl` - Extension pipeline load tests

**Validation Criteria**:
- System handles target load (1000+ requests)
- Latency within acceptable limits
- No memory leaks
- Metrics reflect load accurately

### 4. Stress Tests

**Scope**: System behavior under extreme conditions  
**Location**: `test/router_*_stress_SUITE.erl`  
**Execution**: `rebar3 ct --suite test/<suite_name> --profile stress`

**Key Test Suites**:
- `router_concurrent_faults_stress_SUITE.erl` - Concurrent fault scenarios
- `router_extensions_chaos_SUITE.erl` - Extension chaos scenarios

**Validation Criteria**:
- System remains stable under stress
- Proper error handling
- Circuit breakers activate correctly
- Recovery mechanisms work

## R10 Circuit Breaker Testing

### Trigger Reason Validation

**CRITICAL**: All R10 tests must verify `trigger_reason` metric using `router_r10_metrics` helpers.

**Best Practices**:
1. **Use constants** instead of hardcoded binaries:
   ```erlang
   Reason = router_r10_metrics:trigger_reason_failure_threshold()
   ```

2. **Accept multiple valid reasons** when checking trigger_reason:
   ```erlang
   case router_r10_metrics:wait_for_trigger_reason(TenantId, ProviderId, [
       router_r10_metrics:trigger_reason_failure_threshold(),
       router_r10_metrics:trigger_reason_error_rate()
   ], 3000) of
       ok -> ok;
       {error, Error} -> ct:fail(Error)
   end.
   ```

3. **Use appropriate timeouts**: 3-5 seconds for state, 2-3 seconds for metrics

4. **Check state first, then metrics** in E2E tests

### Unique Tenant/Provider IDs

**CRITICAL**: Each test scenario must use unique `tenant_id` and `provider_id` pairs to avoid metric conflicts.

**Pattern**:
```erlang
TenantId = <<"test_tenant_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
ProviderId = <<"test_provider_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
```

**Rationale**:
- Prevents metric conflicts between test scenarios
- Ensures clean test isolation
- Enables parallel test execution

### Test Execution Procedures

**Pre-Test Setup**:
1. Start Router application: `router_test_utils:start_router_app/0`
2. Ensure circuit breaker is alive: `router_test_utils:ensure_circuit_breaker_alive/0`
3. Clear metrics: `router_r10_metrics:clear_metrics/0`
4. Reset circuit breaker: `router_test_utils:reset_circuit_breaker/0`

**Post-Test Cleanup**:
1. Clear metrics: `router_r10_metrics:clear_metrics/0`
2. Reset circuit breaker: `router_test_utils:reset_circuit_breaker/0`
3. Stop Router application: `router_test_utils:stop_router_app/0`

**During Test**:
1. Use unique tenant/provider IDs
2. Wait for state transitions before checking metrics
3. Use `wait_for_trigger_reason/4` for trigger reason checks
4. Use `wait_for_metric/3` for metric value checks
5. Dump metrics on failure: `router_r10_metrics:dump_metrics/0`

## Test Data Requirements

### Policy Test Data

**Required Fields**:
- `tenant_id` - Tenant identifier
- `policy_id` - Policy identifier
- `version` - Policy version
- `weights` - Provider weights map
- `defaults` - Default provider settings

**Example**:
```erlang
Policy = #policy{
    tenant_id = ~"test_tenant",
    policy_id = ~"test_policy",
    version = ~"1.0",
    defaults = #{},
    escalate_on = [],
    weights = #{
        ~"openai" => 0.7,
        ~"anthropic" => 0.3
    },
    fallback = undefined,
    sticky = undefined,
    metadata = #{}
}
```

### Circuit Breaker Test Data

**Required Configuration**:
- `failure_threshold` - Number of failures before opening
- `error_rate_threshold` - Error rate threshold (0.0-1.0)
- `error_rate_window_seconds` - Error rate window size
- `latency_threshold_ms` - Latency threshold in milliseconds
- `timeout_ms` - Timeout before half-open transition
- `success_threshold` - Successes required to close
- `half_open_max_calls` - Max calls in half-open state

**Example**:
```erlang
Config = #{
    ~"failure_threshold" => 5,
    ~"error_rate_threshold" => 0.5,
    ~"error_rate_window_seconds" => 30,
    ~"latency_threshold_ms" => 5000,
    ~"timeout_ms" => 1000,
    ~"success_threshold" => 2,
    ~"half_open_max_calls" => 3
}
```

## Test Execution Commands

### Run All Tests

```bash
# Run all test suites
rebar3 ct

# Run specific suite
rebar3 ct --suite test/router_circuit_breaker_SUITE

# Run specific test case
rebar3 ct --suite test/router_circuit_breaker_SUITE --case test_circuit_breaker_opens_on_failure_threshold
```

### Run with Profiles

```bash
# Run with load profile
rebar3 ct --profile load

# Run with stress profile
rebar3 ct --profile stress

# Run with ci profile
rebar3 ct --profile ci
```

### Run with Coverage

```bash
# Run with coverage
rebar3 ct --cover

# Generate coverage report
rebar3 cover
```

## Validation Checklist

### Pre-Commit Validation

- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] No compilation errors
- [ ] No Dialyzer warnings (if enabled)
- [ ] All tests use `router_r10_metrics` (no direct ETS access)
- [ ] All tests use unique tenant/provider IDs
- [ ] All tests have proper cleanup

### Pre-Release Validation

- [ ] All test suites pass
- [ ] Load tests pass with target load
- [ ] Stress tests pass
- [ ] Metrics validation passes
- [ ] Documentation is up to date
- [ ] No known regressions

## Troubleshooting

### Common Issues

**Issue**: Tests fail with `noproc` errors  
**Solution**: Ensure circuit breaker process is alive before test:
```erlang
ok = router_test_utils:ensure_circuit_breaker_alive()
```

**Issue**: Metrics not found  
**Solution**: Wait for metrics with timeout:
```erlang
ok = wait_for_metric(Fun, Expected, 3000)
```

**Issue**: Trigger reason mismatch  
**Solution**: Accept multiple valid reasons:
```erlang
case router_r10_metrics:wait_for_trigger_reason(TenantId, ProviderId, [
    router_r10_metrics:trigger_reason_failure_threshold(),
    router_r10_metrics:trigger_reason_error_rate()
], 3000) of
    ok -> ok;
    {error, Error} -> ct:fail(Error)
end.
```

**Issue**: Test conflicts between scenarios  
**Solution**: Use unique tenant/provider IDs per scenario

## References

- `R10_P0_COMPLETE_FINAL.md` - R10 architecture documentation
- `R10_MAINTENANCE_CHECKLIST.md` - R10 maintenance procedures
- `OBSERVABILITY_CONVENTIONS.md` - Metrics conventions
- `router_r10_metrics.erl` - Metrics access layer
- `router_test_utils.erl` - Test utilities

