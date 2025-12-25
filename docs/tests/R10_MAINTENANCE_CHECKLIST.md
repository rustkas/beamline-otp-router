# R10 Maintenance Checklist

**Purpose**: Checklist for making changes to R10 (Circuit Breaker) module to prevent regressions and ensure consistency.

**When to use**: Before making any changes to `router_circuit_breaker.erl`, `router_r10_metrics.erl`, or R10 test suites.

## Pre-Change Checklist

### 1. Understand Current Implementation

- [ ] Read `R10_P0_COMPLETE_FINAL.md` for current architecture
- [ ] Review `router_r10_metrics.erl` for metric constants and helpers
- [ ] Review `router_circuit_breaker.erl` for state machine logic
- [ ] Review test suites: `router_circuit_breaker_SUITE.erl`, `router_publish_failure_e2e_SUITE.erl`

### 2. Check Dependencies

- [ ] Verify no direct ETS access in tests (all tests use `router_r10_metrics`)
- [ ] Verify all metric names use constants from `router_r10_metrics`
- [ ] Verify all trigger reasons use constants from `router_r10_metrics`
- [ ] Check for any hardcoded metric names or trigger reasons

### 3. Review Test Coverage

- [ ] All 7 unit tests in `router_circuit_breaker_SUITE.erl` pass
- [ ] E2E tests in `router_publish_failure_e2e_SUITE.erl` pass
- [ ] Metrics tests in `router_metrics_r10_SUITE.erl` pass
- [ ] Invariant tests in `router_circuit_breaker_invariants_SUITE.erl` pass

## Change Implementation Checklist

### 1. Code Changes

- [ ] **If adding new trigger reason**:
  - [ ] Add constant function to `router_r10_metrics.erl` (e.g., `trigger_reason_new_reason/0`)
  - [ ] Update `R10_P0_COMPLETE_FINAL.md` with new trigger reason
  - [ ] Update tests to use the constant (no hardcoded binaries)

- [ ] **If adding new metric**:
  - [ ] Add metric name constant to `router_r10_metrics.erl`
  - [ ] Add label constants if needed
  - [ ] Update `OBSERVABILITY_CONVENTIONS.md` with new metric
  - [ ] Update tests to use the constant

- [ ] **If modifying state machine**:
  - [ ] Update state transition logic in `router_circuit_breaker.erl`
  - [ ] Ensure state transitions emit correct metrics
  - [ ] Ensure state transitions emit correct trigger reasons
  - [ ] Update tests to verify new behavior

- [ ] **If modifying metrics**:
  - [ ] Never access ETS directly in tests (use `router_r10_metrics`)
  - [ ] Update metric reading functions if needed
  - [ ] Update `dump_metrics/0` if metric structure changes

### 2. Test Updates

- [ ] Update unit tests to reflect new behavior
- [ ] Update E2E tests if needed
- [ ] Update invariant tests if state machine changes
- [ ] Ensure all tests use constants (no hardcoded values)
- [ ] Ensure all tests have proper timeouts (3000ms+ for metrics)

### 3. Documentation Updates

- [ ] Update `R10_P0_COMPLETE_FINAL.md` if architecture changes
- [ ] Update `OBSERVABILITY_CONVENTIONS.md` if metrics change
- [ ] Update `R10_RUNBOOK.md` if operational procedures change
- [ ] Update this checklist if new patterns emerge

## Post-Change Verification

### 1. Compilation

- [ ] All modules compile without warnings
- [ ] All test suites compile without warnings
- [ ] Dialyzer passes (if enabled)

### 2. Test Execution

- [ ] Run unit tests: `rebar3 ct --suite test/router_circuit_breaker_SUITE`
- [ ] Run E2E tests: `rebar3 ct --suite test/router_publish_failure_e2e_SUITE`
- [ ] Run metrics tests: `rebar3 ct --suite test/router_metrics_r10_SUITE`
- [ ] Run invariant tests: `rebar3 ct --suite test/router_circuit_breaker_invariants_SUITE`
- [ ] Run combined suites to check for conflicts

### 3. Metrics Verification

- [ ] Verify metrics are exported via Prometheus (port 9001)
- [ ] Verify metrics have correct labels
- [ ] Verify metrics are readable and filterable
- [ ] Check `dump_metrics/0` output for correctness

### 4. Code Review

- [ ] No direct ETS access in tests
- [ ] All metric names use constants
- [ ] All trigger reasons use constants
- [ ] All tests have proper error handling
- [ ] All tests have proper timeouts
- [ ] Documentation is up to date

## Common Pitfalls to Avoid

### ❌ Direct ETS Access

**Wrong**:
```erlang
case ets:lookup(router_metrics, router_circuit_breaker_state) of
    [] -> ct:fail(metric_not_found);
    [{_, Value}] -> Value
end.
```

**Correct**:
```erlang
Value = router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => TenantId,
    provider_id => ProviderId,
    state => ~"open"
}).
```

### ❌ Hardcoded Trigger Reasons

**Wrong**:
```erlang
?assertEqual(~"failure_threshold_exceeded", TriggerReason).
```

**Correct**:
```erlang
?assertEqual(router_r10_metrics:trigger_reason_failure_threshold(), TriggerReason).
```

### ❌ Insufficient Timeouts

**Wrong**:
```erlang
timer:sleep(100),  %% Too short for metrics to be written
```

**Correct**:
```erlang
ok = wait_for_metric(Fun, Expected, 3000).  %% 3 second timeout
```

### ❌ Missing Process Checks

**Wrong**:
```erlang
router_circuit_breaker:record_failure(TenantId, ProviderId).  %% May fail with noproc
```

**Correct**:
```erlang
ok = ensure_circuit_breaker_alive(),
case catch router_circuit_breaker:record_failure(TenantId, ProviderId) of
    ok -> ok;
    {noproc, _} ->
        ok = ensure_circuit_breaker_alive(),
        router_circuit_breaker:record_failure(TenantId, ProviderId);
    Error -> ct:fail({record_failure_failed, Error})
end.
```

## Emergency Procedures

### If Tests Start Failing

1. **Check process state**:
   - Run `router_test_utils:dump_supervisor_children/0` to check supervisor
   - Run `router_r10_metrics:dump_metrics/0` to check metrics
   - Verify circuit breaker process is alive: `whereis(router_circuit_breaker)`

2. **Check metrics**:
   - Verify metrics table exists: `router_r10_metrics:metrics_table_exists/0`
   - Check for metric name mismatches
   - Check for label mismatches

3. **Check timing**:
   - Increase timeouts if metrics are slow to appear
   - Add delays between state changes and metric checks
   - Use `wait_for_metric/3` instead of immediate checks

### If Metrics Are Missing

1. **Verify metric emission**:
   - Check `router_circuit_breaker.erl` for metric emission calls
   - Verify metric names match constants in `router_r10_metrics.erl`
   - Verify labels match expected format

2. **Check ETS table**:
   - Verify `router_metrics` table exists
   - Check for table cleanup between tests
   - Verify metrics are not pruned too aggressively

## References

- `R10_P0_COMPLETE_FINAL.md` - Complete R10 architecture documentation
- `R10_RUNBOOK.md` - Operational runbook for diagnosing issues
- `OBSERVABILITY_CONVENTIONS.md` - Metrics conventions and standards
- `router_r10_metrics.erl` - Metrics access layer implementation
- `router_circuit_breaker.erl` - Circuit breaker implementation
