# R10 P1: E2E Stabilization Plan

## ✅ Completed: router_r10_metrics Extended

**New Functions Added**:
- ✅ `wait_for_trigger_reason/4` - High-level helper for waiting for trigger reason
- ✅ `get_publish_attempts_total/0` - Get total publish attempts
- ✅ `get_publish_errors_total/0` - Get total publish errors
- ✅ `get_publish_attempts_delta/1` - Get attempts delta before/after action
- ✅ `get_publish_errors_delta/1` - Get errors delta before/after action

## 📝 Next Steps: E2E Suite Migration

### P1.1: Replace Metric Functions in E2E

**Current State**:
- E2E uses `get_publish_attempts/0`, `get_publish_errors/0`, `get_publish_attempts_delta/1`, `get_publish_errors_delta/1` from `router_test_utils`

**Target State**:
- E2E uses `router_r10_metrics:get_publish_attempts_total/0`, `router_r10_metrics:get_publish_errors_total/0`, etc.

**Files to Update**:
- `test/router_publish_failure_e2e_SUITE.erl`

**Changes Needed**:
1. Remove imports: `get_publish_attempts/0`, `get_publish_errors/0`, `get_publish_attempts_delta/1`, `get_publish_errors_delta/1`
2. Replace all calls with `router_r10_metrics:*` equivalents
3. Update `-import` to include `router_r10_metrics` if needed

### P1.2: Unique Tenant/Provider Per Scenario

**Current State**:
- `scenario_mass_failure_opens_breaker`: `tenant1`/`provider1`
- `scenario_recovery_after_failure`: `tenant1`/`provider1` (same!)
- `scenario_latency_based_trigger`: `tenant2`/`provider2`
- `scenario_error_rate_partial_failure`: `tenant3`/`provider3`

**Target State**:
- `scenario_mass_failure_opens_breaker`: `tenant_r10_s1`/`provider_r10_s1`
- `scenario_recovery_after_failure`: `tenant_r10_s2`/`provider_r10_s2`
- `scenario_latency_based_trigger`: `tenant_r10_s3`/`provider_r10_s3`
- `scenario_error_rate_partial_failure`: `tenant_r10_s4`/`provider_r10_s4`
- `scenario_thundering_herd_recovery`: `tenant_r10_s5`/`provider_r10_s5`
- `scenario_deadline_vs_sla`: `tenant_r10_s6`/`provider_r10_s6`

### P1.3: Normalize trigger_reason Checks

**Current State**: Likely hardcoded strings or missing checks

**Target State**: Use `router_r10_metrics:assert_trigger_reason_in/3` or `wait_for_trigger_reason/4`

**Scenarios to Update**:
1. `scenario_mass_failure_opens_breaker`: Check for `failure_threshold_exceeded` OR `error_rate_threshold_exceeded`
2. `scenario_latency_based_trigger`: Check for `latency_threshold_exceeded`
3. `scenario_error_rate_partial_failure`: Check for `error_rate_threshold_exceeded`

### P1.4: Timeouts and Stability

**Current State**: Mixed `timer:sleep` and `wait_for_breaker_state`

**Target State**:
- Use `wait_for_breaker_state/4` with 3-5 second timeouts
- Use `wait_for_trigger_reason/4` with 3-5 second timeouts
- Replace magic sleeps with proper waits
- Set timetrap to 30-60 seconds per scenario

## 🎯 Implementation Order

1. **P1.1**: Replace metric functions (quick win)
2. **P1.2**: Unique tenant/provider (prevents cross-contamination)
3. **P1.3**: Normalize trigger_reason checks (consistency)
4. **P1.4**: Timeouts and stability (reliability)

## 📊 Expected Outcome

After P1:
- ✅ E2E uses `router_r10_metrics` exclusively
- ✅ Each scenario is fully independent (unique tenant/provider)
- ✅ All trigger_reason checks use centralized helpers
- ✅ All timeouts are consistent and reliable
- ✅ No direct ETS access in E2E tests

