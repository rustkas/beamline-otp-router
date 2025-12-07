# R10 Validation and Cleanup Plan

## P0': Validation Current State

### 1. Check for Direct ETS Access in R10 Tests

**Status**: ⏳ In Progress

**Findings**:
- `router_circuit_breaker_SUITE.erl` has `get_metric_value/2` function that uses direct ETS
- `router_publish_failure_e2e_SUITE.erl` has `ets:info(router_metrics)` checks
- Other suites (not R10-specific) also have ETS access, but that's out of scope

**Action Required**:
- Replace `get_metric_value/2` in `router_circuit_breaker_SUITE.erl` with `router_r10_metrics:get_metric_value/2`
- Replace `ets:info(router_metrics)` checks in E2E with `router_r10_metrics` helpers

### 2. Run Combined Test Suites

**Status**: ⏳ Pending

**Command**:
```bash
rebar3 ct --suite test/router_circuit_breaker_SUITE --suite test/router_publish_failure_e2e_SUITE
```

**Expected**: Both suites pass when run together (no lifecycle/ETS conflicts)

## P2: Cleanup router_test_utils

### Current State

**Functions to Remove/Migrate**:
- `get_publish_attempts/0` → Use `router_r10_metrics:get_publish_attempts_total/0`
- `get_publish_errors/0` → Use `router_r10_metrics:get_publish_errors_total/0`
- `get_publish_attempts_by_retry/0` → May need to enhance `router_r10_metrics` or remove
- `get_publish_attempts_delta/1` → Use `router_r10_metrics:get_publish_attempts_delta/1`
- `get_publish_errors_delta/1` → Use `router_r10_metrics:get_publish_errors_delta/1`
- `get_metric_value/2` → Use `router_r10_metrics:get_metric_value/2`

**Functions to Keep**:
- `start_router_app/0`, `stop_router_app/0`
- `ensure_circuit_breaker_alive/0`, `ensure_router_nats_alive/0`
- `reset_circuit_breaker/0`
- `wait_for_breaker_state/4`
- `wait_for_clients/2` (if exists)
- `wait_for_metric/3` (generic waiter, but should call `router_r10_metrics` internally)
- `dump_metrics/0` (should be moved to `router_r10_metrics` or call it)

### Action Plan

1. **Move `dump_metrics/0` to `router_r10_metrics`**:
   - Add `dump_metrics/0` to `router_r10_metrics.erl`
   - Update `wait_for_metric/3` in `router_test_utils` to call `router_r10_metrics:dump_metrics/0`

2. **Remove metric functions from `router_test_utils`**:
   - Remove `get_publish_attempts/0`, `get_publish_errors/0`, `*_delta` functions
   - Remove `get_metric_value/2` (if it's a duplicate)
   - Update all imports in test suites

3. **Update `wait_for_metric/3`**:
   - Make it call `router_r10_metrics` internally for metric fetching
   - Keep it in `router_test_utils` as a generic waiter (not metric-specific)

## P3: CI and Documentation

### CI Profiles

**Current**: Need to verify `ct.config` has `ci` and `heavy` profiles

**Action**:
- Check `ct.config` or `rebar.config` for profile definitions
- Document which profile to use in CI vs nightly

### Documentation Updates

**Files to Update**:
1. `R10_P0_COMPLETE_FINAL.md` - Add "R10 Metrics Access Layer" section
2. `QA_TEST_PLAN.md` - Document trigger_reason checks and unique tenant/provider
3. `OBSERVABILITY_CONVENTIONS.md` - Add R10 metrics section (if exists)

## Next Steps

1. ✅ **P0'.1**: Fix direct ETS access in R10 tests
2. ⏳ **P0'.2**: Run combined test suites
3. ⏳ **P2.1**: Move `dump_metrics` to `router_r10_metrics`
4. ⏳ **P2.2**: Remove metric functions from `router_test_utils`
5. ⏳ **P2.3**: Update `wait_for_metric` to use `router_r10_metrics`
6. ⏳ **P3**: CI profiles and documentation

