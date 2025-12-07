# R10 P2: Cleanup router_test_utils ✅

## ✅ Completed Tasks

### P2.1: Removed Metric Functions from router_test_utils ✅

**Removed Functions**:
- ✅ `get_publish_attempts/0` → Use `router_r10_metrics:get_publish_attempts_total/0`
- ✅ `get_publish_errors/0` → Use `router_r10_metrics:get_publish_errors_total/0`
- ✅ `get_publish_attempts_by_retry/0` → Not used, removed
- ✅ `get_publish_attempts_delta/1` → Use `router_r10_metrics:get_publish_attempts_delta/1`
- ✅ `get_publish_errors_delta/1` → Use `router_r10_metrics:get_publish_errors_delta/1`
- ✅ `assert_max_attempts_not_exceeded/1` → Not used, removed
- ✅ `assert_retry_model_behavior/4` → Not used, removed

**Kept Functions** (Lifecycle/Waiters):
- ✅ `start_router_app/0`, `stop_router_app/0`
- ✅ `ensure_circuit_breaker_alive/0`, `ensure_router_nats_alive/0`
- ✅ `reset_circuit_breaker/0`
- ✅ `wait_for_breaker_state/4`, `get_breaker_state/2`
- ✅ `wait_for_metric/3`, `wait_for_metric_loop/4` (generic waiters)
- ✅ `dump_metrics/0` (delegates to router_r10_metrics)
- ✅ `dump_supervisor_children/0`

**Result**: `router_test_utils` now focuses on lifecycle and waiting, not metric reading.

### P2.2: Verification ✅

**Status**:
- ✅ No test suites use `router_test_utils:get_publish_*` functions
- ✅ E2E suite already uses `router_r10_metrics:*` functions
- ✅ Compilation successful
- ✅ Test execution verified

## 📊 Cleanup Summary

**Before P2**:
- `router_test_utils` had 7 metric-related functions
- Mixed responsibilities: lifecycle + metrics

**After P2**:
- `router_test_utils` has 0 metric-reading functions
- Clear separation: `router_test_utils` = lifecycle/waiters, `router_r10_metrics` = metrics

## 🎯 Next Steps

### P3: CI and Documentation

1. **P3.1**: Verify CI profiles (`ci`/`heavy`) in `ct.config`
2. **P3.2**: Update documentation:
   - `R10_P0_COMPLETE_FINAL.md` - Add "R10 Metrics Access Layer" section
   - `QA_TEST_PLAN.md` - Document trigger_reason checks and unique tenant/provider
   - `OBSERVABILITY_CONVENTIONS.md` - Add R10 section (if exists)

## ✅ Summary

**P2 Tasks**: ✅ **COMPLETE**
- ✅ Removed all metric functions from `router_test_utils`
- ✅ Clear separation of concerns achieved
- ✅ All tests verified to use `router_r10_metrics`

**Status**: Ready for P3 (CI and Documentation)

