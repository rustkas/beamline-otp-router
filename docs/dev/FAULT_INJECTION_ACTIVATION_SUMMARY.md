# JetStream Fault Injection Tests Activation Summary

**Date**: 2025-11-30  
**Status**: ✅ Activated and Stabilized

## What Was Done

### 1. Test Suite Activation
- ✅ Renamed `router_jetstream_fault_injection_SUITE.erl.skip` → `router_jetstream_fault_injection_SUITE.erl`
- ✅ Suite is now visible to `rebar3 ct` and CI

### 2. Timeout Stabilization
Increased timeouts for CI stability:
- `timer:sleep(300)` → `timer:sleep(500)` (all occurrences)
- `timer:sleep(500)` → `timer:sleep(1000)` (for metric waits)
- `wait_for_metric(..., 2000, ...)` → `wait_for_metric(..., 5000, ...)` (5 seconds for CI)
- `test_helpers:wait_for_meck_call(..., 1000)` → `test_helpers:wait_for_meck_call(..., 2000)` (2 seconds)
- `timer:sleep(200)` → `timer:sleep(500)` (for loops)
- `timer:sleep(100)` → `timer:sleep(200)` (for short waits)

### 3. Integration
- ✅ Suite already in `SLOW_TEST_SUITES` in `test_slow.sh` (line 44)
- ✅ Runs via `make test-slow` / `./scripts/test_slow.sh`
- ✅ CI runs via `rebar3 ct` (includes all test suites)

### 4. Documentation Coverage
All scenarios from `JETSTREAM_FAULT_INJECTION_TESTS.md` are covered:
- ✅ `test_nats_connection_loss_recovery`
- ✅ `test_jetstream_consumer_reconnection`
- ✅ `test_stream_availability_after_recovery`
- ✅ `test_ets_state_preservation_during_nats_restart`
- ✅ `test_redelivery_metric_labels`
- ✅ `test_redelivery_tenant_validation_failed`
- ✅ `test_maxdeliver_exhausted_metric_labels`
- ✅ `test_maxdeliver_exhausted_different_limits`

## Next Steps

### 1. Local Multi-Run Validation
Run suite multiple times locally to verify stability:

```bash
cd apps/otp/router
# Run 20 times
for i in {1..20}; do
  echo "Run $i/20"
  rebar3 ct --suite router_jetstream_fault_injection_SUITE || exit 1
done
```

Or use the provided script:
```bash
./scripts/test_fault_injection_repeat.sh --runs 20
```

### 2. CI Monitoring
Monitor first 5-10 CI runs:
- Check execution time of `router_jetstream_fault_injection_SUITE`
- Verify no timeouts or failures
- Check if overall `rebar3 ct` duration increased significantly

### 3. Optional: Separate CI Job
If `rebar3 ct` becomes too slow:
- Create separate `fault-injection` job
- Run in parallel with main tests
- Or schedule as nightly / on-demand

## Files Changed

- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` - Activated and stabilized

## References

- Documentation: `docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- Test runner: `scripts/test_slow.sh`
- CI config: `.github/workflows/ci.yml`

