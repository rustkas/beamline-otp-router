# Progress: T-SOAK-01

**Status**: READY FOR TESTING  
**Last Updated**: 2025-12-21

## Work Log

- [x] Task definition created
- [x] Scope defined
- [x] Acceptance criteria established
- [x] Created `test/router_soak_helper.erl` - Soak testing utilities
- [x] Created `test/router_soak_stability_SUITE.erl` - Soak test suite
- [x] Updated `test/router_test_helpers.erl` - Added test request helpers
- [ ] Run short soak test (10 minutes) for validation
- [ ] Run 6-hour soak test
- [ ] Run 24-hour soak test
- [ ] Generate soak test report

## Implementation Summary

### 1. Soak Helper Module (`test/router_soak_helper.erl`)

Memory monitoring utilities:
- `start_memory_monitor/1` - Start periodic mem sampling
- `stop_memory_monitor/1` - Stop monitoring
- `get_memory_snapshot/0` - Get current memory state
- `analyze_memory_trend/1` - Detect memory leaks

ETS monitoring:
- `get_ets_snapshot/0` - Capture all ETS table sizes  
- `analyze_ets_growth/2` - Detect unbounded growth

JetStream monitoring:
- `get_jetstream_lag/1` - Get consumer lag

Process monitoring:
- `get_process_snapshot/0` - Process count, supervisors

Load generation:
- `generate_sustained_load/3` - Sustained request generation

Stability checking:
- `check_stability_criteria/2` - Pass/fail validation

### 2. Soak Test Suite (`test/router_soak_stability_SUITE.erl`)

Test cases:
- `test_memory_stability_short` - 10-minute stability test (for quick validation)
- `test_memory_stability_6h` - 6-hour memory stability  
- `test_ets_growth_monitoring` - 5-minute ETS bounds check
- `test_jetstream_consumer_stability` - 1-hour consumer lag check
- `test_process_stability` - 5-minute process count stability

Pass/Fail Criteria:
- ✅ Memory growth < 10% over duration
- ✅ ETS tables bounded (no >1000% growth)
- ✅ JetStream lag < 100 messages
- ✅ Process count stable (± 10%)

### 3. Test Helper Updates

Added to `router_test_helpers.erl`:
- `make_test_request/2,3` - Generate routing requests
- `make_test_message/2` - Create test message payload

## Next Steps

1. **Run Short Validation** (10 minutes):
   ```bash
   rebar3 ct --suite=router_soak_stability_SUITE --case=test_memory_stability_short
   ```

2. **Run Full Soak Suite**:
   ```bash
   # 6-hour suite (short soak + ETS + process)
   rebar3 ct --suite=router_soak_stability_SUITE
   
   # 24-hour suite (requires manual execution)
   rebar3 ct --suite=router_soak_stability_SUITE --case=test_memory_stability_24h
   ```

3. **Monitor During Execution**:
   - Check memory usage: `scripts/memory_profile.sh` (to be created)
   - Check Prometheus metrics
   - Watch for ERROR logs

4. **Analyze Results**:
   - Review CT logs in `_build/test/logs`
   - Generate report: `_artifacts/soak_24h_report.md`

## Blockers

None. Ready for testing.

## Notes

- **Short test (10 min)** is enabled by default for quick validation
- **Long tests (6h, 24h)** are commented out; uncomment when ready for full soak
- Requires real NATS (not mock mode)
- Ensure sufficient resources (2GB+ RAM, 10GB+ disk)
