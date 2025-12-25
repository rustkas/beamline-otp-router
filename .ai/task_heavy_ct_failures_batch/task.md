# Task: Heavy CT Failures Batch

## Objective
Make `ROUTER_TEST_LEVEL=heavy ./scripts/ct-heavy.sh` complete without FAIL/ERROR.

## Known Failing Suites (from last heavy run)
1. **`router_deployment_SUITE`**
   - FAIL: `memory_check_failed` (resource validation)
   - Cascaded auto-skips in same suite

2. **`router_headers_propagation_e2e_SUITE`**
   - FAIL: `router_health_integration_SUITE cannot be compiled or loaded` (suite load error)

3. **`router_jetstream_recovery_ext_SUITE`**
   - FAIL: `undef` calling `router_jetstream_recovery_helpers:simulate_network_partition/0`

4. **Mock Hygiene Issue**
   - Repeated warnings: `[WARN] router_mock_helpers: found stray mocks [router_nats] (cleaned up)`
   - Indicates test cleanup leaks

5. **`router_chaos_engineering_SUITE`**
   - SKIPPED (intentional when `RUN_CHAOS_TESTS` not set)
   - Status: EXPECTED_SKIP (acceptable)

## Context
- Heavy run was killed (likely OOM/timeout)
- Goal: Fix earliest hard failures first to enable full run completion
