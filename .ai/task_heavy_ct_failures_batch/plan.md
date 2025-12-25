# Plan

## Execution Order (MANDATORY SEQUENCE)

### Step 1: Baseline Establishment
Run each failing suite individually to capture current state:
1. `router_deployment_SUITE`
2. `router_headers_propagation_e2e_SUITE`
3. `router_jetstream_recovery_ext_SUITE`

Record: exact errors, log paths, failure modes.

### Step 2: Fix `router_jetstream_recovery_ext_SUITE` (undef error)
**Priority**: Earliest to fix (missing helper module)
- Locate `router_jetstream_recovery_helpers` usage
- Check if module exists in `test/` or `test/support/`
- If missing: create minimal helper with `simulate_network_partition/0`
- If exists: verify export list
- Re-run suite (max 3 attempts)

### Step 3: Fix `router_headers_propagation_e2e_SUITE` (suite load error)
**Issue**: Cannot compile/load `router_health_integration_SUITE`
- Check compilation of `router_health_integration_SUITE`
- Identify missing deps, exports, or path issues
- Fix minimally
- Re-run suite (max 3 attempts)

### Step 4: Fix `router_deployment_SUITE` (resource validation)
**Issue**: `memory_check_failed`
- Analyze resource check logic
- Options per `.ai/decisions.md`:
  - Mock resource checks in test mode, OR
  - Make check tolerant/skippable in test env, OR
  - Gate by env var + mark EXPECTED_SKIP if infra unavailable
- Choose approach that doesn't weaken production semantics
- Re-run suite (max 3 attempts)

### Step 5: Eliminate Stray Mock Warnings
**Issue**: `[WARN] router_mock_helpers: found stray mocks [router_nats]`
- Identify suites leaking `router_nats` mocks
- Add proper cleanup in `end_per_testcase/2` or `end_per_suite/1`
- Verify warnings reduced/eliminated

### Step 6: Full Heavy Run Verification
- Run `ROUTER_TEST_LEVEL=heavy ./scripts/ct-heavy.sh --list`
- Verify completion without FAIL/ERROR
- Document any remaining EXPECTED_SKIP cases
