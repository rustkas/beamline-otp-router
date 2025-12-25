# Acceptance Criteria

## DONE Definition
This batch is DONE only if ALL of the following are satisfied:

1. **Compilation**: `rebar3 compile` passes cleanly

2. **Individual Suite Passes** (heavy tier, with retry):
   - `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_deployment_SUITE --readable true --retry` → PASS
   - `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_headers_propagation_e2e_SUITE --readable true --retry` → PASS
   - `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_jetstream_recovery_ext_SUITE --readable true --retry` → PASS

3. **Full Heavy Run Completion**:
   - `ROUTER_TEST_LEVEL=heavy ./scripts/ct-heavy.sh --list` completes without FAIL/ERROR
   - SKIP allowed only for documented EXPECTED_SKIP cases (e.g., `router_chaos_engineering_SUITE`)

4. **Mock Hygiene**:
   - `router_mock_helpers` stray mock warnings eliminated or significantly reduced
   - Root cause identified and fix recorded in `progress.md`

5. **Evidence**:
   - All commands, results, and log paths recorded in `progress.md` ledger
   - Any quarantined tests explicitly documented with justification
