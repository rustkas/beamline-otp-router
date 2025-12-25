# Acceptance Criteria

1. **Compilation**: `rebar3 compile` succeeds with no warnings from `router_jetstream_e2e_cp2_SUITE.erl`
2. **Test Pass**: `ROUTER_TEST_LEVEL=heavy rebar3 ct --suite router_jetstream_e2e_cp2_SUITE --readable true` passes (all tests)
3. **Stability**: ≥5 consecutive clean runs
4. **Mock Discipline**: All `meck:expect` calls replaced with `router_mock_helpers` equivalents
5. **Evidence**: Commands + log paths recorded in `progress.md`
