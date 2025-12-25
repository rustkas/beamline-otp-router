# Acceptance Criteria

1. **Compilation**: `rebar3 compile` succeeds.
2. **Reliability**: `rebar3 ct --suite router_jetstream_fi_connection_SUITE --readable true --retry` passes reliably (3/3 runs).
3. **Determinism**: The fix relies on deterministic mechanisms (waits, mocks, sequence), not sleeps or luck.
4. **Evidence**: Full logs and command outputs recorded in `progress.md`.
5. **No Regressions**: Neighboring JetStream suites still pass.
