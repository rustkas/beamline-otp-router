# Acceptance Criteria

1. **Compilation**: `rebar3 compile` succeeds.
2. **Reliability**: `rebar3 ct --suite router_delivery_count_advanced_SUITE --readable true --retry` passes reliably (10/10 runs or 3/3 depending on phase).
3. **Determinism**: The fix relies on deterministic mechanisms.
4. **Evidence**: Full logs and command outputs recorded in `progress.md`.
5. **No Regressions**: No new warnings introduced.
