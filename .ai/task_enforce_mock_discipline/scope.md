## Scope

- **Canonical helper (approved)**: `router_nats_test_helper` — the central NATS mock helper that wraps `router_nats` calls and already guards passthrough-related pitfalls.
- **Secondary helper (allowed exception list)**: `router_mock_helpers` (provides safe stubs/utility wrappers that call into `router_nats`).
- **Lint goals**:
  1. Detect any `meck:new(..., [passthrough])` uses anywhere under `test/`.
  2. Fail on any `gen_server:call(router_nats, ...)` outside the approved helper modules above.
- **Verification**: the lint must fail when we intentionally create such a violation; this run must be documented in progress.md.
