Deterministic Baseline

- Purpose: define the canonical set of Common Test suites that must remain green locally and serve as the fast quality signal.

Suites

- `router_result_consumer_core_SUITE`
- `router_result_consumer_faults_SUITE`
- `router_decide_consumer_core_SUITE`
- `router_decide_consumer_faults_SUITE`
- `router_decide_consumer_unit_SUITE`

Criteria

- No reliance on `handle_info/2` timing or external subscribers in assertions.
- Use validated-path invocations of exported functions.
- Mocks at public API boundaries; no passthrough for gen_server-backed modules.
- Intent verified via side-effects (publish attempts, logs) with deterministic waits.

Run Commands

- Canonical: `rebar3 as deterministic ct`
