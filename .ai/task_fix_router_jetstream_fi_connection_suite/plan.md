# Plan

## Phase 0: Baseline & Evidence Capture
1. Run `rebar3 compile`.
2. Run `rebar3 ct --suite router_jetstream_fi_connection_SUITE --readable true` to capture baseline failure.
3. Record failure details in `progress.md`.

## Phase 1: Root Cause Triage
1. Analyze dependencies (NATS, ETS, gen_server).
2. Read suite code and helpers.
3. Formulate hypothesis.

## Phase 2: Minimal Fix
1. Apply fix (test-side preferred).
2. Verify with `rebar3 ct ... --retry`.
3. Update `progress.md`.

## Phase 3: Regression Safety
1. Run a neighboring suite (e.g., another JetStream suite).
2. Confirm no regressions.
