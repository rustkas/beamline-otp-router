# Plan

## Phase A: Baseline + Repro
1. Run `rebar3 compile`.
2. Run `rebar3 ct --suite router_delivery_count_advanced_SUITE --readable true` to capture baseline.
3. Run stability loop (10x).

## Phase B: Root Cause from Logs
1. Extract failure details.
2. Trace dependency chain.

## Phase C: Minimal Fix
1. Apply fix (test-side preferred).
2. Verify with stability loop.

## Phase D: Regression + Acceptance
1. Confirm no new warnings.
2. Final verification.
