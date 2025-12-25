# Plan

## Phase 1: Preparation & Analysis
1.  **Baseline Count:** Run `grep` to establish the exact number of legacy patterns remaining.
2.  **Identify Targets:** Find the next batch of suites (5-10) that are easiest to migrate (e.g., those where `groups_for_level` returns the same group for all levels).

## Phase 2: Execution (Iterative)
For each identified suite:
1.  **Refactor:**
    - Replace `all() -> ...` with static `all() -> [{group, ...}].`.
    - Remove `groups_for_level/1` function and its export.
    - Remove `meta_all/0` if present.
    - Ensure `groups()` is correctly defined.
    - If `quarantine` group exists, ensure it is clearly defined and `all()` includes/excludes it correctly based on policy (though for now, we just want static `all` containing runnable groups). *Correction:* `all()` should just return the main test groups. Quarantine logic is now handled via the config file and the `quarantine` group implementation, not dynamic `all`.
2.  **Verify:**
    - Run `rebar3 ct --suite <suite> --readable true`.
    - If failures occur due to strictness/environment, mark as `{skip, Reason}` and log in `progress.md`.
3.  **Commit:** Update `progress.md` with the suite status.

## Phase 3: Final Verification
1.  **Pattern Count:** Verify the reduction target (-50).
2.  **Lint Check:** Run `scripts/lint/check_ct_quarantine_consistency.sh`.
3.  **Deterministic Check:** Run `rebar3 as deterministic ct`.
4.  **Heavy Check:** Run `ROUTER_TEST_LEVEL=heavy ./scripts/ct-heavy.sh --list` (or spot check migrated suites).

## Phase 4: Closure
1.  Update `progress.md` with final metrics.
2.  Mark task as COMPLETED.
