# Acceptance Criteria

1.  **Suites Migrated:** At least 3 additional suites fully migrated to the canonical CT format (static `all/0`, no `groups_for_level`).
2.  **Pattern Reduction:** The count of legacy patterns (`groups_for_level`, `test_level`, `ROUTER_TEST_LEVEL`, `router_ct_groups:all_selection`) must decrease by at least 50 from the starting baseline.
3.  **Compilation & Execution:** All migrated suites must compile without errors/warnings related to the migration and pass `rebar3 ct`.
4.  **Deterministic Baseline:** `rebar3 as deterministic ct` must remain GREEN (no regressions in the deterministic set).
5.  **Technical Debt:** Any tests skipped during migration must be explicitly listed in `progress.md` with a justification (e.g., "Flaky RBAC", "Environment issue").
6.  **Quarantine Integrity:** `scripts/lint/check_ct_quarantine_consistency.sh` must PASS (no changes to quarantine grouping unless explicitly handled).
7.  **Heavy Verification:** Spot checks in heavy tier show no regressions for migrated suites.
