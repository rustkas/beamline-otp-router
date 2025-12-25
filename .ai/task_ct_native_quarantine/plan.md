Phase 1 (infrastructure):
- Add helper module: test/test_utils/router_ct_groups.erl (canonical)
- Define API used by suites:
  - test_level/0 (reads ROUTER_TEST_LEVEL)
  - groups_for_level/1 (returns [{group, Name}] list)
  - groups_definitions/1 (returns CT groups() data structure)
  - quarantine_cases/0 or quarantine_group/0 integration point
- Define how quarantine metadata file is read (and cached) once per run.

Phase 2 (migration batch 1):
- Migrate 5–10 suites to use the canonical helper, including:
  - at least 1 quarantined suite (router_nats_publish_retry_SUITE or whichever is in the quarantine list)
  - at least 1 suite with existing level gating groups_for_level/test_level patterns

Phase 3 (migration batch 2+):
- Expand migration to all suites gradually.

Stop-the-line policy:
- If any migration regresses ct-full.sh baseline or breaks suite listing/gates, rollback or fix immediately.
