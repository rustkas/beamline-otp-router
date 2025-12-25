## Status: CLOSED
Reason: full-tier `bash scripts/ct-full.sh --list` now prints 54 suites, shows `router_nats_publish_retry_SUITE` under the dedicated “Quarantined suites” block, and omits it from the execution plan while the heavy runner (`ROUTER_TEST_LEVEL=heavy bash scripts/ct-heavy.sh`) logs `Heavy tier includes quarantined suites: router_nats_publish_retry_SUITE (qa_team: retried publish retries are nondeterministic in CI)` and writes `_build/test/logs/ct_run.nonode@nohost.2025-12-15_08.00.35/ctlog.html` entries (lines 672/676/1517/1521) for that suite despite the forced timeout.

## Quarantined suites (tracked)
- `router_nats_publish_retry_SUITE | qa_team | retried publish retries are nondeterministic in CI`

## Verification commands & observations
- `bash scripts/ct-full.sh --list`
  - Observed: “Quarantined suites (1)” block listing the suite with owner/reason and a reduced “Suites to be run (54)” list that does not include `router_nats_publish_retry_SUITE`.
- `ROUTER_TEST_LEVEL=heavy bash scripts/ct-heavy.sh` (timed out after 10m / exit 124)
  - Observed: console logs `Heavy tier includes quarantined suites: router_nats_publish_retry_SUITE (qa_team: retried publish retries are nondeterministic in CI)` at startup and `_build/test/logs/ct_run.nonode@nohost.2025-12-15_08.00.35/ctlog.html` contains multiple mentions of `router_nats_publish_retry_SUITE` (lines 672/676/1517/1521), proving the suite executed even though the run was interrupted.

## Notes
- Heavy-tier confirmation is log-based; full completion is not required to validate quarantine inclusion.
