## Acceptance Criteria

1. `test_support/router_test_timeouts` exists, reads `ROUTER_TEST_LEVEL`, and exports tier-specific waits (sanity/fast/full/heavy) plus helpers such as `short_wait/0`, `default_wait/0`, and `long_wait/0`.
2. At least ten literal timeout values in Common Test suites/helpers (`receive ... after`, `gen_server:call(..., Timeout)`, CT timetrap configs) now call the helper instead of hard-coded numbers.
3. Documented tier/timeout coverage within `.ai/task_test_timeouts_policy/progress.md` (updated locations, chosen value per tier, measured runtime vs baseline).
4. `./scripts/ct-full.sh` runtime stays within ±10% of `reports/full_baseline.json` (112 seconds) after the changes.
