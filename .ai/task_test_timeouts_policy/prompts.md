## Prompts

### Adjusting timeouts for new suites
“When a new suite introduces a hard-coded `receive ... after`, `gen_server:call(..., Timeout)`, or CT timetrap that depends on the test tier, add the value to `test_support/router_test_timeouts` and call the helper instead of duplicating a literal.”

### Verifying baseline compliance
“After modifying timeout benchmarks, rerun `./scripts/ct-full.sh` and compare `reports/full_baseline.json` to confirm the new duration stays within ±10% before marking the task complete.”

### Adding tiers or keys
“Need a new tier-specific wait (e.g., `medium_wait`)? Extend `router_test_timeouts` with a new helper plus per-tier values, then update all call sites and document the new key under `.ai/task_test_timeouts_policy/progress.md`.”
