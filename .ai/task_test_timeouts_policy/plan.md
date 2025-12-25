## Plan

1. Inventory timeout literals by scanning `test/` (focusing on `receive ... after`, `gen_server:call` timeouts, and CT timetrap configs) and record the candidate files/numbers to replace.
2. Implement `test_support/router_test_timeouts` that reads `ROUTER_TEST_LEVEL` and exposes tier-specific helpers (`short_wait/0`, `default_wait/0`, `long_wait/0`, etc.).
3. Replace at least ten timeout literals across tests/helpers with calls into `router_test_timeouts`.
4. Run `./scripts/ct-full.sh` and compare its runtime to `reports/full_baseline.json` to ensure the duration stays within ±10%.
5. Update `.ai/task_test_timeouts_policy/progress.md` (updated locations, per-tier values, baseline vs measured runtime) and `README` or other docs if needed.
