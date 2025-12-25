## Scope

- **Tier entrypoints**:
  - `scripts/ct-sanity.sh` sets `ROUTER_TEST_LEVEL=sanity` and runs the critical-path suites.
  - `scripts/ct-fast.sh` sets `ROUTER_TEST_LEVEL=fast` and executes all fast-tier tests for quick CI.
  - `scripts/ct-full.sh` (the main “full tier” run) sets `ROUTER_TEST_LEVEL=full`, compiles the project, runs `rebar3 ct`, and reports to `reports/full_baseline.json`.
  - `scripts/ct-heavy.sh` sets `ROUTER_TEST_LEVEL=heavy` for the nightly/stress suites.

- **Baseline reference**: `reports/full_baseline.json` records the latest full-tier duration (112 seconds) and is the ±10% target for the verification run.
- **Timeout patterns to normalize**: literal delays in `receive ... after Timeout`, `gen_server:call(..., Timeout)`, and CT timeout helpers (`ct:timetrap`, init/testcase timetrap configs) that control waits or guard results in test suites/helpers. Timer sleeps that represent deliberate pacing may stay untouched unless they double as guards.
- **Helper module**: `test_support/router_test_timeouts.erl` (shared by suites/helpers) will provide tier lookup plus exported helpers such as `very_short_wait/0`, `short_wait/0`, `default_wait/0`, `long_wait/0`, plus `timeout/1`, `call_timeout/0`, and `receive_timeout/0`.
- **Tier selection**: `ROUTER_TEST_LEVEL` environment variable (set by `./scripts/ct-sanity.sh`, `./scripts/ct-fast.sh`, `./scripts/ct-full.sh`, `./scripts/ct-heavy.sh`) chooses `sanity`, `fast`, `full`, or `heavy`. When unset, the helper defaults to `full`.
