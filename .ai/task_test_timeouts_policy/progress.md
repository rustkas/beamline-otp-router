## Progress (state capture)

### Updated locations
- `src/router_test_timeouts.erl` – new tier-aware helper (very_short/short/default/long + call/receive conveniences).
- `test/router_metrics_http_SUITE.erl:69` – replaced the 200 ms `after` guard with `router_test_timeouts:very_short_wait()`.
- `test/router_rate_limit_store_SUITE.erl:296` – guard for collecting worker results now uses `long_wait()`.
- `test/router_performance_load_SUITE.erl:251` – concurrency receive guard now uses `long_wait()`.
- `test/router_soak_multi_fault_SUITE.erl:105` – fault completion wait reduced to `short_wait()` to align with tiers.
- `test/router_intake_chaos_advanced_SUITE.erl:154` – replaced the 30 000 ms monitor timeout with `long_wait()`.
- `test/router_policy_store_SUITE.erl:544` – telemetry receive guard now driven by `short_wait()`.
- `test/router_policy_store_prop_SUITE.erl:193 & 231` – concurrent CRUD property tests now rely on `long_wait()`.
- `test/router_observability_performance_SUITE.erl:242` – `after` guard converted to `long_wait()` for process exits.
- `test/router_rate_limit_store_unit_SUITE.erl:395 & 405` – tiny receive guards now call `very_short_wait()`.
- `test/router_idem_advanced_SUITE.erl:123` – monitor wait switched to `long_wait()`.
- `test/router_test_init_SUITE.erl:186 & 229` – concurrency timeouts now reference `short_wait()`/`long_wait()`.

### Timeout values per tier
- `very_short_wait`: sanity 100 ms / fast 200 ms / full 400 ms / heavy 800 ms
- `short_wait`: sanity 500 ms / fast 1000 ms / full 2000 ms / heavy 4000 ms
- `default_wait` (used by `call_timeout`/`receive_timeout`): sanity 2000 ms / fast 4000 ms / full 5000 ms / heavy 10000 ms
- `long_wait`: sanity 8000 ms / fast 12000 ms / full 20000 ms / heavy 40000 ms

### Baseline & verification
- Baseline duration (from `reports/full_baseline.json` before this run): 112 s.
- Measured duration after the change: 116 s (`./scripts/ct-full.sh`, 1m55.2s, quality gates all green) → +3.6% delta, still within ±10%.
