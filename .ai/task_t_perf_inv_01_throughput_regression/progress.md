# Progress

Status: IN PROGRESS

Environment snapshot (before isolated reruns):
- Hostname: LAPTOP-CUE7V9JU
- CPU cores: 4
- Memory: 6069488 kB
- Git commit: 39d9b7a43a535fefc64ff78b46af05de8e028c70
- Cgroup CPU limit: unknown (cpu.max not present)
- Cgroup memory limit: unknown (memory.max not present)
- Perf env (planned):
  - ROUTER_TEST_LEVEL=heavy
  - ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json
  - ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true
  - ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35
  - REBAR3_OFFLINE=1
  - CT_LOGDIR=_build/test/logs/inv_perf_regression_{1,2}
- Snapshot file: `_build/test/logs/inv_perf_regression_env_snapshot.txt`
- Top CPU processes (sample):
  - trae-helper (17.3% CPU)
  - trae-helper (15.0% CPU)
  - ckg_server (9.8% CPU)
  - ckg_server (9.6% CPU)
  - ckg_server (9.6% CPU)

No investigation runs recorded yet.

Isolated rerun #1:
- Command:
  `REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL=heavy ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35 CT_LOGDIR=_build/test/logs/inv_perf_regression_1 rebar3 ct --suite router_performance_regression_SUITE --retry`
- Result: FAIL
  - P95 latency regression: Baseline 44 ms, Current 82 ms, Increase 86.36% (> 50%)
- Logs: `_build/test/logs/inv_perf_regression_1`

Isolated rerun #2:
- Command:
  `REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL=heavy ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35 CT_LOGDIR=_build/test/logs/inv_perf_regression_2 rebar3 ct --suite router_performance_regression_SUITE --retry`
- Result: FAIL
  - Sequential throughput regression: Baseline 36.79 req/s, Current 16.23 req/s, Degradation 55.88% (> 35%)
- Logs: `_build/test/logs/inv_perf_regression_2`

Pinned rerun #1 (CPU 2-5):
- Command:
  `taskset -c 2-5 env REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL=heavy ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35 CT_LOGDIR=_build/test/logs/inv_perf_regression_pinned_1 rebar3 ct --suite router_performance_regression_SUITE --retry`
- Result: FAIL
  - Sequential throughput regression: Baseline 36.79 req/s, Current 6.60 req/s, Degradation 82.07% (> 35%)
- Logs: `_build/test/logs/inv_perf_regression_pinned_1`

Pinned rerun #2 (CPU 2-5):
- Command:
  `taskset -c 2-5 env REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL=heavy ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35 CT_LOGDIR=_build/test/logs/inv_perf_regression_pinned_2 rebar3 ct --suite router_performance_regression_SUITE --retry`
- Result: FAIL
  - Sequential throughput regression: Baseline 36.79 req/s, Current 11.21 req/s, Degradation 69.52% (> 35%)
- Logs: `_build/test/logs/inv_perf_regression_pinned_2`

Scheduler stabilization (soft):
- ERL_FLAGS="+S 4:4"
- Command:
  `taskset -c 2-5 env REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL=heavy ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35 CT_LOGDIR=_build/test/logs/inv_perf_regression_sched_soft_1 rebar3 ct --suite router_performance_regression_SUITE --retry`
- Result: FAIL
  - Sequential throughput regression: Baseline 36.79 req/s, Current 23.72 req/s, Degradation 35.52% (> 35%)
- Logs: `_build/test/logs/inv_perf_regression_sched_soft_1`

Scheduler stabilization (hard):
- ERL_FLAGS="+S 4:4 +sbwt none +sbtu +swt low"
- Command:
  `taskset -c 2-5 env REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL=heavy ROUTER_PERF_BASELINE_JSON=docs/perf/baselines/batch3_heavy_iso.json ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT=true ROUTER_PERF_REGRESSION_MAX_DEGRADATION=35 CT_LOGDIR=_build/test/logs/inv_perf_regression_sched_hard_1 rebar3 ct --suite router_performance_regression_SUITE --retry`
- Result: FAIL
  - Sequential throughput regression: Baseline 36.79 req/s, Current 4.82 req/s, Degradation 86.90% (> 35%)
- Logs: `_build/test/logs/inv_perf_regression_sched_hard_1`

NATS monitoring snapshots:
- `_build/test/logs/inv_nats_snapshot/varz_before.json`
- `_build/test/logs/inv_nats_snapshot/connz_before.json`
- `_build/test/logs/inv_nats_snapshot/varz_after.json`
- `_build/test/logs/inv_nats_snapshot/connz_after.json`
- Note: files are empty (HTTP 8222 endpoints unavailable or returned no data).

NATS varz/connz summary:
- varz: cpu=unknown, mem=unknown, connections=unknown, in/out msgs=unknown, jetstream=unknown
- connz: connections=unknown, slow_consumers=unknown, pending_bytes=unknown
- Note: summaries unavailable because varz/connz endpoints returned no data.

Cgroup throttle snapshot:
- cpu.max: missing
- cpu.stat: nr_throttled=0, throttled_usec=0

Conclusion (preliminary):
- Infra contention suspected (throughput worsens under pinning/scheduler stabilization; throttling not reported by cgroup).
- infra_contention_suspected

Follow-up checklist (infra isolation):
- Dedicated perf runner or isolated VM for Batch #3 regression checks.
- Stop/disable noisy processes (e.g., `trae-helper`, `ckg_server`) during perf runs.
- Enforce fixed cpuset + pinned schedulers for perf runs (document cpuset range).
- Ensure no parallel CT suites or background workloads on the same host.
