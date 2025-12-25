# CT Groups and Empty Suites Policy

## Purpose
This document defines when a Common Test suite with 0 test cases is allowed,
and how such suites are documented.

## Policy: 0-test suites
A suite may be intentionally empty only if:
- it is a placeholder for future tests or a reserved grouping name, and
- the suite contains an explicit INTENTIONAL_EMPTY comment, and
- it is listed in the empty-suites tracking task.

Any suite that is empty without these conditions should be removed or
refactored into a non-suite helper module.

## Intentionally empty suites (current)
- router_caf_adapter_SUITE.erl: placeholder for caf adapter tests (scaffold)
- router_concurrent_faults_SUITE.erl: umbrella placeholder; specific suites cover scenarios
- router_concurrent_faults_stress_SUITE.erl: stress placeholder
- router_decider_SUITE.erl: decider unit test placeholder
- router_decider_prop_SUITE.erl: property-test placeholder
- router_extensions_chaos_SUITE.erl: chaos aggregate placeholder
- router_grpc_integration_SUITE.erl: gRPC integration placeholder
- router_intake_chaos_SUITE.erl: intake chaos placeholder
- router_jetstream_soak_SUITE.erl: jetstream soak placeholder
- router_metrics_under_faults_SUITE.erl: metrics-under-faults placeholder
- router_policy_store_fault_tolerance_SUITE.erl: fault tolerance placeholder
- router_policy_store_load_SUITE.erl: load placeholder
- router_policy_store_prop_SUITE.erl: property-test placeholder
- router_policy_structure_prop_SUITE.erl: property-test placeholder
- router_secrets_logging_SUITE.erl: secrets logging placeholder
- router_state_observability_SUITE.erl: state observability placeholder
- router_stress_soak_SUITE.erl: stress soak placeholder

## CT batch execution (official)
Use the single entrypoint `scripts/ct-batch.sh` for Batch #1 and Batch #2:

Batch #1 (soak + chaos heavy-only)
- `scripts/ct-batch.sh --batch=1 --level=all`
- `scripts/ct-batch.sh --batch=1 --level=heavy --chaos=true`
- `scripts/ct-batch.sh --batch=1 --level=heavy --duration-hours=0.25`
- `scripts/ct-batch.sh --batch=1 --level=heavy --resume-from=test/router_soak_baseline_SUITE.erl`

Batch #2 (fault suites)
- `scripts/ct-batch.sh --batch=2 --level=all`
- `scripts/ct-batch.sh --batch=2 --level=heavy`
- `scripts/ct-batch.sh --batch=2 --level=heavy --resume-from=test/router_concurrent_faults_combo_SUITE.erl`

Batch #3 (integration / performance / long-running)
- `scripts/ct-batch.sh --batch=3 --level=heavy`
- `scripts/ct-batch.sh --batch=3 --level=all` (manual / non-PR)

Legacy scripts (`scripts/run_ct_batch1_*.sh`, `scripts/run_ct_batch2_*.sh`) are
thin wrappers that call `scripts/ct-batch.sh`. Only `ct-batch.sh` is official.

## Confirmed end-to-end runs
- Batch #1 e2e: `scripts/ct-batch.sh --batch=1 --level=all` with
  `RUN_JETSTREAM_SOAK=false`, `RUN_CHAOS_TESTS=false`, and
  `STRESS_SOAK_DURATION_HOURS=0.01`.
- Batch #3 full dry-run: `scripts/ct-batch.sh --batch=3 --level=full` (PASS, no hangs).
- Batch #3 heavy e2e: `scripts/ct-batch.sh --batch=3 --level=heavy` (PASS; `router_performance_regression_SUITE` skips when baseline is not configured).

## Performance suite overrides (optional)
- `ROUTER_PERF_MIN_THROUGHPUT`: minimum throughput gate for `router_performance_load_SUITE` (default: 10).
- `ROUTER_PERF_SUSTAINED_SECONDS`: sustained load duration in seconds for `router_performance_load_SUITE` (defaults to 60 when `ROUTER_TEST_LEVEL=heavy`).
- `ROUTER_PERF_CONCURRENT_THROUGHPUT_ASSERT`: enable concurrent throughput assertion in `router_performance_load_SUITE` (default: false).
- `ROUTER_PERF_CONCURRENT_MIN_THROUGHPUT`: minimum concurrent throughput when assertion is enabled (default: 50).
- `ROUTER_PERF_DEGRADED_DECISION_LATENCY_ASSERT`: enable degraded decision latency assertion (default: false).
- `ROUTER_PERF_DEGRADED_DECISION_MAX_MS`: max decision latency in degraded dependency test when assertion is enabled (default: 100).
- `ROUTER_PERF_DEGRADED_TOTAL_LATENCY_ASSERT`: enable degraded total latency assertion (default: false).
- `ROUTER_PERF_DEGRADED_TOTAL_MIN_MS`: min total latency in degraded dependency test when assertion is enabled (default: 200).
- `ROUTER_EXT_LOAD_ADVANCED_P95_ASSERT`: enable P95 assertion in `router_ext_load_advanced_SUITE` degraded test (default: false).
- `ROUTER_EXT_LOAD_ADVANCED_P95_MAX_MS`: max P95 latency in `router_ext_load_advanced_SUITE` degraded test when assertion is enabled (default: 260).
- `ROUTER_PERF_REGRESSION_THROUGHPUT_ASSERT`: enable throughput regression assertions in `router_performance_regression_SUITE` (default: false).
- `ROUTER_PERF_REGRESSION_MAX_DEGRADATION`: max throughput degradation percentage when assertion is enabled (default: 50).
