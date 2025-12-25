#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: scripts/ct-batch.sh --batch=1|2|3 [--level=fast|full|heavy|all] [--resume-from=<suite>]
                           [--chaos=true|false] [--duration-hours=<hours>]

Notes:
- --batch is required.
- --level defaults to all (fast + full + heavy).
- --resume-from accepts either "test/<suite>.erl" or "<suite>.erl".
- --chaos only affects batch=1 heavy runs (RUN_CHAOS_TESTS).
- --duration-hours only affects batch=1 heavy runs (STRESS_SOAK_DURATION_HOURS).

Timeouts:
- Per-suite timeout is controlled via CT_SUITE_TIMEOUT_S (seconds).
  If not set, defaults are:
    - batch=1 + heavy: 3600
    - otherwise:       900
- If the `timeout` command is not available, suites run without a timeout.
USAGE
}

batch=""
level="all"
resume_from=""
chaos="false"
duration_hours="0.01"
ct_suite_timeout_s="${CT_SUITE_TIMEOUT_S:-}"

for arg in "$@"; do
  case "${arg}" in
    --batch=*)
      batch="${arg#*=}"
      ;;
    --level=*)
      level="${arg#*=}"
      ;;
    --resume-from=*)
      resume_from="${arg#*=}"
      ;;
    --chaos=*)
      chaos="${arg#*=}"
      ;;
    --duration-hours=*)
      duration_hours="${arg#*=}"
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: ${arg}" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ -z "${batch}" ]]; then
  echo "Missing --batch" >&2
  usage
  exit 1
fi

case "${batch}" in
  1|2|3)
    ;;
  *)
    echo "Invalid --batch: ${batch}" >&2
    usage
    exit 1
    ;;
esac

case "${level}" in
  fast|full|heavy|all)
    ;;
  *)
    echo "Invalid --level: ${level}" >&2
    usage
    exit 1
    ;;
esac

batch1_suites=(
  test/router_concurrent_faults_soak_SUITE.erl
  test/router_soak_baseline_SUITE.erl
  test/router_soak_single_fault_SUITE.erl
  test/router_soak_multi_fault_SUITE.erl
  test/router_stress_soak_SUITE.erl
  test/router_jetstream_soak_SUITE.erl
  test/router_jetstream_soak_restart_SUITE.erl
  test/router_jetstream_soak_prod_SUITE.erl
  test/router_jetstream_soak_combined_SUITE.erl
  test/router_jetstream_soak_perf_SUITE.erl
  test/router_ext_chaos_failure_SUITE.erl
  test/router_ext_chaos_recovery_SUITE.erl
  test/router_extensions_chaos_SUITE.erl
  test/router_intake_chaos_SUITE.erl
  test/router_intake_chaos_restart_SUITE.erl
  test/router_intake_chaos_advanced_SUITE.erl
  test/router_chaos_engineering_SUITE.erl
)

batch2_suites=(
  test/router_concurrent_faults_stress_SUITE.erl
  test/router_metrics_faults_aggregation_SUITE.erl
  test/router_metrics_faults_cardinality_SUITE.erl
  test/router_metrics_under_faults_SUITE.erl
  test/router_decide_consumer_faults_SUITE.erl
  test/router_result_consumer_faults_SUITE.erl
  test/router_recovery_faults_SUITE.erl
  test/router_concurrent_faults_basic_SUITE.erl
  test/router_concurrent_faults_combo_SUITE.erl
  test/router_concurrent_faults_extended_SUITE.erl
  test/router_advanced_faults_mixed_SUITE.erl
  test/router_advanced_faults_triple_SUITE.erl
  test/router_advanced_concurrent_faults_SUITE.erl
  test/router_concurrent_faults_SUITE.erl
)

ct_logdir="$(pwd)/_build/test/logs"
mkdir -p "${ct_logdir}"

batch3_suites=(
  test/router_admin_grpc_integration_SUITE.erl
  test/router_caf_integration_SUITE.erl
  test/router_e2e_smoke_SUITE.erl
  test/router_ext_e2e_advanced_SUITE.erl
  test/router_ext_e2e_core_SUITE.erl
  test/router_ext_load_advanced_SUITE.erl
  test/router_ext_load_baseline_SUITE.erl
  test/router_extensions_e2e_SUITE.erl
  test/router_extensions_pipeline_load_SUITE.erl
  test/router_gateway_integration_SUITE.erl
  test/router_grpc_integration_SUITE.erl
  test/router_health_integration_SUITE.erl
  test/router_headers_propagation_e2e_SUITE.erl
  test/router_jetstream_e2e_cp2_SUITE.erl
  test/router_jetstream_e2e_integration_SUITE.erl
  test/router_jetstream_e2e_SUITE.erl
  test/router_metrics_labels_integration_SUITE.erl
  test/router_metrics_labels_performance_SUITE.erl
  test/router_nats_integ_normal_SUITE.erl
  test/router_nats_integ_recovery_SUITE.erl
  test/router_nats_integration_SUITE.erl
  test/router_nats_performance_SUITE.erl
  test/router_observability_otel_SUITE.erl
  test/router_observability_performance_SUITE.erl
  test/router_performance_benchmark_SUITE.erl
  test/router_performance_load_SUITE.erl
  test/router_performance_regression_SUITE.erl
  test/router_provider_integration_SUITE.erl
  test/router_publish_failure_e2e_SUITE.erl
  test/router_publish_failure_e2e_randomized_SUITE.erl
  test/router_resilience_benchmark_SUITE.erl
)

if [[ "${batch}" == "1" ]]; then
  suites=("${batch1_suites[@]}")
elif [[ "${batch}" == "2" ]]; then
  suites=("${batch2_suites[@]}")
else
  suites=("${batch3_suites[@]}")
fi

should_skip_suite() {
  local suite="$1"
  if [[ -z "${resume_from}" ]]; then
    return 1
  fi

  if [[ "${suite}" == "${resume_from}" || "${suite}" == "test/${resume_from}" ]]; then
    return 1
  fi

  return 0
}

has_timeout() {
  command -v timeout >/dev/null 2>&1
}

effective_suite_timeout_s() {
  local lvl="$1"

  if [[ -n "${ct_suite_timeout_s}" ]]; then
    echo "${ct_suite_timeout_s}"
    return 0
  fi

  if [[ "${batch}" == "1" && "${lvl}" == "heavy" ]]; then
    echo "3600"
    return 0
  fi

  echo "900"
}

run_suite_level() {
  local suite="$1"
  local lvl="$2"
  local suite_timeout_s=""

  echo "=== Suite: ${suite} (${lvl}) ==="

  suite_timeout_s="$(effective_suite_timeout_s "${lvl}")"

  if [[ "${lvl}" == "heavy" && "${batch}" == "1" ]]; then
    if has_timeout; then
      timeout --preserve-status "${suite_timeout_s}s" \
        env CT_LOGDIR="${ct_logdir}" \
          REBAR3_OFFLINE=1 \
          ROUTER_TEST_LEVEL=heavy \
          RUN_JETSTREAM_SOAK=false \
          RUN_CHAOS_TESTS="${chaos}" \
          STRESS_SOAK_DURATION_HOURS="${duration_hours}" \
          rebar3 ct --suite "${suite}" --retry
    else
      CT_LOGDIR="${ct_logdir}" \
        REBAR3_OFFLINE=1 \
        ROUTER_TEST_LEVEL=heavy \
        RUN_JETSTREAM_SOAK=false \
        RUN_CHAOS_TESTS="${chaos}" \
        STRESS_SOAK_DURATION_HOURS="${duration_hours}" \
        rebar3 ct --suite "${suite}" --retry
    fi
  else
    if has_timeout; then
      timeout --preserve-status "${suite_timeout_s}s" \
        env CT_LOGDIR="${ct_logdir}" \
          REBAR3_OFFLINE=1 \
          ROUTER_TEST_LEVEL="${lvl}" \
          rebar3 ct --suite "${suite}" --retry
    else
      CT_LOGDIR="${ct_logdir}" REBAR3_OFFLINE=1 ROUTER_TEST_LEVEL="${lvl}" rebar3 ct --suite "${suite}" --retry
    fi
  fi
  echo
}

resume_active="false"
if [[ -n "${resume_from}" ]]; then
  resume_active="true"
fi

for suite in "${suites[@]}"; do
  if [[ "${resume_active}" == "true" ]]; then
    if should_skip_suite "${suite}"; then
      continue
    fi
    resume_active="false"
  fi

  case "${level}" in
    fast)
      run_suite_level "${suite}" fast
      ;;
    full)
      run_suite_level "${suite}" full
      ;;
    heavy)
      run_suite_level "${suite}" heavy
      ;;
    all)
      run_suite_level "${suite}" fast
      run_suite_level "${suite}" full
      run_suite_level "${suite}" heavy
      ;;
  esac
done
