#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/.." && pwd)"

artifacts_dir="${repo_root}/_artifacts"
mkdir -p "${artifacts_dir}"

timestamp="$(date +%Y%m%d_%H%M%S)"
logfile="${artifacts_dir}/bench_router_${timestamp}.log"
report_file="${artifacts_dir}/perf_baseline_${timestamp}.json"
summary_file="${artifacts_dir}/perf_summary_${timestamp}.md"

ct_timeout_seconds="${CT_TIMEOUT_SECONDS:-900}"        # hard timeout for CI
warmup_enabled="${WARMUP_ENABLED:-1}"                  # 1=yes, 0=no
warmup_timeout_seconds="${WARMUP_TIMEOUT_SECONDS:-300}"

suite_path="${SUITE_PATH:-test/router_performance_benchmark_SUITE.erl}"
ct_config="${CT_CONFIG:-config/test_real_nats.config}"

cd "${repo_root}"

log() { echo "$*" | tee -a "${logfile}"; }

log "=== Router Performance Benchmark Harness ==="
log "repo_root=${repo_root}"
log "suite=${suite_path}"
log "ct_config=${ct_config}"
log "ct_timeout_seconds=${ct_timeout_seconds}"

cleanup() {
  # Always attempt to stop NATS, but never fail cleanup.
  ./scripts/nats_stop.sh >> "${logfile}" 2>&1 || true
}
trap cleanup EXIT

log "[1/4] Starting NATS baseline..."
./scripts/nats_start.sh >> "${logfile}" 2>&1

# --- helper: extract [METRIC] lines ---
get_metric() {
  # Expected format in log: [METRIC] name: <value>
  # We take the last occurrence.
  grep -F "[METRIC] ${1}:" "${logfile}" | tail -1 | awk '{print $3}' || true
}

run_ct() {
  local timeout_s="$1"
  local phase="$2"

  log ""
  log "=== CT phase: ${phase} (timeout=${timeout_s}s) ==="
  export ROUTER_TEST_LEVEL=heavy

  # Ensure timeout exists (GNU coreutils). If not, run without it.
  if command -v timeout >/dev/null 2>&1; then
    set +e
    timeout --preserve-status "${timeout_s}" \
      rebar3 as test ct \
        --suite "${suite_path}" \
        --config "${ct_config}" \
        --readable=false --verbose 2>&1 | tee -a "${logfile}"
    ct_rc="${PIPESTATUS[0]}"
    set -e
  else
    set +e
    rebar3 as test ct \
      --suite "${suite_path}" \
        --config "${ct_config}" \
        --readable=false --verbose 2>&1 | tee -a "${logfile}"
    ct_rc="${PIPESTATUS[0]}"
    set -e
  fi

  log "CT phase ${phase} exit_code=${ct_rc}"
  return "${ct_rc}"
}

# [2/4] Warmup (optional)
ct_rc=0
if [ "${warmup_enabled}" = "1" ]; then
  log "[2/4] Warmup run (discard metrics)..."
  run_ct "${warmup_timeout_seconds}" "warmup" || true
fi

# [3/4] Measurement run (policy uses these metrics)
log "[3/4] Measurement run..."
if ! run_ct "${ct_timeout_seconds}" "measure"; then
  ct_rc=$?
else
  ct_rc=0
fi

# [4/4] Parse + report (always)
log "[4/4] Parsing results & generating reports..."

seq_throughput="$(get_metric "sequential_throughput")"
con_throughput="$(get_metric "concurrent_throughput")"
lat_p95="$(get_metric "latency_p95_ms")"
lat_p99="$(get_metric "latency_p99_ms")"
errors_total="$(get_metric "errors_total")"
backpressure_active_total="$(get_metric "backpressure_active_total")"
memory_mb="$(get_metric "memory_mb_total_erlang")"

# Backward-compat fallback: keep old keys if suite still prints them
if [ -z "${seq_throughput}" ]; then
  seq_throughput="$(get_metric "sequential_throughput_rps")"
fi
if [ -z "${con_throughput}" ]; then
  con_throughput="$(get_metric "concurrent_throughput_rps")"
fi

git_commit="$(git rev-parse HEAD 2>/dev/null || echo "unknown")"
otp_release="$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null || echo "unknown")"
cpu_count="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo "unknown")"
kernel="$(uname -sr 2>/dev/null || echo "unknown")"

# Produce JSON even if metrics are missing; gate script can decide what to do.
cat > "${report_file}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "git": { "commit": "${git_commit}" },
  "env": {
    "otp_release": "${otp_release}",
    "cpu_count": "${cpu_count}",
    "kernel": "${kernel}",
    "nats_mode": "real"
  },
  "metrics": {
    "rps": ${con_throughput:-0},
    "latency_p95_ms": ${lat_p95:-0},
    "latency_p99_ms": ${lat_p99:-0},
    "errors_total": ${errors_total:-0},
    "backpressure_active_total": ${backpressure_active_total:-0},
    "mem_mb_total_erlang": ${memory_mb:-0}
  },
  "run": {
    "ct_exit_code": ${ct_rc}
  }
}
EOF

cat > "${summary_file}" <<EOF
# Performance Benchmark Summary (${timestamp})

| Metric | Value |
|--------|------:|
| RPS (candidate) | ${con_throughput:-0} |
| Latency p95 (ms) | ${lat_p95:-0} |
| Latency p99 (ms) | ${lat_p99:-0} |
| Errors total | ${errors_total:-0} |
| Backpressure active total | ${backpressure_active_total:-0} |
| BEAM total memory (MB) | ${memory_mb:-0} |

**Report JSON**: ${report_file}  
**Log File**: ${logfile}  
**CT exit code**: ${ct_rc}
EOF

log ""
log "✅ Benchmark Complete (ct_exit_code=${ct_rc})"
log "JSON Report: ${report_file}"
log "Markdown Summary: ${summary_file}"
log ""

cat "${summary_file}"

# Preserve CT failure as script failure (CI should see it),
# but AFTER artifacts are written.
exit "${ct_rc}"
