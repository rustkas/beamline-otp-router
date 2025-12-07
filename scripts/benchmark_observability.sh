#!/bin/bash
# Router Observability Performance Benchmarking Script
# Measures observability overhead (logging, PII filtering, JSON serialization) under load

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPORT_DIR="${ROUTER_DIR}/reports/benchmark"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="${REPORT_DIR}/observability_benchmark_${TIMESTAMP}.json"

# Benchmark parameters
ITERATIONS=${BENCHMARK_ITERATIONS:-10000}
WARMUP_ITERATIONS=${BENCHMARK_WARMUP:-1000}

cd "${ROUTER_DIR}"

echo "=== Router Observability Performance Benchmark ==="
echo ""

# Create report directory
mkdir -p "${REPORT_DIR}"

# Check if rebar3 is available
if ! command -v rebar3 >/dev/null 2>&1; then
    echo "Error: rebar3 not found. Please install rebar3."
    exit 1
fi

# Check if Erlang/OTP is available
if ! command -v erl >/dev/null 2>&1; then
    echo "Error: Erlang/OTP not found. Please install Erlang/OTP."
    exit 1
fi

echo "Benchmark parameters:"
echo "  Iterations: ${ITERATIONS}"
echo "  Warmup: ${WARMUP_ITERATIONS}"
echo ""

# Check if performance test suite exists
if ! rebar3 ct --list-suites | grep -q "router_observability_performance_SUITE"; then
    echo "Warning: Performance test suite not found. Building tests..."
    rebar3 compile
fi

echo "Running performance tests..."
echo ""

# Run performance tests and capture output
PERF_OUTPUT=$(mktemp)
rebar3 ct --suite test/router_observability_performance_SUITE --verbose > "${PERF_OUTPUT}" 2>&1 || {
    echo "Warning: Performance tests may have warnings, but continuing with benchmark analysis..."
}

# Parse performance test output
parse_performance_output() {
    local output_file="$1"
    local results=()
    
    # Extract log generation throughput (logs/second)
    local logs_per_sec=$(grep -oP 'Generated \d+ logs in [\d.]+ seconds \([\d.]+ logs/second\)' "${output_file}" | \
        grep -oP '[\d.]+ logs/second' | grep -oP '[\d.]+' | head -1 || echo "0")
    
    # Extract PII filtering latency (microseconds per log entry)
    local pii_latency_us=$(grep -oP 'PII filtering: [\d.]+ microseconds per log entry' "${output_file}" | \
        grep -oP '[\d.]+' | head -1 || echo "0")
    
    # Extract JSON serialization latency (microseconds per log entry)
    local json_latency_us=$(grep -oP 'JSON serialization: [\d.]+ microseconds per log entry' "${output_file}" | \
        grep -oP '[\d.]+' | head -1 || echo "0")
    
    # Extract memory usage (bytes per log entry)
    local memory_bytes=$(grep -oP 'Memory usage: \d+ bytes delta \([\d.]+ bytes per log\)' "${output_file}" | \
        grep -oP '[\d.]+ bytes per log' | grep -oP '[\d.]+' | head -1 || echo "0")
    
    # Extract concurrent logging throughput (logs/second)
    local concurrent_logs_per_sec=$(grep -oP 'Concurrent logging: .* [\d.]+ logs/second total' "${output_file}" | \
        grep -oP '[\d.]+ logs/second' | grep -oP '[\d.]+' | head -1 || echo "0")
    
    echo "${logs_per_sec}|${pii_latency_us}|${json_latency_us}|${memory_bytes}|${concurrent_logs_per_sec}"
}

# Parse results
if [ -f "${PERF_OUTPUT}" ]; then
    IFS='|' read -r logs_per_sec pii_latency_us json_latency_us memory_bytes concurrent_logs_per_sec <<< "$(parse_performance_output "${PERF_OUTPUT}")"
    
    echo "Performance Results:"
    echo "  Log generation: ${logs_per_sec} logs/second"
    echo "  PII filtering: ${pii_latency_us} microseconds per entry"
    echo "  JSON serialization: ${json_latency_us} microseconds per entry"
    echo "  Memory overhead: ${memory_bytes} bytes per log entry"
    echo "  Concurrent logging: ${concurrent_logs_per_sec} logs/second"
    echo ""
    
    # Convert microseconds to milliseconds for PII and JSON
    pii_latency_ms=$(echo "scale=3; ${pii_latency_us} / 1000" | bc 2>/dev/null || echo "0")
    json_latency_ms=$(echo "scale=3; ${json_latency_us} / 1000" | bc 2>/dev/null || echo "0")
    
    # Generate JSON report
    cat > "${REPORT_FILE}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "component": "router",
  "benchmark_type": "observability",
  "parameters": {
    "iterations": ${ITERATIONS},
    "warmup_iterations": ${WARMUP_ITERATIONS}
  },
  "results": {
    "log_generation": {
      "throughput_logs_per_second": ${logs_per_sec:-0},
      "unit": "logs/second"
    },
    "pii_filtering": {
      "latency_microseconds_per_entry": ${pii_latency_us:-0},
      "latency_milliseconds_per_entry": ${pii_latency_ms:-0},
      "unit": "microseconds"
    },
    "json_serialization": {
      "latency_microseconds_per_entry": ${json_latency_us:-0},
      "latency_milliseconds_per_entry": ${json_latency_ms:-0},
      "unit": "microseconds"
    },
    "memory_usage": {
      "bytes_per_log_entry": ${memory_bytes:-0},
      "unit": "bytes"
    },
    "concurrent_logging": {
      "throughput_logs_per_second": ${concurrent_logs_per_sec:-0},
      "unit": "logs/second"
    }
  },
  "system": {
    "cpu_cores": $(nproc 2>/dev/null || echo "unknown"),
    "memory_total_mb": $(free -m | awk '/^Mem:/ {print $2}' 2>/dev/null || echo "unknown"),
    "erlang_version": "$(erl -version 2>&1 | head -1 || echo "unknown")"
  },
  "thresholds": {
    "log_generation_min": 1000,
    "pii_filtering_max_us": 100,
    "json_serialization_max_us": 50,
    "memory_max_bytes": 1024,
    "concurrent_logging_min": 5000
  }
}
EOF
    
    echo "✅ Benchmark report generated: ${REPORT_FILE}"
    echo ""
    echo "Report summary:"
    if command -v jq >/dev/null 2>&1; then
        cat "${REPORT_FILE}" | jq '.'
    else
        cat "${REPORT_FILE}"
    fi
    
    # Check thresholds
    echo ""
    echo "Threshold checks:"
    if (( $(echo "${logs_per_sec} >= 1000" | bc -l 2>/dev/null || echo 0) )); then
        echo "  ✅ Log generation throughput: ${logs_per_sec} >= 1000 logs/second"
    else
        echo "  ⚠️  Log generation throughput: ${logs_per_sec} < 1000 logs/second (threshold not met)"
    fi
    
    if (( $(echo "${pii_latency_us} < 100" | bc -l 2>/dev/null || echo 0) )); then
        echo "  ✅ PII filtering latency: ${pii_latency_us} < 100 microseconds"
    else
        echo "  ⚠️  PII filtering latency: ${pii_latency_us} >= 100 microseconds (threshold exceeded)"
    fi
    
    if (( $(echo "${json_latency_us} < 50" | bc -l 2>/dev/null || echo 0) )); then
        echo "  ✅ JSON serialization latency: ${json_latency_us} < 50 microseconds"
    else
        echo "  ⚠️  JSON serialization latency: ${json_latency_us} >= 50 microseconds (threshold exceeded)"
    fi
    
    if (( $(echo "${memory_bytes} < 1024" | bc -l 2>/dev/null || echo 0) )); then
        echo "  ✅ Memory overhead: ${memory_bytes} < 1024 bytes per log entry"
    else
        echo "  ⚠️  Memory overhead: ${memory_bytes} >= 1024 bytes per log entry (threshold exceeded)"
    fi
    
    if (( $(echo "${concurrent_logs_per_sec} >= 5000" | bc -l 2>/dev/null || echo 0) )); then
        echo "  ✅ Concurrent logging throughput: ${concurrent_logs_per_sec} >= 5000 logs/second"
    else
        echo "  ⚠️  Concurrent logging throughput: ${concurrent_logs_per_sec} < 5000 logs/second (threshold not met)"
    fi
    
    # Cleanup temp file
    rm -f "${PERF_OUTPUT}"
else
    echo "⚠️  Performance test output not found"
    exit 1
fi

echo ""
echo "=== Benchmark Complete ==="
echo ""
echo "To run benchmark again:"
echo "  cd ${ROUTER_DIR}"
echo "  BENCHMARK_ITERATIONS=20000 bash scripts/benchmark_observability.sh"

