#!/bin/bash
# Run E2E fault injection tests with stability verification
# 
# This script runs all fault injection E2E tests multiple times to ensure stability.
# Fault injection scenarios may introduce flakiness, so multiple runs are recommended.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPORT_DIR="${ROUTER_DIR}/reports/dry-run-logs/fault-injection-e2e"

# Create report directory
mkdir -p "${REPORT_DIR}"

# Number of test runs for stability verification
RUNS=${RUNS:-3}

echo "=========================================="
echo "E2E Fault Injection Tests"
echo "=========================================="
echo "Router directory: ${ROUTER_DIR}"
echo "Report directory: ${REPORT_DIR}"
echo "Number of runs: ${RUNS}"
echo ""

cd "${ROUTER_DIR}"

# Track overall results
OVERALL_PASSED=true
FAILED_RUNS=0

# Run tests multiple times
for i in $(seq 1 "${RUNS}"); do
    echo "----------------------------------------"
    echo "Run ${i}/${RUNS}"
    echo "----------------------------------------"
    
    RUN_LOG="${REPORT_DIR}/run-${i}.log"
    RUN_XML="${REPORT_DIR}/run-${i}.xml"
    
    if rebar3 ct \
        --suite router_jetstream_e2e_SUITE \
        --suite router_delivery_count_tracking_SUITE \
        --dir "${REPORT_DIR}/run-${i}-ct-logs" \
        --logdir "${REPORT_DIR}/run-${i}-ct-logs" \
        --config "${ROUTER_DIR}/test/ct.config" \
        > "${RUN_LOG}" 2>&1; then
        echo "✅ Run ${i}/${RUNS} PASSED"
    else
        echo "❌ Run ${i}/${RUNS} FAILED"
        OVERALL_PASSED=false
        FAILED_RUNS=$((FAILED_RUNS + 1))
    fi
    
    echo ""
done

# Summary
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo "Total runs: ${RUNS}"
echo "Passed: $((RUNS - FAILED_RUNS))"
echo "Failed: ${FAILED_RUNS}"
echo ""

if [ "${OVERALL_PASSED}" = true ]; then
    echo "✅ All test runs PASSED"
    exit 0
else
    echo "❌ Some test runs FAILED"
    echo "Check logs in: ${REPORT_DIR}"
    exit 1
fi

