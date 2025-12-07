#!/bin/bash
# Stress-run script for router_concurrent_faults_SUITE
# Runs the suite multiple times to detect flaky tests
#
# Usage:
#   ./stress_run_concurrent_faults.sh [iterations] [suite_name]
#
# Examples:
#   ./stress_run_concurrent_faults.sh 10                    # Run 10 iterations of all tests
#   ./stress_run_concurrent_faults.sh 20 router_concurrent_faults_SUITE  # Run specific suite 20 times

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
ITERATIONS=${1:-10}
SUITE_NAME=${2:-"router_concurrent_faults_SUITE"}

# Configurable success rate threshold (default: 95%)
# Can be overridden via STRESS_RUN_SUCCESS_RATE_THRESHOLD environment variable
SUCCESS_RATE_THRESHOLD=${STRESS_RUN_SUCCESS_RATE_THRESHOLD:-95}

# Validate iterations
if ! [[ "$ITERATIONS" =~ ^[0-9]+$ ]] || [ "$ITERATIONS" -lt 1 ]; then
    echo -e "${RED}Error: Iterations must be a positive integer${NC}"
    exit 1
fi

# Check if we're in the right directory
if [ ! -f "rebar.config" ]; then
    echo -e "${RED}Error: Must run from apps/otp/router directory${NC}"
    exit 1
fi

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 not found. Please install rebar3 first.${NC}"
    exit 1
fi

echo -e "${GREEN}=== Stress Run: ${SUITE_NAME} ===${NC}"
echo -e "Iterations: ${ITERATIONS}"
echo -e "Suite: ${SUITE_NAME}"
echo ""

# Create results directory
RESULTS_DIR="stress_run_results_$(date +%Y%m%d_%H%M%S)"
mkdir -p "${RESULTS_DIR}"

# Track results
PASSED=0
FAILED=0
TOTAL_TIME=0

# Run iterations
for i in $(seq 1 "${ITERATIONS}"); do
    echo -e "${YELLOW}[${i}/${ITERATIONS}] Running ${SUITE_NAME}...${NC}"
    
    ITERATION_START=$(date +%s)
    
    # Run test suite
    if rebar3 ct --suite "test/${SUITE_NAME}" --logdir "${RESULTS_DIR}/run_${i}" > "${RESULTS_DIR}/run_${i}.log" 2>&1; then
        ITERATION_END=$(date +%s)
        ITERATION_TIME=$((ITERATION_END - ITERATION_START))
        TOTAL_TIME=$((TOTAL_TIME + ITERATION_TIME))
        PASSED=$((PASSED + 1))
        echo -e "${GREEN}✓ Passed (${ITERATION_TIME}s)${NC}"
    else
        ITERATION_END=$(date +%s)
        ITERATION_TIME=$((ITERATION_END - ITERATION_START))
        TOTAL_TIME=$((TOTAL_TIME + ITERATION_TIME))
        FAILED=$((FAILED + 1))
        echo -e "${RED}✗ Failed (${ITERATION_TIME}s)${NC}"
        echo -e "${YELLOW}  Log: ${RESULTS_DIR}/run_${i}.log${NC}"
    fi
done

# Calculate statistics
AVG_TIME=$((TOTAL_TIME / ITERATIONS))
SUCCESS_RATE=$((PASSED * 100 / ITERATIONS))

echo ""
echo -e "${GREEN}=== Stress Run Results ===${NC}"
echo -e "Total iterations: ${ITERATIONS}"
echo -e "${GREEN}Passed: ${PASSED}${NC}"
echo -e "${RED}Failed: ${FAILED}${NC}"
echo -e "Success rate: ${SUCCESS_RATE}%"
echo -e "Total time: ${TOTAL_TIME}s"
echo -e "Average time per run: ${AVG_TIME}s"
echo -e "Results directory: ${RESULTS_DIR}"

# Generate detailed report
REPORT_FILE="${RESULTS_DIR}/stress_run_report.md"
cat > "${REPORT_FILE}" <<EOF
# Stress Run Report: ${SUITE_NAME}

**Date**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Iterations**: ${ITERATIONS}
**Results Directory**: ${RESULTS_DIR}

## Results Summary

- **Passed**: ${PASSED}
- **Failed**: ${FAILED}
- **Success Rate**: ${SUCCESS_RATE}%
- **Total Time**: ${TOTAL_TIME}s
- **Average Time per Run**: ${AVG_TIME}s

## Status

EOF

if [ "${FAILED}" -eq 0 ]; then
    echo "✅ **All iterations passed** - No flaky tests detected" >> "${REPORT_FILE}"
    EXIT_CODE=0
elif [ "${SUCCESS_RATE}" -ge "${SUCCESS_RATE_THRESHOLD}" ]; then
    echo "⚠️ **Mostly stable** - ${FAILED} iteration(s) failed (${SUCCESS_RATE}% success rate, threshold: ${SUCCESS_RATE_THRESHOLD}%)" >> "${REPORT_FILE}"
    echo "" >> "${REPORT_FILE}"
    echo "**Action**: Review failed iterations in logs" >> "${REPORT_FILE}"
    EXIT_CODE=0  # Don't fail on minor flakiness
else
    echo "❌ **Potential flakiness detected** - ${FAILED} iteration(s) failed (${SUCCESS_RATE}% success rate, threshold: ${SUCCESS_RATE_THRESHOLD}%)" >> "${REPORT_FILE}"
    echo "" >> "${REPORT_FILE}"
    echo "**Action Required**: Investigate flaky tests immediately" >> "${REPORT_FILE}"
    EXIT_CODE=1
fi

# Add threshold info to report
echo "" >> "${REPORT_FILE}"
echo "## Configuration" >> "${REPORT_FILE}"
echo "" >> "${REPORT_FILE}"
echo "- **Success Rate Threshold**: ${SUCCESS_RATE_THRESHOLD}%" >> "${REPORT_FILE}"
echo "- **Threshold can be adjusted via**: \`STRESS_RUN_SUCCESS_RATE_THRESHOLD\` environment variable" >> "${REPORT_FILE}"

# Add failed iterations details
if [ "${FAILED}" -gt 0 ]; then
    echo "" >> "${REPORT_FILE}"
    echo "## Failed Iterations" >> "${REPORT_FILE}"
    echo "" >> "${REPORT_FILE}"
    
    for i in $(seq 1 "${ITERATIONS}"); do
        if [ -f "${RESULTS_DIR}/run_${i}.log" ]; then
            if ! grep -q "All tests passed\|All iterations passed" "${RESULTS_DIR}/run_${i}.log" 2>/dev/null; then
                echo "### Run ${i}" >> "${REPORT_FILE}"
                echo "" >> "${REPORT_FILE}"
                echo "**Log**: \`${RESULTS_DIR}/run_${i}.log\`" >> "${REPORT_FILE}"
                echo "" >> "${REPORT_FILE}"
                # Extract error summary (first few error lines)
                if grep -i "fail\|error\|exception" "${RESULTS_DIR}/run_${i}.log" 2>/dev/null | head -3 > /tmp/errors_${i}.txt; then
                    echo "**Error Summary**:" >> "${REPORT_FILE}"
                    echo '```' >> "${REPORT_FILE}"
                    cat /tmp/errors_${i}.txt >> "${REPORT_FILE}"
                    echo '```' >> "${REPORT_FILE}"
                    rm -f /tmp/errors_${i}.txt
                fi
                echo "" >> "${REPORT_FILE}"
            fi
        fi
    done
fi

echo "" >> "${REPORT_FILE}"
echo "## Report Generated" >> "${REPORT_FILE}"
echo "" >> "${REPORT_FILE}"
echo "Report file: \`${REPORT_FILE}\`" >> "${REPORT_FILE}"

# Determine exit code
if [ "${FAILED}" -eq 0 ]; then
    echo -e "${GREEN}✓ All iterations passed - no flaky tests detected${NC}"
    echo -e "${GREEN}Report: ${REPORT_FILE}${NC}"
    exit 0
else
    echo -e "${RED}✗ ${FAILED} iteration(s) failed - potential flaky tests detected${NC}"
    echo -e "${YELLOW}Review logs in ${RESULTS_DIR}/ for details${NC}"
    echo -e "${YELLOW}Report: ${REPORT_FILE}${NC}"
    exit ${EXIT_CODE}
fi

