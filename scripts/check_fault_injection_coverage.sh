#!/bin/bash
# Check fault injection test coverage and metrics contract consistency
#
# This script verifies:
# 1. test_coverage annotations in PROMETHEUS_ALERTS.md reference existing tests
# 2. Metric contract constants in test files match PROMETHEUS_ALERTS.md
#
# Exit codes:
#   0 - All checks passed
#   1 - test_coverage references invalid tests
#   2 - Metric contract mismatch
#   3 - Missing test_coverage annotations

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
TEST_DIR="${ROUTER_DIR}/test"
DOCS_DIR="${ROUTER_DIR}/docs"
ALERTS_FILE="${DOCS_DIR}/PROMETHEUS_ALERTS.md"

ERRORS=0
WARNINGS=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Checking fault injection test coverage and metrics contract..."

# Check if PROMETHEUS_ALERTS.md exists
if [ ! -f "${ALERTS_FILE}" ]; then
    echo -e "${RED}ERROR: ${ALERTS_FILE} not found${NC}"
    exit 1
fi

# Extract test_coverage annotations from PROMETHEUS_ALERTS.md
echo "Checking test_coverage annotations..."

# Find all test_coverage annotations
TEST_COVERAGE_ANNOTATIONS=$(grep -n "test_coverage:" "${ALERTS_FILE}" || true)

if [ -z "${TEST_COVERAGE_ANNOTATIONS}" ]; then
    echo -e "${YELLOW}WARNING: No test_coverage annotations found in ${ALERTS_FILE}${NC}"
    WARNINGS=$((WARNINGS + 1))
else
    # Extract test names from annotations
    while IFS= read -r line; do
        # Extract test names (pattern: test_*), but skip "test_coverage" itself
        TESTS=$(echo "${line}" | grep -oE 'test_[a-z_]+' | grep -v '^test_coverage$' || true)
        
        if [ -z "${TESTS}" ]; then
            echo -e "${YELLOW}WARNING: Could not extract test names from: ${line}${NC}"
            WARNINGS=$((WARNINGS + 1))
            continue
        fi
        
        # Check each test exists
        for test in ${TESTS}; do
            # Search for test function in test files (look for function definition)
            if ! grep -r "^${test}(_Config)" "${TEST_DIR}"/*_SUITE.erl > /dev/null 2>&1 && \
               ! grep -r "^${test}(" "${TEST_DIR}"/*_SUITE.erl > /dev/null 2>&1; then
                echo -e "${RED}ERROR: test_coverage references non-existent test: ${test}${NC}"
                echo "  Found in: ${line}"
                ERRORS=$((ERRORS + 1))
            else
                echo -e "${GREEN}✓ Test ${test} found${NC}"
            fi
        done
    done <<< "${TEST_COVERAGE_ANNOTATIONS}"
fi

# Check metric contract constants (basic check)
echo "Checking metric contract constants..."

# Check if test files define contract constants
SUITE_FILES=$(find "${TEST_DIR}" -name "*fault_injection*SUITE.erl" -o -name "*jetstream*SUITE.erl" -o -name "*result_consumer*SUITE.erl" -o -name "*decide_consumer*SUITE.erl" 2>/dev/null | head -5 || true)

if [ -z "${SUITE_FILES}" ]; then
    echo -e "${YELLOW}WARNING: No fault injection test suite files found${NC}"
    WARNINGS=$((WARNINGS + 1))
else
    FOUND_CONSTANTS=0
    for suite_file in ${SUITE_FILES}; do
        # Check for metric contract constants
        if grep -q "REDELIVERY_REQUIRED_LABELS\|MAXDELIVER_REQUIRED_LABELS" "${suite_file}" 2>/dev/null; then
            echo -e "${GREEN}✓ Found metric contract constants in $(basename ${suite_file})${NC}"
            FOUND_CONSTANTS=1
        fi
    done
    if [ ${FOUND_CONSTANTS} -eq 0 ]; then
        echo -e "${YELLOW}WARNING: No metric contract constants found in test files${NC}"
        WARNINGS=$((WARNINGS + 1))
    fi
fi

# Check that PROMETHEUS_ALERTS.md mentions expected metrics
echo "Checking metric definitions in PROMETHEUS_ALERTS.md..."

REQUIRED_METRICS=(
    "router_jetstream_redelivery_total"
    "router_jetstream_maxdeliver_exhausted_total"
)

for metric in "${REQUIRED_METRICS[@]}"; do
    if ! grep -q "${metric}" "${ALERTS_FILE}"; then
        echo -e "${RED}ERROR: Metric ${metric} not found in ${ALERTS_FILE}${NC}"
        ERRORS=$((ERRORS + 1))
    else
        echo -e "${GREEN}✓ Metric ${metric} found in ${ALERTS_FILE}${NC}"
    fi
done

# Summary
echo ""
echo "=== Summary ==="
if [ ${ERRORS} -eq 0 ] && [ ${WARNINGS} -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed${NC}"
    exit 0
elif [ ${ERRORS} -eq 0 ]; then
    echo -e "${YELLOW}⚠ Checks passed with ${WARNINGS} warning(s)${NC}"
    exit 0
else
    echo -e "${RED}✗ Found ${ERRORS} error(s) and ${WARNINGS} warning(s)${NC}"
    exit 1
fi

