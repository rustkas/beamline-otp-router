#!/bin/bash
# CI Quality Gates
# Runs all quality checks: Dialyzer, Xref, Coverage gates
#
# Usage: ./scripts/ci_quality_gates.sh [--fail-on-warnings]
# Exit codes:
#   0 - All quality gates passed
#   1 - One or more quality gates failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

FAIL_ON_WARNINGS="${1:-}"

echo "=== CI Quality Gates ==="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

EXIT_CODE=0

# 1. Dialyzer check
echo -e "${GREEN}[1/3] Running Dialyzer...${NC}"
if rebar3 dialyzer > dialyzer_output.log 2>&1; then
    echo -e "${GREEN}✓ Dialyzer passed${NC}"
else
    echo -e "${RED}✗ Dialyzer failed${NC}"
    cat dialyzer_output.log
    EXIT_CODE=1
fi
echo ""

# 2. Xref check
echo -e "${GREEN}[2/3] Running Xref...${NC}"
if rebar3 xref > xref_output.log 2>&1; then
    echo -e "${GREEN}✓ Xref passed${NC}"
else
    echo -e "${RED}✗ Xref failed${NC}"
    cat xref_output.log
    EXIT_CODE=1
fi
echo ""

# 3. Coverage gates
echo -e "${GREEN}[3/3] Checking coverage gates...${NC}"
COVERAGE_DIR="_build/test/cover"
if [ -d "${COVERAGE_DIR}" ] && [ -f "${COVERAGE_DIR}/cover.log" ]; then
    # Extract coverage metrics
    LINE_COV=$(grep -oP "Line coverage: \K[0-9.]+" "${COVERAGE_DIR}/cover.log" | head -1 || echo "0")
    BRANCH_COV=$(grep -oP "Branch coverage: \K[0-9.]+" "${COVERAGE_DIR}/cover.log" | head -1 || echo "0")
    FUNC_COV=$(grep -oP "Function coverage: \K[0-9.]+" "${COVERAGE_DIR}/cover.log" | head -1 || echo "0")
    
    # Check thresholds
    LINE_PASS=$(awk "BEGIN {print (${LINE_COV:-0} >= 80) ? 1 : 0}")
    BRANCH_PASS=$(awk "BEGIN {print (${BRANCH_COV:-0} >= 70) ? 1 : 0}")
    FUNC_PASS=$(awk "BEGIN {print (${FUNC_COV:-0} >= 90) ? 1 : 0}")
    
    if [ "${LINE_PASS}" -eq 1 ] && [ "${BRANCH_PASS}" -eq 1 ] && [ "${FUNC_PASS}" -eq 1 ]; then
        echo -e "${GREEN}✓ Coverage gates passed${NC}"
        echo "  Line coverage: ${LINE_COV}% (threshold: 80%)"
        echo "  Branch coverage: ${BRANCH_COV}% (threshold: 70%)"
        echo "  Function coverage: ${FUNC_COV}% (threshold: 90%)"
    else
        echo -e "${RED}✗ Coverage gates failed${NC}"
        [ "${LINE_PASS}" -eq 0 ] && echo "  Line coverage: ${LINE_COV}% < 80% (FAILED)"
        [ "${BRANCH_PASS}" -eq 0 ] && echo "  Branch coverage: ${BRANCH_COV}% < 70% (FAILED)"
        [ "${FUNC_PASS}" -eq 0 ] && echo "  Function coverage: ${FUNC_COV}% < 90% (FAILED)"
        EXIT_CODE=1
    fi
else
    echo -e "${YELLOW}⚠ Coverage data not found${NC}"
    echo "  Run tests with coverage first: rebar3 ct --cover && rebar3 cover"
    if [ "${FAIL_ON_WARNINGS}" = "--fail-on-warnings" ]; then
        EXIT_CODE=1
    fi
fi
echo ""

# Summary
if [ ${EXIT_CODE} -eq 0 ]; then
    echo -e "${GREEN}=== All Quality Gates Passed ===${NC}"
else
    echo -e "${RED}=== Quality Gates Failed ===${NC}"
fi

exit ${EXIT_CODE}

