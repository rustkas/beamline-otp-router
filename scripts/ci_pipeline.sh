#!/bin/bash
# CI Pipeline Script
# Runs complete CI pipeline: compile, test, quality gates, reporting
#
# Usage: ./scripts/ci_pipeline.sh [--skip-quality-gates] [--skip-coverage]
# Exit codes:
#   0 - All CI checks passed
#   1 - One or more CI checks failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

SKIP_QUALITY_GATES=false
SKIP_COVERAGE=false

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --skip-quality-gates)
            SKIP_QUALITY_GATES=true
            shift
            ;;
        --skip-coverage)
            SKIP_COVERAGE=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=== CI Pipeline ==="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

EXIT_CODE=0

# 1. Compile
echo -e "${GREEN}[1/5] Compiling...${NC}"
if rebar3 compile; then
    echo -e "${GREEN}✓ Compilation successful${NC}"
else
    echo -e "${RED}✗ Compilation failed${NC}"
    exit 1
fi
echo ""

# 2. Get dependencies
echo -e "${GREEN}[2/5] Getting dependencies...${NC}"
if rebar3 deps; then
    echo -e "${GREEN}✓ Dependencies resolved${NC}"
else
    echo -e "${RED}✗ Dependency resolution failed${NC}"
    exit 1
fi
echo ""

# 3. Run tests
echo -e "${GREEN}[3/5] Running tests...${NC}"
if [ "${SKIP_COVERAGE}" = "true" ]; then
    if rebar3 ct; then
        echo -e "${GREEN}✓ Tests passed${NC}"
    else
        echo -e "${RED}✗ Tests failed${NC}"
        EXIT_CODE=1
    fi
else
    if rebar3 ct --cover; then
        echo -e "${GREEN}✓ Tests passed${NC}"
        echo "Generating coverage report..."
        rebar3 cover || echo "Warning: Coverage report generation had issues"
    else
        echo -e "${RED}✗ Tests failed${NC}"
        EXIT_CODE=1
    fi
fi
echo ""

# 4. Generate test reports
echo -e "${GREEN}[4/5] Generating test reports...${NC}"
mkdir -p test_results
bash scripts/ci_test_report.sh test_results || echo "Warning: Test report generation had issues"
if [ "${SKIP_COVERAGE}" != "true" ]; then
    bash scripts/ci_coverage_report.sh test_results || echo "Warning: Coverage report generation had issues"
fi
echo -e "${GREEN}✓ Test reports generated${NC}"
echo ""

# 5. Quality gates
if [ "${SKIP_QUALITY_GATES}" != "true" ]; then
    echo -e "${GREEN}[5/5] Running quality gates...${NC}"
    if bash scripts/ci_quality_gates.sh; then
        echo -e "${GREEN}✓ Quality gates passed${NC}"
    else
        echo -e "${RED}✗ Quality gates failed${NC}"
        EXIT_CODE=1
    fi
else
    echo -e "${YELLOW}[5/5] Skipping quality gates${NC}"
fi
echo ""

# Summary
if [ ${EXIT_CODE} -eq 0 ]; then
    echo -e "${GREEN}=== CI Pipeline Passed ===${NC}"
    echo "  Test results: test_results/junit.xml"
    if [ "${SKIP_COVERAGE}" != "true" ]; then
        echo "  Coverage report: test_results/coverage_report.json"
    fi
else
    echo -e "${RED}=== CI Pipeline Failed ===${NC}"
fi

exit ${EXIT_CODE}

