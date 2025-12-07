#!/bin/bash
# Runtime Validation Script
#
# Runs all test suites and validates runtime behavior.
# This script is used for runtime validation of Router component.
#
# Usage:
#   ./scripts/run_runtime_validation.sh [suite_name]
#
# Examples:
#   ./scripts/run_runtime_validation.sh                    # Run all suites
#   ./scripts/run_runtime_validation.sh router_grpc_SUITE  # Run specific suite

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SUITE="${1:-}"
LOG_DIR="${LOG_DIR:-$PROJECT_ROOT/ct_logs}"
RESULTS_DIR="${RESULTS_DIR:-$PROJECT_ROOT/test_results}"

# Create directories
mkdir -p "$LOG_DIR"
mkdir -p "$RESULTS_DIR"

echo "=========================================="
echo "Router Runtime Validation"
echo "=========================================="
echo "Project: $PROJECT_ROOT"
echo "Suite: ${SUITE:-all}"
echo "Log Dir: $LOG_DIR"
echo "Results Dir: $RESULTS_DIR"
echo ""

# Function to run test suite
run_suite() {
    local suite_name="$1"
    local suite_file="test/${suite_name}.erl"
    
    if [ ! -f "$suite_file" ]; then
        echo -e "${YELLOW}Warning: Suite file not found: $suite_file${NC}"
        return 1
    fi
    
    echo "Running suite: $suite_name"
    echo "----------------------------------------"
    
    if rebar3 ct --suite "test/$suite_name" --dir test 2>&1 | tee "$LOG_DIR/${suite_name}.log"; then
        echo -e "${GREEN}✓ Suite passed: $suite_name${NC}"
        return 0
    else
        echo -e "${RED}✗ Suite failed: $suite_name${NC}"
        return 1
    fi
}

# Function to run all suites
run_all_suites() {
    local failed_suites=()
    local passed_suites=()
    
    # Find all test suites
    local suites=$(find test -name "*_SUITE.erl" -type f | sed 's|test/||;s|\.erl||' | sort)
    
    for suite in $suites; do
        if run_suite "$suite"; then
            passed_suites+=("$suite")
        else
            failed_suites+=("$suite")
        fi
        echo ""
    done
    
    # Summary
    echo "=========================================="
    echo "Runtime Validation Summary"
    echo "=========================================="
    echo "Total suites: $(echo "$suites" | wc -l)"
    echo -e "${GREEN}Passed: ${#passed_suites[@]}${NC}"
    echo -e "${RED}Failed: ${#failed_suites[@]}${NC}"
    
    if [ ${#failed_suites[@]} -gt 0 ]; then
        echo ""
        echo "Failed suites:"
        for suite in "${failed_suites[@]}"; do
            echo -e "  ${RED}✗ $suite${NC}"
        done
        return 1
    else
        echo -e "${GREEN}All suites passed!${NC}"
        return 0
    fi
}

# Main execution
if [ -n "$SUITE" ]; then
    # Run specific suite
    if run_suite "$SUITE"; then
        exit 0
    else
        exit 1
    fi
else
    # Run all suites
    if run_all_suites; then
        exit 0
    else
        exit 1
    fi
fi

