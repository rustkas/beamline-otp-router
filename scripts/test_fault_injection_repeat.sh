#!/bin/bash
# Repeat fault injection test suite multiple times to verify stability
#
# Usage:
#   ./scripts/test_fault_injection_repeat.sh
#   ./scripts/test_fault_injection_repeat.sh --runs 20
#   ./scripts/test_fault_injection_repeat.sh --runs 50 --verbose
#
# Exit codes:
#   0 - All runs passed
#   1 - At least one run failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
RUNS=20
VERBOSE=false
SUITE="router_jetstream_fault_injection_SUITE"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --runs)
            RUNS="$2"
            shift 2
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--runs N] [--verbose]"
            echo ""
            echo "Options:"
            echo "  --runs N     Number of runs (default: 20)"
            echo "  --verbose    Verbose output"
            echo "  --help       Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage"
            exit 1
            ;;
    esac
done

echo -e "${GREEN}=== Fault Injection Test Suite Repeat Runner ===${NC}"
echo ""
echo "Suite: ${SUITE}"
echo "Runs: ${RUNS}"
echo ""

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 not found. Please install rebar3 first.${NC}"
    exit 1
fi

# Compile test suite
echo -e "${GREEN}Compiling test suite...${NC}"
if ! rebar3 compile; then
    echo -e "${RED}Compilation failed${NC}"
    exit 1
fi

# Arrays to store results
declare -a RUN_TIMES
declare -a RUN_EXIT_CODES
TOTAL_START=$(date +%s)

PASSED_RUNS=0
FAILED_RUNS=0

# Run suite multiple times
for i in $(seq 1 ${RUNS}); do
    echo -e "${YELLOW}Run ${i}/${RUNS}...${NC}"
    
    RUN_START=$(date +%s)
    
    if [[ "${VERBOSE}" == "true" ]]; then
        rebar3 ct --dir test --suite test/${SUITE} --verbose
    else
        # Suppress output but keep errors visible
        rebar3 ct --dir test --suite test/${SUITE} > /dev/null 2>&1
    fi
    
    RUN_EXIT=$?
    RUN_END=$(date +%s)
    RUN_DURATION=$((RUN_END - RUN_START))
    
    # Store results
    RUN_TIMES+=(${RUN_DURATION})
    RUN_EXIT_CODES+=(${RUN_EXIT})
    
    # Format duration
    if [[ ${RUN_DURATION} -lt 60 ]]; then
        DURATION_STR="${RUN_DURATION}s"
    else
        MINUTES=$((RUN_DURATION / 60))
        SECONDS=$((RUN_DURATION % 60))
        DURATION_STR="${MINUTES}m ${SECONDS}s"
    fi
    
    if [[ ${RUN_EXIT} -eq 0 ]]; then
        echo -e "  ${GREEN}✓${NC} Run ${i}: ${DURATION_STR}"
        PASSED_RUNS=$((PASSED_RUNS + 1))
    else
        echo -e "  ${RED}✗${NC} Run ${i}: ${DURATION_STR} (FAILED)"
        FAILED_RUNS=$((FAILED_RUNS + 1))
        if [[ "${VERBOSE}" != "true" ]]; then
            echo -e "${YELLOW}  Re-run with --verbose to see details${NC}"
        fi
    fi
done

TOTAL_END=$(date +%s)
TOTAL_DURATION=$((TOTAL_END - TOTAL_START))

# Calculate statistics
TOTAL_TIME=0
for time in "${RUN_TIMES[@]}"; do
    TOTAL_TIME=$((TOTAL_TIME + time))
done
AVG_TIME=$((TOTAL_TIME / RUNS))

MIN_TIME=${RUN_TIMES[0]}
MAX_TIME=${RUN_TIMES[0]}
for time in "${RUN_TIMES[@]}"; do
    if [[ ${time} -lt ${MIN_TIME} ]]; then
        MIN_TIME=${time}
    fi
    if [[ ${time} -gt ${MAX_TIME} ]]; then
        MAX_TIME=${time}
    fi
done

# Format durations
format_duration() {
    local seconds=$1
    if [[ ${seconds} -lt 60 ]]; then
        echo "${seconds}s"
    else
        local minutes=$((seconds / 60))
        local secs=$((seconds % 60))
        echo "${minutes}m ${secs}s"
    fi
}

# Print summary
echo ""
echo -e "${GREEN}=== Repeat Test Summary ===${NC}"
echo ""
echo -e "${YELLOW}Results:${NC}"
echo "  Passed: ${PASSED_RUNS}/${RUNS}"
echo "  Failed: ${FAILED_RUNS}/${RUNS}"
echo ""
echo -e "${YELLOW}Timing Statistics:${NC}"
echo "  Total time: $(format_duration ${TOTAL_DURATION})"
echo "  Average: $(format_duration ${AVG_TIME})"
echo "  Min: $(format_duration ${MIN_TIME})"
echo "  Max: $(format_duration ${MAX_TIME})"
echo ""

# Overall result
if [[ ${FAILED_RUNS} -eq 0 ]]; then
    echo -e "${GREEN}✓ All ${RUNS} runs passed - suite is stable!${NC}"
    EXIT_CODE=0
else
    echo -e "${RED}✗ ${FAILED_RUNS} run(s) failed - suite may be flaky${NC}"
    EXIT_CODE=1
fi

exit ${EXIT_CODE}

