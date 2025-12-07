#!/bin/bash
# Slow Test Runner (Load/JetStream E2E/Property Tests)
# Runs slow test suites only (load, JetStream E2E, property-based tests)
#
# Usage:
#   ./scripts/test_slow.sh
#   ./scripts/test_slow.sh --verbose
#
# Exit codes:
#   0 - All slow tests passed
#   1 - Test execution failed
#   2 - Compilation failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse arguments
VERBOSE=false
if [[ "${1:-}" == "--verbose" ]] || [[ "${1:-}" == "-v" ]]; then
    VERBOSE=true
fi

echo -e "${GREEN}=== Slow Test Suite (Load/JetStream E2E/Property) ===${NC}"
echo ""
echo "Running slow test suites (load, JetStream E2E, property-based tests)"
echo ""

# Slow test suites (load/JetStream E2E/property)
SLOW_TEST_SUITES=(
    # JetStream E2E
    "router_jetstream_e2e_SUITE"
    "router_delivery_count_tracking_SUITE"
    "router_result_consumer_SUITE"
    "router_decide_consumer_SUITE"
    "router_jetstream_fault_injection_SUITE"
    "router_caf_adapter_SUITE"
    "router_caf_adapter_enhanced_SUITE"
    "router_nats_subscriber_caf_SUITE"
    # Property-based
    "router_decider_prop_SUITE"
    "router_policy_store_prop_SUITE"
    "router_normalize_boolean_prop_SUITE"
    "router_options_merge_prop_SUITE"
    # Load
    "router_policy_store_load_SUITE"
    # CP2+ Features
    "router_idempotency_SUITE"
    "router_tenant_allowlist_SUITE"
    # Advanced Integration
    "router_policy_store_fault_tolerance_SUITE"
    "router_admin_grpc_integration_SUITE"
    "router_admin_grpc_concurrency_SUITE"
    "router_assignment_SUITE"
    "router_sticky_store_SUITE"
    "router_policy_SUITE"
    "router_policy_validator_SUITE"
    "router_ets_guard_SUITE"
    "router_error_status_SUITE"
)

echo -e "${YELLOW}Slow Test Suites (${#SLOW_TEST_SUITES[@]} suites):${NC}"
for suite in "${SLOW_TEST_SUITES[@]}"; do
    echo "  ✓ ${suite}"
done
echo ""

echo -e "${YELLOW}Note: These tests may take > 5 minutes to complete${NC}"
echo ""

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 not found. Please install rebar3 first.${NC}"
    exit 1
fi

# Compile test suites
echo -e "${GREEN}Compiling test suites...${NC}"
if ! rebar3 compile; then
    echo -e "${RED}Compilation failed${NC}"
    exit 2
fi

# Run each test suite individually with timing
echo -e "${GREEN}Running slow tests (with timing)...${NC}"
echo ""

# Arrays to store timing data
declare -a SUITE_TIMES
declare -a SUITE_NAMES
declare -a SUITE_EXIT_CODES
TOTAL_START=$(date +%s)

FAILED_SUITES=0
PASSED_SUITES=0

# Run each suite individually
for suite in "${SLOW_TEST_SUITES[@]}"; do
    echo -e "${YELLOW}Running ${suite}...${NC}"
    
    SUITE_START=$(date +%s)
    
    if [[ "${VERBOSE}" == "true" ]]; then
        rebar3 ct --dir test --suite test/${suite} --verbose
    else
        # Suppress output but keep errors visible
        rebar3 ct --dir test --suite test/${suite} > /dev/null 2>&1
    fi
    
    SUITE_EXIT=$?
    SUITE_END=$(date +%s)
    SUITE_DURATION=$((SUITE_END - SUITE_START))
    
    # Store timing data
    SUITE_NAMES+=("${suite}")
    SUITE_TIMES+=(${SUITE_DURATION})
    SUITE_EXIT_CODES+=(${SUITE_EXIT})
    
    # Format duration
    if [[ ${SUITE_DURATION} -lt 60 ]]; then
        DURATION_STR="${SUITE_DURATION}s"
    else
        MINUTES=$((SUITE_DURATION / 60))
        SECONDS=$((SUITE_DURATION % 60))
        DURATION_STR="${MINUTES}m ${SECONDS}s"
    fi
    
    if [[ ${SUITE_EXIT} -eq 0 ]]; then
        echo -e "  ${GREEN}✓${NC} ${suite}: ${DURATION_STR}"
        PASSED_SUITES=$((PASSED_SUITES + 1))
    else
        echo -e "  ${RED}✗${NC} ${suite}: ${DURATION_STR} (FAILED)"
        FAILED_SUITES=$((FAILED_SUITES + 1))
    fi
done

TOTAL_END=$(date +%s)
TOTAL_DURATION=$((TOTAL_END - TOTAL_START))

# Format total duration
if [[ ${TOTAL_DURATION} -lt 60 ]]; then
    TOTAL_DURATION_STR="${TOTAL_DURATION}s"
elif [[ ${TOTAL_DURATION} -lt 3600 ]]; then
    MINUTES=$((TOTAL_DURATION / 60))
    SECONDS=$((TOTAL_DURATION % 60))
    TOTAL_DURATION_STR="${MINUTES}m ${SECONDS}s"
else
    HOURS=$((TOTAL_DURATION / 3600))
    REMAINING=$((TOTAL_DURATION % 3600))
    MINUTES=$((REMAINING / 60))
    SECONDS=$((REMAINING % 60))
    TOTAL_DURATION_STR="${HOURS}h ${MINUTES}m ${SECONDS}s"
fi

# Print summary
echo ""
echo -e "${GREEN}=== Slow Test Execution Summary ===${NC}"
echo ""

# Individual suite timings
echo -e "${YELLOW}Suite Execution Times:${NC}"
for i in "${!SUITE_NAMES[@]}"; do
    SUITE_NAME="${SUITE_NAMES[$i]}"
    SUITE_TIME="${SUITE_TIMES[$i]}"
    SUITE_EXIT="${SUITE_EXIT_CODES[$i]}"
    
    # Format duration
    if [[ ${SUITE_TIME} -lt 60 ]]; then
        TIME_STR="${SUITE_TIME}s"
    else
        MINUTES=$((SUITE_TIME / 60))
        SECONDS=$((SUITE_TIME % 60))
        TIME_STR="${MINUTES}m ${SECONDS}s"
    fi
    
    STATUS=""
    if [[ ${SUITE_EXIT} -eq 0 ]]; then
        STATUS="${GREEN}✓${NC}"
    else
        STATUS="${RED}✗${NC}"
    fi
    
    printf "  %s %-50s %s\n" "${STATUS}" "${SUITE_NAME}" "${TIME_STR}"
done

echo ""

# Total duration
echo -e "${YELLOW}Total Duration:${NC} ${TOTAL_DURATION_STR}"
echo ""

# Top 5 slowest suites
echo -e "${YELLOW}Top 5 Slowest Suites:${NC}"
# Create array of indices sorted by time (descending)
declare -a SORTED_INDICES
for i in "${!SUITE_TIMES[@]}"; do
    SORTED_INDICES+=($i)
done

# Simple bubble sort by time (descending)
for ((i=0; i<${#SORTED_INDICES[@]}-1; i++)); do
    for ((j=0; j<${#SORTED_INDICES[@]}-i-1; j++)); do
        idx1=${SORTED_INDICES[$j]}
        idx2=${SORTED_INDICES[$((j+1))]}
        if [[ ${SUITE_TIMES[$idx1]} -lt ${SUITE_TIMES[$idx2]} ]]; then
            # Swap
            temp=${SORTED_INDICES[$j]}
            SORTED_INDICES[$j]=${SORTED_INDICES[$((j+1))]}
            SORTED_INDICES[$((j+1))]=$temp
        fi
    done
done

# Print top 5
TOP_N=5
if [[ ${#SORTED_INDICES[@]} -lt ${TOP_N} ]]; then
    TOP_N=${#SORTED_INDICES[@]}
fi

for ((i=0; i<TOP_N; i++)); do
    idx=${SORTED_INDICES[$i]}
    SUITE_NAME="${SUITE_NAMES[$idx]}"
    SUITE_TIME="${SUITE_TIMES[$idx]}"
    
    # Format duration
    if [[ ${SUITE_TIME} -lt 60 ]]; then
        TIME_STR="${SUITE_TIME}s"
    else
        MINUTES=$((SUITE_TIME / 60))
        SECONDS=$((SUITE_TIME % 60))
        TIME_STR="${MINUTES}m ${SECONDS}s"
    fi
    
    printf "  %d. %-50s %s\n" $((i+1)) "${SUITE_NAME}" "${TIME_STR}"
done

echo ""

# Overall result
if [[ ${FAILED_SUITES} -eq 0 ]]; then
    echo -e "${GREEN}✓ All slow tests passed (${PASSED_SUITES} suites, ${TOTAL_DURATION_STR})${NC}"
    EXIT_CODE=0
else
    echo -e "${RED}✗ Some slow tests failed (${FAILED_SUITES} failed, ${PASSED_SUITES} passed, ${TOTAL_DURATION_STR})${NC}"
    EXIT_CODE=1
fi

exit ${EXIT_CODE}

