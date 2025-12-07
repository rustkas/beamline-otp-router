#!/bin/bash
# Test script for list_tests_by_tag.sh
# Verifies that the autodiscovery tool works correctly
#
# Usage:
#   ./scripts/test_list_tests_by_tag.sh
#
# Exit codes:
#   0 - All tests passed
#   1 - One or more tests failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${ROUTER_DIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0

test_check() {
    local TEST_NAME="$1"
    local EXPECTED_COUNT="$2"
    local TAG_EXPR="$3"
    
    echo -n "Testing ${TEST_NAME}... "
    
    COUNT=$(./scripts/list_tests_by_tag.sh "${TAG_EXPR}" 2>/dev/null | wc -l)
    
    if [ "${COUNT}" -eq "${EXPECTED_COUNT}" ]; then
        echo -e "${GREEN}PASS${NC} (found ${COUNT} suites)"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC} (expected ${EXPECTED_COUNT}, found ${COUNT})"
        FAILED=$((FAILED + 1))
    fi
}

test_check_exists() {
    local TEST_NAME="$1"
    local TAG="$2"
    local SUITE="$3"
    
    echo -n "Testing ${TEST_NAME} (${SUITE} in ${TAG})... "
    
    if ./scripts/list_tests_by_tag.sh "${TAG}" 2>/dev/null | grep -q "^${SUITE}$"; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC} (${SUITE} not found in ${TAG} tag)"
        FAILED=$((FAILED + 1))
    fi
}

test_check_invalid() {
    local TEST_NAME="$1"
    local TAG_EXPR="$2"
    
    echo -n "Testing ${TEST_NAME} (invalid tag: ${TAG_EXPR})... "
    
    # Check exit code (should be 1 for invalid arguments)
    if ./scripts/list_tests_by_tag.sh "${TAG_EXPR}" 2>&1 >/dev/null; then
        echo -e "${RED}FAIL${NC} (should reject invalid tag, but exit code was 0)"
        FAILED=$((FAILED + 1))
    else
        EXIT_CODE=$?
        if [ ${EXIT_CODE} -eq 1 ]; then
            echo -e "${GREEN}PASS${NC} (correctly rejected with exit code 1)"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}FAIL${NC} (should exit with code 1, got ${EXIT_CODE})"
            FAILED=$((FAILED + 1))
        fi
    fi
}

echo "Running autodiscovery tool tests..."
echo ""

# Test 1: Fast test suites count (should be 13)
test_check "Fast test suites count" 13 "fast"

# Test 2: CP1 smoke test suites count (should be 7)
test_check "CP1 smoke test suites count" 7 "cp1_smoke"

# Test 3: Fast but not CP1 smoke (should be 6: 13 fast - 7 cp1_smoke = 6)
test_check "Fast but not CP1 smoke" 6 "fast,!cp1_smoke"

# Test 4: Verify specific suites are in fast tag
test_check_exists "router_core_SUITE in fast" "fast" "router_core_SUITE"
test_check_exists "router_grpc_SUITE in fast" "fast" "router_grpc_SUITE"

# Test 5: Verify specific suites are in cp1_smoke tag
test_check_exists "router_core_SUITE in cp1_smoke" "cp1_smoke" "router_core_SUITE"
test_check_exists "router_e2e_smoke_SUITE in cp1_smoke" "cp1_smoke" "router_e2e_smoke_SUITE"

# Test 6: Verify router_grpc_SUITE is in fast but not cp1_smoke
test_check_exists "router_grpc_SUITE in fast,!cp1_smoke" "fast,!cp1_smoke" "router_grpc_SUITE"

# Test 7: Verify router_core_SUITE is NOT in fast,!cp1_smoke (it has cp1_smoke tag)
echo -n "Testing router_core_SUITE NOT in fast,!cp1_smoke... "
if ./scripts/list_tests_by_tag.sh "fast,!cp1_smoke" 2>/dev/null | grep -q "^router_core_SUITE$"; then
    echo -e "${RED}FAIL${NC} (router_core_SUITE should not be in fast,!cp1_smoke)"
    FAILED=$((FAILED + 1))
else
    echo -e "${GREEN}PASS${NC}"
    PASSED=$((PASSED + 1))
fi

# Test 8: Invalid tag format
test_check_invalid "Invalid tag (uppercase)" "FAST"
test_check_invalid "Invalid tag (spaces)" "fast test"

# Test 9: Invalid tag expression (only exclude, no include)
echo -n "Testing invalid expression (!cp1_smoke only, no include)... "
if ./scripts/list_tests_by_tag.sh "!cp1_smoke" 2>&1 | grep -q "At least one include tag is required"; then
    echo -e "${GREEN}PASS${NC} (correctly rejected)"
    PASSED=$((PASSED + 1))
else
    # Check exit code (should be 1)
    if ./scripts/list_tests_by_tag.sh "!cp1_smoke" 2>&1 >/dev/null; then
        echo -e "${RED}FAIL${NC} (should reject expression with only exclude tags, but exit code was 0)"
        FAILED=$((FAILED + 1))
    else
        EXIT_CODE=$?
        if [ ${EXIT_CODE} -eq 1 ]; then
            echo -e "${GREEN}PASS${NC} (correctly rejected with exit code 1)"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}FAIL${NC} (should exit with code 1, got ${EXIT_CODE})"
            FAILED=$((FAILED + 1))
        fi
    fi
fi

# Test 10: Non-existent tag (should return exit code 2)
echo -n "Testing non-existent tag (nonexistent_tag)... "
set +e  # Temporarily disable exit on error to capture exit code
OUTPUT=$(./scripts/list_tests_by_tag.sh "nonexistent_tag" 2>&1)
EXIT_CODE=$?
set -e  # Re-enable exit on error
if [ ${EXIT_CODE} -eq 2 ] && echo "${OUTPUT}" | grep -q "No test suites found"; then
    echo -e "${GREEN}PASS${NC} (correctly reported no suites with exit code 2)"
    PASSED=$((PASSED + 1))
else
    echo -e "${RED}FAIL${NC} (should report no suites found with exit code 2, got exit code ${EXIT_CODE})"
    echo "  Output: ${OUTPUT}"
    FAILED=$((FAILED + 1))
fi

echo ""
echo "Test Results:"
echo -e "${GREEN}Passed: ${PASSED}${NC}"
if [ ${FAILED} -gt 0 ]; then
    echo -e "${RED}Failed: ${FAILED}${NC}"
    exit 1
else
    echo -e "${GREEN}Failed: ${FAILED}${NC}"
    exit 0
fi

