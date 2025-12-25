#!/usr/bin/env bash
# ci_full_quality_gates.sh - Quality Gates for Full Tier
#
# Implements quality gates for full tier CI:
#   0. quarantine_policy == ok   - Quarantine metadata is valid (HARD GATE)
#   1. suite_linter == ok        - All suites pass linter checks (HARD GATE)
#   2. failed_tests == 0         - No test failures allowed (HARD GATE)
#   3. unexpected_skips == 0     - No unexpected skips (HARD GATE)
#   4. targeted_coverage >= N%   - Coverage above threshold (SOFT GATE - warning only)
#
# Usage:
#   ./scripts/ci_full_quality_gates.sh                    # Run all gates
#   ./scripts/ci_full_quality_gates.sh --ct-log <path>    # Parse specific CT log
#   ./scripts/ci_full_quality_gates.sh --linter-only      # Run only suite linter
#
# Exit codes:
#   0 - All HARD quality gates passed (soft gates may warn)
#   1 - One or more HARD quality gates failed
#   2 - Script usage error
#
# Environment variables:
#   ROUTER_QG_STRICT              - Fail on any warning (default: false)
#   ROUTER_ALLOWED_SKIPS          - Comma-separated list of allowed skip patterns
#   ROUTER_SUITE_LINTER_STRICT    - Enable strict mode for suite linter
#   ROUTER_COVERAGE_THRESHOLD     - Coverage threshold % for soft gate (default: 12)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Default values
CT_LOG_DIR="_build/test/logs"
LINTER_ONLY=false
EXIT_CODE=0

# Allowed skip patterns (documented/expected skips)
# These are skips that are known and documented in TEST_STATUS.md
ALLOWED_SKIP_PATTERNS=(
    "meck not available"
    "nats not available"
    "docker not available"
    "skip_reason:documented"
    "NATS connection required"
    "JetStream not available"
    "integration test disabled"
    "heavy tier only"
    "disabled in CI"
)

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --ct-log)
            CT_LOG_DIR="$2"
            shift 2
            ;;
        --linter-only)
            LINTER_ONLY=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--ct-log <path>] [--linter-only] [--help]"
            echo ""
            echo "Quality Gates for Full Tier:"
            echo "  1. failed_tests == 0        - No test failures allowed"
            echo "  2. unexpected_skips == 0    - No unexpected skips"
            echo "  3. suite_linter == ok       - All suites pass linter"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 2
            ;;
    esac
done

echo -e "${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"
echo -e "${BOLD}${BLUE}       FULL TIER QUALITY GATES${NC}"
echo -e "${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"
echo ""

# ============================================================================
# GATE 0: Quarantine Policy Check
# ============================================================================
check_quarantine_policy() {
    echo -e "${BLUE}[Gate 0/4] Quarantine Policy Check${NC}"
    echo "────────────────────────────────────────────────────────────────"
    
    local POLICY_SCRIPT="$SCRIPT_DIR/check_quarantine_policy.sh"
    
    if [[ ! -f "$POLICY_SCRIPT" ]]; then
        echo -e "  ${YELLOW}⚠ Quarantine policy script not found: $POLICY_SCRIPT${NC}"
        echo "    Skipping quarantine policy check."
        return 0
    fi
    
    # Run quarantine policy check (uses QUARANTINE_STRICT from environment if set)
    local POLICY_OUTPUT
    if POLICY_OUTPUT=$(bash "$POLICY_SCRIPT" 2>&1); then
        echo -e "  ${GREEN}✓ Quarantine policy check passed${NC}"
        # Show summary line
        echo "$POLICY_OUTPUT" | grep -E "(Passed checks|Warnings|Errors|PASSED)" | tail -4 | sed 's/^/    /'
        return 0
    else
        echo -e "  ${RED}✗ Quarantine policy check FAILED${NC}"
        echo "$POLICY_OUTPUT" | grep -E "(ERROR|FAILED|Missing)" | head -10 | sed 's/^/    /'
        return 1
    fi
}

# ============================================================================
# GATE 1: Suite Linter
# ============================================================================
run_suite_linter() {
    echo ""
    echo -e "${BLUE}[Gate 1/4] Suite Linter Check${NC}"
    echo "────────────────────────────────────────────────────────────────"
    
    # Compile and run linter
    local LINTER_OUTPUT
    if LINTER_OUTPUT=$(erl -noshell -pa test -pa test_support \
        -eval 'case compile:file("test_support/router_suite_linter", [report, {outdir, "test_support"}]) of 
            {ok, _} -> ok; 
            Error -> io:format("Compile failed: ~p~n", [Error]), halt(1) 
        end, 
        case router_suite_linter:run() of 
            ok -> halt(0); 
            {error, Issues} -> 
                io:format("~p issues found~n", [length(Issues)]), 
                halt(1) 
        end.' 2>&1); then
        echo -e "  ${GREEN}✓ Suite linter passed${NC}"
        echo "  $LINTER_OUTPUT"
        return 0
    else
        echo -e "  ${RED}✗ Suite linter FAILED${NC}"
        echo "$LINTER_OUTPUT" | sed 's/^/    /'
        return 1
    fi
}

# ============================================================================
# GATE 2: Failed Tests Check
# ============================================================================
check_failed_tests() {
    echo ""
    echo -e "${BLUE}[Gate 2/4] Failed Tests Check${NC}"
    echo "────────────────────────────────────────────────────────────────"
    
    if [[ ! -d "$CT_LOG_DIR" ]]; then
        echo -e "  ${YELLOW}⚠ No CT log directory found at ${CT_LOG_DIR}${NC}"
        echo "    Run tests first: ./scripts/ct-full.sh"
        return 0  # Not a failure if no logs exist yet
    fi
    
    # Find the LATEST CT run directory (most recent by name)
    local LATEST_RUN
    LATEST_RUN=$(find "$CT_LOG_DIR" -maxdepth 1 -type d -name "ct_run.*" 2>/dev/null | sort -r | head -1)
    
    if [[ -z "$LATEST_RUN" ]]; then
        echo -e "  ${YELLOW}⚠ No CT run found in ${CT_LOG_DIR}${NC}"
        return 0
    fi
    
    echo "  Checking: $(basename "$LATEST_RUN")"
    
    local FAILED_COUNT=0
    local FAILED_TESTS=()
    
    # Method 1: Parse summary from LATEST CT run output only
    # Looking for patterns like "N failed" where N > 0
    local SUMMARY_OUTPUT
    SUMMARY_OUTPUT=$(grep -rh "failed" "$LATEST_RUN" 2>/dev/null | grep -E "[1-9][0-9]* failed" || true)
    
    if [[ -n "$SUMMARY_OUTPUT" ]]; then
        # Count actual failures from summary lines
        while IFS= read -r line; do
            # Extract failure count from lines like "3 ok, 2 failed of 5 test cases"
            local FAIL_NUM
            FAIL_NUM=$(echo "$line" | grep -oE "[1-9][0-9]* failed" | grep -oE "^[0-9]+" || true)
            if [[ -n "$FAIL_NUM" && "$FAIL_NUM" -gt 0 ]]; then
                FAILED_COUNT=$((FAILED_COUNT + FAIL_NUM))
                FAILED_TESTS+=("$line")
            fi
        done <<< "$SUMMARY_OUTPUT"
    fi
    
    # Method 2: Look for explicit FAILED markers in LATEST test output
    local EXPLICIT_FAILURES
    EXPLICIT_FAILURES=$(grep -rh "<<<FAILED>>>\|=== FAILED\|*** FAILED" "$LATEST_RUN" 2>/dev/null | head -20 || true)
    if [[ -n "$EXPLICIT_FAILURES" ]]; then
        while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                # Avoid duplicates
                local IS_DUP=false
                for existing in "${FAILED_TESTS[@]:-}"; do
                    if [[ "$existing" == "$line" ]]; then
                        IS_DUP=true
                        break
                    fi
                done
                if [[ "$IS_DUP" == false ]]; then
                    FAILED_TESTS+=("$line")
                    ((FAILED_COUNT++))
                fi
            fi
        done <<< "$EXPLICIT_FAILURES"
    fi
    
    if [[ "$FAILED_COUNT" -eq 0 ]]; then
        echo -e "  ${GREEN}✓ No test failures detected${NC}"
        return 0
    else
        echo -e "  ${RED}✗ Found $FAILED_COUNT test failure(s)${NC}"
        for fail in "${FAILED_TESTS[@]:0:10}"; do
            echo "    - $fail"
        done
        if [[ ${#FAILED_TESTS[@]} -gt 10 ]]; then
            echo "    ... and $((${#FAILED_TESTS[@]} - 10)) more"
        fi
        return 1
    fi
}

# ============================================================================
# GATE 3: Unexpected Skips Check
# ============================================================================
check_unexpected_skips() {
    echo ""
    echo -e "${BLUE}[Gate 3/4] Unexpected Skips Check${NC}"
    echo "────────────────────────────────────────────────────────────────"
    
    local SKIP_COUNT=0
    local UNEXPECTED_SKIPS=()
    local DOCUMENTED_SKIPS=()
    
    # Find the LATEST CT run directory
    local LATEST_RUN
    LATEST_RUN=$(find "$CT_LOG_DIR" -maxdepth 1 -type d -name "ct_run.*" 2>/dev/null | sort -r | head -1)
    
    # Find skip entries in LATEST CT run only
    if [[ -n "$LATEST_RUN" && -d "$LATEST_RUN" ]]; then
        while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                local IS_EXPECTED=false
                
                # Check if skip matches any allowed pattern
                for pattern in "${ALLOWED_SKIP_PATTERNS[@]}"; do
                    if [[ "$line" == *"$pattern"* ]]; then
                        IS_EXPECTED=true
                        DOCUMENTED_SKIPS+=("$line")
                        break
                    fi
                done
                
                # Check environment variable for additional patterns
                if [[ "${ROUTER_ALLOWED_SKIPS:-}" != "" ]]; then
                    IFS=',' read -ra EXTRA_PATTERNS <<< "$ROUTER_ALLOWED_SKIPS"
                    for pattern in "${EXTRA_PATTERNS[@]}"; do
                        if [[ "$line" == *"$pattern"* ]]; then
                            IS_EXPECTED=true
                            DOCUMENTED_SKIPS+=("$line")
                            break
                        fi
                    done
                fi
                
                if [[ "$IS_EXPECTED" == false ]]; then
                    UNEXPECTED_SKIPS+=("$line")
                    ((SKIP_COUNT++))
                fi
            fi
        done < <(grep -rh -E "(SKIPPED|skipped|{skip," "$LATEST_RUN" 2>/dev/null | head -100 || true)
    fi
    
    # Report documented skips
    if [[ ${#DOCUMENTED_SKIPS[@]} -gt 0 ]]; then
        echo -e "  ${YELLOW}ℹ ${#DOCUMENTED_SKIPS[@]} documented skip(s) (expected)${NC}"
    fi
    
    if [[ "$SKIP_COUNT" -eq 0 ]]; then
        echo -e "  ${GREEN}✓ No unexpected skips detected${NC}"
        return 0
    else
        echo -e "  ${RED}✗ Found $SKIP_COUNT unexpected skip(s)${NC}"
        for skip in "${UNEXPECTED_SKIPS[@]:0:10}"; do
            echo "    - $skip"
        done
        if [[ ${#UNEXPECTED_SKIPS[@]} -gt 10 ]]; then
            echo "    ... and $((${#UNEXPECTED_SKIPS[@]} - 10)) more"
        fi
        echo ""
        echo -e "  ${YELLOW}To allow a skip, either:${NC}"
        echo "    1. Add skip reason to ALLOWED_SKIP_PATTERNS in this script"
        echo "    2. Set ROUTER_ALLOWED_SKIPS='pattern1,pattern2' environment variable"
        echo "    3. Document the skip in docs/testing/TEST_STATUS.md"
        return 1
    fi
}

# ============================================================================
# GATE 4: Coverage Check (SOFT GATE - warning only)
# ============================================================================
COVERAGE_THRESHOLD="${ROUTER_COVERAGE_THRESHOLD:-12}"
COVERAGE_RESULT="unknown"
COVERAGE_PCT="0.00"

check_coverage() {
    echo ""
    echo -e "${BLUE}[Gate 4/4] Targeted Coverage Check (SOFT - warning only)${NC}"
    echo "────────────────────────────────────────────────────────────────"
    
    local COVER_INDEX="$PROJECT_DIR/_build/test/cover/index.html"
    
    # Check if coverage data exists
    if [[ ! -f "$COVER_INDEX" ]]; then
        echo -e "  ${YELLOW}⚠ No coverage data found${NC}"
        echo "    Run tests with coverage: rebar3 ct --cover"
        echo "    Or: make test-coverage"
        COVERAGE_RESULT="no_data"
        return 0  # Soft gate - never fails
    fi
    
    # Parse targeted coverage from existing report if available
    local TARGETED_REPORT="$PROJECT_DIR/reports/coverage_targeted.json"
    if [[ -f "$TARGETED_REPORT" ]]; then
        COVERAGE_PCT=$(grep -oP '"targeted":\s*\K[0-9.]+' "$TARGETED_REPORT" 2>/dev/null | head -1 || echo "0")
    else
        # Fallback: parse aggregate coverage from cover index
        local total_line
        total_line=$(grep -E "<strong>Total</strong>" "$COVER_INDEX" 2>/dev/null | head -1 || true)
        if [[ -n "$total_line" ]]; then
            COVERAGE_PCT=$(echo "$total_line" | grep -oE '[0-9]+%' | head -1 | tr -d '%' || echo "0")
        fi
    fi
    
    # Ensure we have a valid number
    COVERAGE_PCT=${COVERAGE_PCT:-0}
    if ! [[ "$COVERAGE_PCT" =~ ^[0-9.]+$ ]]; then
        COVERAGE_PCT="0"
    fi
    
    echo "  Threshold: >= ${COVERAGE_THRESHOLD}% (soft gate)"
    echo "  Current:   ${COVERAGE_PCT}%"
    
    # Compare with threshold using awk for float comparison
    local ABOVE_THRESHOLD
    ABOVE_THRESHOLD=$(awk -v cov="$COVERAGE_PCT" -v thresh="$COVERAGE_THRESHOLD" 'BEGIN {print (cov >= thresh) ? 1 : 0}')
    
    if [[ "$ABOVE_THRESHOLD" -eq 1 ]]; then
        echo -e "  ${GREEN}✓ Coverage meets threshold${NC}"
        COVERAGE_RESULT="pass"
    else
        echo -e "  ${YELLOW}⚠ Coverage below threshold (warning only)${NC}"
        echo ""
        echo -e "  ${YELLOW}To improve coverage:${NC}"
        echo "    1. Run: make test-coverage"
        echo "    2. Add tests for uncovered modules"
        echo "    3. Review: reports/coverage_targeted.json"
        COVERAGE_RESULT="warning"
    fi
    
    return 0  # Soft gate - never fails CI
}

# ==========================================================================
# Main Execution
# ============================================================================

GATE0_RESULT=0
GATE1_RESULT=0
GATE2_RESULT=0
GATE3_RESULT=0

# Run Gate 0: Quarantine Policy Check (always runs first)
if ! check_quarantine_policy; then
    GATE0_RESULT=1
    EXIT_CODE=1
fi

# Run Gate 1: Suite Linter (always runs)
if ! run_suite_linter; then
    GATE1_RESULT=1
    EXIT_CODE=1
fi

if [[ "$LINTER_ONLY" == false ]]; then
    # Run Gate 2: Failed Tests
    if ! check_failed_tests; then
        GATE2_RESULT=1
        EXIT_CODE=1
    fi
    
    # Run Gate 3: Unexpected Skips
    if ! check_unexpected_skips; then
        GATE3_RESULT=1
        EXIT_CODE=1
    fi
    
    # Run Gate 4: Coverage (soft gate - always passes, just warns)
    check_coverage
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
echo -e "${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"
echo -e "${BOLD}${BLUE}       QUALITY GATES SUMMARY${NC}"
echo -e "${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"
echo ""

gate_status() {
    if [[ $1 -eq 0 ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
    else
        echo -e "${RED}✗ FAIL${NC}"
    fi
}

soft_gate_status() {
    case "$1" in
        "pass")
            echo -e "${GREEN}✓ PASS${NC}"
            ;;
        "warning")
            echo -e "${YELLOW}⚠ WARN${NC}"
            ;;
        "no_data")
            echo -e "${YELLOW}⚠ N/A${NC}"
            ;;
        *)
            echo -e "${YELLOW}⚠ N/A${NC}"
            ;;
    esac
}

echo -e "  ${BOLD}Hard Gates (blocking):${NC}"
echo -e "  Gate 0: quarantine_policy == ok  $(gate_status $GATE0_RESULT)"
echo -e "  Gate 1: suite_linter == ok       $(gate_status $GATE1_RESULT)"
if [[ "$LINTER_ONLY" == false ]]; then
    echo -e "  Gate 2: failed_tests == 0        $(gate_status $GATE2_RESULT)"
    echo -e "  Gate 3: unexpected_skips == 0    $(gate_status $GATE3_RESULT)"
    echo ""
    echo -e "  ${BOLD}Soft Gates (non-blocking):${NC}"
    echo -e "  Gate 4: coverage >= ${COVERAGE_THRESHOLD}%         $(soft_gate_status $COVERAGE_RESULT)  (${COVERAGE_PCT}%)"
fi

echo ""
if [[ $EXIT_CODE -eq 0 ]]; then
    echo -e "${BOLD}${GREEN}══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${GREEN}       ✓ ALL HARD QUALITY GATES PASSED${NC}"
    echo -e "${BOLD}${GREEN}══════════════════════════════════════════════════════════════${NC}"
    if [[ "$COVERAGE_RESULT" == "warning" ]]; then
        echo ""
        echo -e "${YELLOW}Note: Soft gate warning - coverage below threshold.${NC}"
        echo -e "${YELLOW}This is informational only and does not fail CI.${NC}"
    fi
else
    echo -e "${BOLD}${RED}══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${RED}       ✗ QUALITY GATES FAILED${NC}"
    echo -e "${BOLD}${RED}══════════════════════════════════════════════════════════════${NC}"
    echo ""
    echo "CI Pipeline should fail on this result."
    echo "Fix the issues above before merging."
fi

exit $EXIT_CODE
