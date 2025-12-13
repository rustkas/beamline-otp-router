#!/usr/bin/env bash
# generate_baseline.sh - Generate full tier baseline metrics JSON
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

REPORTS_DIR="$PROJECT_DIR/reports"
BASELINE_FILE="$REPORTS_DIR/full_baseline.json"
mkdir -p "$REPORTS_DIR"

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

generate_baseline() {
    local duration=$1
    local suites_count=$2
    local tests_passed=$3
    local tests_failed=$4
    local tests_skipped=$5
    local exit_status=$6
    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local tests_total=$((tests_passed + tests_failed + tests_skipped))
    
    cat > "$BASELINE_FILE" <<EOF
{
  "generated_at": "$timestamp",
  "test_level": "full",
  "duration_seconds": $duration,
  "suites_count": $suites_count,
  "tests": {
    "total": $tests_total,
    "passed": $tests_passed,
    "failed": $tests_failed,
    "skipped": $tests_skipped
  },
  "exit_status": $exit_status,
  "baseline_version": "1.0.0"
}
EOF
    echo -e "${GREEN}Baseline saved: $BASELINE_FILE${NC}"
}

print_summary() {
    local duration=$1
    local suites_count=$2
    local tests_passed=$3
    local tests_failed=$4
    local tests_skipped=$5
    local exit_status=$6
    local tests_total=$((tests_passed + tests_failed + tests_skipped))
    
    echo ""
    echo -e "${CYAN}+----------------------------------------------------+${NC}"
    echo -e "${CYAN}|          FULL TIER BASELINE SUMMARY                |${NC}"
    echo -e "${CYAN}+----------------------------------------------------+${NC}"
    printf "${CYAN}|${NC}  Duration:       %6ds                          ${CYAN}|${NC}\n" "$duration"
    printf "${CYAN}|${NC}  Suites:         %6d                           ${CYAN}|${NC}\n" "$suites_count"
    printf "${CYAN}|${NC}  Tests Total:    %6d                           ${CYAN}|${NC}\n" "$tests_total"
    printf "${CYAN}|${NC}  Passed:         ${GREEN}%6d${NC}                           ${CYAN}|${NC}\n" "$tests_passed"
    if [[ $tests_failed -gt 0 ]]; then
        printf "${CYAN}|${NC}  Failed:         ${RED}%6d${NC}                           ${CYAN}|${NC}\n" "$tests_failed"
    else
        printf "${CYAN}|${NC}  Failed:         %6d                           ${CYAN}|${NC}\n" "$tests_failed"
    fi
    if [[ $tests_skipped -gt 0 ]]; then
        printf "${CYAN}|${NC}  Skipped:        ${YELLOW}%6d${NC}                           ${CYAN}|${NC}\n" "$tests_skipped"
    else
        printf "${CYAN}|${NC}  Skipped:        %6d                           ${CYAN}|${NC}\n" "$tests_skipped"
    fi
    echo -e "${CYAN}+----------------------------------------------------+${NC}"
    if [[ $exit_status -eq 0 ]]; then
        echo -e "${CYAN}|${NC}  Status:         ${GREEN}PASSED${NC}                           ${CYAN}|${NC}"
    else
        echo -e "${CYAN}|${NC}  Status:         ${RED}FAILED${NC}                           ${CYAN}|${NC}"
    fi
    echo -e "${CYAN}+----------------------------------------------------+${NC}"
}

parse_ct_logs() {
    local ct_log_dir="$PROJECT_DIR/_build/test/logs"
    PARSED_PASSED=0
    PARSED_FAILED=0
    PARSED_SKIPPED=0
    
    if [[ -d "$ct_log_dir" ]]; then
        local latest_run
        latest_run=$(ls -td "$ct_log_dir"/ct_run.* 2>/dev/null | head -1)
        if [[ -n "$latest_run" ]]; then
            # Parse summary lines like "=== TEST COMPLETE, 13 ok, 0 failed of 13 test cases"
            # or "=== TEST COMPLETE, 4 ok, 1 failed, 2 skipped of 7 test cases"
            while IFS= read -r line; do
                # Extract "X ok"
                local ok_count
                ok_count=$(echo "$line" | grep -oE '[0-9]+ ok' | grep -oE '[0-9]+' || echo "0")
                PARSED_PASSED=$((PARSED_PASSED + ok_count))
                
                # Extract "X failed"
                local failed_count
                failed_count=$(echo "$line" | grep -oE '[0-9]+ failed' | grep -oE '[0-9]+' || echo "0")
                PARSED_FAILED=$((PARSED_FAILED + failed_count))
                
                # Extract "X skipped"
                local skipped_count
                skipped_count=$(echo "$line" | grep -oE '[0-9]+ skipped' | grep -oE '[0-9]+' || echo "0")
                PARSED_SKIPPED=$((PARSED_SKIPPED + skipped_count))
            done < <(grep -rh "TEST COMPLETE" "$latest_run" 2>/dev/null | grep -E "[0-9]+ ok" || true)
        fi
    fi
}

if [[ "${1:-}" == "--parse-ct" ]]; then
    DURATION="${2:-0}"
    SUITES_COUNT="${3:-0}"
    EXIT_STATUS="${4:-0}"
    parse_ct_logs
    generate_baseline "$DURATION" "$SUITES_COUNT" "$PARSED_PASSED" "$PARSED_FAILED" "$PARSED_SKIPPED" "$EXIT_STATUS"
    print_summary "$DURATION" "$SUITES_COUNT" "$PARSED_PASSED" "$PARSED_FAILED" "$PARSED_SKIPPED" "$EXIT_STATUS"
elif [[ $# -eq 6 ]]; then
    generate_baseline "$1" "$2" "$3" "$4" "$5" "$6"
    print_summary "$1" "$2" "$3" "$4" "$5" "$6"
else
    echo "Usage: $0 <duration> <suites_count> <passed> <failed> <skipped> <exit_status>"
    echo "   or: $0 --parse-ct <duration> <suites_count> <exit_status>"
    exit 1
fi
