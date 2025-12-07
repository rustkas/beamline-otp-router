#!/bin/bash
# Monitor fault injection test stability in CI
# Analyzes CI logs and generates stability report

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
SUITE_NAME="router_jetstream_fault_injection_SUITE"
REPORT_DIR="${ROUTER_DIR}/reports/fault-injection-ci"
LOG_DIR="${ROUTER_DIR}/_build/test/logs"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Create report directory
mkdir -p "${REPORT_DIR}"

echo -e "${GREEN}=== Fault Injection CI Monitoring ===${NC}"
echo "Suite: ${SUITE_NAME}"
echo "Report directory: ${REPORT_DIR}"
echo ""

# Function to parse CT logs
parse_ct_logs() {
    local log_file="$1"
    local output_file="${REPORT_DIR}/parsed_results.json"
    
    if [ ! -f "${log_file}" ]; then
        echo -e "${YELLOW}Warning: Log file not found: ${log_file}${NC}"
        return 1
    fi
    
    # Extract test results from HTML log
    local passed=$(grep -o "passed.*[0-9]" "${log_file}" 2>/dev/null | grep -o "[0-9]*" | head -1 || echo "0")
    local failed=$(grep -o "failed.*[0-9]" "${log_file}" 2>/dev/null | grep -o "[0-9]*" | head -1 || echo "0")
    local skipped=$(grep -o "skipped.*[0-9]" "${log_file}" 2>/dev/null | grep -o "[0-9]*" | head -1 || echo "0")
    
    # Extract execution time
    local duration=$(grep -o "duration.*[0-9]*\.[0-9]*" "${log_file}" 2>/dev/null | grep -o "[0-9]*\.[0-9]*" | head -1 || echo "0")
    
    # Create JSON report
    cat > "${output_file}" <<EOF
{
  "suite": "${SUITE_NAME}",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "results": {
    "passed": ${passed},
    "failed": ${failed},
    "skipped": ${skipped},
    "total": $((passed + failed + skipped))
  },
  "duration_seconds": ${duration},
  "log_file": "${log_file}"
}
EOF
    
    echo "${output_file}"
}

# Function to analyze multiple runs
analyze_stability() {
    local runs_dir="${REPORT_DIR}/runs"
    mkdir -p "${runs_dir}"
    
    # Find all CT log directories
    local log_dirs=($(find "${LOG_DIR}" -type d -name "*${SUITE_NAME}*" 2>/dev/null | head -10))
    
    if [ ${#log_dirs[@]} -eq 0 ]; then
        echo -e "${YELLOW}No log directories found for ${SUITE_NAME}${NC}"
        echo "Looking in: ${LOG_DIR}"
        return 1
    fi
    
    echo -e "${GREEN}Found ${#log_dirs[@]} log directories${NC}"
    
    local total_passed=0
    local total_failed=0
    local total_runs=0
    local durations=()
    
    for log_dir in "${log_dirs[@]}"; do
        local suite_log=$(find "${log_dir}" -name "suite.log.html" 2>/dev/null | head -1)
        
        if [ -n "${suite_log}" ]; then
            local parsed=$(parse_ct_logs "${suite_log}")
            if [ -f "${parsed}" ]; then
                local passed=$(jq -r '.results.passed' "${parsed}" 2>/dev/null || echo "0")
                local failed=$(jq -r '.results.failed' "${parsed}" 2>/dev/null || echo "0")
                local duration=$(jq -r '.duration_seconds' "${parsed}" 2>/dev/null || echo "0")
                
                total_passed=$((total_passed + passed))
                total_failed=$((total_failed + failed))
                total_runs=$((total_runs + 1))
                durations+=("${duration}")
                
                echo "  Run ${total_runs}: ${passed} passed, ${failed} failed, ${duration}s"
            fi
        fi
    done
    
    # Calculate statistics
    local pass_rate=0
    if [ ${total_runs} -gt 0 ]; then
        local total_tests=$((total_passed + total_failed))
        if [ ${total_tests} -gt 0 ]; then
            pass_rate=$(echo "scale=2; ${total_passed} * 100 / ${total_tests}" | bc)
        fi
    fi
    
    # Calculate average duration
    local avg_duration=0
    if [ ${#durations[@]} -gt 0 ]; then
        local sum=0
        for d in "${durations[@]}"; do
            sum=$(echo "${sum} + ${d}" | bc)
        done
        avg_duration=$(echo "scale=2; ${sum} / ${#durations[@]}" | bc)
    fi
    
    # Generate summary report
    cat > "${REPORT_DIR}/stability_summary.json" <<EOF
{
  "suite": "${SUITE_NAME}",
  "analysis_date": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "total_runs": ${total_runs},
  "total_passed": ${total_passed},
  "total_failed": ${total_failed},
  "pass_rate_percent": ${pass_rate},
  "average_duration_seconds": ${avg_duration},
  "status": "$([ ${total_failed} -eq 0 ] && echo "stable" || echo "unstable")"
}
EOF
    
    echo ""
    echo -e "${GREEN}=== Stability Summary ===${NC}"
    echo "Total runs analyzed: ${total_runs}"
    echo "Total passed: ${total_passed}"
    echo "Total failed: ${total_failed}"
    echo "Pass rate: ${pass_rate}%"
    echo "Average duration: ${avg_duration}s"
    
    if [ ${total_failed} -eq 0 ]; then
        echo -e "${GREEN}Status: STABLE${NC}"
    else
        echo -e "${RED}Status: UNSTABLE${NC}"
        echo -e "${YELLOW}Action required: Review failed tests${NC}"
    fi
}

# Function to check CI job duration
check_ci_duration() {
    local threshold_minutes=15
    local avg_duration=$(jq -r '.average_duration_seconds' "${REPORT_DIR}/stability_summary.json" 2>/dev/null || echo "0")
    local avg_minutes=$(echo "scale=2; ${avg_duration} / 60" | bc)
    
    echo ""
    echo -e "${GREEN}=== CI Duration Check ===${NC}"
    echo "Average duration: ${avg_minutes} minutes"
    echo "Threshold: ${threshold_minutes} minutes"
    
    if (( $(echo "${avg_minutes} > ${threshold_minutes}" | bc -l) )); then
        echo -e "${YELLOW}Warning: Tests exceed ${threshold_minutes} minute threshold${NC}"
        echo "Consider creating separate CI job (see docs/dev/FAULT_INJECTION_CI_JOB_GUIDE.md)"
        return 1
    else
        echo -e "${GREEN}Duration within acceptable range${NC}"
        return 0
    fi
}

# Main execution
main() {
    echo "Analyzing CI logs..."
    analyze_stability
    
    if [ -f "${REPORT_DIR}/stability_summary.json" ]; then
        check_ci_duration
    fi
    
    echo ""
    echo -e "${GREEN}Report generated: ${REPORT_DIR}/stability_summary.json${NC}"
}

# Check dependencies
if ! command -v jq &> /dev/null; then
    echo -e "${YELLOW}Warning: jq not found. Install with: sudo apt-get install jq${NC}"
    echo "Continuing without JSON parsing..."
fi

if ! command -v bc &> /dev/null; then
    echo -e "${YELLOW}Warning: bc not found. Install with: sudo apt-get install bc${NC}"
    echo "Continuing without duration calculations..."
fi

main "$@"

