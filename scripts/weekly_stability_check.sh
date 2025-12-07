#!/bin/bash
# Weekly stability check for fault injection tests
# Run every Monday to verify test stability

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPORT_DIR="${ROUTER_DIR}/reports/fault-injection-weekly"
DATE=$(date +"%Y-%m-%d")

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

mkdir -p "${REPORT_DIR}"

echo -e "${GREEN}=== Weekly Fault Injection Stability Check ===${NC}"
echo "Date: ${DATE}"
echo ""

# Run monitoring script
echo "Running CI monitoring analysis..."
"${SCRIPT_DIR}/monitor_fault_injection_ci.sh" || {
    echo -e "${YELLOW}Warning: Monitoring script had issues, continuing...${NC}"
}

# Generate weekly report
if [ -f "${ROUTER_DIR}/reports/fault-injection-ci/stability_summary.json" ]; then
    local summary="${ROUTER_DIR}/reports/fault-injection-ci/stability_summary.json"
    local weekly_report="${REPORT_DIR}/weekly_report_${DATE}.json"
    
    # Copy and enhance summary
    jq --arg date "${DATE}" '. + {
        report_type: "weekly",
        report_date: $date,
        week_number: (now | strftime("%V") | tonumber)
    }' "${summary}" > "${weekly_report}" 2>/dev/null || {
        # Fallback if jq not available
        cp "${summary}" "${weekly_report}"
    }
    
    echo -e "${GREEN}Weekly report generated: ${weekly_report}${NC}"
else
    echo -e "${YELLOW}Warning: Stability summary not found${NC}"
    echo "Run monitor_fault_injection_ci.sh first to generate summary"
fi

# Check thresholds
if [ -f "${ROUTER_DIR}/reports/fault-injection-ci/stability_summary.json" ]; then
    local pass_rate=$(jq -r '.pass_rate_percent' "${ROUTER_DIR}/reports/fault-injection-ci/stability_summary.json" 2>/dev/null || echo "0")
    local status=$(jq -r '.status' "${ROUTER_DIR}/reports/fault-injection-ci/stability_summary.json" 2>/dev/null || echo "unknown")
    
    echo ""
    echo -e "${GREEN}=== Threshold Check ===${NC}"
    echo "Pass rate: ${pass_rate}%"
    echo "Status: ${status}"
    
    if [ "${status}" != "stable" ]; then
        echo -e "${RED}Action required: Tests are unstable${NC}"
        echo "Review failures and fix issues"
    elif (( $(echo "${pass_rate} < 99" | bc -l 2>/dev/null || echo "0") )); then
        echo -e "${YELLOW}Warning: Pass rate below 99% threshold${NC}"
        echo "Monitor closely and fix issues if trend continues"
    else
        echo -e "${GREEN}All thresholds met${NC}"
    fi
fi

echo ""
echo -e "${GREEN}Weekly check complete${NC}"

