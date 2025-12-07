#!/bin/bash
# Monthly stability review for fault injection tests
# Generates trend analysis and comprehensive report

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPORT_DIR="${ROUTER_DIR}/reports/fault-injection-monthly"
WEEKLY_DIR="${ROUTER_DIR}/reports/fault-injection-weekly"
DATE=$(date +"%Y-%m-%d")
MONTH=$(date +"%Y-%m")

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

mkdir -p "${REPORT_DIR}"

echo -e "${BLUE}=== Monthly Fault Injection Stability Review ===${NC}"
echo "Month: ${MONTH}"
echo "Date: ${DATE}"
echo ""

# Collect weekly reports
local weekly_reports=($(find "${WEEKLY_DIR}" -name "weekly_report_*.json" -type f 2>/dev/null | sort))

if [ ${#weekly_reports[@]} -eq 0 ]; then
    echo -e "${YELLOW}No weekly reports found${NC}"
    echo "Run weekly_stability_check.sh first"
    exit 1
fi

echo -e "${GREEN}Found ${#weekly_reports[@]} weekly reports${NC}"

# Generate trend analysis
local monthly_report="${REPORT_DIR}/monthly_report_${MONTH}.json"
local trend_data="${REPORT_DIR}/trend_analysis_${MONTH}.txt"

{
    echo "=== Monthly Trend Analysis ==="
    echo "Month: ${MONTH}"
    echo "Date: ${DATE}"
    echo ""
    echo "Weekly Reports Analyzed: ${#weekly_reports[@]}"
    echo ""
    
    # Calculate averages
    local total_pass_rate=0
    local total_duration=0
    local stable_weeks=0
    local unstable_weeks=0
    
    for report in "${weekly_reports[@]}"; do
        local week_date=$(basename "${report}" | sed 's/weekly_report_\(.*\)\.json/\1/')
        local pass_rate=$(jq -r '.pass_rate_percent' "${report}" 2>/dev/null || echo "0")
        local duration=$(jq -r '.average_duration_seconds' "${report}" 2>/dev/null || echo "0")
        local status=$(jq -r '.status' "${report}" 2>/dev/null || echo "unknown")
        
        total_pass_rate=$(echo "${total_pass_rate} + ${pass_rate}" | bc -l 2>/dev/null || echo "${total_pass_rate}")
        total_duration=$(echo "${total_duration} + ${duration}" | bc -l 2>/dev/null || echo "${total_duration}")
        
        if [ "${status}" = "stable" ]; then
            stable_weeks=$((stable_weeks + 1))
        else
            unstable_weeks=$((unstable_weeks + 1))
        fi
        
        echo "Week ${week_date}: Pass rate ${pass_rate}%, Duration ${duration}s, Status ${status}"
    done
    
    # Calculate averages
    local avg_pass_rate=0
    local avg_duration=0
    if [ ${#weekly_reports[@]} -gt 0 ]; then
        avg_pass_rate=$(echo "scale=2; ${total_pass_rate} / ${#weekly_reports[@]}" | bc -l 2>/dev/null || echo "0")
        avg_duration=$(echo "scale=2; ${total_duration} / ${#weekly_reports[@]}" | bc -l 2>/dev/null || echo "0")
    fi
    
    echo ""
    echo "=== Monthly Summary ==="
    echo "Average Pass Rate: ${avg_pass_rate}%"
    echo "Average Duration: ${avg_duration}s"
    echo "Stable Weeks: ${stable_weeks}"
    echo "Unstable Weeks: ${unstable_weeks}"
    echo ""
    
    # Recommendations
    echo "=== Recommendations ==="
    if [ ${unstable_weeks} -gt 0 ]; then
        echo -e "${RED}Action Required: ${unstable_weeks} unstable week(s) detected${NC}"
        echo "- Review failures and fix issues"
        echo "- Consider increasing timeouts if timing-related"
        echo "- Check for race conditions if intermittent"
    fi
    
    if (( $(echo "${avg_pass_rate} < 99" | bc -l 2>/dev/null || echo "0") )); then
        echo -e "${YELLOW}Warning: Average pass rate below 99% threshold${NC}"
        echo "- Investigate root causes"
        echo "- Fix high-priority issues"
    fi
    
    if (( $(echo "${avg_duration} > 900" | bc -l 2>/dev/null || echo "0") )); then
        echo -e "${YELLOW}Warning: Average duration exceeds 15 minutes${NC}"
        echo "- Consider creating separate CI job"
        echo "- See docs/dev/FAULT_INJECTION_CI_JOB_GUIDE.md"
    fi
    
    if [ ${unstable_weeks} -eq 0 ] && (( $(echo "${avg_pass_rate} >= 99" | bc -l 2>/dev/null || echo "0") )); then
        echo -e "${GREEN}All metrics within acceptable ranges${NC}"
        echo "- Continue monitoring"
        echo "- No immediate action required"
    fi
    
} > "${trend_data}"

cat "${trend_data}"

# Generate JSON report
{
    echo "{"
    echo "  \"report_type\": \"monthly\","
    echo "  \"month\": \"${MONTH}\","
    echo "  \"report_date\": \"${DATE}\","
    echo "  \"weekly_reports_analyzed\": ${#weekly_reports[@]},"
    echo "  \"average_pass_rate_percent\": ${avg_pass_rate},"
    echo "  \"average_duration_seconds\": ${avg_duration},"
    echo "  \"stable_weeks\": ${stable_weeks},"
    echo "  \"unstable_weeks\": ${unstable_weeks}"
    echo "}"
} > "${monthly_report}" 2>/dev/null || {
    echo "{\"error\": \"Failed to generate JSON report\"}" > "${monthly_report}"
}

echo ""
echo -e "${GREEN}Monthly review complete${NC}"
echo "Trend analysis: ${trend_data}"
echo "Monthly report: ${monthly_report}"

