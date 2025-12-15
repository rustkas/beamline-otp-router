#!/usr/bin/env bash
# generate_targeted_coverage.sh - Calculate coverage excluding non-relevant modules
#
# Usage:
#   ./scripts/generate_targeted_coverage.sh              # Parse latest cover data
#   ./scripts/generate_targeted_coverage.sh --run-tests  # Run CT with cover first
#
# Output: reports/coverage_targeted.json
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_DIR"

REPORTS_DIR="$PROJECT_DIR/reports"
OUTPUT_FILE="$REPORTS_DIR/coverage_targeted.json"
EXCLUSIONS_FILE="$PROJECT_DIR/config/coverage_exclusions.json"
COVER_DATA_DIR="$PROJECT_DIR/_build/test/cover"

mkdir -p "$REPORTS_DIR"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Modules to exclude (patterns and explicit names)
EXCLUDE_PATTERNS=(
    "_pb$"
    "_template$"
    "_ctl_"
    "_cli$"
)

EXCLUDE_MODULES=(
    "ack_pb"
    "flow_pb"
    "result_pb"
    "router_error_handling_template"
    "router_gen_server_lifecycle_template"
    "router_rN_metrics_template"
    "router_ctl_r10"
)

# Check if module should be excluded
is_excluded() {
    local module=$1
    
    # Check explicit exclusions
    for excl in "${EXCLUDE_MODULES[@]}"; do
        if [[ "$module" == "$excl" ]]; then
            return 0
        fi
    done
    
    # Check patterns
    for pattern in "${EXCLUDE_PATTERNS[@]}"; do
        if echo "$module" | grep -qE "$pattern"; then
            return 0
        fi
    done
    
    return 1
}

# Run tests with cover if requested
if [[ "${1:-}" == "--run-tests" ]]; then
    echo -e "${BLUE:-\033[0;34m}Running tests with coverage...${NC}"
    
    # Enable cover for this run
    if ! rebar3 ct --cover --dir test 2>&1; then
        echo -e "${YELLOW}Some tests failed, but continuing with coverage analysis${NC}"
    fi
fi

# Find cover data
echo -e "${CYAN}Analyzing coverage data...${NC}"

# Get list of all source modules
ALL_MODULES=()
while IFS= read -r file; do
    module=$(basename "$file" .erl)
    ALL_MODULES+=("$module")
done < <(find "$PROJECT_DIR/src" -name "*.erl" -type f 2>/dev/null)

# Separate into targeted and excluded
TARGETED_MODULES=()
EXCLUDED_LIST=()

for module in "${ALL_MODULES[@]}"; do
    if is_excluded "$module"; then
        EXCLUDED_LIST+=("$module")
    else
        TARGETED_MODULES+=("$module")
    fi
done

echo -e "${CYAN}Modules: ${#ALL_MODULES[@]} total, ${#TARGETED_MODULES[@]} targeted, ${#EXCLUDED_LIST[@]} excluded${NC}"

# Parse cover data if available
TOTAL_LINES=0
COVERED_LINES=0
MODULE_COVERAGE=()

# Aggregate coverage (all modules, as-is from cover)
AGGREGATE_PCT="0.00"

# Try to parse from cover index.html
COVER_INDEX="$PROJECT_DIR/_build/test/cover/index.html"
COVER_CT_DIR="$PROJECT_DIR/_build/test/cover/ct"

if [[ -f "$COVER_INDEX" ]]; then
    # Parse aggregate (Total) coverage from index.html
    # Format: <tr><td><strong>Total</strong></td><td>XX%</td>
    total_line=$(grep -E "<strong>Total</strong>" "$COVER_INDEX" 2>/dev/null | head -1 || true)
    if [[ -n "$total_line" ]]; then
        AGGREGATE_PCT=$(echo "$total_line" | grep -oE '[0-9]+%' | head -1 | tr -d '%\n\r ' || echo "0")
        AGGREGATE_PCT=${AGGREGATE_PCT:-0}
    fi
fi

# Gather per-module coverage data from cover HTML files
MODULE_COVERAGE=()
for module in "${TARGETED_MODULES[@]}"; do
    html_file="$COVER_CT_DIR/${module}.html"
    if [[ -f "$html_file" ]]; then
        line_count=$(grep -c '<td class="line"' "$html_file" 2>/dev/null || true)
        misses=$(grep -c 'class="miss"' "$html_file" 2>/dev/null || true)
        line_count=${line_count:-0}
        misses=${misses:-0}
        hits=$((line_count - misses))
        if (( hits < 0 )); then
            hits=0
        fi
        if (( line_count > 0 )); then
            percentage=$(awk -v c="$hits" -v t="$line_count" 'BEGIN {printf "%.2f", (t > 0) ? (c/t)*100 : 0}')
            TOTAL_LINES=$((TOTAL_LINES + line_count))
            COVERED_LINES=$((COVERED_LINES + hits))
        else
            percentage="0.00"
        fi
    else
        hits=0
        misses=0
        line_count=0
        percentage="0.00"
    fi
    MODULE_COVERAGE+=("{\"module\": \"${module}\", \"covered\": ${hits}, \"misses\": ${misses}, \"total\": ${line_count}, \"percentage\": ${percentage}}")
done

# Calculate overall targeted coverage
if [[ $TOTAL_LINES -gt 0 ]]; then
    OVERALL_PCT=$(awk "BEGIN {printf \"%.2f\", ($COVERED_LINES/$TOTAL_LINES)*100}")
    OVERALL_INT=$(awk "BEGIN {printf \"%d\", $OVERALL_PCT}")
else
    OVERALL_PCT="0.00"
    OVERALL_INT=0
    echo -e "${YELLOW}Warning: No cover data found. Run with --run-tests to generate coverage.${NC}"
fi

# Generate timestamp
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Build JSON output
{
    echo "{"
    echo "  \"generated_at\": \"$TIMESTAMP\","
    echo "  \"version\": \"1.1.0\","
    echo "  \"coverage\": {"
    echo "    \"aggregate\": $AGGREGATE_PCT,"
    echo "    \"targeted\": $OVERALL_PCT"
    echo "  },"
    echo "  \"summary\": {"
    echo "    \"total_modules\": ${#ALL_MODULES[@]},"
    echo "    \"targeted_modules\": ${#TARGETED_MODULES[@]},"
    echo "    \"excluded_modules\": ${#EXCLUDED_LIST[@]},"
    echo "    \"total_lines\": $TOTAL_LINES,"
    echo "    \"covered_lines\": $COVERED_LINES"
    echo "  },"
    echo "  \"exclusions\": {"
    echo "    \"patterns\": ["
    printf '      "%s"' "${EXCLUDE_PATTERNS[0]}"
    for ((i=1; i<${#EXCLUDE_PATTERNS[@]}; i++)); do
        printf ',\n      "%s"' "${EXCLUDE_PATTERNS[i]}"
    done
    echo ""
    echo "    ],"
    echo "    \"modules\": ["
    if [[ ${#EXCLUDED_LIST[@]} -gt 0 ]]; then
        printf '      "%s"' "${EXCLUDED_LIST[0]}"
        for ((i=1; i<${#EXCLUDED_LIST[@]}; i++)); do
            printf ',\n      "%s"' "${EXCLUDED_LIST[i]}"
        done
        echo ""
    fi
    echo "    ]"
    echo "  },"
    echo "  \"targeted_modules\": ["
    if [[ ${#TARGETED_MODULES[@]} -gt 0 ]]; then
        printf '    "%s"' "${TARGETED_MODULES[0]}"
        for ((i=1; i<${#TARGETED_MODULES[@]}; i++)); do
            printf ',\n    "%s"' "${TARGETED_MODULES[i]}"
        done
        echo ""
    fi
    echo "  ],"
    echo "  \"module_coverage\": ["
    if [[ ${#MODULE_COVERAGE[@]} -gt 0 ]]; then
        echo "    ${MODULE_COVERAGE[0]}"
        for ((i=1; i<${#MODULE_COVERAGE[@]}; i++)); do
            echo "    ,${MODULE_COVERAGE[i]}"
        done
    fi
    echo "  ]"
    echo "}"
} > "$OUTPUT_FILE"

echo -e "${GREEN}Targeted coverage report saved: $OUTPUT_FILE${NC}"

# Print summary with both coverage metrics
echo ""
echo -e "${CYAN}+========================================================+${NC}"
echo -e "${CYAN}|              COVERAGE METRICS REPORT                   |${NC}"
echo -e "${CYAN}+========================================================+${NC}"
echo -e "${CYAN}|  Modules                                               |${NC}"
echo -e "${CYAN}+--------------------------------------------------------+${NC}"
printf "${CYAN}|${NC}    Total:            %6d                            ${CYAN}|${NC}\n" "${#ALL_MODULES[@]}"
printf "${CYAN}|${NC}    Targeted:         %6d                            ${CYAN}|${NC}\n" "${#TARGETED_MODULES[@]}"
printf "${CYAN}|${NC}    Excluded:         %6d                            ${CYAN}|${NC}\n" "${#EXCLUDED_LIST[@]}"
echo -e "${CYAN}+--------------------------------------------------------+${NC}"
echo -e "${CYAN}|  Lines (targeted only)                                 |${NC}"
echo -e "${CYAN}+--------------------------------------------------------+${NC}"
printf "${CYAN}|${NC}    Total:            %6d                            ${CYAN}|${NC}\n" "$TOTAL_LINES"
printf "${CYAN}|${NC}    Covered:          %6d                            ${CYAN}|${NC}\n" "$COVERED_LINES"
echo -e "${CYAN}+--------------------------------------------------------+${NC}"
echo -e "${CYAN}|  Coverage Percentages                                  |${NC}"
echo -e "${CYAN}+--------------------------------------------------------+${NC}"
# Aggregate coverage (all modules)
printf "${CYAN}|${NC}    Aggregate (all):  %5s%%  ${YELLOW}(for reference)${NC}        ${CYAN}|${NC}\n" "$AGGREGATE_PCT"
# Targeted coverage (quality gate metric)
if (( OVERALL_INT >= 80 )); then
    printf "${CYAN}|${NC}    Targeted:         ${GREEN}%5s%%${NC}  ${GREEN}(quality gate)${NC}         ${CYAN}|${NC}\n" "$OVERALL_PCT"
elif (( OVERALL_INT >= 60 )); then
    printf "${CYAN}|${NC}    Targeted:         ${YELLOW}%5s%%${NC}  ${GREEN}(quality gate)${NC}         ${CYAN}|${NC}\n" "$OVERALL_PCT"
else
    printf "${CYAN}|${NC}    Targeted:         ${RED}%5s%%${NC}  ${GREEN}(quality gate)${NC}         ${CYAN}|${NC}\n" "$OVERALL_PCT"
fi
echo -e "${CYAN}+========================================================+${NC}"

echo ""
echo -e "${CYAN}Excluded modules (${#EXCLUDED_LIST[@]}):${NC}"
for mod in "${EXCLUDED_LIST[@]}"; do
    echo "  - $mod"
done

echo ""
echo -e "${CYAN}Legend:${NC}"
echo "  Aggregate = coverage of ALL modules (protobuf, templates, etc.)"
echo "  Targeted  = coverage of business logic modules only (quality gate metric)"
