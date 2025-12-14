#!/usr/bin/env bash
# generate_coverage_hotspots.sh - Identify critical modules needing coverage improvement
#
# Generates a prioritized list of "Top N critical modules" based on:
#   1. Module importance (core routing, reliability, security)
#   2. Current coverage percentage
#   3. Module size (lines of code)
#
# Usage:
#   ./scripts/generate_coverage_hotspots.sh              # Generate hotspots report
#   ./scripts/generate_coverage_hotspots.sh --top 10     # Top N (default: 10)
#   ./scripts/generate_coverage_hotspots.sh --threshold 50  # Coverage threshold (default: 60%)
#
# Output: reports/coverage_hotspots.json
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
cd "$PROJECT_DIR"

REPORTS_DIR="$PROJECT_DIR/reports"
OUTPUT_FILE="$REPORTS_DIR/coverage_hotspots.json"
COVER_INDEX="$PROJECT_DIR/_build/test/cover/index.html"
TARGETED_REPORT="$REPORTS_DIR/coverage_targeted.json"

mkdir -p "$REPORTS_DIR"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

# Defaults
TOP_N=10
COVERAGE_THRESHOLD=60

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --top)
            TOP_N="$2"
            shift 2
            ;;
        --threshold)
            COVERAGE_THRESHOLD="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [--top N] [--threshold PCT]"
            echo ""
            echo "Options:"
            echo "  --top N        Number of hotspots to report (default: 10)"
            echo "  --threshold N  Coverage threshold percentage (default: 60)"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# ============================================================================
# Critical Module Definitions
# ============================================================================
# Priority levels: 1=highest (security), 2=high (reliability), 3=important (core)

# Security-critical modules (Priority 1)
SECURITY_MODULES=(
    "router_rbac"
    "router_permissions"
    "router_tenant_allowlist"
    "router_data_privacy"
    "router_secrets"
    "router_auth"
    "router_abuse"
)

# Reliability-critical modules (Priority 2)
RELIABILITY_MODULES=(
    "router_circuit_breaker"
    "router_nats"
    "router_jetstream"
    "router_rate_limiter"
    "router_rate_limit_store"
    "router_backpressure"
    "router_intake_backpressure"
    "router_recovery"
    "router_failover"
)

# Core routing modules (Priority 3)
CORE_MODULES=(
    "router_core"
    "router_decider"
    "router_decide_consumer"
    "router_result_consumer"
    "router_policy"
    "router_policy_store"
    "router_policy_validator"
    "router_policy_applier"
    "router_assignment"
    "router_caf_adapter"
    "router_extension_invoker"
    "router_grpc"
    "router_admin_grpc"
    "router_state"
    "router_idem"
    "router_idempotency"
    "router_sticky_store"
)

# Observability modules (Priority 4)
OBSERVABILITY_MODULES=(
    "router_metrics"
    "router_prometheus"
    "router_r10_metrics"
    "router_telemetry"
    "router_logger"
    "router_alerts"
    "router_health"
)

# Get priority for a module
get_priority() {
    local module=$1
    
    for m in "${SECURITY_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "1" && return
    done
    
    for m in "${RELIABILITY_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "2" && return
    done
    
    for m in "${CORE_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "3" && return
    done
    
    for m in "${OBSERVABILITY_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "4" && return
    done
    
    echo "5"  # Not critical
}

get_category() {
    local module=$1
    
    for m in "${SECURITY_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "security" && return
    done
    
    for m in "${RELIABILITY_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "reliability" && return
    done
    
    for m in "${CORE_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "core" && return
    done
    
    for m in "${OBSERVABILITY_MODULES[@]}"; do
        [[ "$module" == "$m" ]] && echo "observability" && return
    done
    
    echo "other"
}

# ============================================================================
# Parse Coverage Data
# ============================================================================
echo -e "${CYAN}${BOLD}Analyzing coverage hotspots...${NC}"
echo ""

declare -A MODULE_COVERAGE
declare -A MODULE_LINES

# Try to parse from coverage targeted report first
if [[ -f "$TARGETED_REPORT" ]]; then
    echo "  Reading from: $TARGETED_REPORT"
    # Parse module_coverage array from JSON
    while IFS= read -r line; do
        module=$(echo "$line" | grep -oP '"module":\s*"\K[^"]+' || true)
        pct=$(echo "$line" | grep -oP '"percentage":\s*\K[0-9]+' || true)
        total=$(echo "$line" | grep -oP '"total":\s*\K[0-9]+' || true)
        if [[ -n "$module" && -n "$pct" ]]; then
            MODULE_COVERAGE["$module"]=$pct
            MODULE_LINES["$module"]=${total:-0}
        fi
    done < <(grep -oP '\{"module":[^}]+\}' "$TARGETED_REPORT" 2>/dev/null || true)
fi

# If no data from targeted report, parse from cover index
if [[ ${#MODULE_COVERAGE[@]} -eq 0 && -f "$COVER_INDEX" ]]; then
    echo "  Reading from: $COVER_INDEX"
    while IFS= read -r line; do
        # Parse: <tr><td><a href='module.html'>module</a></td><td>XX%</td></tr>
        module=$(echo "$line" | grep -oP "href='[^']+\.html'>\K[^<]+" || true)
        pct=$(echo "$line" | grep -oE '[0-9]+%' | head -1 | tr -d '%' || true)
        if [[ -n "$module" && -n "$pct" ]]; then
            MODULE_COVERAGE["$module"]=$pct
            # Estimate lines from source file
            src_file="$PROJECT_DIR/src/${module}.erl"
            if [[ -f "$src_file" ]]; then
                MODULE_LINES["$module"]=$(wc -l < "$src_file" 2>/dev/null || echo "0")
            else
                MODULE_LINES["$module"]=0
            fi
        fi
    done < <(grep -E "<td><a href='[^']+\.html'>" "$COVER_INDEX" 2>/dev/null || true)
fi

# If still no data, scan source files and assume 0% coverage
if [[ ${#MODULE_COVERAGE[@]} -eq 0 ]]; then
    echo -e "  ${YELLOW}⚠ No coverage data found, assuming 0% for all modules${NC}"
    while IFS= read -r file; do
        module=$(basename "$file" .erl)
        MODULE_COVERAGE["$module"]=0
        MODULE_LINES["$module"]=$(wc -l < "$file" 2>/dev/null || echo "0")
    done < <(find "$PROJECT_DIR/src" -name "*.erl" -type f 2>/dev/null)
fi

echo "  Found coverage data for ${#MODULE_COVERAGE[@]} modules"
echo ""

# ============================================================================
# Calculate Hotspots
# ============================================================================
echo -e "${CYAN}${BOLD}Calculating priority scores...${NC}"
echo ""

# Score = (threshold - coverage) * priority_weight * sqrt(lines)
# Higher score = more urgent
declare -a HOTSPOTS

for module in "${!MODULE_COVERAGE[@]}"; do
    coverage=${MODULE_COVERAGE[$module]:-0}
    lines=${MODULE_LINES[$module]:-0}
    priority=$(get_priority "$module")
    category=$(get_category "$module")
    
    # Skip non-critical modules (priority 5) and modules above threshold
    if [[ "$priority" == "5" ]]; then
        continue
    fi
    
    if [[ "$coverage" -ge "$COVERAGE_THRESHOLD" ]]; then
        continue
    fi
    
    # Calculate priority weight (inverse of priority number)
    case $priority in
        1) weight=100 ;;  # Security
        2) weight=80 ;;   # Reliability
        3) weight=60 ;;   # Core
        4) weight=40 ;;   # Observability
        *) weight=20 ;;
    esac
    
    # Calculate urgency score
    coverage_gap=$((COVERAGE_THRESHOLD - coverage))
    
    # Use awk for float math
    score=$(awk -v gap="$coverage_gap" -v w="$weight" -v l="$lines" 'BEGIN {
        if (l <= 0) l = 1;
        printf "%.2f", gap * w * sqrt(l) / 100
    }')
    
    # Store as "score|module|coverage|priority|category|lines"
    HOTSPOTS+=("${score}|${module}|${coverage}|${priority}|${category}|${lines}")
done

# Sort by score (descending) and take top N
IFS=$'\n' SORTED_HOTSPOTS=($(printf '%s\n' "${HOTSPOTS[@]}" | sort -t'|' -k1 -rn | head -n "$TOP_N"))
unset IFS

# ============================================================================
# Generate JSON Report
# ============================================================================
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

{
    echo "{"
    echo "  \"generated_at\": \"$TIMESTAMP\","
    echo "  \"version\": \"1.0.0\","
    echo "  \"parameters\": {"
    echo "    \"top_n\": $TOP_N,"
    echo "    \"coverage_threshold\": $COVERAGE_THRESHOLD"
    echo "  },"
    echo "  \"summary\": {"
    echo "    \"total_critical_modules\": $((${#SECURITY_MODULES[@]} + ${#RELIABILITY_MODULES[@]} + ${#CORE_MODULES[@]} + ${#OBSERVABILITY_MODULES[@]})),"
    echo "    \"modules_below_threshold\": ${#SORTED_HOTSPOTS[@]},"
    echo "    \"modules_with_coverage_data\": ${#MODULE_COVERAGE[@]}"
    echo "  },"
    echo "  \"hotspots\": ["
    
    first=true
    rank=1
    for entry in "${SORTED_HOTSPOTS[@]}"; do
        IFS='|' read -r score module coverage priority category lines <<< "$entry"
        
        if [[ "$first" != true ]]; then
            echo ","
        fi
        first=false
        
        # Calculate coverage gap and recommended tests
        coverage_gap=$((COVERAGE_THRESHOLD - coverage))
        
        printf "    {\n"
        printf "      \"rank\": %d,\n" "$rank"
        printf "      \"module\": \"%s\",\n" "$module"
        printf "      \"category\": \"%s\",\n" "$category"
        printf "      \"priority\": %d,\n" "$priority"
        printf "      \"current_coverage\": %d,\n" "$coverage"
        printf "      \"target_coverage\": %d,\n" "$COVERAGE_THRESHOLD"
        printf "      \"coverage_gap\": %d,\n" "$coverage_gap"
        printf "      \"lines_of_code\": %d,\n" "$lines"
        printf "      \"urgency_score\": %s\n" "$score"
        printf "    }"
        
        ((rank++))
    done
    
    echo ""
    echo "  ],"
    echo "  \"category_definitions\": {"
    echo "    \"security\": \"Authentication, authorization, data privacy, abuse prevention\","
    echo "    \"reliability\": \"Circuit breaker, connection management, rate limiting, recovery\","
    echo "    \"core\": \"Message routing, policy enforcement, state management\","
    echo "    \"observability\": \"Metrics, logging, telemetry, health checks\""
    echo "  },"
    echo "  \"priority_levels\": {"
    echo "    \"1\": \"Critical (security)\","
    echo "    \"2\": \"High (reliability)\","
    echo "    \"3\": \"Important (core routing)\","
    echo "    \"4\": \"Standard (observability)\""
    echo "  }"
    echo "}"
} > "$OUTPUT_FILE"

echo -e "${GREEN}Hotspots report saved: $OUTPUT_FILE${NC}"
echo ""

# ============================================================================
# Print Summary
# ============================================================================
echo -e "${CYAN}${BOLD}+================================================================+${NC}"
echo -e "${CYAN}${BOLD}|           TOP $TOP_N COVERAGE HOTSPOTS                          |${NC}"
echo -e "${CYAN}${BOLD}+================================================================+${NC}"
echo ""
printf "${BOLD}%-4s %-35s %-12s %-8s %-8s${NC}\n" "Rank" "Module" "Category" "Coverage" "Gap"
echo "--------------------------------------------------------------------------------"

rank=1
for entry in "${SORTED_HOTSPOTS[@]}"; do
    IFS='|' read -r score module coverage priority category lines <<< "$entry"
    coverage_gap=$((COVERAGE_THRESHOLD - coverage))
    
    # Color based on category
    case $category in
        security)    color=$RED ;;
        reliability) color=$YELLOW ;;
        core)        color=$CYAN ;;
        *)           color=$NC ;;
    esac
    
    printf "${color}%-4d %-35s %-12s %5d%%   %+5d%%${NC}\n" \
        "$rank" "$module" "$category" "$coverage" "-$coverage_gap"
    
    ((rank++))
done

echo ""
echo -e "${CYAN}+================================================================+${NC}"
echo ""
echo -e "${BOLD}Legend:${NC}"
echo -e "  ${RED}■${NC} Security (Priority 1) - Authentication, authorization, data privacy"
echo -e "  ${YELLOW}■${NC} Reliability (Priority 2) - Circuit breaker, connections, rate limiting"
echo -e "  ${CYAN}■${NC} Core (Priority 3) - Message routing, policy, state management"
echo ""
echo -e "${BOLD}Recommendations:${NC}"
echo "  1. Start with security modules (Priority 1) - highest risk"
echo "  2. Add unit tests for public API functions"
echo "  3. Add integration tests for critical paths"
echo "  4. Target 80%+ coverage for security, 60%+ for others"
echo ""
