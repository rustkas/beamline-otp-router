#!/bin/bash
# SLO Baseline Measurement Script
#
# Measures current Router performance against SLO targets
# Requires: Prometheus with Router metrics

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ARTIFACTS_DIR="$PROJECT_DIR/_artifacts"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_FILE="$ARTIFACTS_DIR/slo_baseline_report_${TIMESTAMP}.md"

# Prometheus endpoint (override with PROMETHEUS_URL env var)
PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Query Prometheus
query_prometheus() {
    local query="$1"
    local encoded_query=$(echo "$query" | jq -sRr @uri)
    
    curl -s "${PROMETHEUS_URL}/api/v1/query?query=${encoded_query}" | jq -r '.data.result[0].value[1] // "N/A"'
}

# Check if Prometheus is available
check_prometheus() {
    log_info "Checking Prometheus availability at ${PROMETHEUS_URL}..."
    
    if ! curl -sf "${PROMETHEUS_URL}/-/healthy" > /dev/null 2>&1; then
        log_error "Prometheus not available at ${PROMETHEUS_URL}"
        log_warn "Set PROMETHEUS_URL environment variable to override"
        return 1
    fi
    
    log_info "✓ Prometheus is available"
    return 0
}

# Generate baseline report
generate_report() {
    log_info "Querying Router metrics..."
    
    # Query metrics
    P95_LATENCY=$(query_prometheus 'slo:router_latency_p95:5m')
    P99_LATENCY=$(query_prometheus 'slo:router_latency_p99:5m')
    ERROR_RATE=$(query_prometheus 'slo:router_error_rate:5m')
    AVAILABILITY=$(query_prometheus 'slo:router_availability:30d')
    ERROR_BUDGET=$(query_prometheus 'slo:router_error_budget_remaining:30d')
    BURN_RATE=$(query_prometheus 'slo:router_error_budget_burn_rate:1h')
    
    log_info "Generating report..."
    
    cat > "$REPORT_FILE" <<EOF
# Router SLO Baseline Report

**Generated**: $(date -Iseconds)  
**Prometheus**: ${PROMETHEUS_URL}  
**Measurement Window**: Last 30 days

## Summary

| Metric | Current Value | SLO Target | Status |
|--------|---------------|------------|--------|
| **p95 Latency** | ${P95_LATENCY}s | < 100ms (0.100s) | $(compare_latency "$P95_LATENCY" "0.100") |
| **p99 Latency** | ${P99_LATENCY}s | < 200ms (0.200s) | $(compare_latency "$P99_LATENCY" "0.200") |
| **Error Rate** | ${ERROR_RATE} | < 0.1% (0.001) | $(compare_error_rate "$ERROR_RATE" "0.001") |
| **Availability** | ${AVAILABILITY} | 99.9% (0.999) | $(compare_availability "$AVAILABILITY" "0.999") |

## Error Budget

- **Remaining**: ${ERROR_BUDGET} ($(echo "$ERROR_BUDGET * 100" | bc -l | xargs printf "%.2f")%)
- **Burn Rate**: ${BURN_RATE}x (1x is normal)
- **Projected Exhaustion**: $(calculate_exhaustion "$ERROR_BUDGET" "$BURN_RATE")

## Compliance Status

$(determine_compliance "$P95_LATENCY" "$P99_LATENCY" "$AVAILABILITY")

## Recommendations

$(generate_recommendations "$P95_LATENCY" "$P99_LATENCY" "$ERROR_RATE" "$AVAILABILITY" "$ERROR_BUDGET")

## Raw PromQL Queries

\`\`\`promql
# p95 latency
slo:router_latency_p95:5m
# Result: ${P95_LATENCY}

# p99 latency
slo:router_latency_p99:5m
# Result: ${P99_LATENCY}

# Error rate (5-minute)
slo:router_error_rate:5m
# Result: ${ERROR_RATE}

# Availability (30-day)
slo:router_availability:30d
# Result: ${AVAILABILITY}

# Error budget remaining
slo:router_error_budget_remaining:30d
# Result: ${ERROR_BUDGET}

# Burn rate (1-hour)
slo:router_error_budget_burn_rate:1h
# Result: ${BURN_RATE}
\`\`\`

## Next Steps

1. Review compliance status above
2. If SLO violations exist, investigate root cause
3. Adjust SLO targets if unrealistic based on baseline
4. Set up alerting rules (see \`prometheus/router_slo_alerts.yml\`)
EOF
    
    log_info "✓ Report generated: $REPORT_FILE"
    cat "$REPORT_FILE"
}

# Helper functions
compare_latency() {
    local current="$1"
    local target="$2"
    
    if [ "$current" = "N/A" ]; then
        echo "⚠️ No Data"
    elif awk "BEGIN {exit !($current <= $target)}"; then
        echo "✅ PASS"
    else
        echo "❌ FAIL"
    fi
}

compare_error_rate() {
    local current="$1"
    local target="$2"
    
    if [ "$current" = "N/A" ]; then
        echo "⚠️ No Data"
    elif awk "BEGIN {exit !($current <= $target)}"; then
        echo "✅ PASS"
    else
        echo "❌ FAIL"
    fi
}

compare_availability() {
    local current="$1"
    local target="$2"
    
    if [ "$current" = "N/A" ]; then
        echo "⚠️ No Data"
    elif awk "BEGIN {exit !($current >= $target)}"; then
        echo "✅ PASS"
    else
        echo "❌ FAIL"
    fi
}

calculate_exhaustion() {
    local budget="$1"
    local burn_rate="$2"
    
    if [ "$budget" = "N/A" ] || [ "$burn_rate" = "N/A" ]; then
        echo "N/A"
    elif awk "BEGIN {exit !($burn_rate <= 0)}"; then
        echo "Never (budget increasing)"
    else
        local days=$(echo "scale=1; (30 * $budget) / $burn_rate" | bc -l)
        echo "${days} days"
    fi
}

determine_compliance() {
    local p95="$1"
    local p99="$2"
    local avail="$3"
    
    if [ "$p95" = "N/A" ] || [ "$p99" = "N/A" ] || [ "$avail" = "N/A" ]; then
        echo "**Status**: ⚠️ INSUFFICIENT DATA"
        echo ""
        echo "No metrics available. Ensure Prometheus is collecting Router metrics."
    elif awk "BEGIN {exit !($p95 <= 0.100 && $p99 <= 0.200 && $avail >= 0.999)}"; then
        echo "**Status**: ✅ ALL SLOs MET"
        echo ""
        echo "Router is performing within all SLO targets. Excellent!"
    else
        echo "**Status**: ❌ SLO VIOLATIONS DETECTED"
        echo ""
        echo "One or more SLOs are violated. Immediate investigation required."
    fi
}

generate_recommendations() {
    local p95="$1"
    local p99="$2"
    local error_rate="$3"
    local avail="$4"
    local budget="$5"
    
    echo ""
    
    # Latency recommendations
    if [ "$p95" != "N/A" ] && awk "BEGIN {exit !($p95 > 0.100)}"; then
        echo "- **Latency**: p95 exceeds SLO. Investigate slow requests, database queries, or external API calls."
    fi
    
    if [ "$p99" != "N/A" ] && awk "BEGIN {exit !($p99 > 0.200)}"; then
        echo "- **Latency**: p99 exceeds SLO. Check for outliers and tail latency issues."
    fi
    
    # Availability recommendations
    if [ "$avail" != "N/A" ] && awk "BEGIN {exit !($avail < 0.999)}"; then
        echo "- **Availability**: Below 99.9% SLO. Review error logs and circuit breaker status."
    fi
    
    # Error budget recommendations
    if [ "$budget" != "N/A" ]; then
        if awk "BEGIN {exit !($budget < 0.10)}"; then
            echo "- **Error Budget**: Critically low. Consider feature freeze and focus on reliability."
        elif awk "BEGIN {exit !($budget < 0.50)}"; then
            echo "- **Error Budget**: Moderate. Increase monitoring and code review rigor."
        fi
    fi
    
    echo ""
    echo "If all SLOs are met, continue normal operations."
}

main() {
    mkdir -p "$ARTIFACTS_DIR"
    
    echo ""
    echo "======================================"
    echo "  Router SLO Baseline Measurement"
    echo "======================================"
    echo ""
    
    if ! check_prometheus; then
        log_error "Cannot proceed without Prometheus"
        exit 1
    fi
    
    generate_report
    
    echo ""
    log_info "======================================"
    log_info "  Baseline measurement complete!"
    log_info "  Report: $REPORT_FILE"
    log_info "======================================"
}

main "$@"
