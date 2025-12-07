#!/bin/bash
# R10 Protective Rails Validation Script
# Checks for violations of R10 protective rules (direct ETS access, hardcoded trigger reasons)
#
# Usage:
#   bash apps/otp/router/scripts/check_r10_protective_rails.sh
#
# Exit codes:
#   0 - No violations found
#   1 - Violations found
#   2 - Script error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
TEST_DIR="${ROUTER_DIR}/test"
SRC_DIR="${ROUTER_DIR}/src"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

VIOLATIONS=0

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

# Check 1: Prohibit direct ETS access to router_metrics (except cleanup patterns)
check_direct_ets_access() {
    log_info "Checking for direct ETS access to router_metrics..."
    
    # Find all ets:lookup(router_metrics patterns
    VIOLATIONS_FOUND=0
    
    while IFS= read -r file; do
        # Skip router_r10_metrics.erl (it's allowed to access ETS)
        if [[ "$file" == *"router_r10_metrics.erl" ]]; then
            continue
        fi
        
        # Check for ets:lookup(router_metrics (not allowed)
        if grep -n "ets:lookup(router_metrics" "$file" > /dev/null 2>&1; then
            log_error "Direct ETS access found in: $file"
            grep -n "ets:lookup(router_metrics" "$file" | while IFS=: read -r line_num line_content; do
                log_error "  Line $line_num: $line_content"
            done
            VIOLATIONS_FOUND=$((VIOLATIONS_FOUND + 1))
        fi
        
        # Check for ets:lookup(router_provider_circuit_breaker (not allowed)
        if grep -n "ets:lookup(router_provider_circuit_breaker" "$file" > /dev/null 2>&1; then
            log_error "Direct ETS access to circuit breaker table found in: $file"
            grep -n "ets:lookup(router_provider_circuit_breaker" "$file" | while IFS=: read -r line_num line_content; do
                log_error "  Line $line_num: $line_content"
            done
            VIOLATIONS_FOUND=$((VIOLATIONS_FOUND + 1))
        fi
    done < <(find "$TEST_DIR" "$SRC_DIR" -name "*.erl" -type f 2>/dev/null || true)
    
    if [ $VIOLATIONS_FOUND -gt 0 ]; then
        log_error "Found $VIOLATIONS_FOUND violation(s) of direct ETS access rule"
        log_error "Use router_r10_metrics API instead of direct ETS access"
        VIOLATIONS=$((VIOLATIONS + VIOLATIONS_FOUND))
    else
        log_info "✓ No direct ETS access violations found"
    fi
}

# Check 2: Prohibit hardcoded trigger reason binaries
check_hardcoded_trigger_reasons() {
    log_info "Checking for hardcoded trigger reason binaries..."
    
    VIOLATIONS_FOUND=0
    
    # List of prohibited hardcoded trigger reasons
    PROHIBITED_REASONS=(
        "<<\"failure_threshold_exceeded\">>"
        "<<\"error_rate_threshold_exceeded\">>"
        "<<\"latency_threshold_exceeded\">>"
        "<<\"half_open_failure\">>"
        "<<\"timeout_elapsed\">>"
        "<<\"timeout\">>"
    )
    
    while IFS= read -r file; do
        # Skip router_r10_metrics.erl (it defines the constants)
        if [[ "$file" == *"router_r10_metrics.erl" ]]; then
            continue
        fi
        
        for reason in "${PROHIBITED_REASONS[@]}"; do
            if grep -n "$reason" "$file" > /dev/null 2>&1; then
                log_error "Hardcoded trigger reason found in: $file"
                grep -n "$reason" "$file" | while IFS=: read -r line_num line_content; do
                    log_error "  Line $line_num: $line_content"
                done
                log_error "  Use router_r10_metrics:trigger_reason_*() constants instead"
                VIOLATIONS_FOUND=$((VIOLATIONS_FOUND + 1))
            fi
        done
    done < <(find "$TEST_DIR" "$SRC_DIR" -name "*.erl" -type f 2>/dev/null || true)
    
    if [ $VIOLATIONS_FOUND -gt 0 ]; then
        log_error "Found $VIOLATIONS_FOUND violation(s) of hardcoded trigger reason rule"
        VIOLATIONS=$((VIOLATIONS + VIOLATIONS_FOUND))
    else
        log_info "✓ No hardcoded trigger reason violations found"
    fi
}

# Main execution
main() {
    log_info "R10 Protective Rails Validation"
    log_info "Router directory: $ROUTER_DIR"
    echo ""
    
    check_direct_ets_access
    echo ""
    check_hardcoded_trigger_reasons
    echo ""
    
    if [ $VIOLATIONS -eq 0 ]; then
        log_info "✓ All protective rails checks passed"
        exit 0
    else
        log_error "✗ Found $VIOLATIONS total violation(s)"
        log_error "Please fix violations before committing"
        exit 1
    fi
}

main "$@"

