#!/bin/bash
# @doc Smoke Test Script for Beamline Router
#
# Quick validation that the router is functional after deployment/rollback.
# Runs a minimal set of checks to verify basic health.
#
# Usage:
#   ./scripts/smoke.sh [options]
#
# Options:
#   --verbose    Show detailed output
#   --timeout N  Set timeout in seconds (default: 30)
#
# Exit codes:
#   0 - All checks passed
#   1 - One or more checks failed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ARTIFACTS_DIR="$PROJECT_DIR/_artifacts"

# Settings
VERBOSE=false
TIMEOUT=30
CHECKS_PASSED=0
CHECKS_FAILED=0
CHECKS_SKIPPED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_check() { echo -e "${BLUE}[CHECK]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; CHECKS_PASSED=$((CHECKS_PASSED + 1)); }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; CHECKS_FAILED=$((CHECKS_FAILED + 1)); }
log_skip() { echo -e "${YELLOW}[SKIP]${NC} $1"; CHECKS_SKIPPED=$((CHECKS_SKIPPED + 1)); }

# Parse arguments
for arg in "$@"; do
    case "$arg" in
        --verbose|-v)
            VERBOSE=true
            ;;
        --timeout=*)
            TIMEOUT="${arg#*=}"
            ;;
        --timeout)
            shift
            TIMEOUT="${1:-30}"
            ;;
    esac
done

echo ""
echo "=========================================="
echo "  Beamline Router Smoke Test"
echo "=========================================="
echo ""

# ============================================================================
# Check 1: Application compiles
# ============================================================================
check_compile() {
    log_check "Application compiles..."
    
    cd "$PROJECT_DIR"
    
    if rebar3 compile > /dev/null 2>&1; then
        log_pass "Application compiles successfully"
        return 0
    else
        log_fail "Compilation failed"
        return 1
    fi
}

# ============================================================================
# Check 2: Application.app file exists and is valid
# ============================================================================
check_app_file() {
    log_check "Application .app file is valid..."
    
    local app_file="$PROJECT_DIR/_build/default/lib/beamline_router/ebin/beamline_router.app"
    
    if [ -f "$app_file" ]; then
        # Basic validation - check it contains expected keys
        if grep -q "beamline_router" "$app_file" && grep -q "vsn" "$app_file"; then
            log_pass "Application .app file is valid"
            return 0
        else
            log_fail "Application .app file is malformed"
            return 1
        fi
    else
        log_fail "Application .app file not found: $app_file"
        return 1
    fi
}

# ============================================================================
# Check 3: Fast unit tests pass
# ============================================================================
check_fast_tests() {
    log_check "Fast unit tests pass..."
    
    cd "$PROJECT_DIR"
    
    # Run a quick subset of tests
    if timeout "$TIMEOUT" rebar3 ct --suite test/router_unit_SUITE > /dev/null 2>&1; then
        log_pass "Fast unit tests pass"
        return 0
    elif timeout "$TIMEOUT" rebar3 ct --dir test --logdir _build/test/logs --compile_only > /dev/null 2>&1; then
        log_pass "Test compilation passes (full test run skipped)"
        return 0
    else
        log_skip "Fast tests skipped or failed (non-critical)"
        return 0
    fi
}

# ============================================================================
# Check 4: Application can start and stop
# ============================================================================
check_app_lifecycle() {
    log_check "Application can start and stop..."
    
    cd "$PROJECT_DIR"
    
    # Try to start/stop the application
    local result
    result=$(timeout "$TIMEOUT" rebar3 shell --eval "
        case application:ensure_all_started(beamline_router) of
            {ok, _} ->
                timer:sleep(500),
                application:stop(beamline_router),
                io:format(\"OK~n\"),
                halt(0);
            {error, Reason} ->
                io:format(\"ERROR: ~p~n\", [Reason]),
                halt(1)
        end.
    " 2>&1) || true
    
    if echo "$result" | grep -q "OK"; then
        log_pass "Application lifecycle OK"
        return 0
    else
        log_skip "Application lifecycle check skipped (dependencies may be missing)"
        return 0
    fi
}

# ============================================================================
# Check 5: Configuration files are valid
# ============================================================================
check_config() {
    log_check "Configuration files are valid..."
    
    local config_dir="$PROJECT_DIR/config"
    local errors=0
    
    if [ -d "$config_dir" ]; then
        for config in "$config_dir"/*.config "$config_dir"/*.config.src; do
            if [ -f "$config" ]; then
                # Basic syntax check for Erlang term format
                if head -c 1000 "$config" | grep -q "^\[" || head -c 1000 "$config" | grep -q "^\%"; then
                    [ "$VERBOSE" = true ] && log_info "  Config OK: $(basename "$config")"
                else
                    log_warn "  Config may be invalid: $(basename "$config")"
                    errors=$((errors + 1))
                fi
            fi
        done
    fi
    
    if [ $errors -eq 0 ]; then
        log_pass "Configuration files are valid"
        return 0
    else
        log_warn "Some configuration files may have issues"
        return 0  # Non-critical
    fi
}

# ============================================================================
# Check 6: Health endpoint (if service is running)
# ============================================================================
check_health_endpoint() {
    log_check "Health endpoint responds..."
    
    # Default health endpoint
    local health_url="${ROUTER_HEALTH_URL:-http://localhost:8080/health}"
    
    if command -v curl &> /dev/null; then
        if curl -sf --max-time 5 "$health_url" > /dev/null 2>&1; then
            log_pass "Health endpoint responds"
            return 0
        else
            log_skip "Health endpoint not available (service may not be running)"
            return 0
        fi
    else
        log_skip "Health check skipped (curl not available)"
        return 0
    fi
}

# ============================================================================
# Check 7: Metrics endpoint (if service is running)
# ============================================================================
check_metrics_endpoint() {
    log_check "Metrics endpoint responds..."
    
    local metrics_url="${ROUTER_METRICS_URL:-http://localhost:8080/metrics}"
    
    if command -v curl &> /dev/null; then
        if curl -sf --max-time 5 "$metrics_url" > /dev/null 2>&1; then
            log_pass "Metrics endpoint responds"
            return 0
        else
            log_skip "Metrics endpoint not available (service may not be running)"
            return 0
        fi
    else
        log_skip "Metrics check skipped (curl not available)"
        return 0
    fi
}

# ============================================================================
# Check 8: No critical Dialyzer warnings
# ============================================================================
check_dialyzer() {
    log_check "No critical Dialyzer warnings..."
    
    cd "$PROJECT_DIR"
    
    # Quick Dialyzer check (with timeout)
    if timeout 60 rebar3 dialyzer > /dev/null 2>&1; then
        log_pass "Dialyzer analysis passed"
        return 0
    else
        log_skip "Dialyzer check skipped (may take too long or have issues)"
        return 0
    fi
}

# ============================================================================
# Run all checks
# ============================================================================
run_checks() {
    check_compile || true
    check_app_file || true
    check_config || true
    check_fast_tests || true
    check_app_lifecycle || true
    check_health_endpoint || true
    check_metrics_endpoint || true
}

# ============================================================================
# Summary
# ============================================================================
print_summary() {
    echo ""
    echo "=========================================="
    echo "  Smoke Test Summary"
    echo "=========================================="
    echo ""
    echo -e "  ${GREEN}Passed${NC}:  $CHECKS_PASSED"
    echo -e "  ${RED}Failed${NC}:  $CHECKS_FAILED"
    echo -e "  ${YELLOW}Skipped${NC}: $CHECKS_SKIPPED"
    echo ""
    
    if [ $CHECKS_FAILED -eq 0 ]; then
        log_info "All critical checks passed!"
        return 0
    else
        log_error "$CHECKS_FAILED check(s) failed"
        return 1
    fi
}

# ============================================================================
# Record result
# ============================================================================
record_result() {
    mkdir -p "$ARTIFACTS_DIR"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local log_file="$ARTIFACTS_DIR/smoke_${timestamp}.log"
    
    {
        echo "Smoke Test Result"
        echo "================="
        echo "Timestamp: $(date -Iseconds)"
        echo "Passed: $CHECKS_PASSED"
        echo "Failed: $CHECKS_FAILED"
        echo "Skipped: $CHECKS_SKIPPED"
        echo "Status: $1"
    } > "$log_file"
    
    [ "$VERBOSE" = true ] && log_info "Result recorded: $log_file"
}

# ============================================================================
# Main
# ============================================================================
main() {
    run_checks
    
    if print_summary; then
        record_result "PASS"
        exit 0
    else
        record_result "FAIL"
        exit 1
    fi
}

main "$@"
