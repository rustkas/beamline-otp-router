#!/bin/bash
# Chaos Testing Orchestration Script (T-CHAOS-01)
#
# Runs controlled chaos tests and collects evidence
#
# Usage:
#   ./scripts/chaos_test.sh [--dry-run]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ARTIFACTS_DIR="$PROJECT_DIR/_artifacts"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOG_FILE="$ARTIFACTS_DIR/chaos_test_${TIMESTAMP}.log"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

DRY_RUN=false

# Logging
log_info() { echo -e "${GREEN}[INFO]${NC} $1" | tee -a "$LOG_FILE"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1" | tee -a "$LOG_FILE"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"; }
log_step() { echo -e "${BLUE}[STEP]${NC} $1" | tee -a "$LOG_FILE"; }

# Parse arguments
for arg in "$@"; do
    case "$arg" in
        --dry-run)
            DRY_RUN=true
            ;;
        --help|-h)
            cat <<EOF
Chaos Testing Script for Beamline Router

Usage:
  $0 [options]

Options:
  --dry-run    Show what would be done without executing
  --help       Show this help message

Examples:
  $0                # Run full chaos test suite
  $0 --dry-run      # Dry-run mode
EOF
            exit 0
            ;;
    esac
done

main() {
    mkdir -p "$ARTIFACTS_DIR"
    
    echo ""
    echo "==========================================" | tee -a "$LOG_FILE"
    echo "  Beamline Router Chaos Testing" | tee -a "$LOG_FILE"
    echo "==========================================" | tee -a "$LOG_FILE"
    echo "" | tee -a "$LOG_FILE"
    
    if [ "$DRY_RUN" = true ]; then
        log_warn "DRY-RUN MODE: No actual tests will run"
        echo ""
    fi
    
    log_step "Step 1: Pre-flight checks"
    preflight_checks
    
    log_step "Step 2: Start NATS server"
    start_nats
    
    log_step "Step 3: Run chaos test suite"
    run_chaos_tests
    
    log_step "Step 4: Collect evidence"
    collect_evidence
    
    log_step "Step 5: Generate report"
    generate_report
    
    echo ""
    log_info "==========================================" 
    log_info "  Chaos tests completed successfully!"
    log_info "  Log: $LOG_FILE"
    log_info "=========================================="
}

preflight_checks() {
    log_info "Checking prerequisites..."
    
    # Check rebar3
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found"
        exit 1
    fi
    
    # Check NATS server binary
    if ! command -v nats-server &> /dev/null; then
        log_warn "nats-server not in PATH, will use scripts/nats_start.sh"
    fi
    
    log_info "✓ Pre-flight checks passed"
}

start_nats() {
    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would start NATS server"
        return
    fi
    
    log_info "Starting NATS server..."
    
    if [ -x "$SCRIPT_DIR/nats_start.sh" ]; then
        "$SCRIPT_DIR/nats_start.sh" >> "$LOG_FILE" 2>&1
        sleep 2
        log_info "✓ NATS server started"
    else
        log_warn "nats_start.sh not found, assuming NATS is running"
    fi
}

run_chaos_tests() {
    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would run: rebar3 ct --suite=router_chaos_controlled_SUITE"
        return
    fi
    
    log_info "Running chaos test suite..."
    
    cd "$PROJECT_DIR"
    
    # Run chaos tests with extended timeout
    if rebar3 ct \
        --suite=router_chaos_controlled_SUITE \
        --readable=false \
        >> "$LOG_FILE" 2>&1; then
        log_info "✓ Chaos tests PASSED"
        return 0
    else
        log_error "✗ Chaos tests FAILED (see log for details)"
        return 1
    fi
}

collect_evidence() {
    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would collect test evidence"
        return
    fi
    
    log_info "Collecting test evidence..."
    
    # Copy CT logs
    if [ -d "$PROJECT_DIR/_build/test/logs" ]; then
        LATEST_CT_LOG=$(find "$PROJECT_DIR/_build/test/logs" -name "ct_run.*" -type d | sort | tail -n 1)
        if [ -n "$LATEST_CT_LOG" ]; then
            cp -r "$LATEST_CT_LOG" "$ARTIFACTS_DIR/chaos_ct_logs_${TIMESTAMP}/" || true
            log_info "✓ CT logs copied to artifacts"
        fi
    fi
    
    # Collect NATS logs
    if [ -f "$ARTIFACTS_DIR/nats_latest.log" ]; then
        cp "$ARTIFACTS_DIR/nats_latest.log" "$ARTIFACTS_DIR/chaos_nats_${TIMESTAMP}.log" || true
        log_info "✓ NATS logs collected"
    fi
    
    # Collect Router logs
    if [ -d "$PROJECT_DIR/logs" ]; then
        cp -r "$PROJECT_DIR/logs" "$ARTIFACTS_DIR/chaos_router_logs_${TIMESTAMP}/" || true
        log_info "✓ Router logs collected"
    fi
}

generate_report() {
    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would generate test report"
        return
    fi
    
    log_info "Generating chaos test report..."
    
    REPORT_FILE="$ARTIFACTS_DIR/chaos_report_${TIMESTAMP}.md"
    
    cat > "$REPORT_FILE" <<EOF
# Chaos Test Report

**Timestamp**: $(date -Iseconds)
**Suite**: router_chaos_controlled_SUITE
**Duration**: N/A (see log)

## Test Results

See CT logs for detailed results: 
\`_artifacts/chaos_ct_logs_${TIMESTAMP}/\`

## Recovery SLO

Target: < 30 seconds to operational state

- NATS kill recovery: See test output
- Router supervisor kill recovery: See test output

## Evidence

- Main log: \`chaos_test_${TIMESTAMP}.log\`
- CT logs: \`chaos_ct_logs_${TIMESTAMP}/\`
- NATS logs: \`chaos_nats_${TIMESTAMP}.log\`
- Router logs: \`chaos_router_logs_${TIMESTAMP}/\`

## Conclusion

✓ Chaos tests completed
EOF
    
    log_info "✓ Report generated: $REPORT_FILE"
}

main "$@"
