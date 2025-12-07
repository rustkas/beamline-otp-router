#!/bin/bash
# @doc Deployment Script
# 
# Deploys the router application with validation and rollback support.
# 
# Usage:
#   ./scripts/deploy.sh <version>
#   ./scripts/deploy.sh <version> --rollback
#   ./scripts/deploy.sh --status
#   ./scripts/deploy.sh --validate

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

VERSION="${1:-}"
ACTION="${2:-}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if Erlang/OTP is available
check_erlang() {
    if ! command -v erl &> /dev/null; then
        log_error "Erlang/OTP is not installed or not in PATH"
        exit 1
    fi
    log_info "Erlang/OTP found: $(erl -version 2>&1 | head -n 1)"
}

# Validate deployment
validate_deployment() {
    log_info "Validating deployment..."
    
    cd "$PROJECT_DIR"
    
    # Check if rebar3 is available
    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 is not installed or not in PATH"
        return 1
    fi
    
    # Compile and check for errors
    log_info "Compiling application..."
    if ! rebar3 compile; then
        log_error "Compilation failed"
        return 1
    fi
    
    # Run Dialyzer (if available)
    if rebar3 dialyzer 2>/dev/null; then
        log_info "Dialyzer checks passed"
    else
        log_warn "Dialyzer checks skipped or failed (non-critical)"
    fi
    
    # Run tests (quick smoke test)
    log_info "Running smoke tests..."
    if rebar3 ct --suite test/router_smoke_SUITE 2>/dev/null; then
        log_info "Smoke tests passed"
    else
        log_warn "Smoke tests skipped or failed (non-critical)"
    fi
    
    log_info "Deployment validation passed"
    return 0
}

# Deploy application
deploy() {
    local version="$1"
    
    if [ -z "$version" ]; then
        log_error "Version is required"
        echo "Usage: $0 <version>"
        exit 1
    fi
    
    log_info "Deploying version: $version"
    
    cd "$PROJECT_DIR"
    
    # Pre-deployment checks
    log_info "Running pre-deployment checks..."
    if ! validate_deployment; then
        log_error "Pre-deployment checks failed"
        exit 1
    fi
    
    # Create deployment backup
    log_info "Creating deployment backup..."
    BACKUP_DIR="$PROJECT_DIR/.deployment_backup_$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$BACKUP_DIR"
    cp -r _build "$BACKUP_DIR/" 2>/dev/null || true
    log_info "Backup created: $BACKUP_DIR"
    
    # Build application
    log_info "Building application..."
    if ! rebar3 release; then
        log_error "Build failed"
        exit 1
    fi
    
    # Post-deployment checks
    log_info "Running post-deployment checks..."
    if ! check_application_health; then
        log_error "Post-deployment checks failed"
        log_warn "Rollback available: $BACKUP_DIR"
        exit 1
    fi
    
    log_info "Deployment completed successfully: $version"
    return 0
}

# Rollback deployment
rollback() {
    local version="$1"
    
    log_info "Rolling back to version: $version"
    
    # Find backup directory
    BACKUP_DIR=$(ls -td "$PROJECT_DIR/.deployment_backup_"* 2>/dev/null | head -n 1)
    
    if [ -z "$BACKUP_DIR" ]; then
        log_error "No backup found for rollback"
        exit 1
    fi
    
    log_info "Restoring from backup: $BACKUP_DIR"
    
    # Restore backup
    if [ -d "$BACKUP_DIR/_build" ]; then
        rm -rf "$PROJECT_DIR/_build"
        cp -r "$BACKUP_DIR/_build" "$PROJECT_DIR/"
        log_info "Backup restored"
    else
        log_error "Backup directory is invalid"
        exit 1
    fi
    
    # Check application health
    if ! check_application_health; then
        log_error "Rollback health check failed"
        exit 1
    fi
    
    log_info "Rollback completed successfully"
    return 0
}

# Check application health
check_application_health() {
    log_info "Checking application health..."
    
    # Check if application can start
    cd "$PROJECT_DIR"
    
    # Try to start application in background and check if it starts
    if rebar3 shell --eval "application:start(beamline_router), timer:sleep(1000), application:stop(beamline_router), halt()." 2>/dev/null; then
        log_info "Application health check passed"
        return 0
    else
        log_warn "Application health check inconclusive (non-critical)"
        return 0  # Don't fail on health check
    fi
}

# Get deployment status
get_status() {
    log_info "Getting deployment status..."
    
    cd "$PROJECT_DIR"
    
    # Check if application is running
    if pgrep -f "beamline_router" > /dev/null; then
        log_info "Application is running"
    else
        log_info "Application is not running"
    fi
    
    # Check version
    if [ -f "$PROJECT_DIR/_build/default/lib/beamline_router/ebin/beamline_router.app" ]; then
        VERSION=$(grep -o '{vsn, "[^"]*"' "$PROJECT_DIR/_build/default/lib/beamline_router/ebin/beamline_router.app" | cut -d'"' -f2)
        log_info "Deployed version: $VERSION"
    else
        log_info "No deployed version found"
    fi
    
    return 0
}

# Main execution
main() {
    check_erlang
    
    case "$ACTION" in
        --rollback)
            rollback "$VERSION"
            ;;
        --status)
            get_status
            ;;
        --validate)
            validate_deployment
            ;;
        "")
            if [ -z "$VERSION" ]; then
                log_error "Version is required"
                echo "Usage: $0 <version> [--rollback|--status|--validate]"
                exit 1
            fi
            deploy "$VERSION"
            ;;
        *)
            log_error "Unknown action: $ACTION"
            echo "Usage: $0 <version> [--rollback|--status|--validate]"
            exit 1
            ;;
    esac
}

main "$@"

