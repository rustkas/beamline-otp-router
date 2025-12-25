#!/bin/bash
# @doc Rollback Script for Beamline Router
#
# Universal rollback script supporting multiple deployment scenarios:
# - Docker Compose
# - Systemd service
# - Kubernetes
# - Erlang release (local)
#
# Usage:
#   ./scripts/rollback.sh <target> [--dry-run]
#   ./scripts/rollback.sh previous
#   ./scripts/rollback.sh v1.2.3
#   ./scripts/rollback.sh abc1234
#   ./scripts/rollback.sh --help
#
# Environment variables:
#   ROLLBACK_MODE: docker|systemd|k8s|release (auto-detected if not set)
#   COMPOSE_FILE: path to docker-compose.yml (default: docker-compose.yml)
#   K8S_NAMESPACE: Kubernetes namespace (default: default)
#   K8S_DEPLOYMENT: Kubernetes deployment name (default: beamline-router)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ARTIFACTS_DIR="$PROJECT_DIR/_artifacts"

# Rollback settings
TARGET="${1:-}"
DRY_RUN=false
ROLLBACK_MODE="${ROLLBACK_MODE:-auto}"
COMPOSE_FILE="${COMPOSE_FILE:-docker-compose.yml}"
K8S_NAMESPACE="${K8S_NAMESPACE:-default}"
K8S_DEPLOYMENT="${K8S_DEPLOYMENT:-beamline-router}"

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
log_step() { echo -e "${BLUE}[STEP]${NC} $1"; }
log_dry() { echo -e "${YELLOW}[DRY-RUN]${NC} Would execute: $1"; }

# Execute or dry-run
execute() {
    if [ "$DRY_RUN" = true ]; then
        log_dry "$*"
    else
        "$@"
    fi
}

# Usage
show_help() {
    cat << EOF
Rollback Script for Beamline Router

Usage:
  $0 <target> [options]

Targets:
  previous        Roll back to the previous version/deployment
  <tag>           Roll back to a specific git tag (e.g., v1.2.3)
  <commit>        Roll back to a specific git commit

Options:
  --dry-run       Show what would be done without making changes
  --help          Show this help message

Environment Variables:
  ROLLBACK_MODE   Deployment mode: docker|systemd|k8s|release (auto-detected)
  COMPOSE_FILE    Docker Compose file path (default: docker-compose.yml)
  K8S_NAMESPACE   Kubernetes namespace (default: default)
  K8S_DEPLOYMENT  Kubernetes deployment name (default: beamline-router)

Examples:
  $0 previous --dry-run     # Dry-run rollback to previous version
  $0 v1.2.3                 # Rollback to tag v1.2.3
  $0 abc1234                # Rollback to commit abc1234

EOF
    exit 0
}

# Parse arguments
parse_args() {
    for arg in "$@"; do
        case "$arg" in
            --dry-run)
                DRY_RUN=true
                ;;
            --help|-h)
                show_help
                ;;
            *)
                if [ -z "$TARGET" ]; then
                    TARGET="$arg"
                fi
                ;;
        esac
    done

    if [ -z "$TARGET" ]; then
        log_error "Target is required"
        echo "Usage: $0 <target> [--dry-run]"
        echo "Run '$0 --help' for more information."
        exit 1
    fi
}

# Auto-detect deployment mode
detect_mode() {
    if [ "$ROLLBACK_MODE" != "auto" ]; then
        log_info "Using specified rollback mode: $ROLLBACK_MODE"
        return
    fi

    if [ -f "$PROJECT_DIR/$COMPOSE_FILE" ] && command -v docker-compose &> /dev/null; then
        ROLLBACK_MODE="docker"
        log_info "Detected deployment mode: Docker Compose"
    elif systemctl is-active --quiet beamline-router 2>/dev/null; then
        ROLLBACK_MODE="systemd"
        log_info "Detected deployment mode: Systemd"
    elif command -v kubectl &> /dev/null && kubectl get deployment "$K8S_DEPLOYMENT" -n "$K8S_NAMESPACE" &> /dev/null; then
        ROLLBACK_MODE="k8s"
        log_info "Detected deployment mode: Kubernetes"
    elif [ -d "$PROJECT_DIR/_build/default/rel/beamline_router" ]; then
        ROLLBACK_MODE="release"
        log_info "Detected deployment mode: Erlang Release"
    else
        log_warn "Could not auto-detect deployment mode, defaulting to 'release'"
        ROLLBACK_MODE="release"
    fi
}

# Resolve target to a specific version/commit
resolve_target() {
    log_step "Resolving target: $TARGET"

    if [ "$TARGET" = "previous" ]; then
        case "$ROLLBACK_MODE" in
            docker)
                # Get previous image tag from compose history
                log_info "Will use previous Docker image"
                ;;
            k8s)
                # Kubernetes handles 'previous' automatically with undo
                log_info "Will use Kubernetes rollback to previous revision"
                ;;
            systemd|release)
                # Find previous backup
                PREVIOUS_BACKUP=$(ls -td "$PROJECT_DIR/.deployment_backup_"* 2>/dev/null | head -n 1)
                if [ -z "$PREVIOUS_BACKUP" ]; then
                    log_error "No previous backup found"
                    exit 1
                fi
                log_info "Found previous backup: $PREVIOUS_BACKUP"
                TARGET_RESOLVED="$PREVIOUS_BACKUP"
                ;;
        esac
    else
        # Validate git ref exists
        cd "$PROJECT_DIR"
        if git rev-parse --verify "$TARGET" &> /dev/null; then
            TARGET_RESOLVED=$(git rev-parse --short "$TARGET")
            log_info "Resolved target to commit: $TARGET_RESOLVED"
        else
            log_error "Target '$TARGET' is not a valid git reference"
            exit 1
        fi
    fi
}

# ============================================================================
# Docker Compose Rollback
# ============================================================================
rollback_docker() {
    log_step "Executing Docker Compose rollback"
    
    cd "$PROJECT_DIR"

    if [ "$TARGET" = "previous" ]; then
        # Pull previous image and restart
        execute docker-compose -f "$COMPOSE_FILE" down
        execute docker-compose -f "$COMPOSE_FILE" pull
        execute docker-compose -f "$COMPOSE_FILE" up -d
    else
        # Checkout specific version and rebuild
        execute git checkout "$TARGET_RESOLVED"
        execute docker-compose -f "$COMPOSE_FILE" build
        execute docker-compose -f "$COMPOSE_FILE" down
        execute docker-compose -f "$COMPOSE_FILE" up -d
    fi
}

# ============================================================================
# Systemd Rollback
# ============================================================================
rollback_systemd() {
    log_step "Executing Systemd rollback"

    cd "$PROJECT_DIR"

    if [ "$TARGET" = "previous" ]; then
        # Restore from backup
        if [ -n "${TARGET_RESOLVED:-}" ] && [ -d "$TARGET_RESOLVED/_build" ]; then
            log_info "Restoring from backup: $TARGET_RESOLVED"
            execute sudo systemctl stop beamline-router
            execute rm -rf "$PROJECT_DIR/_build"
            execute cp -r "$TARGET_RESOLVED/_build" "$PROJECT_DIR/"
            execute sudo systemctl start beamline-router
        else
            log_error "No valid backup to restore"
            exit 1
        fi
    else
        # Checkout and rebuild
        execute git checkout "$TARGET_RESOLVED"
        execute sudo systemctl stop beamline-router
        execute rebar3 release
        execute sudo systemctl start beamline-router
    fi
}

# ============================================================================
# Kubernetes Rollback
# ============================================================================
rollback_k8s() {
    log_step "Executing Kubernetes rollback"

    if [ "$TARGET" = "previous" ]; then
        execute kubectl rollout undo deployment/"$K8S_DEPLOYMENT" -n "$K8S_NAMESPACE"
    else
        # Update image tag
        execute kubectl set image deployment/"$K8S_DEPLOYMENT" \
            beamline-router="beamline-router:$TARGET_RESOLVED" \
            -n "$K8S_NAMESPACE"
    fi

    # Wait for rollout
    if [ "$DRY_RUN" = false ]; then
        kubectl rollout status deployment/"$K8S_DEPLOYMENT" -n "$K8S_NAMESPACE" --timeout=300s
    fi
}

# ============================================================================
# Erlang Release Rollback
# ============================================================================
rollback_release() {
    log_step "Executing Erlang Release rollback"

    cd "$PROJECT_DIR"

    if [ "$TARGET" = "previous" ]; then
        # Restore from backup
        if [ -n "${TARGET_RESOLVED:-}" ] && [ -d "$TARGET_RESOLVED/_build" ]; then
            log_info "Restoring from backup: $TARGET_RESOLVED"
            execute rm -rf "$PROJECT_DIR/_build"
            execute cp -r "$TARGET_RESOLVED/_build" "$PROJECT_DIR/"
        else
            log_error "No valid backup to restore"
            exit 1
        fi
    else
        # Checkout and rebuild
        execute git checkout "$TARGET_RESOLVED"
        execute rebar3 release
    fi
}

# ============================================================================
# Post-rollback Smoke Test
# ============================================================================
run_smoke_test() {
    log_step "Running post-rollback smoke test"

    if [ "$DRY_RUN" = true ]; then
        log_dry "./scripts/smoke.sh"
        return 0
    fi

    if [ -x "$SCRIPT_DIR/smoke.sh" ]; then
        if "$SCRIPT_DIR/smoke.sh"; then
            log_info "Smoke test PASSED"
            return 0
        else
            log_error "Smoke test FAILED"
            return 1
        fi
    else
        log_warn "Smoke test script not found, skipping post-check"
        return 0
    fi
}

# ============================================================================
# Record rollback event
# ============================================================================
record_rollback() {
    mkdir -p "$ARTIFACTS_DIR"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local log_file="$ARTIFACTS_DIR/rollback_${timestamp}.log"

    {
        echo "Rollback Event"
        echo "=============="
        echo "Timestamp: $(date -Iseconds)"
        echo "Target: $TARGET"
        echo "Resolved: ${TARGET_RESOLVED:-N/A}"
        echo "Mode: $ROLLBACK_MODE"
        echo "Dry-run: $DRY_RUN"
        echo "Status: $1"
    } > "$log_file"

    log_info "Rollback recorded: $log_file"
}

# ============================================================================
# Main
# ============================================================================
main() {
    parse_args "$@"

    echo ""
    echo "=========================================="
    echo "  Beamline Router Rollback"
    echo "=========================================="
    echo ""

    if [ "$DRY_RUN" = true ]; then
        log_warn "DRY-RUN MODE: No changes will be made"
        echo ""
    fi

    detect_mode
    resolve_target

    echo ""
    log_info "Target: $TARGET"
    log_info "Mode: $ROLLBACK_MODE"
    log_info "Dry-run: $DRY_RUN"
    echo ""

    # Execute rollback based on mode
    case "$ROLLBACK_MODE" in
        docker)
            rollback_docker
            ;;
        systemd)
            rollback_systemd
            ;;
        k8s)
            rollback_k8s
            ;;
        release)
            rollback_release
            ;;
        *)
            log_error "Unknown rollback mode: $ROLLBACK_MODE"
            exit 1
            ;;
    esac

    echo ""

    # Run smoke test
    if run_smoke_test; then
        record_rollback "SUCCESS"
        echo ""
        log_info "=========================================="
        log_info "  Rollback completed successfully!"
        log_info "=========================================="
    else
        record_rollback "FAILED"
        echo ""
        log_error "=========================================="
        log_error "  Rollback completed but smoke test failed!"
        log_error "=========================================="
        exit 1
    fi
}

main "$@"
