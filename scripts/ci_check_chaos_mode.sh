#!/bin/bash
# scripts/ci_check_chaos_mode.sh
#
# CI script to enforce chaos tests ran in Docker mode.
# Fails if tests ran in mock mode without explicit CHAOS_MOCK_ALLOWED=true.
#
# Usage:
#   make test-chaos 2>&1 | tee test_output.log
#   ./scripts/ci_check_chaos_mode.sh test_output.log
#
# Environment variables:
#   CHAOS_MOCK_ALLOWED=true - Explicitly allow mock mode (degrades coverage)
#
# Exit codes:
#   0 - Tests ran in Docker mode, or mock was explicitly allowed
#   1 - Tests ran in mock mode without explicit permission

set -e

LOG_FILE="${1:-_build/test/logs/ct_run.*/ct_log.html}"
MOCK_PATTERN="CHAOS TESTS RUNNING IN MOCK MODE"
DOCKER_PATTERN="CHAOS TESTS RUNNING IN DOCKER MODE"

echo "════════════════════════════════════════════════════════════════"
echo " CI Chaos Mode Enforcement Check"
echo "════════════════════════════════════════════════════════════════"

# Check if log file exists
if [ ! -f "$LOG_FILE" ] && [ "$LOG_FILE" == "_build/test/logs/ct_run.*/ct_log.html" ]; then
    # Try to find the latest CT log
    LOG_FILE=$(ls -t _build/test/logs/ct_run.*/ct_log.html 2>/dev/null | head -1 || true)
fi

if [ -z "$LOG_FILE" ] || [ ! -f "$LOG_FILE" ]; then
    echo "⚠️  No test log file found. Skipping chaos mode check."
    echo "    Looked for: $1 or _build/test/logs/ct_run.*/ct_log.html"
    exit 0
fi

echo "📄 Checking log file: $LOG_FILE"

# Check for mock mode indicator
if grep -q "$MOCK_PATTERN" "$LOG_FILE"; then
    echo ""
    echo "🔍 Detected: Tests ran in MOCK MODE"
    
    if [ "$CHAOS_MOCK_ALLOWED" == "true" ]; then
        echo ""
        echo "╔════════════════════════════════════════════════════════════════╗"
        echo "║ ⚠️  CHAOS TESTS RAN IN MOCK MODE (EXPLICITLY ALLOWED)          ║"
        echo "║                                                                ║"
        echo "║ CHAOS_MOCK_ALLOWED=true was set.                              ║"
        echo "║ Test coverage is DEGRADED - no real Docker/NATS interactions. ║"
        echo "║                                                                ║"
        echo "║ For full coverage, run with CHAOS_REQUIRE_DOCKER=true         ║"
        echo "╚════════════════════════════════════════════════════════════════╝"
        echo ""
        exit 0
    else
        echo ""
        echo "╔════════════════════════════════════════════════════════════════╗"
        echo "║ ❌ CI FAILURE: CHAOS TESTS RAN IN MOCK MODE                    ║"
        echo "║                                                                ║"
        echo "║ Mock mode does NOT test real network/Docker interactions.     ║"
        echo "║                                                                ║"
        echo "║ To fix:                                                        ║"
        echo "║   1. Ensure Docker and NATS container are running             ║"
        echo "║   2. Use: CHAOS_REQUIRE_DOCKER=true make test-chaos           ║"
        echo "║                                                                ║"
        echo "║ To explicitly allow mock mode (degrades coverage):            ║"
        echo "║   CHAOS_MOCK_ALLOWED=true make test-chaos                     ║"
        echo "╚════════════════════════════════════════════════════════════════╝"
        echo ""
        exit 1
    fi
elif grep -q "$DOCKER_PATTERN" "$LOG_FILE"; then
    echo ""
    echo "╔════════════════════════════════════════════════════════════════╗"
    echo "║ ✅ CHAOS TESTS RAN IN DOCKER MODE                              ║"
    echo "║                                                                ║"
    echo "║ Full integration testing with real NATS container.            ║"
    echo "╚════════════════════════════════════════════════════════════════╝"
    echo ""
    exit 0
else
    echo "⚠️  No chaos mode indicator found in logs."
    echo "    This is expected if chaos tests were not run."
    exit 0
fi
