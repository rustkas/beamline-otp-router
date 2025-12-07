#!/bin/bash
# Quick script to start Router and check metrics endpoint
# Used for CP2 System Validation Run

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

echo "=== Router CP2 Metrics Check ==="
echo ""

# Check if Router is already running
if curl -fsS http://localhost:9001/metrics >/dev/null 2>&1; then
    echo "✅ Router metrics endpoint is already accessible"
    echo ""
    echo "Checking CP2 metrics..."
    curl -fsS http://localhost:9001/metrics | grep -E "router_(jetstream|idem|acl|circuit_breaker|rate_limit)" | head -20 || echo "⚠️  No CP2-specific metrics found (may be normal if no traffic)"
    exit 0
fi

echo "Router not running. Starting Router in background..."
echo ""

# Start Router in background using rebar3 shell
# Note: This is a simplified approach - in production, use proper service management
rebar3 shell <<EOF &
application:ensure_all_started(beamline_router),
timer:sleep(2000),
router_metrics_http:start(),
timer:sleep(1000),
halt().
EOF

ROUTER_PID=$!
echo "Router started (PID: $ROUTER_PID)"
echo "Waiting for Router to initialize..."
sleep 5

# Check if Router started successfully
if ! kill -0 $ROUTER_PID 2>/dev/null; then
    echo "❌ Router process died"
    exit 1
fi

# Check metrics endpoint
echo ""
echo "Checking metrics endpoint..."
if curl -fsS http://localhost:9001/metrics >/dev/null 2>&1; then
    echo "✅ Router metrics endpoint is accessible"
    echo ""
    echo "CP2 metrics found:"
    curl -fsS http://localhost:9001/metrics | grep -E "router_(jetstream|idem|acl|circuit_breaker|rate_limit)" | head -20 || echo "⚠️  No CP2-specific metrics found (may be normal if no traffic)"
    echo ""
    echo "Full metrics endpoint response (first 50 lines):"
    curl -fsS http://localhost:9001/metrics | head -50
    echo ""
    echo "✅ Metrics check PASSED"
    
    # Cleanup
    echo "Stopping Router..."
    kill $ROUTER_PID 2>/dev/null || true
    wait $ROUTER_PID 2>/dev/null || true
    exit 0
else
    echo "❌ Router metrics endpoint not accessible"
    echo "Stopping Router..."
    kill $ROUTER_PID 2>/dev/null || true
    wait $ROUTER_PID 2>/dev/null || true
    exit 1
fi

