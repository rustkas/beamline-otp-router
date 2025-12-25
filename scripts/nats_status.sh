#!/bin/bash
# Configuration
REPO_ROOT="/home/rustkas/aigroup/apps/otp/router"
ARTIFACTS_DIR="$REPO_ROOT/_artifacts"
PIDFILE="$ARTIFACTS_DIR/nats.pid"

echo "=== NATS Status ==="

if [ -f "$PIDFILE" ]; then
    PID=$(cat "$PIDFILE")
    if kill -0 "$PID" 2>/dev/null; then
        echo "PID: $PID (Running)"
    else
        echo "PID: $PID (Not Running - Stale PID file)"
    fi
else
    echo "PID: (No pidfile)"
fi

echo "--- Ports ---"
ss -tlnp | grep -E "4222|8222" || echo "No active listeners on 4222 or 8222"

echo "--- Health ---"
curl -s http://localhost:8222/healthz || echo "Health check failed (cannot connect to 8222)"

echo "--- Varz (Brief) ---"
curl -s http://localhost:8222/varz | json_pp 2>/dev/null | head -n 20 || curl -s http://localhost:8222/varz | head -n 20 || echo "Varz failed"

echo "--- Latest Logs ---"
LATEST_LOG=$(ls -t "$ARTIFACTS_DIR"/nats_*.log 2>/dev/null | head -n 1)
if [ -n "$LATEST_LOG" ]; then
    echo "Log file: $LATEST_LOG"
    tail -n 10 "$LATEST_LOG"
else
    echo "No log files found."
fi
