#!/bin/bash
# Configuration
REPO_ROOT="/home/rustkas/aigroup/apps/otp/router"
ARTIFACTS_DIR="$REPO_ROOT/_artifacts"
PIDFILE="$ARTIFACTS_DIR/nats.pid"

if [ ! -f "$PIDFILE" ]; then
    echo "No pidfile found at $PIDFILE. NATS might not be running or started manually."
    exit 0
fi

PID=$(cat "$PIDFILE")

echo "Stopping NATS (PID $PID)..."
if kill "$PID" 2>/dev/null; then
    # Wait for process to exit
    MAX_WAIT=10
    WAIT=0
    while [ $WAIT -lt $MAX_WAIT ]; do
        if ! kill -0 "$PID" 2>/dev/null; then
            echo "NATS stopped successfully."
            rm "$PIDFILE"
            exit 0
        fi
        sleep 1
        WAIT=$((WAIT + 1))
    done
    echo "NATS did not stop within $MAX_WAIT seconds. Force killing..."
    kill -9 "$PID" 2>/dev/null || true
    rm "$PIDFILE"
else
    echo "Process $PID not found. Removing stale pidfile."
    rm "$PIDFILE"
fi

# Cleanup check
if ss -tln | grep -qE ":4222 |:8222 "; then
    echo "Warning: Ports 4222 or 8222 are still in use."
fi
