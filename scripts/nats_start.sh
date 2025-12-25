#!/bin/bash
set -e

# Configuration
REPO_ROOT="/home/rustkas/aigroup/apps/otp/router"
NATS_BIN="$REPO_ROOT/nats-server-v2.10.7-linux-amd64/nats-server"
STORE_DIR="/tmp/nats-store"
ARTIFACTS_DIR="$REPO_ROOT/_artifacts"
PIDFILE="$ARTIFACTS_DIR/nats.pid"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOGFILE="$ARTIFACTS_DIR/nats_$TIMESTAMP.log"

mkdir -p "$ARTIFACTS_DIR"
mkdir -p "$STORE_DIR"

# Idempotency check
if [ -f "$PIDFILE" ]; then
    PID=$(cat "$PIDFILE")
    if kill -0 "$PID" 2>/dev/null; then
        echo "NATS is already running with PID $PID"
        exit 0
    else
        echo "Stale pidfile found, removing."
        rm "$PIDFILE"
    fi
fi

# Port check
for PORT in 4222 8222; do
    if ss -tln | grep -q ":$PORT "; then
        echo "Error: Port $PORT is already in use."
        exit 1
    fi
done

# Handle TLS
EXTRA_ARGS=""
if [[ "${1:-}" == "--tls" ]]; then
    echo "Using TLS configuration..."
    EXTRA_ARGS="-c $REPO_ROOT/config/nats_tls.conf"
else
    EXTRA_ARGS="--jetstream --store_dir $STORE_DIR -p 4222 -m 8222"
fi

echo "Starting NATS Server..."
nohup "$NATS_BIN" $EXTRA_ARGS > "$LOGFILE" 2>&1 &

PID=$!
echo "$PID" > "$PIDFILE"
echo "NATS started with PID $PID. Logs: $LOGFILE"

# Wait for health check
echo "Waiting for NATS to be healthy..."
MAX_ATTEMPTS=10
ATTEMPT=1
while [ $ATTEMPT -le $MAX_ATTEMPTS ]; do
    if curl -fsS http://localhost:8222/healthz > /dev/null 2>&1; then
        echo "NATS is healthy!"
        curl -s http://localhost:8222/healthz
        exit 0
    fi
    echo "Attempt $ATTEMPT/$MAX_ATTEMPTS: NATS not ready yet..."
    sleep 1
    ATTEMPT=$((ATTEMPT + 1))
done

echo "Error: NATS failed to become healthy within timeout."
tail -n 20 "$LOGFILE"
exit 1
