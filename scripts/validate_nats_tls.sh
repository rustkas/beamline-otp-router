#!/bin/bash
set -e

# Configuration
REPO_ROOT="/home/rustkas/aigroup/apps/otp/router"
ARTIFACTS_DIR="$REPO_ROOT/_artifacts"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
LOGFILE="$ARTIFACTS_DIR/nats_tls_validation_$TIMESTAMP.log"

echo "=== NATS TLS Configuration Validation ===" | tee -a "$LOGFILE"

# 1. Ensure certificates exist
if [ ! -f "$REPO_ROOT/_artifacts/certs/ca-cert.pem" ]; then
    echo "Certificates not found. Generating..."
    ./scripts/generate_certs.sh
fi

# 2. Restart NATS with TLS
echo "[1/3] Restarting NATS with TLS..." | tee -a "$LOGFILE"
./scripts/nats_stop.sh >> "$LOGFILE" 2>&1 || true
./scripts/nats_start.sh --tls >> "$LOGFILE" 2>&1

# 3. Run Router with TLS config
echo "[2/3] Running NATS connection test with TLS..." | tee -a "$LOGFILE"
# We need to use the TLS config
rebar3 as test ct \
    --suite router_nats_tls_validation_SUITE \
    --sys_config config/test_real_nats_tls.config \
    --readable=true 2>&1 | tee -a "$LOGFILE"

# 4. Check results
if grep -q "All 1 tests passed" "$LOGFILE" || grep -q "1 ok, 0 failed" "$LOGFILE"; then
    echo ""
    echo "✅ NATS TLS Validation SUCCESS!" | tee -a "$LOGFILE"
    exit 0
else
    echo ""
    echo "❌ NATS TLS Validation FAILED!" | tee -a "$LOGFILE"
    exit 1
fi
