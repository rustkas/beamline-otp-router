#!/bin/bash
# @doc Check router metrics endpoint for labeled metrics
# 
# Verifies:
# - Metrics endpoint is accessible
# - router_jetstream_redelivery_total appears with labels
# - Label format is correct (Prometheus format)
# 
# Usage:
#   ./scripts/check_metrics_endpoint.sh [metrics_url]
# 
# Default: http://localhost:9000/metrics

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

METRICS_URL="${1:-http://localhost:9000/metrics}"

echo "=== Checking Router Metrics Endpoint ==="
echo "URL: $METRICS_URL"
echo ""

# Check if endpoint is accessible
if ! curl -sf "$METRICS_URL" > /dev/null 2>&1; then
    echo "❌ ERROR: Metrics endpoint not accessible at $METRICS_URL"
    echo ""
    echo "Make sure router is running and metrics endpoint is enabled."
    echo "Default metrics endpoint: http://localhost:9000/metrics"
    exit 1
fi

echo "✓ Metrics endpoint is accessible"
echo ""

# Fetch metrics
METRICS_OUTPUT=$(curl -sf "$METRICS_URL" || echo "")

if [ -z "$METRICS_OUTPUT" ]; then
    echo "❌ ERROR: Empty response from metrics endpoint"
    exit 1
fi

# Check for router_jetstream_redelivery_total
if echo "$METRICS_OUTPUT" | grep -q "router_jetstream_redelivery_total"; then
    echo "✓ Found router_jetstream_redelivery_total metric"
    
    # Check if metric has labels
    if echo "$METRICS_OUTPUT" | grep -q "router_jetstream_redelivery_total{"; then
        echo "✓ Metric has labels (labeled format)"
        
        # Extract and display labeled metric lines
        echo ""
        echo "Labeled metric lines:"
        echo "$METRICS_OUTPUT" | grep "router_jetstream_redelivery_total{" | head -5 | while read -r line; do
            echo "  $line"
        done
        
        # Check for required labels
        REQUIRED_LABELS=("assignment_id" "request_id" "reason" "source")
        MISSING_LABELS=()
        
        for label in "${REQUIRED_LABELS[@]}"; do
            if ! echo "$METRICS_OUTPUT" | grep -q "router_jetstream_redelivery_total{" | grep -q "$label="; then
                MISSING_LABELS+=("$label")
            fi
        done
        
        if [ ${#MISSING_LABELS[@]} -eq 0 ]; then
            echo ""
            echo "✓ All required labels present: ${REQUIRED_LABELS[*]}"
        else
            echo ""
            echo "⚠ WARNING: Missing labels: ${MISSING_LABELS[*]}"
        fi
    else
        echo "⚠ WARNING: Metric found but without labels (unlabeled format)"
    fi
else
    echo "⚠ WARNING: router_jetstream_redelivery_total not found in metrics"
    echo "This may be normal if no redeliveries have occurred yet."
fi

# Check for HELP and TYPE lines
if echo "$METRICS_OUTPUT" | grep -q "# HELP router_jetstream_redelivery_total"; then
    echo "✓ HELP line present"
fi

if echo "$METRICS_OUTPUT" | grep -q "# TYPE router_jetstream_redelivery_total"; then
    echo "✓ TYPE line present"
fi

echo ""
echo "=== Metrics Endpoint Check Complete ==="
echo ""
echo "To trigger redelivery metrics, run fault injection scenarios:"
echo "  - ACK/NAK errors"
echo "  - Tenant validation failures"
echo "  - Backpressure scenarios"

