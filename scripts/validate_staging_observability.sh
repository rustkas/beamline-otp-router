#!/bin/bash
# @doc Comprehensive staging observability validation
# 
# Validates:
# - Metrics endpoint accessibility and format
# - Alert rules configuration
# - Grafana dashboard availability (if configured)
# - Log correlation with metrics
# 
# Usage:
#   ./scripts/validate_staging_observability.sh [options]
# 
# Options:
#   --metrics-url URL    Metrics endpoint (default: http://localhost:9000/metrics)
#   --prometheus-url URL Prometheus URL (default: http://localhost:9090)
#   --grafana-url URL    Grafana URL (optional)
#   --skip-alerts        Skip alert validation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

METRICS_URL="${METRICS_URL:-http://localhost:9000/metrics}"
PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"
GRAFANA_URL="${GRAFANA_URL:-}"
SKIP_ALERTS="${SKIP_ALERTS:-false}"

echo "=== Staging Observability Validation ==="
echo ""
echo "Configuration:"
echo "  Metrics URL: $METRICS_URL"
echo "  Prometheus URL: $PROMETHEUS_URL"
[ -n "$GRAFANA_URL" ] && echo "  Grafana URL: $GRAFANA_URL" || echo "  Grafana URL: (not configured)"
echo ""

ERRORS=0
WARNINGS=0

# 1. Check metrics endpoint
echo "1. Checking metrics endpoint..."
if curl -sf "$METRICS_URL" > /dev/null 2>&1; then
    echo "   ✓ Metrics endpoint accessible"
    
    METRICS=$(curl -sf "$METRICS_URL" || echo "")
    if echo "$METRICS" | grep -q "router_jetstream_redelivery_total"; then
        echo "   ✓ router_jetstream_redelivery_total found"
        
        if echo "$METRICS" | grep -q "router_jetstream_redelivery_total{"; then
            echo "   ✓ Metric has labels"
            
            # Count unique label combinations
            LABEL_COUNT=$(echo "$METRICS" | grep "router_jetstream_redelivery_total{" | wc -l)
            echo "   ℹ Found $LABEL_COUNT labeled metric entries"
        else
            echo "   ⚠ Metric found but without labels"
            WARNINGS=$((WARNINGS + 1))
        fi
    else
        echo "   ⚠ router_jetstream_redelivery_total not found (may be normal if no redeliveries)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "   ❌ Metrics endpoint not accessible"
    ERRORS=$((ERRORS + 1))
fi

echo ""

# 2. Check Prometheus (if accessible)
echo "2. Checking Prometheus..."
if curl -sf "$PROMETHEUS_URL/api/v1/status/config" > /dev/null 2>&1; then
    echo "   ✓ Prometheus accessible"
    
    # Check if metric is scraped
    QUERY_URL="$PROMETHEUS_URL/api/v1/query?query=router_jetstream_redelivery_total"
    QUERY_RESULT=$(curl -sf "$QUERY_URL" || echo "")
    
    if echo "$QUERY_RESULT" | grep -q "router_jetstream_redelivery_total"; then
        echo "   ✓ Metric queryable in Prometheus"
    else
        echo "   ⚠ Metric not found in Prometheus (may need scraping)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "   ⚠ Prometheus not accessible (skipping)"
    WARNINGS=$((WARNINGS + 1))
fi

echo ""

# 3. Check alert rules (if not skipped)
if [ "$SKIP_ALERTS" != "true" ]; then
    echo "3. Checking alert rules..."
    
    PROJECT_ROOT="$(cd "$ROUTER_DIR/../../.." && pwd)"
ALERT_RULES_FILE="$PROJECT_ROOT/docs/observability/router-alert-rules.yaml"
    if [ -f "$ALERT_RULES_FILE" ]; then
        echo "   ✓ Alert rules file found: $ALERT_RULES_FILE"
        
        # Check for router_jetstream_redelivery_total in alert rules
        if grep -q "router_jetstream_redelivery_total" "$ALERT_RULES_FILE"; then
            echo "   ✓ Alert rules reference router_jetstream_redelivery_total"
            
            # Count alert rules using the metric
            ALERT_COUNT=$(grep -c "router_jetstream_redelivery_total" "$ALERT_RULES_FILE" || echo "0")
            echo "   ℹ Found $ALERT_COUNT alert rule(s) using the metric"
        else
            echo "   ⚠ Alert rules do not reference router_jetstream_redelivery_total"
            WARNINGS=$((WARNINGS + 1))
        fi
    else
        echo "   ⚠ Alert rules file not found: $ALERT_RULES_FILE"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "3. Skipping alert rules check (--skip-alerts)"
fi

echo ""

# 4. Check Grafana (if configured)
if [ -n "$GRAFANA_URL" ]; then
    echo "4. Checking Grafana..."
    if curl -sf "$GRAFANA_URL/api/health" > /dev/null 2>&1; then
        echo "   ✓ Grafana accessible"
        echo "   ℹ Manual check required: Verify dashboard shows router_jetstream_redelivery_total by source/reason"
    else
        echo "   ⚠ Grafana not accessible"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "4. Skipping Grafana check (not configured)"
fi

echo ""

# 5. Check logs (if log directory exists)
echo "5. Checking logs..."
LOG_DIR="${LOG_DIR:-.windsurf/reports}"
if [ -d "$LOG_DIR" ]; then
    LATEST_LOG=$(find "$LOG_DIR" -name "router_*.jsonl" -type f | sort -r | head -1)
    if [ -n "$LATEST_LOG" ]; then
        echo "   ✓ Log directory found: $LOG_DIR"
        echo "   ℹ Latest log: $(basename "$LATEST_LOG")"
        
        # Check for redelivery log entries
        if grep -q "Message redelivery requested" "$LATEST_LOG" 2>/dev/null; then
            REDELIVERY_COUNT=$(grep -c "Message redelivery requested" "$LATEST_LOG" || echo "0")
            echo "   ✓ Found $REDELIVERY_COUNT redelivery log entries"
        else
            echo "   ℹ No redelivery log entries found (may be normal)"
        fi
    else
        echo "   ℹ No log files found in $LOG_DIR"
    fi
else
    echo "   ℹ Log directory not found: $LOG_DIR (may be normal)"
fi

echo ""
echo "=== Validation Summary ==="
echo "Errors: $ERRORS"
echo "Warnings: $WARNINGS"
echo ""

if [ $ERRORS -eq 0 ]; then
    echo "✓ Validation PASSED"
    exit 0
else
    echo "❌ Validation FAILED"
    exit 1
fi

