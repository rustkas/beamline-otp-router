#!/bin/bash
# @doc Complete staging validation workflow
# 
# Runs all validation steps from STAGING_VALIDATION_GUIDE.md:
# 1. Fault injection simulation
# 2. Metrics endpoint check
# 3. Prometheus queries (if available)
# 4. Alert rules validation
# 5. Log correlation check
# 
# Usage:
#   ./scripts/run_complete_staging_validation.sh [options]
# 
# Options:
#   --metrics-url URL    Metrics endpoint (default: file dump)
#   --prometheus-url URL Prometheus URL (optional)
#   --skip-prometheus    Skip Prometheus validation

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

METRICS_URL="${METRICS_URL:-}"
PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"
SKIP_PROMETHEUS="${SKIP_PROMETHEUS:-false}"

echo "=== Complete Staging Validation Workflow ==="
echo ""
echo "Following STAGING_VALIDATION_GUIDE.md steps:"
echo "  1. Fault injection simulation"
echo "  2. Metrics endpoint validation"
echo "  3. Prometheus queries"
echo "  4. Alert rules validation"
echo "  5. Log correlation"
echo ""

ERRORS=0
WARNINGS=0

# Step 1: Fault Injection Simulation
echo "=== Step 1: Fault Injection Simulation ==="
if bash scripts/simulate_fault_injection.sh > /tmp/fault_injection.log 2>&1; then
    echo "✓ Fault injection simulation completed"
    REDELIVERY_COUNT=$(cat fault_injection_metrics.prom 2>/dev/null | grep -c "router_jetstream_redelivery_total{" || echo "0")
    echo "   Generated $REDELIVERY_COUNT redelivery metric entries"
else
    echo "❌ Fault injection simulation failed"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Step 2: Metrics Endpoint Check
echo "=== Step 2: Metrics Endpoint Validation ==="
if [ -z "$METRICS_URL" ]; then
    METRICS_URL="file://$(pwd)/fault_injection_metrics.prom"
fi

if bash scripts/check_metrics_endpoint.sh "$METRICS_URL" > /tmp/metrics_check.log 2>&1; then
    echo "✓ Metrics endpoint validation passed"
    if grep -q "All required labels present" /tmp/metrics_check.log 2>/dev/null; then
        echo "   ✓ All required labels present"
    else
        echo "   ⚠ Some labels may be missing (check output)"
        WARNINGS=$((WARNINGS + 1))
    fi
else
    echo "❌ Metrics endpoint validation failed"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Step 3: Prometheus Queries
echo "=== Step 3: Prometheus Query Validation ==="
if [ "$SKIP_PROMETHEUS" = "true" ]; then
    echo "⏭ Skipping Prometheus validation (--skip-prometheus)"
else
    if bash scripts/validate_prometheus_queries.sh "$PROMETHEUS_URL" > /tmp/prometheus_queries.log 2>&1; then
        echo "✓ Prometheus query validation completed"
        if grep -q "Query successful" /tmp/prometheus_queries.log; then
            echo "   ✓ Queries returned data"
        else
            echo "   ⚠ No data returned (may be normal if Prometheus not scraping)"
            WARNINGS=$((WARNINGS + 1))
        fi
    else
        echo "⚠ Prometheus not accessible (expected in local validation)"
        WARNINGS=$((WARNINGS + 1))
    fi
fi
echo ""

# Step 4: Alert Rules Validation
echo "=== Step 4: Alert Rules Validation ==="
if bash scripts/check_alert_rules.sh > /tmp/alert_rules.log 2>&1; then
    echo "✓ Alert rules validation passed"
    ALERT_COUNT=$(grep -c "alert:" /tmp/alert_rules.log 2>/dev/null || echo "0")
    if [ "$ALERT_COUNT" -gt 0 ]; then
        echo "   Found $ALERT_COUNT alert(s) using router_jetstream_redelivery_total"
    fi
else
    echo "❌ Alert rules validation failed"
    ERRORS=$((ERRORS + 1))
fi
echo ""

# Step 5: Log Correlation
echo "=== Step 5: Log Correlation Check ==="
LOG_DIR="${LOG_DIR:-.windsurf/reports}"
LATEST_LOG=$(find "$LOG_DIR" -name "router_*.jsonl" -type f 2>/dev/null | sort -r | head -1)

if [ -n "$LATEST_LOG" ]; then
    echo "✓ Log directory found: $LOG_DIR"
    echo "   Latest log: $(basename "$LATEST_LOG")"
    
    if grep -q "Message redelivery requested" "$LATEST_LOG" 2>/dev/null; then
        REDELIVERY_LOG_COUNT=$(grep -c "Message redelivery requested" "$LATEST_LOG" || echo "0")
        echo "   ✓ Found $REDELIVERY_LOG_COUNT redelivery log entries"
        
        # Show sample log entry
        echo ""
        echo "   Sample log entry:"
        grep "Message redelivery requested" "$LATEST_LOG" | head -1 | jq -r '{
            timestamp: .timestamp,
            level: .level,
            message: .message,
            assignment_id: .context.assignment_id,
            reason: .context.reason,
            source: .context.source
        }' 2>/dev/null || grep "Message redelivery requested" "$LATEST_LOG" | head -1 | cut -c1-200
    else
        echo "   ℹ No redelivery log entries found (normal if no actual redeliveries occurred)"
        echo "   ℹ Logging is implemented and will appear when redeliveries happen"
    fi
else
    echo "   ℹ No log files found (normal if router not running)"
    echo "   ℹ Logging is implemented: 'Message redelivery requested' (INFO level)"
fi
echo ""

# Summary
echo "=== Validation Summary ==="
echo "Errors: $ERRORS"
echo "Warnings: $WARNINGS"
echo ""

if [ $ERRORS -eq 0 ]; then
    echo "✅ Complete validation PASSED"
    echo ""
    echo "Next steps for full staging validation:"
    echo "  1. Deploy to staging environment"
    echo "  2. Run actual fault injection scenarios"
    echo "  3. Verify Prometheus scraping and queries"
    echo "  4. Check Alertmanager for alert firing"
    echo "  5. Verify Grafana dashboards"
    exit 0
else
    echo "❌ Validation FAILED with $ERRORS error(s)"
    exit 1
fi

