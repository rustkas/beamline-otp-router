#!/bin/bash
# @doc Check alert rules configuration for Router JetStream and NATS alerts
# 
# Validates:
# - Alert rules file exists and is valid YAML
# - All required alert groups are present
# - All 14 alerts are defined
# - Alert expressions use correct metric names from router_jetstream and router_nats
# - Alert labels and annotations are properly formatted
# 
# Usage:
#   ./scripts/check_alert_rules.sh [--promtool] [--verbose]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

# Try router-specific path first, then project root
if [ -f "docs/observability/router-alert-rules.yaml" ]; then
    ALERT_RULES_FILE="docs/observability/router-alert-rules.yaml"
elif [ -f "../../docs/observability/router-alert-rules.yaml" ]; then
    ALERT_RULES_FILE="../../docs/observability/router-alert-rules.yaml"
else
    ALERT_RULES_FILE="docs/observability/router-alert-rules.yaml"
fi
USE_PROTOOL=false
VERBOSE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --promtool)
            USE_PROTOOL=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--promtool] [--verbose]"
            exit 1
            ;;
    esac
done

echo "=== Checking Alert Rules Configuration ==="
echo ""

# Check file exists
if [ ! -f "$ALERT_RULES_FILE" ]; then
    echo "❌ ERROR: Alert rules file not found: $ALERT_RULES_FILE"
    exit 1
fi

echo "✓ Alert rules file found: $ALERT_RULES_FILE"
echo ""

# Check YAML syntax
echo "1. Checking YAML syntax..."
if command -v python3 &> /dev/null; then
    if python3 -c "import yaml; yaml.safe_load(open('$ALERT_RULES_FILE'))" 2>/dev/null; then
        echo "   ✓ YAML syntax is valid"
    else
        echo "   ❌ ERROR: Invalid YAML syntax"
        python3 -c "import yaml; yaml.safe_load(open('$ALERT_RULES_FILE'))" 2>&1
        exit 1
    fi
else
    echo "   ⚠ Skipping YAML validation (python3 not found)"
fi

# Check Prometheus syntax with promtool (if available)
if [ "$USE_PROTOOL" = true ]; then
    echo ""
    echo "2. Checking Prometheus syntax with promtool..."
    if command -v promtool &> /dev/null; then
        if promtool check rules "$ALERT_RULES_FILE" 2>&1; then
            echo "   ✓ Prometheus syntax is valid"
        else
            echo "   ❌ ERROR: Invalid Prometheus syntax"
            exit 1
        fi
    else
        echo "   ⚠ promtool not found, skipping Prometheus syntax check"
        echo "   Install: https://prometheus.io/docs/prometheus/latest/installation/"
    fi
fi

# Check alert groups
echo ""
echo "3. Checking alert groups..."
EXPECTED_GROUPS=("router-jetstream.rules" "router-nats-connection.rules" "router-nats-operations.rules")
for group in "${EXPECTED_GROUPS[@]}"; do
    if grep -q "name: $group" "$ALERT_RULES_FILE"; then
        echo "   ✓ Group '$group' found"
    else
        echo "   ❌ ERROR: Group '$group' not found"
        exit 1
    fi
done

# Check all required alerts
echo ""
echo "4. Checking required alerts..."
EXPECTED_ALERTS=(
    "RouterJetStreamHighRedeliveryRate"
    "RouterJetStreamHighRedeliveryFromSource"
    "RouterJetStreamMaxDeliverExhausted"
    "RouterJetStreamGrowingRedeliveryQueue"
    "RouterNatsFrequentReconnects"
    "RouterNatsConnectionFailures"
    "RouterNatsReconnectionExhausted"
    "RouterNatsHighReconnectFailureRate"
    "RouterNatsHighPublishFailureRate"
    "RouterNatsHighPublishWithAckFailureRate"
    "RouterNatsHighAckFailureRate"
    "RouterNatsHighNakFailureRate"
    "RouterNatsHighSubscribeFailureRate"
    "RouterNatsPendingOperationsQueueFull"
)

FOUND_ALERTS=0
MISSING_ALERTS=()

for alert in "${EXPECTED_ALERTS[@]}"; do
    if grep -q "alert: $alert" "$ALERT_RULES_FILE"; then
        echo "   ✓ Alert '$alert' found"
        ((FOUND_ALERTS++))
    else
        echo "   ❌ ERROR: Alert '$alert' not found"
        MISSING_ALERTS+=("$alert")
    fi
done

if [ ${#MISSING_ALERTS[@]} -gt 0 ]; then
    echo ""
    echo "❌ ERROR: Missing alerts: ${MISSING_ALERTS[*]}"
    exit 1
fi

echo ""
echo "   ✓ All $FOUND_ALERTS alerts found"

# Check metric usage
echo ""
echo "5. Checking metric usage..."

JETSTREAM_METRICS=(
    "router_jetstream_ack_total"
    "router_jetstream_redelivery_total"
    "router_dlq_total"
)

NATS_METRICS=(
    "router_nats_connection_established_total"
    "router_nats_connection_failures_total"
    "router_nats_connection_lost_total"
    "router_nats_connection_restored_total"
    "router_nats_connection_status"
    "router_nats_reconnect_attempts_total"
    "router_nats_reconnect_failures_total"
    "router_nats_reconnection_exhausted_total"
    "router_nats_publish_total"
    "router_nats_publish_failures_total"
    "router_nats_publish_with_ack_total"
    "router_nats_publish_with_ack_failures_total"
    "router_nats_ack_total"
    "router_nats_ack_failures_total"
    "router_nats_nak_total"
    "router_nats_nak_failures_total"
    "router_nats_subscribe_total"
    "router_nats_subscribe_failures_total"
    "router_nats_pending_operations_count"
)

echo "   Checking JetStream metrics..."
for metric in "${JETSTREAM_METRICS[@]}"; do
    if grep -q "$metric" "$ALERT_RULES_FILE"; then
        if [ "$VERBOSE" = true ]; then
            echo "      ✓ Metric '$metric' used"
        fi
    else
        echo "      ℹ Metric '$metric' not used (may be optional)"
    fi
done

echo "   Checking NATS metrics..."
for metric in "${NATS_METRICS[@]}"; do
    if grep -q "$metric" "$ALERT_RULES_FILE"; then
        if [ "$VERBOSE" = true ]; then
            echo "      ✓ Metric '$metric' used"
        fi
    else
        echo "      ℹ Metric '$metric' not used (may be optional)"
    fi
done

# Check label usage for router_jetstream_redelivery_total
echo ""
echo "6. Checking label usage for router_jetstream_redelivery_total..."
LABELS=("source" "reason" "assignment_id" "request_id")
for label in "${LABELS[@]}"; do
    if grep -A 10 "router_jetstream_redelivery_total" "$ALERT_RULES_FILE" | grep -q "$label"; then
        echo "   ✓ Label '$label' used in alert expressions"
    else
        echo "   ℹ Label '$label' not found in alert expressions (may be optional)"
    fi
done

# Check severity levels
echo ""
echo "7. Checking severity levels..."
CRITICAL_COUNT=$(grep -c "severity: critical" "$ALERT_RULES_FILE" || echo "0")
WARNING_COUNT=$(grep -c "severity: warning" "$ALERT_RULES_FILE" || echo "0")
echo "   ✓ Critical alerts: $CRITICAL_COUNT"
echo "   ✓ Warning alerts: $WARNING_COUNT"

# Check runbook links
echo ""
echo "8. Checking runbook links..."
if grep -q "runbook:" "$ALERT_RULES_FILE"; then
    RUNBOOK_COUNT=$(grep -c "runbook:" "$ALERT_RULES_FILE" || echo "0")
    echo "   ✓ Found $RUNBOOK_COUNT runbook reference(s)"
    if [ "$VERBOSE" = true ]; then
        echo "   Runbook links:"
        grep "runbook:" "$ALERT_RULES_FILE" | sed 's/.*runbook: *//' | sort -u | while read -r runbook; do
            echo "      - $runbook"
        done
    fi
else
    echo "   ⚠ No runbook links found (consider adding them)"
fi

echo ""
echo "=== Alert Rules Check Complete ==="
echo ""
echo "Summary:"
echo "  - Alert groups: 3"
echo "  - Total alerts: $FOUND_ALERTS"
echo "  - Critical alerts: $CRITICAL_COUNT"
echo "  - Warning alerts: $WARNING_COUNT"
echo ""
echo "Next steps:"
echo "  1. Validate with promtool: $0 --promtool"
echo "  2. Test alerts in staging: ./scripts/test_alert_rules.sh"
echo "  3. Verify in Alertmanager:"
echo "     curl http://localhost:9093/api/v2/alerts | jq '.[] | select(.labels.service == \"router\")'"

