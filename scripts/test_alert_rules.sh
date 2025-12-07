#!/bin/bash
# @doc Test alert rules in staging environment
# 
# Tests alert rules by:
# 1. Validating YAML syntax
# 2. Validating Prometheus syntax (if promtool available)
# 3. Checking alert rules are loaded in Prometheus/Alertmanager
# 4. Verifying metrics exist and are queryable
# 
# Usage:
#   ./scripts/test_alert_rules.sh [--prometheus-url URL] [--alertmanager-url URL]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$ROUTER_DIR/../../.." && pwd)"
cd "$ROUTER_DIR"

ALERT_RULES_FILE="$PROJECT_ROOT/docs/observability/router-alert-rules.yaml"
PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"
ALERTMANAGER_URL="${ALERTMANAGER_URL:-http://localhost:9093}"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --prometheus-url)
            PROMETHEUS_URL="$2"
            shift 2
            ;;
        --alertmanager-url)
            ALERTMANAGER_URL="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--prometheus-url URL] [--alertmanager-url URL]"
            exit 1
            ;;
    esac
done

echo "=== Testing Alert Rules in Staging ==="
echo ""
echo "Configuration:"
echo "  - Alert rules file: $ALERT_RULES_FILE"
echo "  - Prometheus URL: $PROMETHEUS_URL"
echo "  - Alertmanager URL: $ALERTMANAGER_URL"
echo ""

# Step 1: Validate YAML syntax
echo "1. Validating YAML syntax..."
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

# Step 2: Validate Prometheus syntax
echo ""
echo "2. Validating Prometheus syntax..."
if command -v promtool &> /dev/null; then
    if promtool check rules "$ALERT_RULES_FILE" 2>&1; then
        echo "   ✓ Prometheus syntax is valid"
    else
        echo "   ❌ ERROR: Invalid Prometheus syntax"
        exit 1
    fi
else
    echo "   ⚠ Skipping Prometheus validation (promtool not found)"
    echo "   Install: https://prometheus.io/docs/prometheus/latest/installation/"
fi

# Step 3: Check Prometheus is accessible
echo ""
echo "3. Checking Prometheus connectivity..."
if curl -sf "${PROMETHEUS_URL}/-/healthy" > /dev/null 2>&1; then
    echo "   ✓ Prometheus is accessible"
    
    # Check if rules are loaded
    echo ""
    echo "4. Checking if alert rules are loaded in Prometheus..."
    RULES_RESPONSE=$(curl -sf "${PROMETHEUS_URL}/api/v1/rules" 2>/dev/null || echo "")
    if [ -n "$RULES_RESPONSE" ]; then
        if echo "$RULES_RESPONSE" | grep -q "router-jetstream\|router-nats"; then
            echo "   ✓ Alert rules are loaded in Prometheus"
            
            # List loaded rule groups
            if command -v jq &> /dev/null; then
                echo ""
                echo "   Loaded rule groups:"
                echo "$RULES_RESPONSE" | jq -r '.data.groups[]?.name // empty' | grep -E "router-jetstream|router-nats" | while read -r group; do
                    echo "      - $group"
                done
            fi
        else
            echo "   ⚠ Alert rules not found in Prometheus (may need to reload)"
            echo "   To reload rules:"
            echo "     curl -X POST ${PROMETHEUS_URL}/-/reload"
        fi
    else
        echo "   ⚠ Could not fetch rules from Prometheus"
    fi
else
    echo "   ⚠ Prometheus is not accessible at $PROMETHEUS_URL"
    echo "   Skipping Prometheus checks"
fi

# Step 4: Check Alertmanager is accessible
echo ""
echo "5. Checking Alertmanager connectivity..."
if curl -sf "${ALERTMANAGER_URL}/-/healthy" > /dev/null 2>&1; then
    echo "   ✓ Alertmanager is accessible"
    
    # Check active alerts
    echo ""
    echo "6. Checking active alerts in Alertmanager..."
    ALERTS_RESPONSE=$(curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null || echo "")
    if [ -n "$ALERTS_RESPONSE" ]; then
        if command -v jq &> /dev/null; then
            ROUTER_ALERTS=$(echo "$ALERTS_RESPONSE" | jq '[.[] | select(.labels.service == "router")] | length')
            if [ "$ROUTER_ALERTS" -gt 0 ]; then
                echo "   ⚠ Found $ROUTER_ALERTS active Router alert(s)"
                echo ""
                echo "   Active alerts:"
                echo "$ALERTS_RESPONSE" | jq -r '.[] | select(.labels.service == "router") | "      - \(.labels.alertname) [\(.status.state)]"' | head -10
            else
                echo "   ✓ No active Router alerts (all clear)"
            fi
        else
            echo "   ℹ Install jq for detailed alert information"
        fi
    else
        echo "   ⚠ Could not fetch alerts from Alertmanager"
    fi
else
    echo "   ⚠ Alertmanager is not accessible at $ALERTMANAGER_URL"
    echo "   Skipping Alertmanager checks"
fi

# Step 5: Verify metrics exist
echo ""
echo "7. Verifying metrics are queryable..."
METRICS_TO_CHECK=(
    "router_jetstream_ack_total"
    "router_jetstream_redelivery_total"
    "router_dlq_total"
    "router_nats_connection_status"
    "router_nats_publish_total"
    "router_nats_ack_total"
)

if curl -sf "${PROMETHEUS_URL}/-/healthy" > /dev/null 2>&1; then
    FOUND_METRICS=0
    MISSING_METRICS=()
    
    for metric in "${METRICS_TO_CHECK[@]}"; do
        QUERY_RESPONSE=$(curl -sf "${PROMETHEUS_URL}/api/v1/query?query=${metric}" 2>/dev/null || echo "")
        if [ -n "$QUERY_RESPONSE" ]; then
            if echo "$QUERY_RESPONSE" | grep -q '"result":\['; then
                RESULT_COUNT=$(echo "$QUERY_RESPONSE" | jq -r '.data.result | length' 2>/dev/null || echo "0")
                if [ "$RESULT_COUNT" -gt 0 ]; then
                    echo "   ✓ Metric '$metric' is queryable ($RESULT_COUNT series)"
                    ((FOUND_METRICS++))
                else
                    echo "   ⚠ Metric '$metric' exists but has no data"
                    MISSING_METRICS+=("$metric")
                fi
            else
                echo "   ⚠ Metric '$metric' not found in Prometheus"
                MISSING_METRICS+=("$metric")
            fi
        else
            echo "   ⚠ Could not query metric '$metric'"
            MISSING_METRICS+=("$metric")
        fi
    done
    
    echo ""
    if [ ${#MISSING_METRICS[@]} -eq 0 ]; then
        echo "   ✓ All metrics are queryable"
    else
        echo "   ⚠ Some metrics are missing or have no data:"
        for metric in "${MISSING_METRICS[@]}"; do
            echo "      - $metric"
        done
        echo "   This is normal if Router is not running or has no traffic"
    fi
else
    echo "   ⚠ Skipping metric verification (Prometheus not accessible)"
fi

echo ""
echo "=== Alert Rules Test Complete ==="
echo ""
echo "Next steps:"
echo "  1. If rules are not loaded, reload Prometheus:"
echo "     curl -X POST ${PROMETHEUS_URL}/-/reload"
echo "  2. Monitor alerts in Alertmanager:"
echo "     ${ALERTMANAGER_URL}/#/alerts"
echo "  3. Test alert expressions in Prometheus:"
echo "     ${PROMETHEUS_URL}/graph?g0.expr=rate(router_jetstream_redelivery_total[5m])"

