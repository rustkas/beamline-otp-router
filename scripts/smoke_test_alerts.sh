#!/bin/bash
# @doc Smoke test alert rules in staging environment
# 
# Triggers fault injection scenarios to verify alerts fire correctly:
# 1. MaxDeliver exhaustion (via fault injection)
# 2. NATS connection failure (via fault injection)
# 3. High redelivery rate (via fault injection)
#
# Usage:
#   ./scripts/smoke_test_alerts.sh [--router-url URL] [--prometheus-url URL] [--alertmanager-url URL] [--wait-time SECONDS]
#
# Prerequisites:
#   - Router running in staging with fault injection enabled
#   - Prometheus scraping Router metrics
#   - Alertmanager configured to receive alerts
#   - curl, jq installed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROUTER_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$ROUTER_DIR"

ROUTER_URL="${ROUTER_URL:-http://localhost:9001}"
PROMETHEUS_URL="${PROMETHEUS_URL:-http://localhost:9090}"
ALERTMANAGER_URL="${ALERTMANAGER_URL:-http://localhost:9093}"
WAIT_TIME="${WAIT_TIME:-120}"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --router-url)
            ROUTER_URL="$2"
            shift 2
            ;;
        --prometheus-url)
            PROMETHEUS_URL="$2"
            shift 2
            ;;
        --alertmanager-url)
            ALERTMANAGER_URL="$2"
            shift 2
            ;;
        --wait-time)
            WAIT_TIME="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--router-url URL] [--prometheus-url URL] [--alertmanager-url URL] [--wait-time SECONDS]"
            exit 1
            ;;
    esac
done

echo "=== Smoke Test: Alert Rules in Staging ==="
echo ""
echo "Configuration:"
echo "  - Router URL: $ROUTER_URL"
echo "  - Prometheus URL: $PROMETHEUS_URL"
echo "  - Alertmanager URL: $ALERTMANAGER_URL"
echo "  - Wait time: ${WAIT_TIME}s"
echo ""

# Check prerequisites
echo "1. Checking prerequisites..."

if ! command -v curl &> /dev/null; then
    echo "   ❌ ERROR: curl not found"
    exit 1
fi
echo "   ✓ curl found"

if ! command -v jq &> /dev/null; then
    echo "   ⚠ WARNING: jq not found (some checks will be skipped)"
    HAS_JQ=false
else
    echo "   ✓ jq found"
    HAS_JQ=true
fi

# Check Router is accessible
if ! curl -sf "${ROUTER_URL}/_health" > /dev/null 2>&1; then
    echo "   ⚠ WARNING: Router not accessible at $ROUTER_URL"
    echo "   Continuing anyway (fault injection may work via Erlang console)"
else
    echo "   ✓ Router is accessible"
fi

# Check Prometheus is accessible
if ! curl -sf "${PROMETHEUS_URL}/-/healthy" > /dev/null 2>&1; then
    echo "   ❌ ERROR: Prometheus not accessible at $PROMETHEUS_URL"
    exit 1
fi
echo "   ✓ Prometheus is accessible"

# Check Alertmanager is accessible
if ! curl -sf "${ALERTMANAGER_URL}/-/healthy" > /dev/null 2>&1; then
    echo "   ⚠ WARNING: Alertmanager not accessible at $ALERTMANAGER_URL"
    echo "   Alerts will be checked in Prometheus only"
    HAS_ALERTMANAGER=false
else
    echo "   ✓ Alertmanager is accessible"
    HAS_ALERTMANAGER=true
fi

echo ""
echo "2. Baseline: Checking current alert state..."

if [ "$HAS_ALERTMANAGER" = true ] && [ "$HAS_JQ" = true ]; then
    BASELINE_ALERTS=$(curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null | jq '[.[] | select(.labels.service == "router")] | length' || echo "0")
    echo "   Current Router alerts in Alertmanager: $BASELINE_ALERTS"
else
    echo "   ⚠ Skipping baseline check (Alertmanager or jq not available)"
fi

echo ""
echo "3. Test Scenarios:"
echo ""

# Test 1: MaxDeliver Exhaustion
echo "   Test 1: MaxDeliver Exhaustion"
echo "   ============================="
echo "   Triggering MaxDeliver exhaustion via fault injection..."
echo ""
echo "   Note: This requires fault injection to be enabled in Router."
echo "   To trigger manually:"
echo "     1. Connect to Router Erlang console"
echo "     2. Enable fault: router_nats_fault_injection:enable_fault(ack, {error, timeout})"
echo "     3. Send messages that will fail ACK repeatedly"
echo "     4. Wait for MaxDeliver to be exhausted"
echo ""
echo "   Expected alert: RouterJetStreamMaxDeliverExhausted (critical)"
echo "   Expected duration: 2 minutes"
echo ""

read -p "   Press Enter when MaxDeliver exhaustion is triggered, or Ctrl+C to skip..."

echo "   Waiting ${WAIT_TIME}s for alert to fire..."
sleep "$WAIT_TIME"

if [ "$HAS_ALERTMANAGER" = true ] && [ "$HAS_JQ" = true ]; then
    MAXDELIVER_ALERT=$(curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null | \
        jq '[.[] | select(.labels.alertname == "RouterJetStreamMaxDeliverExhausted")] | length' || echo "0")
    if [ "$MAXDELIVER_ALERT" -gt 0 ]; then
        echo "   ✓ Alert 'RouterJetStreamMaxDeliverExhausted' is firing"
    else
        echo "   ⚠ Alert 'RouterJetStreamMaxDeliverExhausted' not found"
        echo "   Check Prometheus: ${PROMETHEUS_URL}/graph?g0.expr=rate(router_jetstream_maxdeliver_exhausted_total[5m])"
    fi
else
    echo "   ⚠ Skipping alert check (Alertmanager or jq not available)"
fi

echo ""

# Test 2: NATS Connection Failure
echo "   Test 2: NATS Connection Failure"
echo "   ================================="
echo "   Triggering NATS connection failure..."
echo ""
echo "   Note: This requires fault injection to be enabled in Router."
echo "   To trigger manually:"
echo "     1. Connect to Router Erlang console"
echo "     2. Enable fault: router_nats_fault_injection:enable_fault(connect, {error, connection_refused})"
echo "     3. Or stop NATS server temporarily"
echo "     4. Wait for connection failures"
echo ""
echo "   Expected alert: RouterNatsConnectionFailures (critical)"
echo "   Expected duration: 1 minute"
echo ""

read -p "   Press Enter when NATS connection failure is triggered, or Ctrl+C to skip..."

echo "   Waiting ${WAIT_TIME}s for alert to fire..."
sleep "$WAIT_TIME"

if [ "$HAS_ALERTMANAGER" = true ] && [ "$HAS_JQ" = true ]; then
    CONNECTION_ALERT=$(curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null | \
        jq '[.[] | select(.labels.alertname == "RouterNatsConnectionFailures")] | length' || echo "0")
    if [ "$CONNECTION_ALERT" -gt 0 ]; then
        echo "   ✓ Alert 'RouterNatsConnectionFailures' is firing"
    else
        echo "   ⚠ Alert 'RouterNatsConnectionFailures' not found"
        echo "   Check Prometheus: ${PROMETHEUS_URL}/graph?g0.expr=router_nats_connection_status{state=\"disconnected\"}"
    fi
else
    echo "   ⚠ Skipping alert check (Alertmanager or jq not available)"
fi

echo ""

# Test 3: High Redelivery Rate
echo "   Test 3: High Redelivery Rate"
echo "   ============================="
echo "   Triggering high redelivery rate..."
echo ""
echo "   Note: This requires fault injection to be enabled in Router."
echo "   To trigger manually:"
echo "     1. Connect to Router Erlang console"
echo "     2. Enable fault: router_nats_fault_injection:enable_fault(ack, {error, timeout})"
echo "     3. Send messages that will fail processing"
echo "     4. Wait for redelivery rate to exceed threshold (10%)"
echo ""
echo "   Expected alert: RouterJetStreamHighRedeliveryRate (warning)"
echo "   Expected duration: 10 minutes"
echo ""

read -p "   Press Enter when high redelivery rate is triggered, or Ctrl+C to skip..."

echo "   Waiting ${WAIT_TIME}s for alert to fire..."
sleep "$WAIT_TIME"

if [ "$HAS_ALERTMANAGER" = true ] && [ "$HAS_JQ" = true ]; then
    REDELIVERY_ALERT=$(curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null | \
        jq '[.[] | select(.labels.alertname == "RouterJetStreamHighRedeliveryRate")] | length' || echo "0")
    if [ "$REDELIVERY_ALERT" -gt 0 ]; then
        echo "   ✓ Alert 'RouterJetStreamHighRedeliveryRate' is firing"
    else
        echo "   ⚠ Alert 'RouterJetStreamHighRedeliveryRate' not found"
        echo "   Check Prometheus: ${PROMETHEUS_URL}/graph?g0.expr=rate(router_jetstream_redelivery_total[5m])"
    fi
else
    echo "   ⚠ Skipping alert check (Alertmanager or jq not available)"
fi

echo ""

# Summary
echo "4. Summary"
echo "   ======="
echo ""

if [ "$HAS_ALERTMANAGER" = true ] && [ "$HAS_JQ" = true ]; then
    FINAL_ALERTS=$(curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null | \
        jq '[.[] | select(.labels.service == "router")] | length' || echo "0")
    echo "   Total Router alerts in Alertmanager: $FINAL_ALERTS"
    echo ""
    
    if [ "$FINAL_ALERTS" -gt 0 ]; then
        echo "   Active alerts:"
        curl -sf "${ALERTMANAGER_URL}/api/v2/alerts" 2>/dev/null | \
            jq -r '.[] | select(.labels.service == "router") | "      - \(.labels.alertname) [\(.status.state)] (severity: \(.labels.severity))"' || true
    fi
else
    echo "   ⚠ Summary skipped (Alertmanager or jq not available)"
fi

echo ""
echo "=== Smoke Test Complete ==="
echo ""
echo "Next steps:"
echo "  1. Verify alerts in Alertmanager UI: ${ALERTMANAGER_URL}/#/alerts"
echo "  2. Check alert routing (Slack/email/PagerDuty)"
echo "  3. Verify alert labels (service, component, team, env, cluster)"
echo "  4. Review alert descriptions and runbooks"
echo "  5. Disable fault injection after testing:"
echo "     router_nats_fault_injection:clear_all_faults()"
echo ""

