#!/bin/bash
# @doc Validate Prometheus queries for router_jetstream_redelivery_total
# 
# Tests Prometheus queries from STAGING_VALIDATION_GUIDE.md
# 
# Usage:
#   ./scripts/validate_prometheus_queries.sh [prometheus_url]
# 
# Default: http://localhost:9090

set -euo pipefail

PROMETHEUS_URL="${1:-http://localhost:9090}"

echo "=== Validating Prometheus Queries ==="
echo "Prometheus URL: $PROMETHEUS_URL"
echo ""

# Check if Prometheus is accessible
if ! curl -sf "$PROMETHEUS_URL/api/v1/status/config" > /dev/null 2>&1; then
    echo "⚠ WARNING: Prometheus not accessible at $PROMETHEUS_URL"
    echo "Skipping query validation. Run this script when Prometheus is available."
    exit 0
fi

echo "✓ Prometheus is accessible"
echo ""

# Query 1: Total redelivery rate
echo "Query 1: Total redelivery rate"
QUERY1="sum(rate(router_jetstream_redelivery_total[5m]))"
QUERY1_URL="$PROMETHEUS_URL/api/v1/query?query=$(echo "$QUERY1" | sed 's/ /%20/g')"
RESULT1=$(curl -sf "$QUERY1_URL" || echo "")

if echo "$RESULT1" | grep -q "router_jetstream_redelivery_total"; then
    echo "   ✓ Query successful"
    echo "$RESULT1" | jq -r '.data.result[] | "   Value: \(.value[1])"' 2>/dev/null || echo "   (Response received)"
else
    echo "   ⚠ No data returned (may be normal if no redeliveries)"
fi

echo ""

# Query 2: Redelivery rate by source
echo "Query 2: Redelivery rate by source"
QUERY2="sum by (source) (rate(router_jetstream_redelivery_total[5m]))"
QUERY2_URL="$PROMETHEUS_URL/api/v1/query?query=$(echo "$QUERY2" | sed 's/ /%20/g')"
RESULT2=$(curl -sf "$QUERY2_URL" || echo "")

if echo "$RESULT2" | grep -q "router_jetstream_redelivery_total"; then
    echo "   ✓ Query successful"
    echo "$RESULT2" | jq -r '.data.result[] | "   source=\(.metric.source): \(.value[1])"' 2>/dev/null || echo "   (Response received)"
else
    echo "   ⚠ No data returned"
fi

echo ""

# Query 3: Redelivery rate by reason
echo "Query 3: Redelivery rate by reason"
QUERY3="sum by (reason) (rate(router_jetstream_redelivery_total[5m]))"
QUERY3_URL="$PROMETHEUS_URL/api/v1/query?query=$(echo "$QUERY3" | sed 's/ /%20/g')"
RESULT3=$(curl -sf "$QUERY3_URL" || echo "")

if echo "$RESULT3" | grep -q "router_jetstream_redelivery_total"; then
    echo "   ✓ Query successful"
    echo "$RESULT3" | jq -r '.data.result[] | "   reason=\(.metric.reason): \(.value[1])"' 2>/dev/null || echo "   (Response received)"
else
    echo "   ⚠ No data returned"
fi

echo ""

# Query 4: Redelivery rate by source and reason
echo "Query 4: Redelivery rate by source and reason"
QUERY4="sum by (source, reason) (rate(router_jetstream_redelivery_total[5m]))"
QUERY4_URL="$PROMETHEUS_URL/api/v1/query?query=$(echo "$QUERY4" | sed 's/ /%20/g')"
RESULT4=$(curl -sf "$QUERY4_URL" || echo "")

if echo "$RESULT4" | grep -q "router_jetstream_redelivery_total"; then
    echo "   ✓ Query successful"
    echo "$RESULT4" | jq -r '.data.result[] | "   source=\(.metric.source), reason=\(.metric.reason): \(.value[1])"' 2>/dev/null || echo "   (Response received)"
else
    echo "   ⚠ No data returned"
fi

echo ""
echo "=== Prometheus Query Validation Complete ==="
echo ""
echo "Note: If no data is returned, ensure:"
echo "  1. Router is running and emitting metrics"
echo "  2. Prometheus is scraping router metrics"
echo "  3. Fault injection scenarios have been triggered"

