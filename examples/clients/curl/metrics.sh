#!/bin/bash
# Get Prometheus metrics from c-gateway

set -e

# Configuration
BASE_URL="${BASE_URL:-http://localhost:8080}"
FILTER="${1:-}"  # Optional filter pattern

echo "=== Beamline Router - Prometheus Metrics ==="
echo ""
echo "Fetching from: $BASE_URL/metrics"
if [ -n "$FILTER" ]; then
    echo "Filter: $FILTER"
fi
echo ""

# Make request
response=$(curl -s -w "\n%{http_code}" "$BASE_URL/metrics")

# Extract HTTP code and response body
http_code=$(echo "$response" | tail -n1)
body=$(echo "$response" | sed '$d')

# Check status
if [ "$http_code" -eq 200 ]; then
    echo "✅ Success (HTTP $http_code)"
    echo ""
    
    if [ -n "$FILTER" ]; then
        # Apply filter
        echo "Metrics (filtered by '$FILTER'):"
        echo "$body" | grep "$FILTER" || echo "(no matches)"
    else
        # Show all metrics
        echo "Metrics:"
        echo "$body"
    fi
else
    echo "❌ Failed (HTTP $http_code)"
    echo ""
    echo "Response:"
    echo "$body"
    exit 1
fi

echo ""
echo "---"
echo "Tip: Filter metrics by pattern:"
echo "  ./metrics.sh gateway_requests"
echo "  ./metrics.sh gateway_rate_limit"
