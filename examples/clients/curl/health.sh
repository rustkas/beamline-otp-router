#!/bin/bash
# Check c-gateway health status

set -e

# Configuration
BASE_URL="${BASE_URL:-http://localhost:8080}"

echo "=== Beamline Router - Health Check ==="
echo ""
echo "Checking: $BASE_URL/_health"
echo ""

# Make request
response=$(curl -s -w "\n%{http_code}" "$BASE_URL/_health")

# Extract HTTP code and response body
http_code=$(echo "$response" | tail -n1)
body=$(echo "$response" | sed '$d')

# Check status
if [ "$http_code" -eq 200 ]; then
    status=$(echo "$body" | jq -r '.status' 2>/dev/null || echo "unknown")
    
    if [ "$status" = "healthy" ]; then
        echo "✅ Healthy (HTTP $http_code)"
        echo ""
        echo "Response:"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        exit 0
    else
        echo "⚠️  Unhealthy (HTTP $http_code)"
        echo ""
        echo "Response:"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        exit 1
    fi
elif [ "$http_code" -eq 503 ]; then
    echo "❌ Service Unavailable (HTTP $http_code)"
    echo ""
    echo "Response:"
    echo "$body" | jq '.' 2>/dev/null || echo "$body"
    exit 1
else
    echo "❌ Failed (HTTP $http_code)"
    echo ""
    echo "Response:"
    echo "$body"
    exit 1
fi
