#!/bin/bash
# Basic routing decision via c-gateway

set -e

# Configuration
BASE_URL="${BASE_URL:-http://localhost:8080}"
TENANT_ID="${TENANT_ID:-demo-tenant}"
API_KEY="${API_KEY:-your-api-key}"
POLICY_ID="${POLICY_ID:-demo-policy}"

echo "=== Beamline Router - Routing Decision ==="
echo ""
echo "Configuration:"
echo "  Base URL: $BASE_URL"
echo "  Tenant ID: $TENANT_ID"
echo "  Policy ID: $POLICY_ID"
echo ""

# Make request
echo "Sending request..."
response=$(curl -s -w "\n%{http_code}" \
  -X POST "$BASE_URL/api/v1/routes/decide" \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: $TENANT_ID" \
  -H "X-API-Key: $API_KEY" \
  -H "X-Request-ID: curl-$(date +%s)" \
  -d '{
    "version": "1",
    "message": {
      "type": "text.generate",
      "payload": "Hello, world!",
      "metadata": {
        "source": "curl"
      }
    },
    "policy_id": "'"$POLICY_ID"'"
  }')

# Extract HTTP code and response body
http_code=$(echo "$response" | tail -n1)
body=$(echo "$response" | sed '$d')

# Check status
if [ "$http_code" -eq 200 ]; then
    echo "✅ Success (HTTP $http_code)"
    echo ""
    echo "Response:"
    echo "$body" | jq '.' 2>/dev/null || echo "$body"
else
    echo "❌ Failed (HTTP $http_code)"
    echo ""
    echo "Response:"
    echo "$body" | jq '.' 2>/dev/null || echo "$body"
    exit 1
fi
