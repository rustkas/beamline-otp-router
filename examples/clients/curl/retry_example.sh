#!/bin/bash
# Routing decision with retry and exponential backoff

set -e

# Configuration
BASE_URL="${BASE_URL:-http://localhost:8080}"
TENANT_ID="${TENANT_ID:-demo-tenant}"
API_KEY="${API_KEY:-your-api-key}"
POLICY_ID="${POLICY_ID:-demo-policy}"

MAX_ATTEMPTS="${MAX_ATTEMPTS:-3}"
BASE_DELAY_MS="${BASE_DELAY_MS:-100}"
MAX_DELAY_MS="${MAX_DELAY_MS:-5000}"

echo "=== Beamline Router - Request with Retry ==="
echo ""
echo "Configuration:"
echo "  Base URL: $BASE_URL"
echo "  Max Attempts: $MAX_ATTEMPTS"
echo "  Base Delay: ${BASE_DELAY_MS}ms"
echo "  Max Delay: ${MAX_DELAY_MS}ms"
echo ""

# Function to calculate delay with exponential backoff
calculate_delay() {
    local attempt=$1
    local base_delay=$BASE_DELAY_MS
    local max_delay=$MAX_DELAY_MS
    
    # Exponential: base * 2^(attempt-1)
    local delay=$(echo "$base_delay * (2 ^ ($attempt - 1))" | bc)
    
    # Cap at max_delay
    if (( $(echo "$delay > $max_delay" | bc -l) )); then
        delay=$max_delay
    fi
    
    # Add jitter (±10%)
    local jitter=$(echo "$delay * 0.1 * (2 * $RANDOM / 32767 - 1)" | bc -l)
    delay=$(echo "$delay + $jitter" | bc -l)
    
    # Ensure positive
    if (( $(echo "$delay < 0" | bc -l) )); then
        delay=0
    fi
    
    echo "$delay"
}

# Function to check if status code is retryable
is_retryable() {
    local code=$1
    case $code in
        408|429|500|502|503|504)
            return 0  # Retryable
            ;;
        *)
            return 1  # Not retryable
            ;;
    esac
}

# Retry loop
for attempt in $(seq 1 $MAX_ATTEMPTS); do
    echo "Attempt $attempt of $MAX_ATTEMPTS..."
    
    # Make request
    response=$(curl -s -w "\n%{http_code}\n%{header_json}" \
      -X POST "$BASE_URL/api/v1/routes/decide" \
      -H "Content-Type: application/json" \
      -H "X-Tenant-ID: $TENANT_ID" \
      -H "X-API-Key: $API_KEY" \
      -H "X-Request-ID: curl-retry-$(date +%s)" \
      -d '{
        "version": "1",
        "message": {
          "type": "text.generate",
          "payload": "Hello with retry!",
          "metadata": {}
        },
        "policy_id": "'"$POLICY_ID"'"
      }' || echo -e "\n000\n{}")
    
    # Extract parts
    http_code=$(echo "$response" | tail -n2 | head -n1)
    body=$(echo "$response" | sed '$d' | sed '$d')
    headers=$(echo "$response" | tail -n1)
    
    # Check success
    if [ "$http_code" -eq 200 ]; then
        echo "✅ Success (HTTP $http_code) on attempt $attempt"
        echo ""
        echo "Response:"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        exit 0
    fi
    
    # Check if retryable
    if ! is_retryable "$http_code" && [ "$http_code" -ne 0 ]; then
        echo "❌ Non-retryable error (HTTP $http_code)"
        echo ""
        echo "Response:"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        exit 1
    fi
    
    # Check for rate limiting (429)
    if [ "$http_code" -eq 429 ]; then
        retry_after=$(echo "$headers" | jq -r '."retry-after" // "60"' 2>/dev/null || echo "60")
        echo "⚠️  Rate limited (HTTP 429). Retry after: ${retry_after}s"
        
        if [ $attempt -lt $MAX_ATTEMPTS ]; then
            echo "Waiting ${retry_after}s..."
            sleep "$retry_after"
            continue
        fi
    fi
    
    # Last attempt?
    if [ $attempt -eq $MAX_ATTEMPTS ]; then
        echo "❌ Failed after $MAX_ATTEMPTS attempts (HTTP $http_code)"
        echo ""
        echo "Response:"
        echo "$body" | jq '.' 2>/dev/null || echo "$body"
        exit 1
    fi
    
    # Calculate backoff delay
    delay_ms=$(calculate_delay $attempt)
    delay_s=$(echo "scale=3; $delay_ms / 1000" | bc)
    
    echo "⚠️  Request failed (HTTP $http_code). Retrying in ${delay_s}s..."
    sleep "$delay_s"
done

echo "❌ Failed after $MAX_ATTEMPTS attempts"
exit 1
