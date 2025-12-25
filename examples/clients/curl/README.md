# Beamline Router cURL Examples

Shell scripts for testing Beamline Router integration via HTTP (c-gateway) using cURL.

## Scripts

### 1. `decide.sh` - Basic Routing Decision
Make a routing decision request to c-gateway.

### 2. `health.sh` - Health Check
Check c-gateway health status.

### 3. `metrics.sh` - Prometheus Metrics
Retrieve Prometheus metrics from c-gateway.

### 4. `retry_example.sh` - Request with Retry
Demonstrate retry logic with exponential backoff and rate limit handling.

## Prerequisites

- **cURL**: Command-line HTTP client
- **jq**: JSON processor (optional, for pretty printing)
- **bc**: Calculator (for retry_example.sh)
- **c-gateway**: Running on http://localhost:8080

Install dependencies:
```bash
# Ubuntu/Debian
sudo apt-get install curl jq bc

# macOS
brew install curl jq bc

# Alpine
apk add curl jq bc
```

## Quick Start

### 1. Make Scripts Executable

```bash
chmod +x *.sh
```

### 2. Start c-gateway

```bash
cd ~/aigroup/apps/c-gateway
./build/gateway
```

### 3. Run Examples

**Check health**:
```bash
./health.sh
```

**Make routing decision**:
```bash
./decide.sh
```

**Get metrics**:
```bash
./metrics.sh
```

**Test retry logic**:
```bash
./retry_example.sh
```

## Usage

### decide.sh

**Basic usage**:
```bash
./decide.sh
```

**With environment variables**:
```bash
BASE_URL=http://localhost:8080 \
TENANT_ID=my-tenant \
API_KEY=my-api-key \
POLICY_ID=my-policy \
./decide.sh
```

**Environment variables**:
- `BASE_URL` - c-gateway base URL (default: http://localhost:8080)
- `TENANT_ID` - Tenant identifier (default: demo-tenant)
- `API_KEY` - API key for authentication (default: your-api-key)
- `POLICY_ID` - Policy identifier (default: demo-policy)

**Expected output**:
```
=== Beamline Router - Routing Decision ===

Configuration:
  Base URL: http://localhost:8080
  Tenant ID: demo-tenant
  Policy ID: demo-policy

Sending request...
✅ Success (HTTP 200)

Response:
{
  "decision_id": "dec-123456",
  "provider_id": "openai",
  "reason": "weighted_random",
  "priority": 1,
  "expected_cost": 0.0001
}
```

### health.sh

**Usage**:
```bash
./health.sh
```

**With custom URL**:
```bash
BASE_URL=http://localhost:8080 ./health.sh
```

**Expected output (healthy)**:
```
=== Beamline Router - Health Check ===

Checking: http://localhost:8080/_health

✅ Healthy (HTTP 200)

Response:
{
  "status": "healthy"
}
```

**Expected output (unhealthy)**:
```
=== Beamline Router - Health Check ===

Checking: http://localhost:8080/_health

❌ Service Unavailable (HTTP 503)

Response:
{
  "status": "unhealthy",
  "reason": "NATS disconnected"
}
```

**Exit codes**:
- `0`: Healthy
- `1`: Unhealthy or error

### metrics.sh

**Get all metrics**:
```bash
./metrics.sh
```

**Filter metrics**:
```bash
./metrics.sh gateway_requests
./metrics.sh gateway_rate_limit
./metrics.sh duration
```

**With custom URL**:
```bash
BASE_URL=http://localhost:8080 ./metrics.sh
```

**Expected output**:
```
=== Beamline Router - Prometheus Metrics ===

Fetching from: http://localhost:8080/metrics

✅ Success (HTTP 200)

Metrics:
# HELP gateway_requests_total Total HTTP requests
# TYPE gateway_requests_total counter
gateway_requests_total{endpoint="/api/v1/routes/decide",status="200"} 1234

# HELP gateway_request_duration_seconds HTTP request duration
# TYPE gateway_request_duration_seconds histogram
gateway_request_duration_seconds_bucket{endpoint="/api/v1/routes/decide",le="0.005"} 800
...
```

### retry_example.sh

**Basic usage**:
```bash
./retry_example.sh
```

**With custom retry configuration**:
```bash
MAX_ATTEMPTS=5 \
BASE_DELAY_MS=200 \
MAX_DELAY_MS=10000 \
./retry_example.sh
```

**Environment variables**:
- `MAX_ATTEMPTS` - Maximum retry attempts (default: 3)
- `BASE_DELAY_MS` - Initial delay in milliseconds (default: 100)
- `MAX_DELAY_MS` - Maximum delay in milliseconds (default: 5000)
- Plus all variables from decide.sh

**Expected output (success)**:
```
=== Beamline Router - Request with Retry ===

Configuration:
  Base URL: http://localhost:8080
  Max Attempts: 3
  Base Delay: 100ms
  Max Delay: 5000ms

Attempt 1 of 3...
✅ Success (HTTP 200) on attempt 1

Response:
{
  "decision_id": "dec-123456",
  "provider_id": "openai",
  "reason": "weighted_random"
}
```

**Expected output (with retries)**:
```
Attempt 1 of 3...
⚠️  Request failed (HTTP 503). Retrying in 0.105s...
Attempt 2 of 3...
⚠️  Request failed (HTTP 503). Retrying in 0.218s...
Attempt 3 of 3...
✅ Success (HTTP 200) on attempt 3

Response:
{...}
```

**Expected output (rate limited)**:
```
Attempt 1 of 3...
⚠️  Rate limited (HTTP 429). Retry after: 60s
Waiting 60s...
Attempt 2 of 3...
✅ Success (HTTP 200) on attempt 2
```

## Features

### Retry Logic

**Exponential Backoff**:
- Attempt 1: Immediate
- Attempt 2: ~100ms ± jitter
- Attempt 3: ~200ms ± jitter
- Attempt 4: ~400ms ± jitter
- etc.

**Jitter**: ±10% randomization to prevent thundering herd

**Retryable Status Codes**:
- `408`: Request Timeout
- `429`: Too Many Requests
- `500`: Internal Server Error
- `502`: Bad Gateway
- `503`: Service Unavailable
- `504`: Gateway Timeout

**Non-Retryable Status Codes**:
- `400`: Bad Request
- `401`: Unauthorized
- `403`: Forbidden
- `404`: Not Found

### Rate Limit Handling

**429 Response**:
1. Detects 429 status code
2. Extracts `Retry-After` header
3. Waits specified time
4. Retries request

**Example**:
```bash
# Request 1: Gets rate limited
curl ... → HTTP 429, Retry-After: 60

# Waits 60 seconds

# Request 2: Succeeds
curl ... → HTTP 200
```

## Testing

### Prerequisites

```bash
# Terminal 1: Start c-gateway
cd ~/aigroup/apps/c-gateway
./build/gateway

# Terminal 2: Run tests
cd ~/aigroup/apps/otp/router/examples/clients/curl
```

### Test Health Check

```bash
./health.sh
# Should return: ✅ Healthy (HTTP 200)
```

### Test Request

```bash
./decide.sh
# Should return: ✅ Success (HTTP 200)
```

### Test Metrics

```bash
./metrics.sh gateway_requests
# Should show: gateway_requests_total metrics
```

### Test Retry

```bash
# Test with retries (may need to simulate failures)
MAX_ATTEMPTS=5 ./retry_example.sh
```

## Troubleshooting

### "Connection refused"

**Cause**: c-gateway not running

**Solution**:
```bash
# Check c-gateway is running
curl http://localhost:8080/_health

# Start c-gateway
cd ~/aigroup/apps/c-gateway
./build/gateway
```

### "jq: command not found"

**Cause**: jq not installed

**Solution**:
```bash
# Ubuntu/Debian
sudo apt-get install jq

# macOS
brew install jq

# Or edit scripts to remove jq dependency
```

### "bc: command not found"

**Cause**: bc not installed (for retry_example.sh)

**Solution**:
```bash
# Ubuntu/Debian
sudo apt-get install bc

# macOS
brew install bc
```

### "401 Unauthorized"

**Cause**: Invalid API key

**Solution**:
```bash
# Set correct API key
API_KEY=correct-key ./decide.sh
```

### "429 Too Many Requests"

**Cause**: Rate limit exceeded

**Solution**:
- Wait for rate limit window to reset
- Use retry_example.sh (handles 429 automatically)
- Increase rate limits in c-gateway configuration

## Production Use

### CI/CD Integration

**Health Check**:
```bash
# In deployment script
if ./health.sh; then
    echo "Deployment successful"
else
    echo "Deployment failed - service unhealthy"
    exit 1
fi
```

**Smoke Test**:
```bash
# After deployment
if ./decide.sh; then
    echo "Smoke test passed"
else
    echo "Smoke test failed"
    exit 1
fi
```

### Monitoring

**Periodic Health Checks**:
```bash
# Cron job (every minute)
* * * * * /path/to/health.sh || alert-admin
```

**Metrics Collection**:
```bash
# Export metrics to file
./metrics.sh > /var/metrics/beamline-$(date +%Y%m%d-%H%M%S).txt
```

## Comparison with Other Clients

**Advantages**:
- ✅ No dependencies (except cURL, jq, bc)
- ✅ Quick testing
- ✅ Shell script friendly
- ✅ CI/CD integration
- ✅ Debugging

**Disadvantages**:
- ❌ No type safety
- ❌ Limited error handling
- ❌ No connection pooling
- ❌ Not for production application code

**When to use cURL**:
- Quick testing
- CI/CD smoke tests
- Debugging
- Shell scripting
- One-off requests

**When to use client libraries**:
- Production applications
- Complex retry logic
- Connection pooling
- Type safety
- Advanced features

## Next Steps

- **Client Libraries**: See Python, Node.js, Go examples
- **Integration Guide**: See `~/aigroup/apps/otp/router/docs/INTEGRATION_GUIDE.md`
- **c-gateway Details**: See `.ai/task_intgration_descritption/c_gateway.md`

## License

Same as Beamline Router project.
