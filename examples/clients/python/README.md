# Beamline Router Python Clients

Python examples for integrating with Beamline Router via gRPC (direct) or HTTP (via c-gateway).

## Clients

### 1. gRPC Client (`grpc_client.py`)
Direct integration with Router using gRPC protocol.

### 2. HTTP Client (`http_client.py`)
Integration via c-gateway using HTTP/REST API.

## Features

**gRPC Client**:
- **Type-safe gRPC calls**: Direct Router integration
- **Retry Logic**: Exponential backoff with jitter
- **Circuit Breaker**: Prevents cascading failures
- **Error Handling**: Graceful handling of transient errors

**HTTP Client**:
- **REST API**: Integration via c-gateway
- **Rate Limit Handling**: Respects 429 + Retry-After header
- **Retry Logic**: Exponential backoff with jitter
- **Error Handling**: Retryable vs non-retryable errors
- **API Key Auth**: X-API-Key authentication
- **Health & Metrics**: Built-in endpoints

## Quick Start

### 1. Install Dependencies

```bash
pip install grpcio grpcio-tools protobuf
```

Or using virtual environment:
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install grpcio grpcio-tools protobuf
```

### 2. Generate Protobuf Code

**Note**: Python example includes protobuf generation. In a real scenario, run:

```bash
cd ../../../  # Navigate to router root
python -m grpc_tools.protoc \
  -I proto \
  --python_out=examples/clients/python \
  --grpc_python_out=examples/clients/python \
  proto/beamline/flow/v1/flow.proto
```

### 3. Run Example

```bash
python decide_client.py
```

## Usage

### Basic Example

```python
from decide_client import BeamlineRouterClient

# Create client
client = BeamlineRouterClient(host="localhost", port=50051)

try:
    # Make decision
    decision = client.decide(
        tenant_id="my-tenant",
        policy_id="my-policy",
        task_type="text.generate",
        task_payload="Hello, world!"
    )
    
    print(f"Provider: {decision['provider_id']}")
    print(f"Reason: {decision['reason']}")

finally:
    client.close()
```

### With Custom Retry Configuration

```python
from decide_client import BeamlineRouterClient, RetryConfig

client = BeamlineRouterClient(
    host="localhost",
    port=50051,
    retry_config=RetryConfig(
        max_attempts=5,
        base_delay_ms=200,
        max_delay_ms=10000,
        jitter=0.2
    )
)
```

### Disable Circuit Breaker

```python
client = BeamlineRouterClient(
    host="localhost",
    port=50051,
    use_circuit_breaker=False
)
```

## Configuration

### RetryConfig

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max_attempts` | 3 | Maximum number of retry attempts |
| `base_delay_ms` | 100 | Initial delay in milliseconds |
| `max_delay_ms` | 5000 | Maximum delay between retries |
| `jitter` | 0.1 | Jitter factor (0.0 - 1.0) |

### CircuitBreaker

| Parameter | Default | Description |
|-----------|---------|-------------|
| `failure_threshold` | 5 | Number of failures before opening |
| `recovery_timeout` | 60.0 | Seconds before attempting recovery |

## Retry Behavior

**Exponential Backoff with Jitter**:
- Attempt 1: Immediate
- Attempt 2: ~100ms ± jitter
- Attempt 3: ~200ms ± jitter
- Attempt 4: ~400ms ± jitter
- etc.

**Retryable Errors**:
- `UNAVAILABLE`: Service temporarily unavailable
- `DEADLINE_EXCEEDED`: Request timeout
- `RESOURCE_EXHAUSTED`: Rate limit exceeded
- `ABORTED`: Operation aborted

**Non-Retryable Errors**:
- `INVALID_ARGUMENT`: Bad request data
- `UNAUTHENTICATED`: Authentication failed
- `PERMISSION_DENIED`: Authorization failed
- `NOT_FOUND`: Resource not found

## Circuit Breaker States

```
CLOSED → (failures >= threshold) → OPEN
OPEN → (recovery_timeout elapsed) → HALF_OPEN
HALF_OPEN → (success) → CLOSED
HALF_OPEN → (failure) → OPEN
```

## Production Recommendations

1. **Configure Timeouts**: Set appropriate gRPC timeouts
   ```python
   channel = grpc.insecure_channel(
       address,
       options=[('grpc.keepalive_time_ms', 10000)]
   )
   ```

2. **Use TLS**: Enable TLS for production
   ```python
   credentials = grpc.ssl_channel_credentials()
   channel = grpc.secure_channel(address, credentials)
   ```

3. **Connection Pooling**: Reuse channels across requests
   ```python
   # Share one client instance across application
   global_client = BeamlineRouterClient(...)
   ```

4. **Monitoring**: Log retries and circuit breaker events
   ```python
   import logging
   logging.basicConfig(level=logging.INFO)
   ```

5. **Graceful Shutdown**: Always close the client
   ```python
   try:
       decision = client.decide(...)
   finally:
       client.close()
   ```

## Testing

### Unit Tests

```bash
pytest test_decide_client.py
```

### Integration Tests

Requires running Router:
```bash
# Terminal 1: Start NATS
nats-server -js

# Terminal 2: Start Router
cd ../../..
rebar3 shell

# Terminal 3: Run client
python decide_client.py
```

## Troubleshooting

### "failed to connect to all addresses"

- Check Router is running: `curl localhost:50051`
- Verify host/port configuration
- Check firewall rules

### "UNAVAILABLE: DNS resolution failed"

- Verify hostname is correct
- Check network connectivity
- Try using IP address instead

### Circuit breaker always OPEN

- Increase `failure_threshold`
- Check Router health
- Review logs for root cause

## License

Same as Beamline Router project.

---

## HTTP Client Usage

### Basic Example

```python
from http_client import BeamlineHTTPClient

# Create client
client = BeamlineHTTPClient(
    base_url="http://localhost:8080",
    api_key="your-api-key",
    tenant_id="demo-tenant"
)

try:
    # Check health
    health = client.health()
    print(f"Health: {health['status']}")
    
    # Make routing decision
    decision = client.decide(
        message_type="text.generate",
        payload="Hello, world!",
        policy_id="demo-policy"
    )
    
    print(f"Provider: {decision['provider_id']}")
    print(f"Reason: {decision['reason']}")

finally:
    client.close()
```

### With Custom Retry Configuration

```python
from http_client import BeamlineHTTPClient, RetryConfig

client = BeamlineHTTPClient(
    base_url="http://localhost:8080",
    api_key="your-api-key",
    tenant_id="demo-tenant",
    retry_config=RetryConfig(
        max_attempts=5,
        base_delay_ms=200,
        max_delay_ms=10000,
        jitter=0.2
    )
)
```

### Rate Limit Handling

The HTTP client automatically handles rate limiting:

```python
# Client will automatically:
# 1. Detect 429 Too Many Requests
# 2. Parse Retry-After header
# 3. Wait the specified time
# 4. Retry the request

decision = client.decide(
    message_type="text.generate",
    payload="Your input",
    policy_id="my-policy"
)
# Retries automatically on 429
```

### Health Check

```python
try:
    health = client.health()
    if health['status'] == 'healthy':
        print("✅ c-gateway is healthy")
    else:
        print(f"⚠️  c-gateway unhealthy: {health.get('reason')}")
except Exception as e:
    print(f"❌ Health check failed: {e}")
```

### Get Metrics

```python
metrics = client.metrics()
print(metrics)  # Prometheus text format
```

## HTTP Client Configuration

### Request Headers

**Required**:
- `X-Tenant-ID`: Tenant identifier
- `X-API-Key`: API key for authentication

**Optional**:
- `X-Request-ID`: Request tracking ID
- `X-Trace-ID`: Distributed tracing ID

### Retry Behavior

**Exponential Backoff**:
- Attempt 1: Immediate
- Attempt 2: ~200ms ± jitter
- Attempt 3: ~400ms ± jitter
- Attempt 4: ~800ms ± jitter

**Retryable Status Codes**:
- `408`: Request Timeout
- `429`: Too Many Requests (with Retry-After)
- `500`: Internal Server Error
- `502`: Bad Gateway
- `503`: Service Unavailable
- `504`: Gateway Timeout

**Non-Retryable Status Codes**:
- `400`: Bad Request
- `401`: Unauthorized
- `403`: Forbidden
- `404`: Not Found

### Rate Limiting

**Default c-gateway limits**:
- 50 requests per 60-second window
- Per tenant-ID + API key

**Response Headers** (on 429):
```
Retry-After: 60
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1640000060
```

**Client behavior**:
1. Receives 429 response
2. Parses `Retry-After` header (defaults to 60s)
3. Waits specified time
4. Retries request (up to max_attempts)

## HTTP vs gRPC: When to Use

### Use HTTP Client (`http_client.py`) when:
- ✅ You need REST API integration
- ✅ You're behind a firewall that blocks gRPC
- ✅ You want simpler debugging (HTTP tools)
- ✅ You need rate limiting per tenant
- ✅ You prefer stateless requests

### Use gRPC Client (`grpc_client.py`) when:
- ✅ You need maximum performance (binary protocol)
- ✅ You want type safety (protobuf schemas)
- ✅ You need bidirectional streaming
- ✅ You have direct network access to Router
- ✅ You prefer strongly-typed interfaces

## Testing HTTP Client

### Prerequisites

```bash
# Start NATS
nats-server -js

# Start Router
cd ../../..
rebar3 shell

# Start c-gateway
cd ~/aigroup/apps/c-gateway
./build/gateway
```

### Run Example

```bash
python http_client.py
```

### Integration Tests

Requires running c-gateway:
```bash
# Terminal 1: Start c-gateway
cd ~/aigroup/apps/c-gateway
./build/gateway

# Terminal 2: Run client
cd ~/aigroup/apps/otp/router/examples/clients/python
python http_client.py
```

## HTTP Client Troubleshooting

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

### "429 Too Many Requests"

**Cause**: Rate limit exceeded

**Solution**:
- Client will auto-retry with Retry-After
- Reduce request rate
- Increase rate limits (GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT)

### "503 Service Unavailable"

**Cause**: c-gateway unhealthy (NATS disconnected)

**Solution**:
```bash
# Check NATS is running
nats-server -js

# Check c-gateway logs
tail -f ~/aigroup/apps/c-gateway/gateway.log
```

### "401 Unauthorized"

**Cause**: Invalid API key

**Solution**:
- Verify API key is correct
- Check c-gateway API key configuration

## Dependencies

**For both clients**:
```txt
requests>=2.31.0
grpcio>=1.59.0
grpcio-tools>=1.59.0
protobuf>=4.24.4
```

Install:
```bash
pip install requests grpcio grpcio-tools protobuf
```

## Next Steps

- **Main Integration Guide**: See `~/aigroup/apps/otp/router/docs/INTEGRATION_GUIDE.md`
- **c-gateway Details**: See `.ai/task_intgration_descritption/c_gateway.md`
- **Extensions**: See `~/aigroup/apps/otp/router/docs/EXTENSIONS_INTEGRATION.md`
- **CAF Worker**: See `~/aigroup/apps/otp/router/docs/CAF_WORKER_INTEGRATION.md`

