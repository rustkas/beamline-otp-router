# Beamline Router Node.js Clients

Node.js examples for integrating with Beamline Router via gRPC (direct) or HTTP (via c-gateway).

## Clients

### 1. gRPC Client (`grpc_client.js`)
Direct integration with Router using gRPC protocol.

### 2. HTTP Client (`http_client.js`)
Integration via c-gateway using HTTP/REST API.

## Features

**gRPC Client**:
- **Type-safe gRPC calls**: Direct Router integration
- **Exponential backoff retry**: Automatic retries with jitter
- **Circuit breaker**: Prevents cascading failures (CLOSED/OPEN/HALF_OPEN)
- **Error handling**: Graceful handling of transient errors

**HTTP Client**:
- **REST API**: Integration via c-gateway
- **Rate limit handling**: Respects 429 + Retry-After header
- **Exponential backoff**: Automatic retries with jitter
- **Error handling**: Retryable vs non-retryable errors
- **API Key auth**: X-API-Key authentication
- **Health & Metrics**: Built-in endpoints

## Quick Start

### 1. Install Dependencies

```bash
npm install
```

Or manually:
```bash
npm install @grpc/grpc-js @grpc/proto-loader axios
```

### 2. Run Examples

**gRPC Client**:
```bash
npm run grpc-example
# or
node grpc_client.js
```

**HTTP Client**:
```bash
npm run http-example
# or
node http_client.js
```

## Usage

### gRPC Client

#### Basic Example

```javascript
const { BeamlineGRPCClient } = require('./grpc_client');

const client = new BeamlineGRPCClient({
    host: 'localhost',
    port: 50051
});

try {
    const decision = await client.decide({
        tenantId: 'my-tenant',
        policyId: 'my-policy',
        taskType: 'text.generate',
        taskPayload: 'Hello, world!'
    });
    
    console.log(`Provider: ${decision.provider_id}`);
    console.log(`Reason: ${decision.reason}`);
} finally {
    client.close();
}
```

#### With Custom Retry

```javascript
const { BeamlineGRPCClient, RetryConfig } = require('./grpc_client');

const client = new BeamlineGRPCClient({
    host: 'localhost',
    port: 50051,
    retryConfig: new RetryConfig({
        maxAttempts: 5,
        baseDelayMs: 200,
        maxDelayMs: 10000,
        jitter: 0.2
    })
});
```

#### Disable Circuit Breaker

```javascript
const client = new BeamlineGRPCClient({
    host: 'localhost',
    port: 50051,
    useCircuitBreaker: false
});
```

### HTTP Client

#### Basic Example

```javascript
const { BeamlineHTTPClient } = require('./http_client');

const client = new BeamlineHTTPClient({
    baseUrl: 'http://localhost:8080',
    apiKey: 'your-api-key',
    tenantId: 'demo-tenant'
});

try {
    // Check health
    const health = await client.health();
    console.log(`Health: ${health.status}`);
    
    // Make decision
    const decision = await client.decide({
        messageType: 'text.generate',
        payload: 'Hello, world!',
        policyId: 'demo-policy'
    });
    
    console.log(`Provider: ${decision.provider_id}`);
} catch (error) {
    console.error('Error:', error.message);
}
```

#### With Custom Retry

```javascript
const { BeamlineHTTPClient, RetryConfig } = require('./http_client');

const client = new BeamlineHTTPClient({
    baseUrl: 'http://localhost:8080',
    apiKey: 'your-api-key',
    tenantId: 'demo-tenant',
    retryConfig: new RetryConfig({
        maxAttempts: 5,
        baseDelayMs: 200,
        maxDelayMs: 10000,
        jitter: 0.2
    })
});
```

#### Rate Limit Handling

```javascript
// Client automatically handles rate limiting
try {
    const decision = await client.decide({
        messageType: 'text.generate',
        payload: 'Your input',
        policyId: 'my-policy'
    });
    // Retries automatically on 429
} catch (error) {
    console.error('Failed after retries:', error.message);
}
```

#### Health Check

```javascript
try {
    const health = await client.health();
    if (health.status === 'healthy') {
        console.log('✅ c-gateway is healthy');
    } else {
        console.log(`⚠️  c-gateway unhealthy: ${health.reason}`);
    }
} catch (error) {
    console.error('❌ Health check failed:', error.message);
}
```

#### Get Metrics

```javascript
const metrics = await client.metrics();
console.log(metrics); // Prometheus text format
```

## Configuration

### RetryConfig

| Parameter | Default | Description |
|-----------|---------|-------------|
| `maxAttempts` | 3 | Maximum number of retry attempts |
| `baseDelayMs` | 100 | Initial delay in milliseconds |
| `maxDelayMs` | 5000 | Maximum delay between retries |
| `jitter` | 0.1 | Jitter factor (0.0 - 1.0) |

### CircuitBreaker (gRPC only)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `failureThreshold` | 5 | Number of failures before opening |
| `recoveryTimeoutSeconds` | 60 | Seconds before attempting recovery |

## Retry Behavior

**Exponential Backoff with Jitter**:
- Attempt 1: Immediate
- Attempt 2: ~100ms ± jitter
- Attempt 3: ~200ms ± jitter
- Attempt 4: ~400ms ± jitter
- etc.

**gRPC Retryable Errors**:
- `UNAVAILABLE`: Service temporarily unavailable
- `DEADLINE_EXCEEDED`: Request timeout
- `RESOURCE_EXHAUSTED`: Rate limit exceeded
- `ABORTED`: Operation aborted

**HTTP Retryable Status Codes**:
- `408`: Request Timeout
- `429`: Too Many Requests
- `500`: Internal Server Error
- `502`: Bad Gateway
- `503`: Service Unavailable
- `504`: Gateway Timeout

## Circuit Breaker States

**gRPC client only**:

```
CLOSED → (failures >= threshold) → OPEN
OPEN → (recovery_timeout elapsed) → HALF_OPEN
HALF_OPEN → (success) → CLOSED
HALF_OPEN → (failure) → OPEN
```

## HTTP vs gRPC: When to Use

### Use HTTP Client when:
- ✅ You need REST API integration
- ✅ You're behind a firewall that blocks gRPC
- ✅ You want simpler debugging (HTTP tools)
- ✅ You need rate limiting per tenant
- ✅ You prefer stateless requests

### Use gRPC Client when:
- ✅ You need maximum performance (binary protocol)
- ✅ You want type safety (protobuf schemas)
- ✅ You need bidirectional streaming
- ✅ You have direct network access to Router
- ✅ You prefer strongly-typed interfaces

## Testing

### Prerequisites

**For gRPC Client**:
```bash
# Terminal 1: Start NATS
nats-server -js

# Terminal 2: Start Router
cd ../../..
rebar3 shell

# Terminal 3: Run client
node grpc_client.js
```

**For HTTP Client**:
```bash
# Terminal 1: Start c-gateway
cd ~/aigroup/apps/c-gateway
./build/gateway

# Terminal 2: Run client
node http_client.js
```

## Troubleshooting

### gRPC "failed to connect to all addresses"

**Cause**: Router not running or wrong address

**Solution**:
```bash
# Check Router is running
ps aux | grep beam

# Check gRPC port (default: 50051)
netstat -tuln | grep 50051

# Start Router
cd ~/aigroup/apps/otp/router
rebar3 shell
```

### HTTP "Connection refused"

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

### Circuit breaker always OPEN

**Cause**: Too many failures

**Solution**:
- Increase `failureThreshold`
- Check Router health
- Review logs for root cause
- Disable circuit breaker temporarily

## Production Recommendations

### 1. Configure Timeouts

**gRPC**:
```javascript
const grpc = require('@grpc/grpc-js');

const client = new grpc.Client(
    address,
    credentials,
    {
        'grpc.keepalive_time_ms': 10000,
        'grpc.keepalive_timeout_ms': 5000
    }
);
```

**HTTP**:
```javascript
const client = new BeamlineHTTPClient({
    baseUrl: 'http://localhost:8080',
    timeout: 5000  // 5 seconds
});
```

### 2. Use TLS

**gRPC** (for production):
```javascript
const grpc = require('@grpc/grpc-js');
const credentials = grpc.credentials.createSsl();

const client = new BeamlineGRPCClient({
    host: 'router.example.com',
    port: 50051,
    credentials: credentials
});
```

### 3. Connection Pooling

Reuse client instances across requests:
```javascript
// Create one client instance
const globalClient = new BeamlineHTTPClient({...});

// Reuse across application
app.post('/route', async (req, res) => {
    const decision = await globalClient.decide({...});
    res.json(decision);
});
```

### 4. Monitoring

Log retries and circuit breaker events:
```javascript
console.log = require('winston').createLogger({...});
```

### 5. Graceful Shutdown

Always close clients:
```javascript
process.on('SIGTERM', () => {
    client.close();
    process.exit(0);
});
```

## Dependencies

```json
{
  "@grpc/grpc-js": "^1.9.0",
  "@grpc/proto-loader": "^0.7.10",
  "axios": "^1.6.0"
}
```

Install:
```bash
npm install @grpc/grpc-js @grpc/proto-loader axios
```

## Next Steps

- **Main Integration Guide**: See `~/aigroup/apps/otp/router/docs/INTEGRATION_GUIDE.md`
- **c-gateway Details**: See `.ai/task_intgration_descritption/c_gateway.md`
- **Extensions**: See `~/aigroup/apps/otp/router/docs/EXTENSIONS_INTEGRATION.md`
- **CAF Worker**: See `~/aigroup/apps/otp/router/docs/CAF_WORKER_INTEGRATION.md`

## License

Same as Beamline Router project.
