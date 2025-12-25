# Integration Guide: Beamline AI Router

Comprehensive guide for integrating with the Beamline AI Router and Gateway stack.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Quick Start](#quick-start)
3. [HTTP Integration via c-gateway](#http-integration-via-c-gateway)
4. [gRPC Direct Integration](#grpc-direct-integration)
5. [Resilience Patterns](#resilience-patterns)
6. [Production Deployment](#production-deployment)
7. [Troubleshooting](#troubleshooting)
8. [Advanced Topics](#advanced-topics)

---

## Architecture Overview

### Component Overview

```
┌─────────────┐
│   Client    │
└──────┬──────┘
       │
       ├─── HTTP ────▶ ┌─────────────┐
       │               │  c-gateway  │───── NATS ────▶ ┌─────────┐
       │               └─────────────┘                  │  Router │
       │                                                │ (Erlang)│
       └─── gRPC ───────────────────────────────────────▶ └─────────┘
                                                          │
                                                          ▼
                                                    ┌──────────┐
                                                    │   CAF    │
                                                    └──────────┘
```

### Services

**c-gateway** (`~/aigroup/apps/c-gateway`):
- **Language**: C++
- **Purpose**: HTTP-to-NATS gateway
- **Features**: Rate limiting, metrics, tracing, abuse detection
- **Endpoints**:
  - `POST /api/v1/routes/decide` - Routing decisions
  - `GET /_health` - Health check
  - `GET /metrics` - Prometheus metrics

**Router** (`~/aigroup/apps/otp/router`):
- **Language**: Erlang/OTP
- **Purpose**: Core routing logic and policy evaluation
- **Protocols**: gRPC, NATS
- **Features**: Policy management, load balancing, telemetry

**CAF Worker** (`~/aigroup/apps/caf/processor`):
- **Language**: C++ (CAF - C++ Actor Framework)
- **Purpose**: Execution engine for Flow DSL blocks
- **Features**: 
  - Actor-based parallelism with resource pools (CPU, GPU, I/O)
  - Block executors: HTTP, FS, SQL, Human approval
  - Multi-tenant isolation with resource quotas
  - Retry logic with exponential backoff
- **NATS Subjects**:
  - Input: `caf.exec.assign.v1` (assignments from Router)
  - Output: `caf.exec.result.v1` (execution results)
  - ACK: `caf.exec.assign.v1.ack` (assignment acknowledgments)
- **Health**: `GET /_health`
- **Metrics**: Prometheus endpoint on port 9090
- **Detailed Guide**: See [CAF_WORKER_INTEGRATION.md](CAF_WORKER_INTEGRATION.md)

**Integration Flow**:
```
Client → c-gateway → Router (decision) → NATS → CAF Worker (execution) → NATS → Router (result)
```

---

## Quick Start

### Prerequisites

- **NATS Server** with JetStream enabled
- **c-gateway** built and running
- **Router** running (Erlang/OTP application)
- **cURL**, Python, Node.js, or Go (for examples)

### Start Services

**1. Start NATS**:
```bash
cd ~/aigroup/apps/otp/router
./scripts/nats_start.sh
```

**2. Start Router**:
```bash
cd ~/aigroup/apps/otp/router
rebar3 shell
```

**3. Start c-gateway**:
```bash
cd ~/aigroup/apps/c-gateway
./build/gateway
```

### Verify Setup

**Check c-gateway health**:
```bash
curl http://localhost:8080/_health
```

**Expected**: `{"status":"healthy"}`

### Make Your First Request

```bash
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: demo-tenant" \
  -H "X-API-Key: your-api-key" \
  -d '{
    "version": "1",
    "message": {
      "type": "text.generate",
      "payload": "Hello, world!"
    },
    "policy_id": "demo-policy"
  }'
```

**Expected Response**:
```json
{
  "decision_id": "dec-123456",
  "provider_id": "openai",
  "reason": "weighted_random",
  "priority": 1,
  "expected_cost": 0.0001
}
```

---

## HTTP Integration via c-gateway

### Endpoints

#### POST /api/v1/routes/decide

**Purpose**: Request a routing decision for AI task execution.

**Request Headers**:
```
Content-Type: application/json
X-Tenant-ID: <tenant_id>          (required)
X-API-Key: <api_key>               (required for auth)
X-Trace-ID: <trace_id>             (optional, for distributed tracing)
X-Request-ID: <request_id>         (optional, for request tracking)
```

**Request Body**:
```json
{
  "version": "1",
  "message": {
    "message_id": "msg-001",           // optional
    "type": "text.generate",           // required
    "payload": "Your input here",      // required
    "metadata": {                      // optional
      "model": "gpt-4",
      "temperature": 0.7
    }
  },
  "policy_id": "my-policy",            // required
  "context": {                         // optional
    "user_id": "user-123",
    "session_id": "sess-456"
  },
  "run_id": "run-789"                  // optional
}
```

**Response (200 OK)**:
```json
{
  "decision_id": "dec-123456",
  "provider_id": "openai",
  "reason": "weighted_random",
  "priority": 1,
  "expected_latency_ms": 500,
  "expected_cost": 0.0001,
  "metadata": {}
}
```

**Error Responses**:

**400 Bad Request**:
```json
{
  "error": "bad_request",
  "message": "Missing required field: policy_id"
}
```

**429 Too Many Requests** (Rate limit exceeded):
```json
{
  "error": "rate_limit_exceeded",
  "message": "Rate limit exceeded for endpoint /api/v1/routes/decide",
  "details": {
    "endpoint": "/api/v1/routes/decide",
    "limit": 50,
    "retry_after_seconds": 60
  }
}
```

**Response Headers**:
```
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 49
X-RateLimit-Reset: 1640000000
```

#### GET /_health

**Purpose**: Check c-gateway health status.

**Response (200 OK)**:
```json
{
  "status": "healthy"
}
```

**Response (503 Service Unavailable)**:
```json
{
  "status": "unhealthy",
  "reason": "NATS disconnected"
}
```

#### GET /metrics

**Purpose**: Get Prometheus metrics.

**Response** (Prometheus text format):
```
# HELP gateway_requests_total Total HTTP requests
# TYPE gateway_requests_total counter
gateway_requests_total{endpoint="/api/v1/routes/decide",status="200"} 1234

# HELP gateway_request_duration_seconds HTTP request duration
# TYPE gateway_request_duration_seconds histogram
gateway_request_duration_seconds_bucket{endpoint="/api/v1/routes/decide",le="0.005"} 800
gateway_request_duration_seconds_bucket{endpoint="/api/v1/routes/decide",le="0.01"} 1000
```

### Rate Limiting

**Configuration**:
- `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT` - Requests per window (default: 50)
- `GATEWAY_RATE_LIMIT_WINDOW_SECONDS` - Window size (default: 60)

**Distributed Mode** (Redis):
- `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=true`
- `GATEWAY_REDIS_URL=redis://localhost:6379`

**Per-Tenant Limits**:
Rate limits are applied per tenant-ID + API key combination.

### Authentication

**API Key Authentication**:
- Header: `X-API-Key: your-api-key`
- Validated against configured keys
- Per-tenant keys supported

**Future**: OAuth2, JWT tokens

### Python HTTP Client Example

```python
import requests
from typing import Dict, Optional

class BeamlineHTTPClient:
    def __init__(self, base_url: str = "http://localhost:8080",
                 api_key: str = None, tenant_id: str = None):
        self.base_url = base_url
        self.api_key = api_key
        self.tenant_id = tenant_id
        self.session = requests.Session()
    
    def decide(self, message_type: str, payload: str, policy_id: str,
              metadata: Optional[Dict] = None) -> Dict:
        """Request a routing decision"""
        headers = {
            "Content-Type": "application/json",
            "X-Tenant-ID": self.tenant_id,
            "X-API-Key": self.api_key
        }
        
        body = {
            "version": "1",
            "message": {
                "type": message_type,
                "payload": payload,
                "metadata": metadata or {}
            },
            "policy_id": policy_id
        }
        
        response = self.session.post(
            f"{self.base_url}/api/v1/routes/decide",
            json=body,
            headers=headers,
            timeout=5
        )
        response.raise_for_status()
        return response.json()
    
    def health(self) -> Dict:
        """Check gateway health"""
        response = self.session.get(f"{self.base_url}/_health")
        response.raise_for_status()
        return response.json()

# Usage
client = BeamlineHTTPClient(
    api_key="your-api-key",
    tenant_id="demo-tenant"
)

decision = client.decide(
    message_type="text.generate",
    payload="Hello, world!",
    policy_id="demo-policy"
)
print(f"Provider: {decision['provider_id']}")
```

---

## gRPC Direct Integration

### Proto Definitions

**Location**: `~/aigroup/apps/otp/router/proto/beamline/flow/v1/flow.proto`

**Service**: `RouterService`

**Methods**:
- `Decide(RouteRequest) returns (RouteDecision)` - Request routing decision
- `GetValidatorsHealth(GetValidatorsHealthRequest) returns (GetValidatorsHealthResponse)` - Check Router health
- `GetCheckpointStatus(GetCheckpointStatusRequest) returns (GetCheckpointStatusResponse)` - Get checkpoint status

### Generate Client Code

**Python**:
```bash
cd ~/aigroup/apps/otp/router
python -m grpc_tools.protoc \
  -I proto \
  --python_out=. \
  --grpc_python_out=. \
  proto/beamline/flow/v1/flow.proto
```

**Node.js**:
```bash
npm install @grpc/grpc-js @grpc/proto-loader
```

**Go**:
```bash
protoc -I proto \
  --go_out=. --go_opt=paths=source_relative \
  --go-grpc_out=. --go-grpc_opt=paths=source_relative \
  proto/beamline/flow/v1/flow.proto
```

### Python gRPC Client

See: `~/aigroup/apps/otp/router/examples/clients/python/grpc_client.py`

**Features**:
- Exponential backoff retry
- Circuit breaker pattern
- Timeout handling
- Error recovery

**Quick Example**:
```python
from grpc_client import BeamlineRouterClient

client = BeamlineRouterClient(host="localhost", port=50051)
decision = client.decide(
    tenant_id="demo-tenant",
    policy_id="demo-policy",
    task_type="text.generate",
    task_payload="Hello, world!"
)
client.close()
```

---

## Resilience Patterns

### Retry with Exponential Backoff

**Pattern**: Retry failed requests with increasing delays.

**When to use**:
- Transient network errors
- Service temporary unavailability
- Rate limit errors (with backoff)

**Python Example**:
```python
import time
import random

def exponential_backoff_retry(func, max_attempts=3, base_delay_ms=100):
    for attempt in range(1, max_attempts + 1):
        try:
            return func()
        except Exception as e:
            if attempt == max_attempts:
                raise
            
            # Calculate delay with jitter
            delay_ms = min(base_delay_ms * (2 ** (attempt - 1)), 5000)
            jitter = delay_ms * 0.1 * random.uniform(-1, 1)
            delay_ms += jitter
            
            print(f"Attempt {attempt} failed: {e}. Retrying in {delay_ms:.0f}ms...")
            time.sleep(delay_ms / 1000.0)
```

### Circuit Breaker

**Pattern**: Prevent cascading failures by failing fast when service is unhealthy.

**States**:
- **CLOSED**: Normal operation
- **OPEN**: Too many failures, reject requests immediately
- **HALF_OPEN**: Testing if service recovered

**Python Example**: See `examples/clients/python/grpc_client.py`

### Rate Limiting

**Client-Side**: Respect `X-RateLimit-*` headers and `Retry-After`.

**Example**:
```python
response = requests.post(url, json=data)
if response.status_code == 429:
    retry_after = int(response.headers.get('Retry-After', 60))
    time.sleep(retry_after)
    # Retry request
```

### Timeouts

**HTTP**:
```python
requests.post(url, json=data, timeout=5)  # 5 seconds
```

**gRPC**:
```python
stub.Decide(request, timeout=5)
```

---

## Production Deployment

### Service Dependencies

```
NATS Server (JetStream)
   ↓
Router (Erlang)
   ↓
c-gateway (C++)
   ↓
Client Applications
```

### Configuration

**c-gateway Environment Variables**:
```bash
# NATS
NATS_URL=nats://localhost:4222

# Rate Limiting
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=50
GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false

# Distributed Rate Limiting (Redis)
GATEWAY_REDIS_URL=redis://localhost:6379

# Admin API
GATEWAY_ADMIN_API_KEY=your-secret-key

# Tracing
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
```

**Router Configuration**: See `config/prod.config`

### Monitoring

**Key Metrics**:
- `gateway_requests_total` - Request count by endpoint/status
- `gateway_request_duration_seconds` - Request latency histogram
- `gateway_rate_limit_exceeded_total` - Rate limit violations
- `router_decide_latency_ms` - Router processing time
- `router_nats_messages_total` - NATS message count

**Health Checks**:
- c-gateway: `GET /_health`
- Router: gRPC `GetValidatorsHealth()`
- NATS: Check connection status

### High Availability

**c-gateway**:
- Run multiple instances behind load balancer
- Enable distributed rate limiting with Redis
- Configure health checks

**Router**:
- Run cluster of Erlang nodes
- Configure NATS JetStream for persistence
- Enable circuit breaking for external services

---

## Troubleshooting

### "Rate limit exceeded" (429)

**Cause**: Too many requests in window.

**Solution**:
- Implement exponential backoff
- Respect `Retry-After` header
- Increase rate limits if legitimate
- Use distributed rate limiting for multi-instance

### "Service Unavailable" (503)

**Cause**: c-gateway or Router unhealthy.

**Check**:
```bash
# c-gateway health
curl http://localhost:8080/_health

# NATS status  
./scripts/nats_status.sh
```

**Solution**:
- Restart unhealthy service
- Check logs for errors
- Verify NATS connection

### "NATS disconnected"

**Cause**: NATS server not running or unreachable.

**Solution**:
```bash
# Check NATS
./scripts/nats_status.sh

# Restart NATS
./scripts/nats_start.sh
```

### High Latency

**Check**:
- c-gateway metrics: `gateway_request_duration_seconds`
- Router metrics: `router_decide_latency_ms`
- NATS round-trip time

**Solutions**:
- Scale Router instances
- Optimize policies
- Check network latency
- Enable caching (if applicable)

### gRPC "Unavailable" Errors

**Cause**: Router not reachable or not started.

**Check**:
```bash
# Check Router is running
ps aux | grep beam

# Check gRPC port (default: 50051)
netstat -tuln | grep 50051
```

**Solution**:
- Start Router: `rebar3 shell`
- Verify firewall rules
- Check gRPC endpoint configuration

---

## Next Steps

- **Explore Examples**: `~/aigroup/apps/otp/router/examples/clients/`
- **Review API Docs**: `~/aigroup/apps/c-gateway/docs/ADMIN_GRPC_API.md`
- **Check Architecture**: `~/aigroup/apps/otp/router/docs/ARCHITECTURE.md`
- **Monitor Metrics**: Configure Prometheus + Grafana
- **Enable Tracing**: Configure OpenTelemetry for distributed tracing

## Support

- **Documentation**: `~/aigroup/docs/`
- **Issues**: Check project issue tracker
- **Logs**: `_build/test/logs/` (Router), `./gateway.log` (c-gateway)

---

## Advanced Topics

### CAF Worker Integration

For comprehensive information about integrating with the CAF Worker execution engine:

**See**: [CAF_WORKER_INTEGRATION.md](C AF_WORKER_INTEGRATION.md)

**Topics covered**:
- NATS subject contracts (`caf.exec.assign.v1`, `caf.exec.result.v1`)
- Block executor types: HTTP, FS, SQL, Human approval
- Message formats (ExecAssignment, ExecResult)
- Error handling and retry behavior
- Multi-tenancy and resource quotas
- Performance targets (500 tasks/s)
- Observability (metrics, tracing, logs)

**Quick Example**:
```erlang
%% Router publishes assignment to CAF Worker
Assignment = #{
    <<"version">> => <<"1">>,
    <<"assignment_id">> => <<"assign-123">>,
    <<"executor">> => #{<<"provider_id">> => <<"openai">>},
    <<"job">> => #{
        <<"type">> => <<"http.request">>,
        <<"url">> => <<"https://api.openai.com/v1/chat/completions">>
    }
},
router_nats:publish(<<"caf.exec.assign.v1">>, jsx:encode(Assignment)).
```

### Extensions Pipeline

For information about extending Router behavior with custom extensions:

**See**: [EXTENSIONS_INTEGRATION.md](EXTENSIONS_INTEGRATION.md)

**Topics covered**:
- Extension types: Pre-processor, Validator, Post-processor, Custom Provider
- NATS subjects pattern (`beamline.ext.{type}.{id}.v{version}`)
- Extension Registry and Routing Policy configuration
- Message contracts (request/response)
- Creating extensions in any language (Node.js, Go, Rust, Python)
- Version routing per tenant/environment
- Testing and E2E scenarios

**Quick Example** (Node.js Pre-processor):
```javascript
const { connect, JSONCodec } = require('nats');

async function main() {
  const nc = await connect({ servers: 'nats://localhost:4222' });
  const jc = JSONCodec();
  
  const sub = nc.subscribe('beamline.ext.pre.normalize_text.v1');
  
  for await (const msg of sub) {
    const request = jc.decode(msg.data);
    
    // Process message
    const normalized = request.payload.payload.toLowerCase().trim();
    
    const response = {
      payload: {
        ...request.payload,
        payload: normalized
      },
      metadata: request.metadata
    };
    
    msg.respond(jc.encode(response));
  }
}

main();
```

### Integration Architecture

**Complete flow**:
```
Client
  ↓ HTTP (API Key, Rate Limiting)
c-gateway
  ↓ NATS (beamline.router.v1.decide)
Router
  ├→ Pre-processors (NATS beamline.ext.pre.*.v1)
  │   └→ Normalize, enrich, transform request
  ├→ Validators (NATS beamline.ext.validate.*.v1)
  │   └→ PII guard, content policy, accept/reject
  ├→ Provider Selection (Policy evaluation)
  ├→ Provider Execution
  │   ├→ OpenAI/Anthropic (standard providers)
  │   ├→ Custom Providers (NATS beamline.provider.*.v1)
  │   └→ CAF Worker (NATS caf.exec.assign.v1 → caf.exec.result.v1)
  │       └→ Block execution (HTTP, FS, SQL, Human)
  └→ Post-processors (NATS beamline.ext.post.*.v1)
      └→ Mask PII, translate, transform response
  ↓
Response to Client
```

### Key Integration Points

1. **Client → c-gateway** (HTTP)
   - Authentication, rate limiting
   - Entry point for all requests

2. **c-gateway → Router** (NATS)
   - CP1/CP2 protocol
   - Message transformation

3. **Router → Extensions** (NATS)
   - Pre-processors, Validators, Post-processors
   - Custom Providers
   - Configuration-driven behavior

4. **Router → CAF Worker** (NATS)
   - Block execution
   - Assignment and result flow
   - Multi-tenant isolation

### NATS Subjects Summary

**Router Core**:
- `beamline.router.v1.decide` - Routing decisions

**Extensions**:
- `beamline.ext.pre.{id}.v{version}` - Pre-processors
- `beamline.ext.validate.{id}.v{version}` - Validators
- `beamline.ext.post.{id}.v{version}` - Post-processors
- `beamline.provider.{id}.v{version}` - Custom providers

**CAF Worker**:
- `caf.exec.assign.v1` - Assignments from Router
- `caf.exec.assign.v1.ack` - Assignment acknowledgments
- `caf.exec.result.v1` - Execution results
- `caf.worker.heartbeat.v1` - Worker heartbeats
- `caf.exec.dlq.v1` - Dead Letter Queue

### Performance Considerations

**Target Latency** (p95): < 500ms end-to-end

**Breakdown**:
- c-gateway overhead: < 10ms
- Pre-processors: 50-100ms total
- Validators: 50-100ms total
- Provider execution: 200-300ms
- Post-processors: 50-100ms total
- CAF Worker blocks: Variable (HTTP≤500ms, FS≤200ms, SQL≤300ms)

**Throughput Targets**:
- c-gateway: 1000+ req/s
- Router with extensions: 500+ req/s
- CAF Worker: 500+ tasks/s (CP3 target)

### Configuration-Driven Design

**Key Principle**: Add functionality via configuration, not code changes

**Extension Registry** (PostgreSQL + Mnesia):
```json
{
  "extension_id": {
    "type": "pre",
    "subject": "beamline.ext.pre.normalize_text.v1",
    "timeout_ms": 80,
    "retry": 0
  }
}
```

**Routing Policy** (JSON):
```json
{
  "policy_id": "my_policy",
  "pre": [{"id": "normalize_text", "mode": "required"}],
  "validators": [{"id": "pii_guard", "on_fail": "block"}],
  "post": [{"id": "mask_pii", "mode": "required"}]
}
```

**Benefits**:
- ✅ No Router code changes for new extensions
- ✅ Language-agnostic (Node.js, Go, Rust, Python, C++)
- ✅ Version routing per tenant/environment
- ✅ Full observability out-of-the-box

### Further Reading

- **CAF Worker**: [CAF_WORKER_INTEGRATION.md](CAF_WORKER_INTEGRATION.md)
- **Extensions**: [EXTENSIONS_INTEGRATION.md](EXTENSIONS_INTEGRATION.md)
- **Integration Descriptions**: See `.ai/task_intgration_descritption/` for detailed component descriptions
- **API Contracts**: `~/aigroup/docs/EXTENSIONS_API.md`
- **E2E Testing**: `~/aigroup/apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md`
