# Scope: T-INTEG-02 (Revised)

## In Scope

### 1. gRPC Client Examples
**Purpose**: Reference implementations for integrating with Beamline Router via gRPC

**Leverages**: Existing `c-gateway` and Router gRPC endpoints

#### Python Client (`examples/clients/python/`)
- gRPC client using `grpcio`
- Retry with exponential backoff
- Circuit breaker pattern
- Streaming support (if applicable)
- Integration with existing c-gateway

#### Node.js Client (`examples/clients/nodejs/`)
- gRPC client using `@grpc/grpc-js`
- Promise-based API
- Retry middleware
- TypeScript definitions
- Integration examples

#### Go Client (`examples/clients/go/`)
- gRPC client using `google.golang.org/grpc`
- Context-based timeouts
- Interceptors for retry
- Graceful shutdown
- Integration with c-gateway

### 2. HTTP Client Examples
**Purpose**: Show how to integrate with existing `c-gateway` via HTTP

**Note**: Uses existing `~/aigroup/apps/c-gateway` - no new gateway needed

#### Python HTTP Client (`examples/clients/python/http_client.py`)
- `requests` library with retry
- Connection pooling
- Error handling
- Examples for c-gateway endpoints

#### Node.js HTTP Client (`examples/clients/nodejs/http_client.js`)
- `axios` with interceptors
- Retry logic
- Promise-based

#### cURL Examples (`examples/clients/curl/`)
- Shell scripts for testing
- Common scenarios
- Authentication examples

### 3. Retry & Backpressure Patterns

#### Retry Patterns (`examples/patterns/retry/`)
- **Exponential Backoff**: `exponential_backoff.{py,js,go}`
- **Circuit Breaker**: `circuit_breaker.{py,js,go}`
- **Jitter**: Random jitter to prevent thundering herd
- **Max Attempts**: Configurable retry limits
- **README**: When to use each pattern

#### Backpressure Handling (`examples/patterns/backpressure/`)
- **Rate Limiting**: Token bucket, leaky bucket
- **Queue Management**: Max queue size, reject when full
- **Adaptive Throttling**: Based on error rate
- **Examples**: `rate_limiter.{py,js,go}`

### 4. Integration Guide

**Document**: `docs/INTEGRATION_GUIDE.md`

**Sections**:
1. **Architecture Overview** (5 min)
   - c-gateway: HTTP → Router
   - caf: CAF integration
   - ui_web: Web interface
   - Router: Core routing logic

2. **Quick Start** (5 min)
   - Start services (c-gateway, Router, NATS)
   - Make first request
   - Verify response

3. **HTTP Integration via c-gateway** (10 min)
   - c-gateway endpoints
   - Request/response format
   - Authentication
   - Error handling

4. **gRPC Direct Integration** (15 min)
   - Router gRPC service
   - Proto definitions
   - Client examples
   - Streaming (if applicable)

5. **Resilience Patterns** (20 min)
   - Retry strategies
   - Circuit breakers
   - Rate limiting
   - Timeouts

6. **Production Deployment** (30 min)
   - Service dependencies
   - Configuration
   - Monitoring
   - Troubleshooting

### 5. Docker Compose Environment

**File**: `examples/docker-compose.yml`

**Services**:
- `router`: Beamline Router (Erlang/OTP)
- `nats`: NATS with JetStream
- `c-gateway`: Existing C gateway (if containerized)
- `prometheus`: Metrics collection
- `grafana`: Dashboards (optional)

**Note**: Integrates with existing services, doesn't replace them

## Out of Scope

- **New Gateway Implementation** - use existing c-gateway
- **Replacing c-gateway** - work with existing architecture
- **CAF modifications** - integration examples only
- **UI_web changes** - use existing web interface
- **Production-ready SDKs** - only examples and reference implementations
- **Language-specific SDKs** beyond Python/Node.js/Go
- **Multi-region deployment** - single-region examples only

## Revised Deliverables

```
examples/
├── clients/
│   ├── python/
│   │   ├── grpc_client.py          # gRPC client (existing)
│   │   ├── http_client.py          # HTTP client for c-gateway
│   │   ├── README.md               # (existing)
│   │   └── requirements.txt        # Dependencies
│   ├── nodejs/
│   │   ├── grpc_client.js          # gRPC client
│   │   ├── http_client.js          # HTTP client for c-gateway
│   │   ├── package.json
│   │   └── README.md
│   ├── go/
│   │   ├── grpc_client.go          # gRPC client
│   │   ├── http_client.go          # HTTP client for c-gateway
│   │   ├── go.mod
│   │   └── README.md
│   └── curl/
│       ├── decide.sh               # Basic decision request
│       ├── streaming.sh            # Streaming example
│       └── README.md
├── patterns/
│   ├── retry/
│   │   ├── exponential_backoff.py
│   │   ├── exponential_backoff.js
│   │   ├── exponential_backoff.go
│   │   ├── circuit_breaker.py
│   │   ├── circuit_breaker.js
│   │   ├── circuit_breaker go
│   │   └── README.md
│   └── backpressure/
│       ├── rate_limiter.py
│       ├── rate_limiter.js
│       ├── rate_limiter.go
│       ├── adaptive_throttling.py
│       └── README.md
└── docker-compose.yml              # Full stack setup

docs/
└── INTEGRATION_GUIDE.md            # Complete integration guide
```

## Dependencies

- **Existing Services**:
  - c-gateway (~/aigroup/apps/c-gateway)
  - caf (~/aigroup/apps/caf)
  - ui_web (~/aigroup/apps/ui_web)
  - Router (~/aigroup/apps/otp/router)
  
- **Infrastructure**:
  - NATS server with JetStream
  - Docker & Docker Compose (for testing)

- **Client Languages**:
  - Python 3.9+
  - Node.js 18+
  - Go 1.19+

## Integration Points

1. **c-gateway**: Primary HTTP entry point
   - Document existing endpoints
   - Provide client examples
   - Show authentication flow

2. **Router gRPC**: Direct gRPC access
   - Use existing proto definitions
   - Client examples for each language
   - Demonstrate streaming (if applicable)

3. **NATS**: Message bus
   - Show pub/sub patterns
   - Request-reply examples
   - JetStream usage

## References

- Existing gRPC proto: `proto/beamline/flow/v1/flow.proto`
- c-gateway documentation: `~/aigroup/apps/c-gateway/README.md`
- CAF integration: `~/aigroup/apps/caf/`
- Router architecture: `docs/ARCHITECTURE.md`
