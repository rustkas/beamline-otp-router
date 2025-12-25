# Beamline Router Go Clients

Go examples for integrating with Beamline Router via gRPC (direct) or HTTP (via c-gateway).

## Clients

### 1. gRPC Client (`grpc_client.go`)
Direct integration with Router using gRPC protocol.

### 2. HTTP Client (`http_client.go`)
Integration via c-gateway using HTTP/REST API.

## Features

**gRPC Client**:
- **Type-safe gRPC calls**: Using google.golang.org/grpc
- **Exponential backoff retry**: Automatic retries with jitter
- **Circuit breaker**: Prevents cascading failures (CLOSED/OPEN/HALF_OPEN)
- **Error handling**: Graceful handling of transient errors

**HTTP Client**:
- **REST API**: Integration via c-gateway with net/http
- **Rate limit handling**: Respects 429 + Retry-After header
- **Exponential backoff**: Automatic retries with jitter
- **Error handling**: Retryable vs non-retryable errors
- **API Key auth**: X-API-Key authentication
- **Health & Metrics**: Built-in endpoints

## Quick Start

### 1. Initialize Module

```bash
go mod download
```

### 2. Run Examples

**HTTP Client**:
```bash
go run http_client.go
```

**gRPC Client**:
```bash
go run grpc_client.go
```

## Usage

### HTTP Client

#### Basic Example

```go
package main

import (
    "fmt"
    "log"
)

func main() {
    client := NewBeamlineHTTPClient(
        "http://localhost:8080",
        "your-api-key",
        "demo-tenant",
        nil, // Use default retry config
    )

    // Check health
    health, err := client.Health()
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Health: %s\n", health.Status)

    // Make routing decision
    decision, err := client.Decide(
        "text.generate",
        "Hello, world!",
        "demo-policy",
        map[string]interface{}{
            "request_id": "example-001",
        },
    )
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Provider: %s\n", decision.ProviderID)
    fmt.Printf("Reason: %s\n", decision.Reason)
}
```

#### With Custom Retry

```go
client := NewBeamlineHTTPClient(
    "http://localhost:8080",
    "your-api-key",
    "demo-tenant",
    &RetryConfig{
        MaxAttempts: 5,
        BaseDelayMs: 200,
        MaxDelayMs:  10000,
        Jitter:      0.2,
    },
)
```

#### Rate Limit Handling

```go
// Client automatically handles rate limiting
decision, err := client.Decide(
    "text.generate",
    "Your input",
    "my-policy",
    map[string]interface{}{},
)
// Automatically retries on 429 with Retry-After
```

### gRPC Client

#### Basic Example

```go
package main

import (
    "fmt"
    "log"
)

func main() {
    client, err := NewBeamlineGRPCClient(
        "localhost:50051",
        nil, // Use default retry config
        true, // Enable circuit breaker
    )
    if err != nil {
        log.Fatal(err)
    }
    defer client.Close()

    decision, err := client.Decide(GRPCDecideRequest{
        Version:     "1",
        RequestID:   "example-001",
        TenantID:    "demo-tenant",
        PolicyID:    "demo-policy",
        TaskType:    "text.generate",
        TaskPayload: "Hello, world!",
    })
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Provider: %s\n", decision.ProviderID)
    fmt.Printf("Reason: %s\n", decision.Reason)
}
```

#### With Custom Retry

```go
client, err := NewBeamlineGRPCClient(
    "localhost:50051",
    &GRPCRetryConfig{
        MaxAttempts: 5,
        BaseDelayMs: 200,
        MaxDelayMs:  10000,
        Jitter:      0.2,
    },
    true,
)
```

#### Disable Circuit Breaker

```go
client, err := NewBeamlineGRPCClient(
    "localhost:50051",
    nil,
    false, // Disable circuit breaker
)
```

## Configuration

### RetryConfig (HTTP)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `MaxAttempts` | 3 | Maximum number of retry attempts |
| `BaseDelayMs` | 100 | Initial delay in milliseconds |
| `MaxDelayMs` | 5000 | Maximum delay between retries |
| `Jitter` | 0.1 | Jitter factor (0.0 - 1.0) |

### GRPCRetryConfig

| Parameter | Default | Description |
|-----------|---------|-------------|
| `MaxAttempts` | 3 | Maximum number of retry attempts |
| `BaseDelayMs` | 100 | Initial delay in milliseconds |
| `MaxDelayMs` | 5000 | Maximum delay between retries |
| `Jitter` | 0.1 | Jitter factor (0.0 - 1.0) |

### CircuitBreaker

| Parameter | Default | Description |
|-----------|---------|-------------|
| `FailureThreshold` | 5 | Number of failures before opening |
| `RecoveryTimeoutSeconds` | 60 | Seconds before attempting recovery |

## Retry Behavior

**Exponential Backoff with Jitter**:
- Attempt 1: Immediate
- Attempt 2: ~100ms ± jitter
- Attempt 3: ~200ms ± jitter
- Attempt 4: ~400ms ± jitter
- etc.

**HTTP Retryable Status Codes**:
- `408`: Request Timeout
- `429`: Too Many Requests
- `500`: Internal Server Error
- `502`: Bad Gateway
- `503`: Service Unavailable
- `504`: Gateway Timeout

**gRPC Retryable Codes**:
- `UNAVAILABLE`: Service temporarily unavailable
- `DEADLINE_EXCEEDED`: Request timeout
- `RESOURCE_EXHAUSTED`: Rate limit exceeded
- `ABORTED`: Operation aborted

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
- ✅ You want simpler debugging
- ✅ You need rate limiting per tenant
- ✅ You prefer stateless requests

### Use gRPC Client when:
- ✅ You need maximum performance
- ✅ You want type safety (protobuf)
- ✅ You need bidirectional streaming
- ✅ You have direct network access
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
go run grpc_client.go
```

**For HTTP Client**:
```bash
# Terminal 1: Start c-gateway
cd ~/aigroup/apps/c-gateway
./build/gateway

# Terminal 2: Run client
go run http_client.go
```

## Production Notes

### gRPC Client

**Important**: The provided gRPC example is simplified. For production:

1. **Generate protobuf code**:
```bash
protoc --go_out=. --go_opt=paths=source_relative \
    --go-grpc_out=. --go-grpc_opt=paths=source_relative \
    proto/beamline/flow/v1/flow.proto
```

2. **Import generated code**:
```go
import pb "path/to/generated/proto/beamline/flow/v1"
```

3. **Create proper client**:
```go
conn, _ := grpc.Dial(address, opts...)
client := pb.NewRouterServiceClient(conn)
```

4. **Make actual RPC calls**:
```go
response, err := client.Decide(ctx, &pb.RouteRequest{...})
```

### HTTP Client

The HTTP client is production-ready as-is. Just configure:
- Base URL
- API Key
- Tenant ID
- Retry config

## Troubleshooting

### "connection refused"

**HTTP**:
```bash
# Check c-gateway is running
curl http://localhost:8080/_health
```

**gRPC**:
```bash
# Check Router is running
netstat -tuln | grep 50051
```

### "429 Too Many Requests"

- Client will auto-retry with Retry-After
- Reduce request rate
- Increase rate limits

### Circuit breaker always OPEN

- Increase `FailureThreshold`
- Check service health
- Disable temporarily

## Dependencies

```go
require (
    google.golang.org/grpc v1.59.0
    google.golang.org/protobuf v1.31.0
)
```

Install:
```bash
go get google.golang.org/grpc
go get google.golang.org/protobuf
```

## Next Steps

- **Main Integration Guide**: See `~/aigroup/apps/otp/router/docs/INTEGRATION_GUIDE.md`
- **c-gateway Details**: See `.ai/task_intgration_descritption/c_gateway.md`
- **Extensions**: See `~/aigroup/apps/otp/router/docs/EXTENSIONS_INTEGRATION.md`
- **CAF Worker**: See `~/aigroup/apps/otp/router/docs/CAF_WORKER_INTEGRATION.md`

## License

Same as Beamline Router project.
