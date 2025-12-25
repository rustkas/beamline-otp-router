# c-gateway Integration Description

## Обзор

**Компонент**: c-gateway  
**Язык**: C++  
**Расположение**: `~/aigroup/apps/c-gateway`  
**Назначение**: HTTP-to-NATS gateway с rate limiting, metrics, и tracing

## Найденные Endpoints

### 1. POST /api/v1/routes/decide

**Назначение**: Запрос маршрутизации для AI задач

**HTTP Headers**:
```
Content-Type: application/json
X-Tenant-ID: <tenant_id>          (обязательно)
X-API-Key: <api_key>               (обязательно)
X-Trace-ID: <trace_id>             (опционально)
X-Request-ID: <request_id>         (опционально)
```

**Request Body**:
```json
{
  "version": "1",
  "message": {
    "message_id": "msg-001",
    "type": "text.generate",
    "payload": "Your input text",
    "metadata": {}
  },
  "policy_id": "my-policy",
  "context": {},
  "run_id": "run-789"
}
```

**Response (200 OK)**:
```json
{
  "decision_id": "dec-123",
  "provider_id": "openai",
  "reason": "weighted_random",
  "priority": 1,
  "expected_latency_ms": 500,
  "expected_cost": 0.0001
}
```

**Response Headers**:
```
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 49
X-RateLimit-Reset: 1640000000
```

**Error Responses**:
- `400 Bad Request` - Невалидный запрос
- `429 Too Many Requests` - Rate limit превышен
- `503 Service Unavailable` - Сервис недоступен

### 2. GET /_health

**Назначение**: Health check gateway

**Response (200 OK)**:
```json
{
  "status": "healthy"
}
```

**Response (503)**:
```json
{
  "status": "unhealthy",
  "reason": "NATS disconnected"
}
```

### 3. GET /metrics

**Назначение**: Prometheus метрики

**Format**: Prometheus text format

**Примеры метрик**:
```
gateway_requests_total{endpoint="/api/v1/routes/decide",status="200"} 1234
gateway_request_duration_seconds_bucket{endpoint="/api/v1/routes/decide",le="0.005"} 800
gateway_rate_limit_exceeded_total{endpoint="/api/v1/routes/decide"} 42
```

## Rate Limiting

### Конфигурация

**Environment Variables**:
- `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT` - Requests per window (default: 50)
- `GATEWAY_RATE_LIMIT_WINDOW_SECONDS` - Window size (default: 60)
- `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED` - Enable Redis (default: false)
- `GATEWAY_REDIS_URL` - Redis URL (для distributed mode)

### Режимы

**1. In-Memory (default)**:
- Fixed-window rate limiting
- Per-process limits
- Fast, но не работает для multi-instance

**2. Distributed (Redis)**:
- Shared limits across instances
- Slower (~2-5ms overhead)
- Production-ready для horizontal scaling

### Per-Tenant Limits

Rate limits применяются на комбинацию:
- `tenant_id` (из X-Tenant-ID header)
- `api_key` (из X-API-Key header)

## Authentication

**Метод**: API Key

**Header**: `X-API-Key: your-api-key`

**Validation**: Против сконфигурированных ключей

**Future**: OAuth2, JWT tokens

## NATS Integration

### Subjects

**Outbound** (c-gateway → Router):
- `beamline.router.v1.decide` - Routing decisions

**Message Format**: CP1/CP2 contract (JSON)

### Connection

**Configuration**:
- `NATS_URL` - NATS server URL (default: nats://localhost:4222)
- Auto-reconnect on disconnect
- Connection pooling

## Observability

### Metrics (Prometheus)

**Request Metrics**:
- `gateway_requests_total{endpoint, status}` - Total requests
- `gateway_request_duration_seconds{endpoint}` - Latency histogram
- `gateway_rate_limit_exceeded_total{endpoint}` - Rate limit hits

**NATS Metrics**:
- `gateway_nats_requests_total{status}` - NATS calls
- `gateway_nats_errors_total` - NATS errors

### Tracing (OpenTelemetry)

**Configuration**:
- `OTEL_EXPORTER_OTLP_ENDPOINT` - OTLP endpoint

**Spans**:
- HTTP request span
- NATS request span
- Propagates trace_id to Router

### Logs

**Format**: Structured JSON (likely stderr)

**Fields**:
- timestamp
- level
- message
- request_id
- trace_id
- tenant_id

## Error Handling

### Rate Limit Exceeded (429)

**Response**:
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

**Headers**:
```
Retry-After: 60
X-RateLimit-Limit: 50
X-RateLimit-Remaining: 0
X-RateLimit-Reset: 1640000060
```

### NATS Disconnection (503)

**Behavior**:
- Health check returns 503
- Requests return 503
- Auto-reconnect in background

### Request Timeout (504)

**Cause**: NATS request timeout (default: 5s)

**Response**:
```json
{
  "error": "gateway_timeout",
  "message": "Request timeout"
}
```

## Building and Running

### Build

```bash
cd ~/aigroup/apps/c-gateway
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Run

```bash
./build/gateway
```

### Configuration

```bash
export NATS_URL=nats://localhost:4222
export GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=100
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
./build/gateway
```

## Integration Points

### Upstream (Client → c-gateway)

**Protocol**: HTTP/1.1

**Authentication**: API Key

**Format**: JSON

### Downstream (c-gateway → Router)

**Protocol**: NATS

**Subject**: `beamline.router.v1.decide`

**Format**: CP1/CP2 JSON contract

## Production Considerations

### Scaling

**Horizontal**:
- Run multiple instances behind load balancer
- Enable distributed rate limiting (Redis)
- Share NATS connection pool

**Vertical**:
- CPU: 0.5-1 core per instance
- Memory: 256-512MB per instance

### Monitoring

**Key Metrics**:
- Request rate (RPS)
- Latency (p50, p95, p99)
- Error rate (%)
- Rate limit hits

**Alerts**:
- High error rate (> 5%)
- High latency (p95 > 500ms)
- NATS disconnected
- Rate limit exceeded (per tenant)

### High Availability

**Requirements**:
- Load balancer with health checks
- Auto-scaling based on RPS
- Circuit breaking for NATS

### Security

**Current**:
- API Key authentication
- Rate limiting per tenant
- Input validation

**Future**:
- OAuth2/JWT
- IP allowlisting
- Request signing

## Documentation References

- **Admin API**: `~/aigroup/apps/c-gateway/docs/ADMIN_GRPC_API.md`
- **Implementation Plan**: `~/aigroup/apps/c-gateway/IMPLEMENTATION_PLAN.md`
- **Observability**: `~/airgroup/apps/c-gateway/docs/OBSERVABILITY.md`

## Summary

c-gateway - это **HTTP-to-NATS адаптер** с:
- ✅ Rate limiting (in-memory или Redis)
- ✅ API Key authentication
- ✅ Prometheus metrics
- ✅ OpenTelemetry tracing
- ✅ Health checks
- ✅ Auto-reconnect to NATS

**Ключевой принцип**: c-gateway НЕ знает об extensions - все extension logic в Router.
