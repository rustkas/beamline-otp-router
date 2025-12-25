# Integration Descriptions

Детальные описания всех найденных integration points в Beamline AI Router.

## Созданные Документы

### 1. c_gateway.md (c-gateway Integration)

**Что описано**:
- HTTP endpoints (`/api/v1/routes/decide`, `/_health`, `/metrics`)
- Rate limiting (in-memory и Redis)
- API Key authentication
- NATS integration (`beamline.router.v1.decide`)
- Observability (Prometheus metrics, OpenTelemetry)
- Error handling (429, 503, 504)
- Production deployment

**Ключевые находки**:
- Default rate limit: 50 req/window
- X-API-Key authentication
- Distributed rate limiting с Redis
- c-gateway НЕ знает об extensions

### 2. caf_worker.md (CAF Worker Integration)

**Что описано**:
- NATS subjects (`caf.exec.assign.v1`, `caf.exec.result.v1`, `caf.exec.assign.v1.ack`)
- 4 block executors (HTTP, FS, SQL, Human)
- Error codes (1xxx-5xxx)
- StepResult contract (CP1 invariant)
- Observability (Prometheus port 9090)
- Multi-tenancy (resource quotas, isolation)
- Performance targets (500 tasks/s)

**Ключевые находки**:
- Actor-based parallelism (CPU/GPU/IO pools)
- Retry при transient errors (100ms, 200ms, 400ms)
- Worker → Router ACK + Result flow
- Sandbox mode для testing

### 3. extensions.md (Extensions Pipeline)

**Что описано**:
- 4 типа extensions (pre, validator, post, provider)
- NATS subjects pattern (`beamline.ext.{type}.{id}.v{version}`)
- Extension Registry (PostgreSQL + Mnesia cache)
- Routing Policy configuration
- Message contracts (request/response)
- Extension lifecycle (create, register, deploy)
- Version routing mechanism
- E2E testing

**Ключевые находки**:
- Configuration-driven extensibility (NO code changes)
- Any language support (Node.js, Go, Rust, Python)
- Pre → Validators → Provider → Post pipeline
- Version routing per tenant/environment
- Full observability (metrics, tracing, logs)

## Архитектура Полного Потока

```
Client HTTP Request
   ↓
┌─────────────────────────────────────────────────────────────┐
│ c-gateway (HTTP → NATS)                                     │
│ - Rate limiting (50 req/window default)                     │
│ - API Key authentication                                    │
│ - Prometheus metrics on /metrics                            │
└──────────────────────────┬──────────────────────────────────┘
                          │
                  NATS beamline.router.v1.decide
                          │
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Router (Policy Evaluation & Extension Orchestration)        │
│                                                             │
│  Pre-processors (NATS beamline.ext.pre.*.v1)               │
│  ├→ normalize_text                                         │
│  ├→ language_detect                                        │
│  └→ context_enrichment                                     │
│                                                             │
│  Validators (NATS beamline.ext.validate.*.v1)              │
│  ├→ pii_guard (on_fail: block)                            │
│  ├→ content_policy                                         │
│  └→ rate_limit                                             │
│                                                             │
│  Provider Selection                                         │
│  └→ From Routing Policy providers[]                        │
│                                                             │
│  Provider Execution                                         │
│  ├→ OpenAI/Anthropic (standard)                           │
│  ├→ Custom Providers (NATS beamline.provider.*.v1)        │
│  └→ CAF Worker                                             │
│     └→ NATS caf.exec.assign.v1                           │
│        ├→ Block Execution (HTTP, FS, SQL, Human)          │
│        └→ NATS caf.exec.result.v1                        │
│                                                             │
│  Post-processors (NATS beamline.ext.post.*.v1)             │
│  ├→ mask_pii                                               │
│  ├→ translate                                              │
│  └→ format_response                                        │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ↓
                    Response to Client
```

## Ключевые Integration Points

### 1. Client → c-gateway (HTTP)
- **Protocol**: HTTP/1.1
- **Auth**: API Key (X-API-Key header)
- **Format**: JSON
- **Rate Limit**: 50 req/window (configurable)

### 2. c-gateway → Router (NATS)
- **Subject**: `beamline.router.v1.decide`
- **Format**: CP1/CP2 JSON contract
- **Timeout**: 5s default

### 3. Router → Pre-processors (NATS)
- **Subjects**: `beamline.ext.pre.{id}.v{version}`
- **Contract**: CP-Ext (payload + metadata)
- **Timeout**: 80-100ms typical
- **Behavior**: Transform/enrich request

### 4. Router → Validators (NATS)
- **Subjects**: `beamline.ext.validate.{id}.v{version}`
- **Contract**: CP-Ext request, `{status: "ok"|"reject"}` response
- **Timeout**: 100ms typical
- **Behavior**: Accept/reject with on_fail policy

### 5. Router → Custom Providers (NATS)
- **Subjects**: `beamline.provider.{id}.v{version}`
- **Contract**: CP2 provider request/response
- **Timeout**: 5000ms typical
- **Behavior**: Acts like OpenAI/Anthropic

### 6. Router → CAF Worker (NATS)
- **Subjects**: 
  - Request: `caf.exec.assign.v1`
  - ACK: `caf.exec.assign.v1.ack`
  - Result: `caf.exec.result.v1`
- **Contract**: ExecAssignment → ExecResult
- **Blocks**: HTTP, FS, SQL, Human
- **Timeout**: per block type

### 7. Router → Post-processors (NATS)
- **Subjects**: `beamline.ext.post.{id}.v{version}`
- **Contract**: CP-Ext (payload + metadata)
- **Timeout**: 100ms typical
- **Behavior**: Transform response

### 8. Router → Client (Response)
- **Format**: JSON
- **Fields**: decision_id, provider_id, reason, etc.

## Configuration Points

### c-gateway
```bash
NATS_URL=nats://localhost:4222
GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT=50
GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED=false
GATEWAY_REDIS_URL=redis://localhost:6379
GATEWAY_ADMIN_API_KEY=secret
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
```

### CAF Worker
```bash
NATS_URL=nats://localhost:4222
WORKER_CPU_POOL_SIZE=8
WORKER_IO_POOL_SIZE=16
WORKER_MAX_MEMORY_MB=2048
PROMETHEUS_ENDPOINT=0.0.0.0:9090
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
WORKER_SANDBOX_MODE=false
```

### Extensions
- **Extension Registry**: PostgreSQL + Mnesia cache
- **Routing Policy**: JSON configuration
- **Version Routing**: Per tenant/environment

## Observability

### Metrics Endpoints

**c-gateway**: `http://localhost:8080/metrics` (Prometheus)

**CAF Worker**: `http://localhost:9090/metrics` (Prometheus)

**Router**: Internal Prometheus metrics

### Key Metrics

**c-gateway**:
- `gateway_requests_total{endpoint,status}`
- `gateway_request_duration_seconds{endpoint}`
- `gateway_rate_limit_exceeded_total{endpoint}`

**CAF Worker**:
- `worker_tasks_total{type,status}`
- `worker_task_latency_ms{type}`
- `worker_pool_queue_depth{pool}`

**Router Extensions**:
- `router_extension_calls_total{extension_id,status}`
- `router_extension_latency_ms{extension_id}`
- `router_extension_errors_total{extension_id,error_type}`

### Tracing

**All components** support OpenTelemetry:
- trace_id propagation через NATS
- Span context в headers
- Distributed tracing across full pipeline

## Performance Targets

**Total Latency** (p95): < 500ms

**Breakdown**:
- c-gateway overhead: < 10ms
- Pre-processors: 50-100ms total
- Validators: 50-100ms total
- Provider: 200-300ms
- Post-processors: 50-100ms total
- CAF Worker blocks: type-dependent (HTTP≤500ms, FS≤200ms, SQL≤300ms)

**Throughput**:
- c-gateway: 1000+ req/s
- Router: 500+ req/s with extensions
- CAF Worker: 500+ tasks/s (CP3 target)

## NATS Subjects Summary

### Router Core
- `beamline.router.v1.decide` - Routing decisions

### Extensions
- `beamline.ext.pre.{id}.v{version}` - Pre-processors
- `beamline.ext.validate.{id}.v{version}` - Validators
- `beamline.ext.post.{id}.v{version}` - Post-processors
- `beamline.provider.{id}.v{version}` - Custom providers

### CAF Worker
- `caf.exec.assign.v1` - Assignments from Router
- `caf.exec.assign.v1.ack` - Assignment acknowledgments
- `caf.exec.result.v1` - Execution results
- `caf.worker.heartbeat.v1` - Worker heartbeats
- `caf.exec.dlq.v1` - Dead Letter Queue

## Ключевые Принципы

1. **Configuration over Code**
   - Extensions добавляются через Registry + Policy
   - NO code changes в Router/Gateway/CAF

2. **NATS-Based Communication**
   - Все inter-service через NATS
   - Decoupled architecture
   - Easy scaling

3. **Language Agnostic**
   - c-gateway: C++
   - Router: Erlang/OTP
   - CAF Worker: C++ (CAF)
   - Extensions: Any language (Node.js, Go, Rust, Python)

4. **Observable by Design**
   - Prometheus metrics everywhere
   - OpenTelemetry distributed tracing
   - Structured JSON logs
   - Correlation IDs (trace_id, request_id, run_id)

5. **Fail-Safe Defaults**
   - Rate limiting (fail-closed)
   - Validators on_fail policy
   - Extension mode (required/optional)
   - Timeouts на всех уровнях

## Документация

**Созданные Integration Guides**:
1. `~/aigroup/apps/otp/router/docs/INTEGRATION_GUIDE.md` (628 lines)
2. `~/aigroup/apps/otp/router/docs/CAF_WORKER_INTEGRATION.md` (400 lines)
3. `~/aigroup/apps/otp/router/docs/EXTENSIONS_INTEGRATION.md` (500 lines)

**Всего**: 1,500+ lines comprehensive documentation

## Использовано данных из

### c-gateway
- `~/aigroup/apps/c-gateway/src/http_server.c`
- `~/aigroup/apps/c-gateway/docs/ADMIN_GRPC_API.md`
- `~/aigroup/apps/c-gateway/IMPLEMENTATION_PLAN.md`

### CAF Worker
- `~/aigroup/apps/caf/processor/README.md`
- `~/airgroup/apps/caf/processor/docs/ARCHITECTURE_ROLE.md`
- `~/aigroup/apps/caf/processor/src/` (source code)

### Extensions
- `~/aigroup/docs/EXTENSIONS_API.md`
- `~/aigroup/apps/otp/router/docs/EXTENSIONS_E2E_GUIDE.md`
- `~/aigroup/docs/ADR/ADR-023-remove-provider-use-extensions.md`

## Статус

**Исследование**: ✅ Complete  
**Документация**: ✅ Complete  
**Найдено Integration Points**: 8  
**Создано Описаний**: 3 detailed files  
**Создано Integration Guides**: 3 comprehensive guides (1,500+ lines)

**Дата**: 2025-12-21
