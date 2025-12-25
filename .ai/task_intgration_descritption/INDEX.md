# Integration Descriptions Index

Навигация по найденным и задокументированным integration points.

## Быстрая Навигация

- [README.md](README.md) - Полный обзор всех integration points
- [c_gateway.md](c_gateway.md) - HTTP Gateway интеграция
- [caf_worker.md](caf_worker.md) - CAF Worker интеграция
- [extensions.md](extensions.md) - Extensions Pipeline

## По Компонентам

### c-gateway (HTTP Gateway)
**Файл**: [c_gateway.md](c_gateway.md)

**Содержит**:
- HTTP endpoints (POST /decide, GET /_health, GET /metrics)
- Rate limiting (in-memory/Redis, 50 req/window)
- API Key authentication (X-API-Key header)
- NATS integration (beamline.router.v1.decide)
- Observability (Prometheus, OpenTelemetry)
- Production deployment guide

**Ключевые Данные**:
- Default rate limit: 50 req/window
- Distributed mode: Redis-backed
- Metrics endpoint: http://localhost:8080/metrics

### CAF Worker (Execution Engine)
**Файл**: [caf_worker.md](caf_worker.md)

**Содержит**:
- NATS subjects (caf.exec.assign.v1, caf.exec.result.v1)
- 4 block executors (HTTP, FS, SQL, Human)
- Error codes (1xxx-5xxx)
- StepResult contract
- Multi-tenancy (quotas, isolation)
- Performance targets (500 tasks/s)

**Ключевые Данные**:
- Actor-based pools (CPU/GPU/IO)
- Prometheus metrics: http://localhost:9090/metrics
- Retry: exponential backoff (100ms, 200ms, 400ms)

### Extensions Pipeline
**Файл**: [extensions.md](extensions.md)

**Содержит**:
- 4 extension types (pre, validator, post, provider)
- NATS subjects pattern (beamline.ext.{type}.{id}.v{version})
- Extension Registry (PostgreSQL + Mnesia)
- Routing Policy configuration
- Pipeline flow (pre → validate → provider → post)
- Version routing mechanism

**Ключевые Данные**:
- Configuration-driven (NO code changes)
- Any language support
- Version routing per tenant/environment

## По Integration Points

### 1. Client → c-gateway (HTTP)
**Описание**: [c_gateway.md](c_gateway.md#http-integration)

- Protocol: HTTP/1.1
- Auth: API Key (X-API-Key)
- Rate Limit: 50 req/window
- Endpoints: /decide, /_health, /metrics

### 2. c-gateway → Router (NATS)
**Описание**: [c_gateway.md](c_gateway.md#nats-integration)

- Subject: beamline.router.v1.decide
- Format: CP1/CP2 JSON
- Timeout: 5s default

### 3. Router → Pre-processors (NATS)
**Описание**: [extensions.md](extensions.md#1-pre-processor)

- Subjects: beamline.ext.pre.*.v1
- Contract: CP-Ext
- Behavior: Transform/enrich

### 4. Router → Validators (NATS)
**Описание**: [extensions.md](extensions.md#2-validator)

- Subjects: beamline.ext.validate.*.v1
- Contract: {status: "ok"|"reject"}
- Behavior: Accept/reject with on_fail

### 5. Router → Custom Providers (NATS)
**Описание**: [extensions.md](extensions.md#4-custom-provider)

- Subjects: beamline.provider.*.v1
- Contract: CP2 provider
- Behavior: Acts like standard provider

### 6. Router → CAF Worker (NATS)
**Описание**: [caf_worker.md](caf_worker.md#nats-integration)

- Subjects: caf.exec.assign.v1, caf.exec.result.v1
- Contract: ExecAssignment → ExecResult
- Behavior: Block execution

### 7. Router → Post-processors (NATS)
**Описание**: [extensions.md](extensions.md#3-post-processor)

- Subjects: beamline.ext.post.*.v1
- Contract: CP-Ext
- Behavior: Transform response

### 8. Router → Client (Response)
**Описание**: [c_gateway.md](c_gateway.md#response-flow)

- Format: JSON decision response
- Via c-gateway HTTP

## NATS Subjects Reference

### Router Core
- `beamline.router.v1.decide` - Routing decisions

### Extensions
- `beamline.ext.pre.{id}.v{version}` - Pre-processors
- `beamline.ext.validate.{id}.v{version}` - Validators
- `beamline.ext.post.{id}.v{version}` - Post-processors
- `beamline.provider.{id}.v{version}` - Custom providers

### CAF Worker
- `caf.exec.assign.v1` - Assignments
- `caf.exec.assign.v1.ack` - ACKs
- `caf.exec.result.v1` - Results
- `caf.worker.heartbeat.v1` - Heartbeats
- `caf.exec.dlq.v1` - Dead Letter Queue

## Architecture Flow

**См.**: [README.md](README.md#архитектура-полного-потока)

```
Client → c-gateway → Router → Extensions → Provider/CAF → Extensions → Response
```

## Configuration Quick Reference

### c-gateway
- NATS_URL
- GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT
- GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED
- GATEWAY_REDIS_URL

### CAF Worker
- NATS_URL
- WORKER_CPU_POOL_SIZE
- WORKER_IO_POOL_SIZE
- WORKER_MAX_MEMORY_MB
- PROMETHEUS_ENDPOINT

### Extensions
- Extension Registry (PostgreSQL)
- Routing Policy (JSON config)
- Version Routing rules

## Observability Quick Reference

### Metrics Endpoints
- c-gateway: http://localhost:8080/metrics
- CAF Worker: http://localhost:9090/metrics

### Key Metrics
**См.**: [README.md](README.md#observability)

## Related Documentation

**Integration Guides** (в docs/):
- `INTEGRATION_GUIDE.md` - Main integration guide (628 lines)
- `CAF_WORKER_INTEGRATION.md` - CAF Worker guide (400 lines)
- `EXTENSIONS_INTEGRATION.md` - Extensions guide (500 lines)

**Source Documentation**:
- c-gateway: `~/aigroup/apps/c-gateway/docs/`
- CAF: `~/aigroup/apps/caf/processor/docs/`
- Extensions: `~/aigroup/docs/EXTENSIONS_API.md`

## Last Updated

**Date**: 2025-12-21  
**Status**: Complete  
**Files**: 4 integration descriptions  
**Lines**: 1,500+ combined (guides + descriptions)
