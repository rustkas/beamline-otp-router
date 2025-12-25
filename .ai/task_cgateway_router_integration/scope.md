# Scope — T-CGW-ROUTER-01

## In Scope

### 1. Contract Documentation
- HTTP → NATS mapping (endpoints, headers, body)
- NATS → HTTP response mapping
- Subject usage (`beamline.router.v1.decide`)
- Metadata flow (tenant_id, trace_id, request_id)

### 2. Backpressure Propagation
- Router backpressure → NATS reply
- NATS reply → c-gateway
- c-gateway → HTTP client (status code, headers, body)
- Caching/throttling in c-gateway

### 3. Observability
- Log correlation (trace_id end-to-end)
- Metrics alignment (Router SLO vs gateway metrics)
- Error taxonomy matching

### 4. Failure Modes
- Router down
- NATS timeout
- Backpressure active
- Partial/malformed replies
- Slow Router (timeout behavior)

### 5. Timeouts & Retries
- Request timeout alignment
- Retry policy (if any)
- Idempotency handling

## Out of Scope

### Explicitly NOT Included
- ❌ Performance optimization
- ❌ Changing Router code (CP1 frozen)
- ❌ New subjects (CP2 only)
- ❌ HA/multi-node setup
- ❌ Refactoring c-gateway
- ❌ Load testing (separate task)

## Constraints

- **CP1 Router is frozen** (no changes allowed)
- **Audit mode only** (document reality, don't fix)
- **Findings may inform CP2**, but no CP2 implementation here
