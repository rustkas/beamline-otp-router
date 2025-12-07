# Router Architecture Documentation

**Version**: 1.0  
**Last Updated**: 2025-01-27

## Overview

The Router is an Erlang/OTP application that processes routing requests, applies routing policies, and returns provider selection decisions.

## Process Tree

```
beamline_router_sup (supervisor)
  ├── router_telemetry_handler (worker) - Telemetry event handler
  ├── router_nats (worker) - NATS publish/subscribe interface
  ├── router_policy_store (worker) - ETS policy cache with Heir
  ├── router_extension_registry (worker) - ETS-based extension metadata cache
  ├── router_rate_limit_store (worker) - Token bucket for per-policy rate limiting
  ├── router_rbac (worker) - Role-Based Access Control
  ├── router_rate_limiter (worker) - Per-tenant/user rate limiting
  ├── router_circuit_breaker (worker) - Per-provider circuit breaker state machine
  ├── router_grpc_sup (supervisor) - gRPC server supervisor
  │   └── gRPC server processes
  ├── router_decide_consumer (worker) - DecideRequest via JetStream
  ├── router_result_consumer (worker) - ExecResult from CAF
  ├── router_admin_nats_subscriber (worker) - Admin endpoints via NATS
  ├── [optional] router_ack_consumer (worker) - ACK consumer (if ack_enabled=true)
  ├── [optional] router_idempotency (worker) - Idempotency layer (if idempotency_enabled=true)
  └── [optional] router_metrics_http (worker) - Metrics HTTP endpoint (if metrics_export_enabled=true)
```

## Data Flow

### Request Flow (gRPC)

1. **Client** → gRPC Request → `router_grpc:decide/2`
2. **router_grpc** → Decode Request → Extract tenant_id, correlation_id
3. **router_grpc** → Rate Limit Check → `router_rate_limiter`
4. **router_grpc** → Route Request → `router_core:route/2`
5. **router_core** → Policy Lookup → `router_policy_store:get_policy/2`
6. **router_core** → Provider Selection → `router_decider:decide/3`
7. **router_decider** → Circuit Breaker Check → `router_circuit_breaker:should_allow/2`
8. **router_core** → Return Decision → `router_grpc`
9. **router_grpc** → Encode Response → Client

### Request Flow (NATS/JetStream)

1. **NATS** → JetStream Message → `router_decide_consumer`
2. **router_decide_consumer** → Decode Message → Extract RouteRequest
3. **router_decide_consumer** → Route Request → `router_core:route/2`
4. **router_core** → (same as gRPC flow)
5. **router_core** → Return Decision → `router_decide_consumer`
6. **router_decide_consumer** → Publish Decision → NATS

### Result Flow (CAF → Router)

1. **CAF** → ExecResult → NATS → `router_result_consumer`
2. **router_result_consumer** → Process Result → Update metrics
3. **router_result_consumer** → Circuit Breaker Update → `router_circuit_breaker:record_success/failure/2`

## Core Modules

### router_core.erl

**Purpose**: Main routing interface  
**Key Functions**:
- `route/2` - Main routing function
- `route/3` - Routing with context

**Dependencies**:
- `router_policy_store` - Policy lookup
- `router_decider` - Provider selection
- `router_circuit_breaker` - Circuit breaker checks

### router_decider.erl

**Purpose**: Provider selection algorithm  
**Key Functions**:
- `decide/3` - Select provider based on policy
- Supports: weighted, sticky, fallback selection

**Dependencies**:
- `router_policy_store` - Policy retrieval
- `router_circuit_breaker` - Circuit breaker state

### router_policy_store.erl

**Purpose**: Policy storage and retrieval  
**Storage**: ETS table with Heir for fault tolerance  
**Key Functions**:
- `get_policy/2` - Retrieve policy by tenant_id
- `upsert_policy/3` - Create or update policy
- `delete_policy/3` - Delete policy
- `list_policies/2` - List all policies for tenant

### router_circuit_breaker.erl

**Purpose**: Circuit breaker state machine  
**Key Functions**:
- `should_allow/2` - Check if request should be allowed
- `record_success/2` - Record successful request
- `record_failure/2` - Record failed request

**States**: closed, open, half_open

### router_rate_limiter.erl

**Purpose**: Per-tenant/user rate limiting  
**Key Functions**:
- `check_rate_limit/3` - Check if request is within rate limit
- Returns `{error, rate_limit_exceeded}` when limit exceeded

## ETS Tables

| Table | Purpose | Access Pattern |
|-------|---------|----------------|
| `router_metrics` | Metrics storage | Read/Write (via router_metrics module) |
| `router_policies` | Policy cache | Read/Write (via router_policy_store) |
| `router_extensions` | Extension metadata | Read/Write (via router_extension_registry) |
| `router_rate_limits` | Rate limit buckets | Read/Write (via router_rate_limit_store) |
| `router_rbac` | RBAC state | Read/Write (via router_rbac) |
| `router_idem` | Idempotency cache | Read/Write (via router_idempotency) |

## Configuration

See [Configuration Reference](./CONFIGURATION_REFERENCE.md) for complete configuration options.

## See Also

- `src/beamline_router_sup.erl` - Supervisor tree implementation
- `src/router_core.erl` - Core routing logic
- `src/router_decider.erl` - Provider selection

