# Router Architecture Documentation

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: Complete

## Overview

This document provides comprehensive architecture documentation for Router component, including process tree, data flow diagrams, and component interactions.

## System Architecture

### High-Level Architecture

```
┌─────────────┐
│   Gateway   │
│  (NestJS)   │
└──────┬──────┘
       │ gRPC
       ▼
┌─────────────┐
│   Router    │
│  (Erlang)   │
└──────┬──────┘
       │ NATS
       ▼
┌─────────────┐
│     CAF     │
│    (C++)    │
└─────────────┘
```

### Component Overview

**Router** is the core routing component that:
- Receives routing requests via gRPC (Router.Decide) or NATS (DecideRequest)
- Loads and applies routing policies
- Executes extension pipeline (pre-processors, validators, post-processors)
- Selects provider based on policy (weights, sticky sessions, fallback)
- Publishes assignments to CAF via NATS (ExecAssignment)
- Tracks circuit breaker state per provider
- Enforces rate limits and quotas
- Provides admin API for policy management (RouterAdmin)

## Process Tree

### Supervisor Hierarchy

```
beamline_router_sup (supervisor)
├── router_circuit_breaker (gen_server)
├── router_policy_store (gen_server)
├── router_rate_limiter (gen_server)
├── router_rbac (gen_server)
├── router_quota (gen_server)
├── router_audit (gen_server)
├── router_metrics_http (gen_server)
├── router_grpc_sup (supervisor)
│   ├── router_grpc (grpcbox service)
│   └── router_admin_grpc (grpcbox service)
└── router_nats_subscriber (gen_server)
    ├── router_decide_consumer (gen_server)
    ├── router_result_consumer (gen_server)
    ├── router_ack_consumer (gen_server)
    └── router_admin_nats_subscriber (gen_server)
```

### Process Responsibilities

**router_circuit_breaker**:
- Manages circuit breaker state per tenant/provider pair
- Tracks failures, successes, error rates, latency
- Transitions between closed/open/half_open states
- Emits metrics for state and transitions

**router_policy_store**:
- Stores and retrieves routing policies
- Manages policy cache (ETS + PostgreSQL)
- Handles ETS table transfer on node restart
- Provides RBAC-based access control

**router_rate_limiter**:
- Enforces rate limits per tenant/user
- Uses sliding window counters
- Tracks request rates and blocks when exceeded

**router_rbac**:
- Manages role-based access control
- Assigns roles (admin, operator, viewer) to users
- Checks permissions for policy operations

**router_quota**:
- Manages quotas per tenant
- Tracks policy counts, rules per policy, providers per policy
- Enforces quota limits

**router_audit**:
- Logs all policy and RBAC operations
- Provides audit trail for compliance
- Stores audit entries with timestamps and user context

**router_metrics_http**:
- Exports Prometheus metrics on port 9001
- Provides `/metrics` endpoint
- Formats metrics in Prometheus text format

**router_grpc_sup**:
- Supervises gRPC services
- Manages Router.Decide and RouterAdmin services
- Handles gRPC server lifecycle

**router_nats_subscriber**:
- Manages NATS subscriptions
- Handles DecideRequest messages from CAF
- Publishes ExecAssignment messages to CAF
- Manages JetStream consumers

## Data Flow Diagrams

### Request Flow: gRPC Decide

```
Client
  │
  │ gRPC Request (RouteRequest)
  ▼
router_grpc:decide/2
  │
  │ Extract correlation_id, tenant_id
  ▼
router_core:route/2
  │
  │ Load policy
  ▼
router_policy_store:get_policy/2
  │
  │ Apply policy
  ▼
router_policy_applier:apply_policy/4
  │
  │ Check rate limits
  ▼
router_rate_limiter:check_rate_limit/3
  │
  │ Execute extensions
  ▼
router_decider:decide/3
  │
  │ Pre-processors → Validators → Provider Selection → Post-processors
  ▼
router_extension_invoker:invoke/3 (for each extension)
  │
  │ Select provider
  ▼
router_provider_selector:select/3
  │
  │ Return decision
  ▼
router_grpc:decide/2
  │
  │ Encode RouteDecision
  ▼
Client (gRPC Response)
```

### Request Flow: NATS DecideRequest

```
CAF (via NATS)
  │
  │ NATS Message (DecideRequest JSON)
  ▼
router_nats_subscriber:handle_message/2
  │
  │ Parse and validate
  ▼
router_decide_consumer:handle_decide_request/4
  │
  │ Route message
  ▼
router_core:route/2
  │
  │ (Same flow as gRPC)
  ▼
router_decider:decide/3
  │
  │ Publish assignment (if push_assignment=true)
  ▼
router_caf_adapter:publish_assignment/2
  │
  │ NATS Publish (ExecAssignment JSON)
  ▼
CAF (via NATS)
```

### Circuit Breaker Flow

```
Request arrives
  │
  │ Check circuit breaker
  ▼
router_circuit_breaker:should_allow/2
  │
  │ State: closed → allow
  │ State: open → reject
  │ State: half_open → allow (limited)
  ▼
Provider call
  │
  │ Success/Failure
  ▼
router_circuit_breaker:record_success/2
router_circuit_breaker:record_failure/2
  │
  │ Update state
  ▼
router_circuit_breaker:maybe_transition_to_open/1
router_circuit_breaker:maybe_transition_on_timeout/1
  │
  │ Emit metrics
  ▼
router_metrics:emit_metric/3
```

## Component Interactions

### Policy Management

```
Admin Client
  │
  │ gRPC UpsertPolicy
  ▼
router_admin_grpc:upsert_policy/2
  │
  │ Check authorization
  ▼
router_admin_grpc:check_auth/1
  │
  │ Check RBAC permissions
  ▼
router_rbac:check_permission/4
  │
  │ Store policy
  ▼
router_policy_store:upsert_policy/3
  │
  │ Check quota
  ▼
router_quota:check_policy_quota/1
  │
  │ Audit log
  ▼
router_audit:log_policy_operation/4
  │
  │ Return response
  ▼
Admin Client
```

### Extension Pipeline

```
Request
  │
  │ Load policy
  ▼
router_policy:load_policy/2
  │
  │ Execute pre-processors
  ▼
router_extension_invoker:invoke/3 (pre)
  │
  │ Execute validators
  ▼
router_extension_invoker:invoke/3 (validator)
  │
  │ Select provider
  ▼
router_provider_selector:select/3
  │
  │ Execute post-processors
  ▼
router_extension_invoker:invoke/3 (post)
  │
  │ Return decision
  ▼
Response
```

## ETS Tables

### router_metrics

**Purpose**: Store metrics for Prometheus export  
**Type**: `named_table`, `public`, `{read_concurrency, true}`, `{write_concurrency, true}`  
**Key Format**: `{MetricName, LabelsKey}` or `MetricName` (for metrics without labels)  
**Value Format**: `Value` (integer or float)

**Access**: Use `router_r10_metrics` module (no direct ETS access)

### router_provider_circuit_breaker

**Purpose**: Store circuit breaker state per tenant/provider  
**Type**: `named_table`, `private`  
**Key Format**: `{TenantId, ProviderId}`  
**Value Format**: `#circuit_breaker_state{}` record

**Access**: Use `router_circuit_breaker` module API

### router_policy_cache

**Purpose**: Cache policies for fast lookup  
**Type**: `named_table`, `private`  
**Key Format**: `{TenantId, PolicyId}`  
**Value Format**: `#policy{}` record

**Access**: Use `router_policy_store` module API

### router_rate_limit_counters

**Purpose**: Store rate limit counters per tenant/user  
**Type**: `named_table`, `private`  
**Key Format**: `{TenantId, UserId, WindowStart}`  
**Value Format**: `Counter` (integer)

**Access**: Use `router_rate_limiter` module API

### router_rbac_roles

**Purpose**: Store RBAC role assignments  
**Type**: `named_table`, `private`  
**Key Format**: `{UserId, TenantId}`  
**Value Format**: `Role` (atom: admin, operator, viewer)

**Access**: Use `router_rbac` module API

### router_quota_limits

**Purpose**: Store quota limits per tenant  
**Type**: `named_table`, `private`  
**Key Format**: `TenantId`  
**Value Format**: `#quota{}` record

**Access**: Use `router_quota` module API

### router_audit_entries

**Purpose**: Store audit log entries  
**Type**: `named_table`, `private`  
**Key Format**: `EntryId` (auto-increment)  
**Value Format**: `#audit_entry{}` record

**Access**: Use `router_audit` module API

## Message Flow

### NATS Subjects

**Incoming**:
- `beamline.router.v1.decide` - DecideRequest from CAF
- `beamline.router.v1.admin.*` - Admin requests via NATS

**Outgoing**:
- `caf.exec.assign.v1` - ExecAssignment to CAF (default, configurable)
- `beamline.router.v1.decide.reply.{request_id}` - Reply to DecideRequest (if reply subject provided)

### gRPC Services

**Router.Decide**:
- Endpoint: `Router.Decide`
- Port: 9000 (configurable)
- Authorization: Not required

**RouterAdmin**:
- Endpoints: `UpsertPolicy`, `DeletePolicy`, `GetPolicy`, `ListPolicies`, `GetCheckpointStatus`, `GetValidatorsHealth`, `GetExtensionHealth`, `GetCircuitBreakerStates`, `DryRunPipeline`, `GetPipelineComplexity`
- Port: 9000 (same as Router.Decide)
- Authorization: Required (API key in metadata)

## Configuration Flow

```
Application Start
  │
  │ Load configuration
  ▼
application:get_env/2
  │
  │ Initialize components
  ▼
beamline_router_sup:init/1
  │
  │ Start supervisors
  ▼
Start Children
  │
  │ Initialize ETS tables
  ▼
router_metrics:ensure/0
router_circuit_breaker:init/1
router_policy_store:init/1
  │
  │ Ready
  ▼
Application Running
```

## Error Handling Flow

```
Request
  │
  │ Validation
  ▼
router_grpc:decide/2
  │
  │ Try
  ▼
router_core:route/2
  │
  │ Catch errors
  ▼
router_error:to_grpc/2
  │
  │ Map to gRPC status
  ▼
{grpc_error, {Status, Message}}
  │
  │ Return to client
  ▼
Client
```

## References

- `src/beamline_router_sup.erl` - Supervisor tree
- `src/router_core.erl` - Core routing logic
- `src/router_decider.erl` - Decision algorithm
- `src/router_policy_applier.erl` - Policy application
- `src/router_extension_invoker.erl` - Extension invocation
- `docs/API_CONTRACTS.md` - API contracts
- `docs/NATS_SUBJECTS.md` - NATS subject reference

