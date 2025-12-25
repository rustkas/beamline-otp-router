# API Contracts: Router ↔ CAF Integration

## Scope

This document defines JSON message contracts for Router ↔ CAF integration via NATS. All contracts are **enforced at runtime** by Router validation logic.

**Implementation**: `apps/otp/router/src/router_nats_subscriber.erl`  
**Tests**: `apps/otp/router/test/router_nats_subscriber_caf_SUITE.erl`, `apps/otp/router/test/router_nats_contract_validation_SUITE.erl`

## Core Message Fields

**CRITICAL**: All messages must comply with the **Core Message Fields Specification** (`docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md`).

**Required fields** (see specification for details):
- `tenant_id` (required) - Tenant identifier
- `version` / `schema_version` (required) - Schema version
- `trace_id` (optional in CP1, required in CP2+) - Distributed tracing ID
- `idempotency_key` (optional in CP1, required in CP2+) - Idempotency key
- `run_id`, `flow_id`, `step_id` (CP2+ only) - Multi-step workflow identifiers

**See**: `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete field specifications, validation rules, and test requirements.

## Message Versioning

### Invariants

Router **guarantees**:
- All messages **must** include `version` field (string `"1"` for CP1/CP2-LC)
- Missing or unsupported `version` **always** returns `invalid_request` error before any processing
- Version validation **always** occurs before request parsing

### Implementation

**Module**: `router_nats_subscriber.erl`  
**Validation**: Version checked in `handle_decide_request/2` before JSON parsing

**Validation Rules**:
- Missing `version` → `invalid_request` error: `"Missing version field"`
- Unsupported version (not `"1"`) → `invalid_request` error: `"Unsupported version"` with `supported_versions: ["1"]` in logs
- Valid version `"1"` → Processing continues

### Tests

**Coverage**:
- `router_nats_subscriber_caf_SUITE.erl`: Version validation tests
- `router_nats_contract_validation_SUITE.erl`: Contract validation tests

**Example Error Response** (missing version):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing version field"
  },
  "context": {
    "request_id": "..."
  }
}
```

**Example Error Response** (unsupported version):
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Unsupported version"
  },
  "context": {
    "request_id": "..."
  }
}
```

## Common Fields

**Required in all messages** (see `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete specification):
- `version`: `"1"` (string, required) - Schema version (see `schema_version` in Core Fields Spec)
- `request_id`: UUID string (required) - Request correlation ID (NATS-specific, not in Core Fields)
- `tenant_id`: string (required) - Multi-tenant identifier (see Core Fields Spec)

**Optional in all messages**:
- `trace_id`: string (optional in CP1, required in CP2+) - Distributed tracing ID (see Core Fields Spec)

**CP2+ Optional Fields** (see Core Fields Spec for details):
- `run_id`: string (optional, CP2+) - Run identifier for multi-step workflows
- `flow_id`: string (optional, CP2+) - Flow definition identifier
- `step_id`: string (optional, CP2+) - Step identifier within a flow
- `idempotency_key`: string (optional in CP1, required in CP2+) - Idempotency key

**Validation**: Router **rejects** messages missing required fields with `invalid_request` error. See `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` for complete validation rules.

## DecideRequest

### Invariants

Router **guarantees**:
- `DecideRequest` **must** include `version`, `request_id`, `tenant_id`, `task.type`
- Either `task.payload_ref` or `task.payload` **must** be present
- Invalid `DecideRequest` **always** returns `invalid_request` error

### Fields
```json
{
  "version": "1",
  "request_id": "uuid-string",
  "trace_id": "string (optional)",
  "tenant_id": "string",
  "task": {
    "type": "string",
    "payload_ref": "string (optional)",
    "payload": "any (optional)"
  },
  "policy_id": "string (optional)",
  "constraints": {
    "max_latency_ms": "number (optional)",
    "max_cost": "number (optional)"
  },
  "metadata": "object (optional)",
  "push_assignment": "boolean (default: false)",
  "assignment_subject": "string (optional, default: 'caf.exec.assign.v1')"
}
```

**Field Requirements**:
- `version`: `"1"` (string, required)
- `request_id`: UUID string (required)
- `tenant_id`: string (required)
- `task.type`: string (required) - Task type identifier
- `task.payload_ref`: string (optional) - Payload reference (e.g., S3 URI)
- `task.payload`: any (optional) - Inline payload (alternative to `payload_ref`)
- `policy_id`: string (optional) - Policy ID; if omitted, Router uses default policy
- `constraints.max_latency_ms`: number (optional) - Maximum latency constraint
- `constraints.max_cost`: number (optional) - Maximum cost constraint
- `metadata`: object (optional) - Arbitrary metadata map
- `push_assignment`: boolean (optional, default: `false`) - If `true`, Router publishes `ExecAssignment` to CAF
- `assignment_subject`: string (optional, default: `"caf.exec.assign.v1"`) - NATS subject for `ExecAssignment`

**Implementation**: `router_nats_subscriber.erl` → `router_core.erl`  
**Tests**: `router_nats_subscriber_caf_SUITE.erl`, `router_core_SUITE.erl`

**Example**:
```json
{
  "version": "1",
  "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242",
  "trace_id": "tr-123",
  "tenant_id": "acme",
  "task": {
    "type": "text.generate",
    "payload_ref": "s3://bucket/key"
  },
  "policy_id": "policy:default",
  "constraints": {
    "max_latency_ms": 2000
  },
  "metadata": {
    "user_id": "u-42"
  },
  "push_assignment": true,
  "assignment_subject": "caf.exec.assign.v1"
}
```

## DecideResponse

### Invariants

Router **guarantees**:
- `DecideResponse` **always** includes `ok: true` for success responses
- `decision.provider_id` **always** non-empty string
- `decision.priority` **always** 0-100
- `decision.expected_latency_ms` **always** >= 0
- `decision.expected_cost` **always** >= 0
- `decision.reason` **always** one of: `"weighted"`, `"sticky"`, `"fallback"`, `"best_score"`

### Fields
```json
{
  "ok": true,
  "decision": {
    "provider_id": "string",
    "provider_label": "string (optional)",
    "priority": "number",
    "expected_latency_ms": "number",
    "expected_cost": "number",
    "reason": "string",
    "sticky_key": "string (optional)",
    "fallback_used": "boolean (optional)",
    "policy_id": "string (optional)",
    "expires_at_ms": "number (optional)",
    "metadata": "object (optional)"
  },
  "context": {
    "request_id": "string",
    "trace_id": "string (optional)"
  }
}
```

**Field Requirements**:
- `ok`: `true` (boolean, required) - Always `true` for success responses
- `decision.provider_id`: string (required) - Selected provider identifier
- `decision.priority`: number (required) - Decision priority (0-100)
- `decision.expected_latency_ms`: number (required) - Expected latency in milliseconds (>= 0)
- `decision.expected_cost`: number (required) - Expected cost (>= 0)
- `decision.reason`: string (required) - Routing reason: `"weighted"`, `"sticky"`, `"fallback"`, `"best_score"`
- `decision.provider_label`: string (optional) - Human-readable provider name
- `decision.sticky_key`: string (optional) - Sticky session key (if sticky routing used)
- `decision.fallback_used`: boolean (optional) - Whether fallback provider was used
- `decision.policy_id`: string (optional) - Policy ID used for decision
- `decision.expires_at_ms`: number (optional) - Decision expiration timestamp
- `decision.metadata`: object (optional) - Additional decision metadata
- `context.request_id`: string (required) - Request ID from `DecideRequest`
- `context.trace_id`: string (optional) - Trace ID from `DecideRequest`

**Implementation**: `router_core.erl` → `router_decider.erl`  
**Tests**: `router_core_SUITE.erl`, `router_decider_SUITE.erl`

**Example**:
```json
{
  "ok": true,
  "decision": {
    "provider_id": "openai:gpt-4o",
    "priority": 50,
    "expected_latency_ms": 850,
    "expected_cost": 0.012,
    "reason": "best_score",
    "policy_id": "policy:default"
  },
  "context": {
    "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242",
    "trace_id": "tr-123"
  }
}
```

## ErrorResponse

### Invariants

Router **guarantees**:
- `ErrorResponse` **always** includes `ok: false`
- `error.code` **always** one of: `"unauthorized"`, `"invalid_request"`, `"policy_not_found"`, `"decision_failed"`, `"internal"`
- `error.message` **always** non-empty string
- Error code **always** maps to specific HTTP/gRPC status (see Error Code Mapping)

### Fields
```json
{
  "ok": false,
  "error": {
    "code": "string",
    "message": "string",
    "details": "object (optional)"
  },
  "context": {
    "request_id": "string",
    "trace_id": "string (optional)"
  }
}
```

**Error Codes** (required):
- `"unauthorized"`: Authentication/authorization failed → HTTP 401 / gRPC UNAUTHENTICATED
- `"invalid_request"`: Request validation failed → HTTP 400 / gRPC INVALID_ARGUMENT
- `"policy_not_found"`: Policy not found → HTTP 404 / gRPC NOT_FOUND
- `"decision_failed"`: Routing decision failed (no provider available) → HTTP 500 / gRPC INTERNAL
- `"internal"`: Internal server error → HTTP 500 / gRPC INTERNAL

**Implementation**: `router_error.erl`  
**Tests**: `router_error_SUITE.erl`, `router_errors_mapping_SUITE.erl`

**Example**:
```json
{
  "ok": false,
  "error": {
    "code": "policy_not_found",
    "message": "Policy not found in store",
    "details": {
      "tenant_id": "acme",
      "policy_id": "policy:nonexistent"
    }
  },
  "context": {
    "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242",
    "trace_id": "tr-123"
  }
}
```

## ExecAssignment

### Invariants

Router **guarantees**:
- `ExecAssignment` **only** published if `DecideRequest.push_assignment: true`
- `assignment_id` **always** UUID v4 (generated by Router)
- `executor.provider_id` **always** matches `DecideResponse.decision.provider_id`
- `job.type` **always** matches `DecideRequest.task.type`
- `options.deadline_ms` **always** calculated from `expected_latency_ms` if not provided

### Fields
```json
{
  "version": "1",
  "assignment_id": "uuid-string",
  "request_id": "uuid-string",
  "executor": {
    "provider_id": "string",
    "channel": "nats" | "grpc",
    "endpoint": "string (optional)"
  },
  "job": {
    "type": "string",
    "payload_ref": "string (optional)",
    "payload": "any (optional)"
  },
  "options": {
    "priority": "number (optional)",
    "deadline_ms": "number (optional, calculated from expected_latency_ms if not provided)",
    "retry": {
      "max_attempts": "number (optional, default: 2)",
      "backoff_ms": "number (optional, default: 200)"
    }
  },
  "correlation": {
    "trace_id": "string (optional)"
  },
  "decision": {
    "provider_id": "string",
    "expected_latency_ms": "number",
    "expected_cost": "number",
    "reason": "string"
  },
  "metadata": "object (optional)"
}
```

**Field Requirements**:
- `version`: `"1"` (string, required)
- `assignment_id`: UUID v4 string (required) - Generated by Router
- `request_id`: UUID string (required) - From `DecideRequest.request_id`
- `executor.provider_id`: string (required) - Provider to execute job
- `executor.channel`: `"nats"` or `"grpc"` (string, required)
- `executor.endpoint`: string (optional) - Provider endpoint (for gRPC)
- `job.type`: string (required) - From `DecideRequest.task.type`
- `job.payload_ref`: string (optional) - From `DecideRequest.task.payload_ref`
- `job.payload`: any (optional) - From `DecideRequest.task.payload`
- `options.priority`: number (optional) - From `DecideResponse.decision.priority`
- `options.deadline_ms`: number (optional) - Calculated if not provided: `max(min_ms, min(max_ms, expected_latency_ms * multiplier))`
  - Default multiplier: 5x (config: `caf_deadline_multiplier`)
  - Default min: 5000ms (config: `caf_deadline_min_ms`)
  - Default max: 60000ms (config: `caf_deadline_max_ms`)
- `options.retry.max_attempts`: number (optional, default: 2)
- `options.retry.backoff_ms`: number (optional, default: 200)
- `correlation.trace_id`: string (optional) - From `DecideRequest.trace_id`
- `decision.provider_id`: string (required) - From `DecideResponse.decision.provider_id`
- `decision.expected_latency_ms`: number (required) - From `DecideResponse.decision.expected_latency_ms`
- `decision.expected_cost`: number (required) - From `DecideResponse.decision.expected_cost`
- `decision.reason`: string (required) - From `DecideResponse.decision.reason`
- `metadata`: object (optional) - From `DecideRequest.metadata`
- `tenant_id`: string (optional) - From `DecideRequest.tenant_id`

**Implementation**: `router_caf_adapter.erl`  
**Tests**: `router_caf_adapter_SUITE.erl`, `router_assignment_SUITE.erl`

**Example**:
```json
{
  "version": "1",
  "assignment_id": "a1f5b1d2-3c4e-5f6a-7b8c-9d0e1f2a3b4c",
  "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242",
  "executor": {
    "provider_id": "openai:gpt-4o",
    "channel": "nats"
  },
  "job": {
    "type": "text.generate",
    "payload_ref": "s3://bucket/key"
  },
  "options": {
    "priority": 50,
    "deadline_ms": 5000,
    "retry": {
      "max_attempts": 2,
      "backoff_ms": 200
    }
  },
  "correlation": {
    "trace_id": "tr-123"
  },
  "decision": {
    "provider_id": "openai:gpt-4o",
    "expected_latency_ms": 850,
    "expected_cost": 0.012,
    "reason": "best_score"
  },
  "metadata": {
    "user_id": "u-42"
  }
}
```

## ExecAssignmentAck

### Invariants

Router **guarantees**:
- `ExecAssignmentAck` **only** processed if received on `caf.exec.assign.v1.ack` subject
- `assignment_id` **always** matches `ExecAssignment.assignment_id`
- `status` **always** one of: `"accepted"`, `"rejected"`, `"error"`

### Fields
```json
{
  "assignment_id": "uuid-string",
  "status": "accepted" | "rejected" | "error",
  "message": "string (optional)"
}
```

**Field Requirements**:
- `assignment_id`: UUID string (required) - From `ExecAssignment.assignment_id`
- `status`: `"accepted"` | `"rejected"` | `"error"` (string, required)
- `message`: string (optional) - Status message

**Implementation**: `router_ack_consumer.erl`  
**Tests**: `router_ack_consumer_SUITE.erl`

**Example**:
```json
{
  "assignment_id": "a1f5b1d2-3c4e-5f6a-7b8c-9d0e1f2a3b4c",
  "status": "accepted",
  "message": "Assignment queued for execution"
}
```

## ExecResult

### CP1 StepResult Contract (CAF Worker Internal)

**CRITICAL**: CAF Worker internally uses a unified `StepResult` type (C++ struct) for all block executions. This type is converted to `ExecResult` JSON format via `ResultConverter::to_exec_result_json()` before publishing to NATS.

**StepResult → ExecResult Mapping**:
- `StepStatus::ok` → `ExecResult.status = "success"`
- `StepStatus::error` → `ExecResult.status = "error"`
- `StepStatus::timeout` → `ExecResult.status = "timeout"`
- `StepStatus::cancelled` → `ExecResult.status = "cancelled"`
- `ErrorCode` (1xxx-5xxx) → `ExecResult.error_code` (string format, if present)
- `ResultMetadata` (trace_id, flow_id, step_id, tenant_id) → `ExecResult` correlation fields

**StepResult Contract Definition**: See `apps/caf/processor/docs/ARCHITECTURE_ROLE.md#43-stepresult-contract-cp1-invariant` for complete contract specification.

**Implementation**: 
- StepResult type: `apps/caf/processor/include/beamline/worker/core.hpp`
- ResultConverter: `apps/caf/processor/include/beamline/worker/result_converter.hpp`

### Invariants

Router **guarantees**:
- `ExecResult` **only** processed if received on `caf.exec.result.v1` subject
- `assignment_id` **always** matches `ExecAssignment.assignment_id`
- `request_id` **always** matches `DecideRequest.request_id`
- `status` **always** one of: `"success"`, `"error"`, `"timeout"`, `"cancelled"`
- `latency_ms` **always** >= 0
- `cost` **always** >= 0

### Fields
```json
{
  "assignment_id": "uuid-string",
  "request_id": "uuid-string",
  "status": "success" | "error" | "timeout" | "cancelled",
  "provider_id": "string",
  "job": {
    "type": "string"
  },
  "latency_ms": "number",
  "cost": "number",
  "trace_id": "string (optional)",
  "tenant_id": "string (optional)",
  "timestamp": "number (optional, milliseconds since epoch)"
}
```

**Field Requirements**:
- `assignment_id`: UUID string (required) - From `ExecAssignment.assignment_id`
- `request_id`: UUID string (required) - From `DecideRequest.request_id`
- `status`: `"success"` | `"error"` | `"timeout"` | `"cancelled"` (string, required)
- `provider_id`: string (required) - Provider that executed the job
- `job.type`: string (required) - From `ExecAssignment.job.type`
- `latency_ms`: number (required) - Actual execution latency in milliseconds (>= 0)
- `cost`: number (required) - Actual execution cost (>= 0)
- `trace_id`: string (optional) - From `ExecAssignment.correlation.trace_id`
- `tenant_id`: string (optional) - From `ExecAssignment.tenant_id`
- `timestamp`: number (optional) - Result timestamp in milliseconds (default: current time)

**Implementation**: `router_result_consumer.erl`  
**Tests**: `router_result_consumer_SUITE.erl`

**Example (Success)**:
```json
{
  "assignment_id": "a1f5b1d2-3c4e-5f6a-7b8c-9d0e1f2a3b4c",
  "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242",
  "status": "success",
  "provider_id": "openai:gpt-4o",
  "job": {
    "type": "text.generate"
  },
  "latency_ms": 850,
  "cost": 0.012,
  "trace_id": "tr-123",
  "tenant_id": "acme",
  "timestamp": 1706371200000
}
```

**Example (Error)**:
```json
{
  "assignment_id": "a1f5b1d2-3c4e-5f6a-7b8c-9d0e1f2a3b4c",
  "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242",
  "status": "error",
  "provider_id": "openai:gpt-4o",
  "job": {
    "type": "text.generate"
  },
  "latency_ms": 200,
  "cost": 0.0,
  "trace_id": "tr-123",
  "tenant_id": "acme",
  "timestamp": 1706371200000
}
```

## Usage Event (beamline.usage.v1.metered)

### Invariants

Router **guarantees**:
- Usage event **always** published after processing `ExecResult` (if `status: "success"`)
- `tenant_id` **always** from `ExecResult.tenant_id`
- `provider_id` **always** from `ExecResult.provider_id`
- `latency_ms` **always** from `ExecResult.latency_ms`
- `cost` **always** from `ExecResult.cost`
- `status` **always** from `ExecResult.status`

### Fields
```json
{
  "version": "1",
  "tenant_id": "string",
  "provider_id": "string",
  "event_type": "string",
  "latency_ms": "number",
  "cost": "number",
  "status": "success" | "error" | "timeout" | "cancelled",
  "trace_id": "string (optional)",
  "timestamp": "number",
  "assignment_id": "string (optional)",
  "request_id": "string (optional)"
}
```

**Field Requirements**:
- `version`: `"1"` (string, required)
- `tenant_id`: string (required) - From `ExecResult.tenant_id`
- `provider_id`: string (required) - From `ExecResult.provider_id`
- `event_type`: string (required) - From `ExecResult.job.type`
- `latency_ms`: number (required) - From `ExecResult.latency_ms` (>= 0)
- `cost`: number (required) - From `ExecResult.cost` (>= 0)
- `status`: `"success"` | `"error"` | `"timeout"` | `"cancelled"` (string, required) - From `ExecResult.status`
- `trace_id`: string (optional) - From `ExecResult.trace_id`
- `timestamp`: number (required) - Event timestamp in milliseconds
- `assignment_id`: string (optional) - From `ExecResult.assignment_id`
- `request_id`: string (optional) - From `ExecResult.request_id`

**Implementation**: `router_result_consumer.erl`  
**Tests**: `router_result_consumer_SUITE.erl`

**Example**:
```json
{
  "version": "1",
  "tenant_id": "acme",
  "provider_id": "openai:gpt-4o",
  "event_type": "text.generate",
  "latency_ms": 850,
  "cost": 0.012,
  "status": "success",
  "trace_id": "tr-123",
  "timestamp": 1706371200000,
  "assignment_id": "a1f5b1d2-3c4e-5f6a-7b8c-9d0e1f2a3b4c",
  "request_id": "e3b0c442-98fc-1c14-9afb-4c8996fb9242"
}
```

## Interaction Flows

### Flow 1: Request/Reply (Simple)

**Invariants**:
- CAF **always** publishes `DecideRequest` to `beamline.router.v1.decide`
- Router **always** publishes `DecideResponse` to NATS reply-inbox
- Router **never** publishes `ExecAssignment` if `push_assignment: false`

**Steps**:
1. CAF publishes `DecideRequest` to `beamline.router.v1.decide`
2. Router processes request via `router_nats_subscriber.erl`
3. Router publishes `DecideResponse` to reply-inbox
4. CAF receives response and executes job

**Tests**: `router_nats_subscriber_caf_SUITE.erl`

### Flow 2: Push Assignment

**Invariants**:
- Router **only** publishes `ExecAssignment` if `DecideRequest.push_assignment: true`
- Router **always** publishes `DecideResponse` before `ExecAssignment`
- Router **always** processes `ExecAssignmentAck` if received on `caf.exec.assign.v1.ack`

**Steps**:
1. CAF publishes `DecideRequest` with `push_assignment: true`
2. Router publishes `DecideResponse` to reply-inbox
3. Router publishes `ExecAssignment` to `caf.exec.assign.v1` (via `router_caf_adapter.erl`)
4. CAF receives both messages and processes assignment
5. CAF optionally publishes `ExecAssignmentAck` to `caf.exec.assign.v1.ack`
6. Router processes `ExecAssignmentAck` via `router_ack_consumer.erl`

**Tests**: `router_caf_adapter_SUITE.erl`, `router_ack_consumer_SUITE.erl`

## Validation Rules

### DecideRequest

Router **rejects** `DecideRequest` with `invalid_request` error if:
- `version` is not `"1"` (string)
- `request_id` is not valid UUID string
- `tenant_id` is empty or missing
- `task.type` is empty or missing
- Both `task.payload_ref` and `task.payload` are missing
- `policy_id` is provided but empty string
- `push_assignment` is not boolean (if provided)
- `assignment_subject` is provided but invalid NATS subject

**Implementation**: `router_nats_subscriber.erl`  
**Tests**: `router_nats_contract_validation_SUITE.erl`

### DecideResponse

Router **guarantees**:
- `ok` is always `true` for success responses
- `decision.provider_id` is always non-empty string
- `decision.priority` is always 0-100
- `decision.expected_latency_ms` is always >= 0
- `decision.expected_cost` is always >= 0
- `decision.reason` is always one of: `"weighted"`, `"sticky"`, `"fallback"`, `"best_score"`

**Implementation**: `router_core.erl`, `router_decider.erl`  
**Tests**: `router_core_SUITE.erl`, `router_decider_SUITE.erl`

### ErrorResponse

Router **guarantees**:
- `ok` is always `false` for error responses
- `error.code` is always one of: `"unauthorized"`, `"invalid_request"`, `"policy_not_found"`, `"decision_failed"`, `"internal"`
- `error.message` is always non-empty string

**Implementation**: `router_error.erl`  
**Tests**: `router_error_SUITE.erl`

### ExecAssignment

Router **guarantees**:
- `version` is always `"1"` (string)
- `assignment_id` is always valid UUID v4 string
- `request_id` always matches original `DecideRequest.request_id`
- `executor.provider_id` is always non-empty string
- `executor.channel` is always `"nats"` or `"grpc"`
- `job.type` always matches `DecideRequest.task.type`

**Implementation**: `router_caf_adapter.erl`  
**Tests**: `router_caf_adapter_SUITE.erl`, `router_assignment_SUITE.erl`

## Error Code Mapping

Router **always** maps error reasons to error codes as follows:

### Business Logic Errors

| Router Error Reason | Error Code | HTTP Status | gRPC Status |
|---------------------|------------|-------------|-------------|
| `missing_tenant_id` | `invalid_request` | 400 | INVALID_ARGUMENT |
| `policy_not_found` | `policy_not_found` | 404 | NOT_FOUND |
| `no_provider_available` | `decision_failed` | 500 | INTERNAL |
| `invalid_policy` | `invalid_request` | 400 | INVALID_ARGUMENT |
| `unauthorized` | `unauthorized` | 401 | UNAUTHENTICATED |
| Other errors | `internal` | 500 | INTERNAL |

**Implementation**: `router_error.erl` → `router_error:to_grpc/1`  
**Tests**: `router_error_SUITE.erl`, `router_errors_mapping_SUITE.erl`

### Intake Validation Errors (CP2+)

Router intake validation layer uses standardized error codes that map to HTTP status codes as follows:

| Intake Error Code | Router Error Code | HTTP Status | gRPC Status | Description |
|-------------------|-------------------|-------------|-------------|-------------|
| `SCHEMA_VALIDATION_FAILED` | `invalid_request` | 400 | INVALID_ARGUMENT | Message schema validation failed (protobuf/JSON decode error, missing required fields) |
| `VERSION_UNSUPPORTED` | `invalid_request` | 400 | INVALID_ARGUMENT | Unsupported schema version |
| `CORRELATION_FIELDS_INVALID` | `invalid_request` | 400 | INVALID_ARGUMENT | Correlation fields validation failed (missing/invalid run_id, flow_id, step_id, trace_id, idempotency_key) |
| `TENANT_FORBIDDEN` | `unauthorized` | 401 | UNAUTHENTICATED | Tenant validation failed (not in allowlist, ACL check failed) |
| `IDEMPOTENCY_VIOLATION` | `invalid_request` | 400 | INVALID_ARGUMENT | Idempotency violation (duplicate request with conflicting data) |
| `INTERNAL_VALIDATION_ERROR` | `internal` | 500 | INTERNAL | Internal validation error (unexpected exception, system error) |

**Implementation**: 
- Error codes: `router_intake_error_codes.erl`
- Error handling: `router_intake_error_handler.erl` → `build_error_response/3`
- Error response format: `{ok: false, error: {code, message}, context: {...}}`

**Gateway Mapping**:
- Gateway maps Router error codes to HTTP status codes via `map_router_error_status()` function
- Intake error codes are mapped to standard Router error codes (see table above)
- Gateway should be updated to recognize intake error codes directly (future enhancement)

**Error Response Format** (for request-reply pattern):
```json
{
  "ok": false,
  "error": {
    "code": "SCHEMA_VALIDATION_FAILED",
    "message": "Schema validation failed: missing tenant_id"
  },
  "context": {
    "request_id": "req-123",
    "trace_id": "trace-456"
  }
}
```

**Tests**: `router_intake_error_codes_SUITE.erl`, `router_intake_error_handler_SUITE.erl`, `router_intake_e2e_SUITE.erl`

## Limits

### Request Limits
- Maximum payload size: 1MB (config: `nats_max_payload_size`)
- Request timeout: 5 seconds (config: `router_request_timeout_ms`)

### Response Limits
- Maximum response size: 1MB
- Response timeout: 5 seconds

**Implementation**: `router_nats_subscriber.erl`  
**Tests**: `router_nats_subscriber_caf_SUITE.erl`

## References

- **Core Message Fields**: `docs/CORE_MESSAGE_FIELDS_SPECIFICATION.md` - Complete specification for all core fields
- **NATS Subjects**: `docs/NATS_SUBJECTS.md`
- **Proto-NATS Mapping**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- **Request Handling**: `apps/otp/router/src/router_nats_subscriber.erl`
- **Assignment Publishing**: `apps/otp/router/src/router_caf_adapter.erl`
- **Core Routing**: `apps/otp/router/src/router_core.erl`
- **Error Mapping**: `apps/otp/router/src/router_error.erl`

## Gateway Backpressure Protocol (T-DOCS-01)

This protocol allows Gateway (NestJS) to proactively detect Router overload and throttle traffic before it reaches JetStream.

### Invariants

Router **guarantees**:
- Backpressure status query **always** returns the current state for a specific subject.
- `status` field indicates the level of backpressure, NOT the overall health of the router itself.
- `retry_after_seconds` is **always** present when backpressure is detected (`active` or `warning`) within the `policy` object.

### Backpressure Status Query

**NATS Subject**: `beamline.router.v1.status.backpressure` (Request-Reply)

**Request Payload**:
```json
{
  "subject": "beamline.router.v1.decide"
}
```

**Response Payload**:
```json
{
  "success": true,
  "data": {
    "subject": "string",
    "status": "active" | "warning" | "inactive" | "unknown",
    "metrics": {
      "pending_messages": 1200,
      "latency_p95_ms": 5500,
      "inflight_messages": 450
    },
    "thresholds": {
      "queue_overload": 1000,
      "latency_overload_ms": 5000,
      "inflight_overload": 500
    },
    "policy": {
      "retry_after_seconds": 30,
      "max_retry_attempts": 3
    },
    "timestamp": 1706371200000
  }
}
```

### Protocol Fields

- `status`:
    - `"active"`: Critical overload. Gateway **MUST** reject requests with HTTP 429 / gRPC RESOURCE_EXHAUSTED.
    - `"warning"`: Approaching overload. Gateway **SHOULD** apply background throttling or prioritize critical traffic.
    - `"inactive"`: Normal operation.
- `metrics.pending_messages`: Current number of messages waiting in the JetStream consumer.
- `metrics.latency_p95_ms`: 95th percentile of processing time for the last 1000 messages.
- `metrics.inflight_messages`: Number of messages currently being processed by Router workers.
- `policy.retry_after_seconds`: Recommended wait time before retrying.

### Status and Error Mapping

When Gateway receives an overload signal (either via proactive query or 429 response), it MUST map it as follows:

| Router State | HTTP Status | gRPC Status | Gateway Action |
|--------------|-------------|-------------|----------------|
| `active`     | 429 Too Many Requests | 8 RESOURCE_EXHAUSTED | Immediate rejection, include `Retry-After` header |
| `warning`    | 200 OK (with Warning) | 0 OK | Continue, but log warning and monitor metrics |

**Response Header**: `Retry-After: <retry_after_seconds>`

### Gateway Implementation Recommendations

1.  **Proactive Monitoring**: Gateways should query `beamline.router.v1.status.backpressure` every 5-10 seconds for high-traffic subjects.
2.  **Local Cache**: Cache the backpressure status for 5 seconds to avoid excessive status queries.
3.  **Graceful Degradation**:
    - If `active`, stop sending new requests immediately for the duration of `retry_after_seconds`.
    - If `warning`, stop sending low-priority background requests (e.g., telemetry cleanup, pre-fetching).
4.  **Circuit Breaker Integration**: If status remains `active` for > 3 consecutive checks (15-30s), Gateway should open its own circuit breaker to the Router.

**Implementation**: `router_gateway_backpressure.erl`, `router_intake_backpressure.erl`  
**Tests**: `router_gateway_integration_SUITE.erl`

