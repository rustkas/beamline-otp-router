# Router ↔ CAF Integration Guide

## Overview

This document describes the Router ↔ CAF integration via NATS using JSON message contracts.

## Architecture

```
CAF → [NATS] → Router → [NATS] → CAF
         ↓                    ↓
    DecideRequest      DecideResponse
                        ExecAssignment (optional)
```

## Integration Modes

### Mode 1: Request/Reply (Simple)

CAF requests routing decision, Router responds with decision:

1. CAF publishes `DecideRequest` to `beamline.router.v1.decide`
2. Router processes and publishes `DecideResponse` to reply-inbox
3. CAF receives response and executes job independently

### Mode 2: Push Assignment

CAF requests routing decision with `push_assignment: true`, Router responds and pushes assignment:

1. CAF publishes `DecideRequest` with `push_assignment: true`
2. Router publishes `DecideResponse` to reply-inbox
3. Router publishes `ExecAssignment` to `caf.exec.assign.v1`
4. CAF receives both messages and processes assignment
5. CAF optionally publishes `ExecAssignmentAck` to `caf.exec.assign.v1.ack`

## Implementation

### Router Side

**Modules**:
- `router_nats_subscriber.erl` - Subscribes to `beamline.router.v1.decide` and handles requests
- `router_caf_adapter.erl` - Publishes `ExecAssignment` to CAF

**Key Functions**:
- `router_nats_subscriber:handle_nats_message/2` - Processes DecideRequest
- `router_caf_adapter:publish_assignment/2` - Publishes ExecAssignment

### CAF Side (Requirements)

**Required**:
- Subscribe to `beamline.router.v1.decide` for DecideRequest
- Subscribe to `caf.exec.assign.v1` for ExecAssignment (if using push mode)
- Publish DecideResponse to reply-inbox
- Optionally publish ExecAssignmentAck to `caf.exec.assign.v1.ack`

**Validation**:
- Validate `version`, `assignment_id`, `executor.provider_id`, `job.type`
- Handle `payload_ref` (download) or `payload` (inline)

## Configuration

### Router Configuration

```erlang
{beamline_router, [
    {nats_url, "nats://localhost:4222"},
    {nats_mode, real},  %% or mock
    {nats_subject, <<"beamline.router.v1.decide">>},
    {nats_timeout_ms, 5000}
]}
```

### CAF Configuration

- NATS connection URL
- Subject subscriptions:
  - `beamline.router.v1.decide` (input)
  - `caf.exec.assign.v1` (input, if using push mode)
  - `caf.exec.assign.v1.ack` (output, optional)

## Message Examples

### DecideRequest (CAF → Router)

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

### DecideResponse (Router → CAF)

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

### ExecAssignment (Router → CAF)

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

## Error Handling

### ErrorResponse Format

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

### Error Codes

- `unauthorized` - Authentication/authorization failed
- `invalid_request` - Request validation failed
- `policy_not_found` - Policy not found
- `decision_failed` - Routing decision failed (no provider available)
- `internal` - Internal server error

## Testing

### Unit Tests

- `router_caf_adapter_SUITE.erl` - Tests ExecAssignment publishing
- `router_nats_subscriber_caf_SUITE.erl` - Tests DecideRequest handling

### Integration Tests

- Mock CAF subscriber for `caf.exec.assign.v1`
- Verify ExecAssignment format and content
- Test error scenarios

## Security

- **mTLS**: NATS connection should use mTLS
- **JWT**: Subject-based authentication
- **Subject Whitelist**: Validate allowed subjects
- **Multi-tenancy**: Enforce `tenant_id` validation

## Tracing

- `trace_id` propagated through all messages
- `request_id` for request correlation
- Telemetry spans include correlation IDs

## References

- `docs/NATS_SUBJECTS.md` - NATS subject specifications
- `docs/API_CONTRACTS.md` - Message format specifications
- `src/router_nats_subscriber.erl` - Request handling
- `src/router_caf_adapter.erl` - Assignment publishing

