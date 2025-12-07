# Router Intake Error Handling

This document describes error handling for Router intake validation failures.

## Overview

Router uses a unified error handling layer for all intake validation failures. When a message fails validation, Router:

1. **Logs audit event** with error details
2. **Emits metrics** for monitoring
3. **Publishes to DLQ** (if enabled) for failed messages
4. **Handles NATS message fate** (ACK/NAK) based on error type
5. **Sends error response** (for request-reply patterns)

## Error Codes

Router defines a fixed set of domain-specific error codes for intake validation:

### SCHEMA_VALIDATION_FAILED

**Code**: `SCHEMA_VALIDATION_FAILED`

**Description**: Message schema validation failed (protobuf/JSON decode error, missing required fields).

**Severity**: `ERROR`

**Examples**:
- `"Schema validation failed: protobuf_decode_failed"`
- `"Schema validation failed: missing tenant_id"`
- `"Schema validation failed: invalid_json_format"`

**Action**: ACK message, send to DLQ (don't retry).

### VERSION_UNSUPPORTED

**Code**: `VERSION_UNSUPPORTED`

**Description**: Message version is not supported by Router.

**Severity**: `ERROR`

**Examples**:
- `"Unsupported schema version: 2, supported versions: [1]"`

**Action**: ACK message, send to DLQ (don't retry).

### CORRELATION_FIELDS_INVALID

**Code**: `CORRELATION_FIELDS_INVALID`

**Description**: Correlation fields validation failed (missing fields, invalid formats, dependency violations).

**Severity**: `ERROR`

**Examples**:
- `"Correlation fields validation failed: missing_run_id"`
- `"Correlation fields validation failed: invalid_trace_id_format"`
- `"Correlation fields validation failed: flow_id_required_when_run_id_present"`

**Action**: ACK message, send to DLQ (don't retry).

### TENANT_FORBIDDEN

**Code**: `TENANT_FORBIDDEN`

**Description**: Tenant validation failed (not in allowlist, ACL check failed).

**Severity**: `ERROR`

**Examples**:
- `"Tenant validation failed: tenant_not_in_allowlist"`
- `"Tenant validation failed: missing_tenant_id"`

**Action**: ACK message, send to DLQ (don't retry).

### IDEMPOTENCY_VIOLATION

**Code**: `IDEMPOTENCY_VIOLATION`

**Description**: Idempotency violation (duplicate request with conflicting data).

**Severity**: `WARN` (not critical, may be retry with same data)

**Examples**:
- `"Idempotency violation: duplicate_request_with_conflicting_data"`

**Action**: ACK message (already processed), don't send to DLQ.

### INTERNAL_VALIDATION_ERROR

**Code**: `INTERNAL_VALIDATION_ERROR`

**Description**: Internal validation error (unexpected exception, system error).

**Severity**: `ERROR`

**Examples**:
- `"Internal validation error: ets_table_not_available"`
- `"Internal validation error: protobuf_decode_exception"`

**Action**: NAK message (may be temporary), retry with MaxDeliver limit.

## Error Handling Flow

```
Validation Error Detected
  ↓
router_intake_error_handler:handle_intake_error/7
  ├─ 1. Audit Logging
  │   └─ router_logger:error/warn (structured JSON)
  ├─ 2. Metrics Emission
  │   └─ telemetry:execute (router_intake_validation_errors_total)
  ├─ 3. DLQ Publication (if enabled and error type requires)
  │   └─ router_nats:publish_with_ack (DLQ subject)
  ├─ 4. NATS Message Fate
  │   ├─ Schema errors → ACK (don't retry)
  │   └─ Temporary errors → NAK (retry with MaxDeliver)
  └─ 5. Error Response (for request-reply)
      └─ router_nats:publish (reply subject)
```

## DLQ (Dead-Letter Queue)

### DLQ Subject Pattern

**Default**: `{original_subject}.dlq` (e.g., `beamline.router.v1.decide.dlq`)

**Configurable**: `dlq_subject_pattern` (e.g., `beamline.router.v1.intake.dlq` for unified DLQ)

**Configuration**:
```erlang
{beamline_router, [
    {dlq_subject_pattern, undefined},  %% undefined = append .dlq
    {dlq_enabled, true}  %% Enable/disable DLQ
]}
```

### DLQ Message Format

DLQ messages contain:
- **Original subject**: Subject of the failed message
- **Payload hash**: SHA256 hash of original payload (not full payload for security)
- **Validation error**: Error code, message, severity
- **Context**: Filtered context (PII removed)
- **Metadata**: received_at, router_node_id

**Example DLQ Message**:
```json
{
  "original_subject": "beamline.router.v1.decide",
  "original_payload_hash": "abc123def456...",
  "validation_error": {
    "code": "SCHEMA_VALIDATION_FAILED",
    "message": "Schema validation failed: missing tenant_id",
    "severity": "error"
  },
  "context": {
    "tenant_id": "tenant_123",
    "run_id": "run_abc123",
    "trace_id": "trace_def456"
  },
  "received_at": 1706367600123,
  "router_node_id": "router@node1"
}
```

### DLQ Publication Errors

DLQ publication is **best-effort**:
- Errors are logged but don't fail processing
- Metrics are emitted for DLQ failures
- Always returns `ok` (doesn't block message processing)

## Audit Logging

### Audit Entry Format

Audit entries are logged via `router_logger` with structured JSON format:

```json
{
  "timestamp": "2025-11-30T12:00:00.123456Z",
  "level": "ERROR",
  "component": "router",
  "message": "Intake validation failed",
  "event_type": "router.intake.validation_failed",
  "error_code": "SCHEMA_VALIDATION_FAILED",
  "error_message": "Schema validation failed: missing tenant_id",
  "subject": "beamline.router.v1.decide",
  "tenant_id": "tenant_123",
  "run_id": "run_abc123",
  "flow_id": "flow_xyz789",
  "trace_id": "trace_def456",
  "idempotency_key": "idempotency_key_123",
  "received_at": 1706367600123,
  "router_node_id": "router@node1",
  "msg_id": "msg_uuid"
}
```

### Audit Fields

**Required fields**:
- `event_type`: `"router.intake.validation_failed"`
- `error_code`: Machine-readable error code
- `error_message`: Human-readable error message
- `subject`: NATS subject of failed message
- `received_at`: Timestamp (milliseconds)
- `router_node_id`: Router node identifier

**Optional fields** (if available):
- `tenant_id`, `run_id`, `flow_id`, `step_id`
- `idempotency_key`, `trace_id`, `msg_id`

**Forbidden fields**:
- ❌ Full payload (may contain PII/secrets)
- ❌ Secrets (api_key, password, token, etc.)

## Metrics

### Validation Error Metrics

**Metric**: `router_intake_validation_errors_total`

**Type**: Counter

**Labels**:
- `error_code`: Error code (SCHEMA_VALIDATION_FAILED, VERSION_UNSUPPORTED, etc.)
- `subject`: NATS subject
- `tenant_id`: Tenant identifier (if available)

**Example**:
```
router_intake_validation_errors_total{error_code="SCHEMA_VALIDATION_FAILED", subject="beamline.router.v1.decide", tenant_id="tenant_123"} 5
```

### DLQ Metrics

**Metric**: `router_intake_dlq_messages_total`

**Type**: Counter

**Labels**:
- `reason`: `"validation_failed"`
- `error_code`: Error code
- `subject`: Original subject

**Metric**: `router_intake_dlq_publish_failed_total`

**Type**: Counter

**Labels**:
- `reason`: `"validation_failed"`
- `error_code`: Error code
- `subject`: Original subject
- `failure_reason`: Reason for DLQ publication failure

### Total Messages Metric

**Metric**: `router_intake_messages_total`

**Type**: Counter

**Labels**:
- `subject`: NATS subject
- `status`: `"ok"` or `"failed"`

## NATS Message Fate

### ACK (Acknowledge)

**When**: Schema errors (don't retry)
- `SCHEMA_VALIDATION_FAILED`
- `VERSION_UNSUPPORTED`
- `CORRELATION_FIELDS_INVALID`
- `TENANT_FORBIDDEN`

**Action**: `router_nats:ack_message(MsgId)`

**Result**: Message removed from JetStream (won't be redelivered)

### NAK (Negative Acknowledge)

**When**: Temporary errors (may be retryable)
- `INTERNAL_VALIDATION_ERROR` (if temporary)

**Action**: `router_nats:nak_message(MsgId)`

**Result**: Message redelivered (with MaxDeliver limit)

### MaxDeliver Exhaustion

**When**: Message exceeds `max_deliver` attempts

**Action**: ACK message, send to DLQ

**Result**: Message removed from JetStream, sent to DLQ for manual review

## Error Response (Request-Reply Pattern)

For request-reply subjects (e.g., `beamline.router.v1.decide`), Router sends error response:

**Response Format**:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Schema validation failed: missing tenant_id",
    "intake_error_code": "SCHEMA_VALIDATION_FAILED"
  },
  "context": {
    "request_id": "req-123",
    "trace_id": "trace-456"
  }
}
```

**Response Subject**: `{original_subject}.reply` (e.g., `beamline.router.v1.decide.reply`)

**Error Code Mapping**:
- Router maps intake error codes to Gateway-compatible error codes:
  - `SCHEMA_VALIDATION_FAILED` → `invalid_request` (HTTP 400)
  - `VERSION_UNSUPPORTED` → `invalid_request` (HTTP 400)
  - `CORRELATION_FIELDS_INVALID` → `invalid_request` (HTTP 400)
  - `TENANT_FORBIDDEN` → `unauthorized` (HTTP 401)
  - `IDEMPOTENCY_VIOLATION` → `invalid_request` (HTTP 400)
  - `INTERNAL_VALIDATION_ERROR` → `internal` (HTTP 500)

**Gateway HTTP Status Mapping**:
- Gateway `map_router_error_status()` function maps Router error codes to HTTP status:
  - `invalid_request` → `400 Bad Request`
  - `unauthorized` → `401 Unauthorized`
  - `internal` → `500 Internal Server Error`

**Note**: Original intake error code is included in `error.intake_error_code` field for debugging and detailed error tracking.

## Usage Examples

### Using Error Codes

```erlang
%% In validator
ErrorCode = schema_validation_failed,
ErrorMessage = router_intake_error_codes:error_code_message(ErrorCode, #{
    <<"reason">> => <<"protobuf_decode_failed">>
}),
{error, {ErrorCode, ErrorMessage, Context}}
```

### Handling Errors

```erlang
%% In consumer
case router_intake_validator:validate_intake_message(Subject, Payload, Headers, decide) of
    {ok, ValidatedMessage} ->
        %% Process message
        handle_message(ValidatedMessage);
    {error, {ErrorCode, ErrorMessage, ErrorContext}} ->
        %% Handle error
        router_intake_error_handler:handle_intake_error(
            ErrorCode, ErrorMessage, Subject, Payload, Headers, MsgId, ErrorContext
        )
end
```

### Checking Error Severity

```erlang
Severity = router_intake_error_codes:error_code_severity(ErrorCode),
case Severity of
    error -> router_logger:error(<<"Validation failed">>, AuditEntry);
    warn -> router_logger:warn(<<"Validation failed">>, AuditEntry)
end
```

## Configuration

### DLQ Configuration

```erlang
{beamline_router, [
    %% DLQ subject pattern (undefined = append .dlq to original subject)
    {dlq_subject_pattern, undefined},
    
    %% Enable/disable DLQ publication
    {dlq_enabled, true}
]}
```

### MaxDeliver Configuration

```erlang
{beamline_router, [
    %% Maximum delivery attempts before DLQ
    {nats_js_max_deliver, 3}
]}
```

## References

- `router_intake_error_codes.erl` - Error code definitions
- `router_intake_error_handler.erl` - Error handling implementation
- `router_intake_validator.erl` - Validation layer
- `docs/ROUTER_INTAKE_ERROR_HANDLING_SPEC.md` - Detailed specification

