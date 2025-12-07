# JetStream Forwarding and NAK Implementation Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Partially Complete** (Forwarding and NAK done, Idempotency and Headers pending)

## Overview

Implemented improved JetStream message forwarding with headers support and NAK on validator errors for controlled redelivery.

## Completed Tasks

### 1. ✅ JetStream Forwarding with Headers

**Changes in `router_nats.erl`**:
- Updated `js_consumers` record to include `Subject`: `{SubscriberPid, ConsumerName, StreamName, Subject}`
- Enhanced `forward_to_subscribers/6` to:
  - Forward messages to regular subscribers with headers and msg_id
  - Forward to JetStream consumers matching subject pattern
  - Include `MsgId` in forwarded messages: `{nats_message, Subject, Payload, Headers, MsgId}`
- Added `match_subject/2` for subject pattern matching (exact match, can be extended with wildcards)

**Changes in `router_result_consumer.erl` and `router_ack_consumer.erl`**:
- Updated `handle_info/2` to accept `{nats_message, Subject, Payload, Headers, MsgId}`
- Backward compatibility: support messages without headers/msg_id

### 2. ✅ NAK on Validator Errors

**Changes in `router_result_consumer.erl`**:
- Added NAK call when `router_tenant_validator:validate_tenant/2` returns `{error, ...}`
- NAK respects `MaxDeliver` configuration for controlled redelivery
- Emits audit events before NAK

**Changes in `router_ack_consumer.erl`**:
- Added NAK call when `router_tenant_validator:validate_tenant/2` returns `{error, ...}`
- NAK respects `MaxDeliver` configuration for controlled redelivery
- Emits audit events before NAK

**Implementation**:
```erlang
case router_tenant_validator:validate_tenant(TenantId, ValidationContext) of
    {ok, ValidatedTenantId} ->
        %% Process normally
        ...
    {error, Reason, ErrorContext} ->
        %% Emit audit event
        emit_counter(router_results_tenant_rejected_total, ...),
        %% NAK for controlled redelivery
        case MsgId of
            undefined -> ok;
            _ -> router_nats:nak_message(MsgId)
        end
end
```

### 3. ✅ MsgId Extraction from Headers

**Changes in `router_nats.erl`**:
- Updated `parse_nats_message/1` to extract `msg_id` from headers:
  - Checks `nats-msg-id` (lowercase) and `Nats-Msg-Id` (original case)
  - Returns `{ok, {Subject, ReplyTo, Payload, Headers, MsgId}}`
- `MsgId` passed through forwarding chain to consumers

## Pending Tasks

### 3. ⏳ Idempotency Layer

**Required**:
- Create ETS table for processed message IDs (`assignment_id`, `request_id`, `ack_id`)
- TTL-based expiration (configurable, default: 1 hour)
- Check before processing results/ACK
- Check before usage emission
- Module: `router_idempotency.erl`

### 4. ⏳ Headers for Assignments

**Required**:
- Extend `router_nats:publish_with_ack/2` to `publish_with_ack/3` with `Headers` parameter
- Format headers as `NATS/1.0` block with `trace_id`, `tenant_id`, `version`
- Update `router_caf_adapter.erl` to:
  - Extract `trace_id`, `tenant_id`, `version` from request
  - Build headers map
  - Pass headers to `publish_with_ack/3`

### 5. ⏳ Metrics and Alerts

**Required**:
- Add alerts for:
  - `router_results_tenant_rejected_total`
  - `router_acks_tenant_rejected_total`
  - `router_tenant_audit_total`
  - JetStream redelivery rate
  - MaxDeliver exhaustion
- Update `PROMETHEUS_ALERTS.md`

### 6. ⏳ Tests

**Required**:
- E2E tests for headers in assignment publications
- E2E tests for NAK redelivery on validator errors
- E2E tests for JetStream forwarding after fixes
- Update `router_jetstream_e2e_SUITE.erl`

## Implementation Details

### Message Flow with Headers and MsgId

1. **NATS Message Received**:
   ```
   MSG <subject> [reply-to] [sid] [size]\r\n
   NATS/1.0\r\n
   Nats-Msg-Id: <msg_id>\r\n
   trace_id: <trace_id>\r\n
   tenant_id: <tenant_id>\r\n
   version: 1\r\n
   \r\n
   <payload>
   ```

2. **Parsing**:
   - `parse_nats_message/1` extracts headers and `msg_id`
   - Returns `{ok, {Subject, ReplyTo, Payload, Headers, MsgId}}`

3. **Forwarding**:
   - `forward_to_subscribers/6` matches subject to JetStream consumers
   - Sends `{nats_message, Subject, Payload, Headers, MsgId}` to matching consumers

4. **Processing**:
   - Consumer extracts `trace_id`, `tenant_id`, `version` from headers (priority) or payload (fallback)
   - Validates tenant
   - On validation error: NAK with `msg_id` for controlled redelivery

### NAK Behavior

- **Respects MaxDeliver**: NAK triggers redelivery up to `MaxDeliver` times
- **Backoff**: Redelivery uses exponential backoff (configurable)
- **Audit**: All NAK events are logged and emitted as metrics

## Configuration

### JetStream Consumer Configuration

```erlang
{nats_js_max_deliver, 3},  %% Maximum redelivery attempts
{nats_js_ack_wait_seconds, 30},  %% ACK wait time
{nats_js_backoff_seconds, [1, 2, 4]},  %% Exponential backoff
```

## References

- `src/router_nats.erl`: JetStream forwarding and msg_id extraction
- `src/router_result_consumer.erl`: NAK on validator errors
- `src/router_ack_consumer.erl`: NAK on validator errors
- `docs/NATS_BEST_PRACTICES.md`: Best practices for JetStream

