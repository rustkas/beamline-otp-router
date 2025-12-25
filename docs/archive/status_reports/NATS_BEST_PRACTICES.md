# NATS/JetStream Best Practices for Router

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Guidelines**

## Overview

This document provides best practices and recommendations for NATS/JetStream integration in the Router project, based on production-ready patterns.

## Subject Naming and Versioning

### Recommended Subjects

**Router**:
- **Input**: `beamline.router.v1.decide` (request)
- **Output**: `beamline.router.v1.decide.reply` (response)

**CAF Integration**:
- **Assignment Publication**: `caf.exec.assign.v1`
- **Result Subscription**: `caf.exec.result.v1` (durable consumer)
- **ACK Subscription** (optional): `caf.exec.assign.v1.ack` (durable consumer)

**Usage Metering**:
- **Usage Events**: `beamline.usage.v1.metered`

### Versioning Strategy

- ✅ **Version in subject name**: All subjects include `.v1` for versioning
- ✅ **Strict contract**: Subjects defined in `docs/NATS_SUBJECTS.md`
- ✅ **No drift**: Producers/consumers must use exact subject names

**Example**:
```
beamline.router.v1.decide        # Version 1
beamline.router.v2.decide        # Future version 2 (backward compatible)
```

## JetStream Durable Consumers

### Configuration for Result and ACK Consumers

**Recommended Settings**:

```erlang
%% Result Consumer
router_nats:subscribe_jetstream(
    ~"caf.exec.result.v1",      % Subject
    ~"router-results",           % Durable Group
    explicit,                       % AckPolicy: Explicit (manual ACK)
    ~"router-results-group",     % DeliverGroup (for load balancing)
    push                            % Mode: Push (or pull)
)

%% ACK Consumer
router_nats:subscribe_jetstream(
    ~"caf.exec.assign.v1.ack",  % Subject
    ~"router-acks",              % Durable Group
    explicit,                       % AckPolicy: Explicit (manual ACK)
    ~"router-acks-group",        % DeliverGroup (for load balancing)
    push                            % Mode: Push (or pull)
)
```

### Consumer Configuration Parameters

**AckPolicy**: `explicit`
- Manual acknowledgment required
- Messages are redelivered if not ACKed within ack wait time
- Provides at-least-once delivery guarantee

**MaxDeliver**: `3` (recommended)
- Maximum number of delivery attempts
- Prevents infinite redelivery loops
- After MaxDeliver, message goes to DLQ or is discarded

**Backoff**: Exponential backoff between redeliveries
- First redelivery: 1 second
- Second redelivery: 2 seconds
- Third redelivery: 4 seconds

**DeliverGroup**: Queue group name
- Enables horizontal scaling
- Multiple consumers in same group share message load
- Each message delivered to only one consumer in group

**Example Consumer Config**:
```json
{
  "durable_name": "router-results",
  "ack_policy": "explicit",
  "max_deliver": 3,
  "ack_wait": 30000000000,
  "backoff": [1000000000, 2000000000, 4000000000],
  "deliver_group": "router-results-group",
  "deliver_subject": "_INBOX.router-results",
  "deliver_policy": "all"
}
```

## Message Headers

### Required Headers

All messages should include headers for correlation and policy enforcement:

**Headers**:
- `trace_id`: Distributed tracing identifier (W3C Trace Context format)
- `tenant_id`: Multi-tenant isolation
- `version`: Schema version (e.g., `"1"`)

**Additional Headers (OpenTelemetry)**:
- `traceparent`: W3C Trace Context format (`00-{trace_id}-{span_id}-01`)
- `span_id`: Current span identifier
- `X-Trace-Id`: Compatibility header (same as `trace_id`)
- `X-Span-Id`: Compatibility header (same as `span_id`)

**Example Headers**:
```
NATS/1.0\r\n
trace_id: tr-123\r\n
traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01\r\n
tenant_id: acme\r\n
version: 1\r\n
span_id: sp-456\r\n
X-Trace-Id: tr-123\r\n
X-Span-Id: sp-456\r\n
\r\n
```

### Header Usage in Router

**Publishing Assignments**:
```erlang
%% Extract trace_id from request context
TraceId = maps:get(~"trace_id", RequestMap, undefined),
ParentContext = case TraceId of
    undefined -> undefined;
    _ -> #{~"trace_id" => TraceId}
end,

%% Build base headers
BaseHeaders = #{
    ~"trace_id" => TraceId,
    ~"tenant_id" => TenantId,
    ~"version" => ~"1"
},

%% Inject OpenTelemetry trace context (W3C format)
Headers = router_tracing:inject_trace_context(BaseHeaders, ParentContext),

%% Publish with headers
router_nats:publish_with_ack(Subject, Payload, Headers).
```

**Consuming Messages**:
```erlang
handle_info({nats_message, Subject, Payload, Headers, MsgId}, State) ->
    %% Extract from headers (priority) or payload (fallback)
    TraceId = extract_header_or_payload(Headers, Payload, ~"trace_id", ~"trace_id"),
    TenantId = extract_header_or_payload(Headers, Payload, ~"tenant_id", ~"tenant_id"),
    Version = extract_header_or_payload(Headers, Payload, ~"version", ~"version"),
    
    %% Create parent context for OpenTelemetry spans
    ParentContext = case TraceId of
        undefined -> undefined;
        _ -> #{~"trace_id" => TraceId}
    end,
    
    %% Process message with headers and trace context
    ...
```

**Implementation**:
- ✅ Headers are parsed from NATS messages in `router_nats.erl`
- ✅ Headers are passed to consumers: `{nats_message, Subject, Payload, Headers, MsgId}`
- ✅ Consumers extract `trace_id`, `tenant_id`, `version` from headers (priority) or payload (fallback)
- ✅ Headers are used for correlation and policy enforcement
- ✅ OpenTelemetry trace context is injected into headers when publishing assignments
- ✅ W3C Trace Context format (`traceparent`) is supported for distributed tracing

## Message Size Limits

### Recommended Limits

**Default Maximum Payload Size**: `1 MB` (1,048,576 bytes)

**Configuration**:
```erlang
{beamline_router, [
    {nats_max_payload_size, 1048576}  %% 1 MB in bytes
]}
```

### Large Payload Handling

**Strategy**: Use Object Store for large payloads

**Pattern**:
1. **Small payloads** (< 1 MB): Include in message body
2. **Large payloads** (≥ 1 MB): Store in Object Store (S3, etc.) and include reference

**Example**:
```json
{
  "version": "1",
  "request_id": "...",
  "task": {
    "type": "text.generate",
    "payload_ref": "s3://bucket/key"  // Large payload reference
  }
}
```

**Validation**:
```erlang
validate_payload_size(Payload) ->
    MaxSize = application:get_env(beamline_router, nats_max_payload_size, 1048576),
    PayloadSize = byte_size(Payload),
    case PayloadSize > MaxSize of
        true ->
            {error, payload_too_large};
        false ->
            ok
    end.
```

## Queue Groups for Horizontal Scaling

### Configuration

**Result Consumer Queue Group**:
```erlang
DeliverGroup = ~"router-results-group",
router_nats:subscribe_jetstream(
    ~"caf.exec.result.v1",
    ~"router-results",
    explicit,
    DeliverGroup,  %% Queue group for load balancing
    push
)
```

**ACK Consumer Queue Group**:
```erlang
DeliverGroup = ~"router-acks-group",
router_nats:subscribe_jetstream(
    ~"caf.exec.assign.v1.ack",
    ~"router-acks",
    explicit,
    DeliverGroup,  %% Queue group for load balancing
    push
)
```

### Benefits

- **Horizontal Scaling**: Multiple Router instances share message load
- **Load Balancing**: Each message delivered to only one consumer
- **Fault Tolerance**: If one consumer fails, others continue processing

## Implementation Checklist

### ✅ Completed (CP2+)

1. **Subject Naming**: ✅
   - All subjects versioned (`.v1`)
   - Subjects defined in `docs/NATS_SUBJECTS.md`
   - No subject drift

2. **Durable Consumers**: ✅
   - `router_result_consumer.erl` uses `subscribe_jetstream/5`
   - `router_ack_consumer.erl` uses `subscribe_jetstream/5`
   - `AckPolicy=explicit` configured

3. **Queue Groups**: ✅
   - `DeliverGroup` parameter supported in `subscribe_jetstream/5`
   - Ready for horizontal scaling

### ✅ Completed Enhancements

4. **MaxDeliver and Backoff**: ✅ **IMPLEMENTED**
   - `max_deliver` configurable (default: 3)
   - Exponential backoff between redeliveries (default: [1, 2, 4] seconds)
   - **Status**: Fully implemented and configurable

5. **Message Headers**: ✅ **IMPLEMENTED**
   - Headers parsed from NATS messages in `router_nats.erl`
   - Headers passed to consumers: `{nats_message, Subject, Payload, Headers, MsgId}`
   - `trace_id`, `tenant_id`, `version` extracted from headers (priority) or payload (fallback)
   - OpenTelemetry trace context injected into headers when publishing assignments
   - W3C Trace Context format (`traceparent`) supported
   - Headers used for correlation and policy enforcement
   - **Status**: Fully implemented

6. **Payload Size Validation**: ✅ **IMPLEMENTED**
   - `nats_max_payload_size` configurable (default: 1 MB)
   - Payload size validated before processing
   - Oversized messages rejected with clear error
   - **Status**: Fully implemented

7. **NAK on Validation Errors**: ✅ **IMPLEMENTED**
   - NAK called when tenant validation fails in `router_result_consumer` and `router_ack_consumer`
   - NAK respects `MaxDeliver` configuration for controlled redelivery
   - Backoff delays applied between redeliveries
   - Redelivery metric `router_jetstream_redelivery_total` emitted on each NAK
   - **Status**: Fully implemented

8. **Idempotency**: ✅ **IMPLEMENTED**
   - ETS-based idempotency cache with TTL (default: 1 hour)
   - Supports keys: `assignment_id`, `request_id`, `ack_id`, `usage_id`
   - Atomic check-and-mark operations via `gen_server:call`
   - Prevents duplicate processing of results, ACKs, and usage events
   - Metrics: `router_idempotency_hit_total`, `router_idempotency_miss_total`
   - **Status**: Fully implemented

## Configuration Example

### Complete Configuration

```erlang
{beamline_router, [
    %% NATS Connection
    {nats_mode, real},
    {nats_host, "localhost"},
    {nats_port, 4222},
    {nats_tls_enabled, false},
    
    %% Subject Configuration
    {nats_subject, ~"beamline.router.v1.decide"},
    {nats_reply_subject, ~"beamline.router.v1.decide.reply"},
    {caf_assignment_subject, ~"caf.exec.assign.v1"},
    {result_subject, ~"caf.exec.result.v1"},
    {ack_subject, ~"caf.exec.assign.v1.ack"},
    {usage_subject, ~"beamline.usage.v1.metered"},
    
    %% JetStream Durable Groups
    {nats_js_durable_group_results, ~"router-results"},
    {nats_js_durable_group_acks, ~"router-acks"},
    
    %% Message Limits
    {nats_max_payload_size, 1048576},  %% 1 MB
    
    %% Consumer Configuration
    {nats_js_max_deliver, 3},
    {nats_js_ack_wait_seconds, 30},
    {nats_js_backoff_seconds, [1, 2, 4]},
    
    %% Queue Groups
    {nats_js_deliver_group_results, ~"router-results-group"},
    {nats_js_deliver_group_acks, ~"router-acks-group"}
]}
```

## References

- `docs/NATS_SUBJECTS.md`: Complete subject specification
- `docs/API_CONTRACTS.md`: Message format specifications
- `src/router_nats.erl`: NATS client implementation
- `src/router_result_consumer.erl`: Result consumer implementation
- `src/router_ack_consumer.erl`: ACK consumer implementation

