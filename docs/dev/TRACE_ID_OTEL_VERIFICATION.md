# Trace ID OpenTelemetry Verification Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

This report verifies that `trace_id` is visible end-to-end in OpenTelemetry across the Router → CAF → Result/ACK flow.

## Implementation

### 1. Router → CAF (Assignment Publication)

**Flow**:
1. `router_caf_adapter:do_publish_assignment/3` extracts `trace_id` from `RequestMap`
2. Creates `ParentContext` from `trace_id`: `#{<<"trace_id">> => TraceId}`
3. Starts OpenTelemetry span `beamline.router.publish.assignment` with `ParentContext`
4. Builds headers with `trace_id`, `tenant_id`, `version`
5. **Injects OpenTelemetry trace context** using `router_tracing:inject_trace_context/2`:
   - W3C Trace Context format: `traceparent` header
   - Custom format: `trace_id`, `span_id` headers
   - Compatibility headers: `X-Trace-Id`, `X-Span-Id`
6. Publishes assignment with headers via `router_nats:publish_with_ack/3`

**Code Location**:
- `src/router_caf_adapter.erl`: Lines 65-171
- `src/router_tracing.erl`: Lines 178-212 (inject_trace_context)

**Headers Published**:
```erlang
#{
    <<"trace_id">> => TraceId,           %% Custom format
    <<"tenant_id">> => TenantId,
    <<"version">> => <<"1">>,
    <<"traceparent">> => TraceParent,    %% W3C Trace Context
    <<"span_id">> => SpanId,             %% Current span ID
    <<"X-Trace-Id">> => TraceId,         %% Compatibility
    <<"X-Span-Id">> => SpanId            %% Compatibility
}
```

### 2. CAF → Router (Result Processing)

**Flow**:
1. `router_result_consumer` receives message with headers and payload
2. Extracts `trace_id` from headers (priority) or payload (fallback)
3. Creates `ParentContext` from `trace_id`: `#{<<"trace_id">> => TraceId}`
4. Starts OpenTelemetry span `beamline.router.process.result` with `ParentContext`
5. Processes result and emits usage event with same `trace_id`

**Code Location**:
- `src/router_result_consumer.erl`: Lines 125-159
- `src/router_tracing.erl`: Lines 156-176 (extract_trace_context)

**Trace Context Extraction**:
```erlang
TraceId = extract_header_or_payload(Headers, Result, <<"trace_id">>, <<"trace_id">>),
ParentContext = case TraceId of
    undefined -> undefined;
    _ -> #{<<"trace_id">> => TraceId}
end,
router_tracing:with_span(?SPAN_ROUTER_PROCESS_RESULT, Attributes, ParentContext, Fun)
```

### 3. CAF → Router (ACK Processing)

**Flow**:
1. `router_ack_consumer` receives message with headers and payload
2. Extracts `trace_id` from headers (priority) or payload (fallback)
3. Creates `ParentContext` from `trace_id`: `#{<<"trace_id">> => TraceId}`
4. Starts OpenTelemetry span `beamline.router.process.ack` with `ParentContext`

**Code Location**:
- `src/router_ack_consumer.erl`: Lines 125-159
- Same trace context extraction as result consumer

### 4. Usage Event Emission

**Flow**:
1. After processing result, `emit_usage_event/2` is called
2. Extracts `trace_id` from result context
3. Creates `TraceContext` from `trace_id`: `#{<<"trace_id">> => TraceId}`
4. Starts OpenTelemetry span `beamline.router.emit.usage` with `TraceContext`
5. Publishes usage event with `trace_id` in payload

**Code Location**:
- `src/router_result_consumer.erl`: Lines 194-237

## OpenTelemetry Integration

### Span Names

| Component | Span Name | Parent Context |
|-----------|----------|----------------|
| Assignment Publication | `beamline.router.publish.assignment` | From `RequestMap.trace_id` |
| Result Processing | `beamline.router.process.result` | From headers/payload `trace_id` |
| ACK Processing | `beamline.router.process.ack` | From headers/payload `trace_id` |
| Usage Emission | `beamline.router.emit.usage` | From result `trace_id` |

### Trace Context Propagation

**W3C Trace Context Format**:
- Header: `traceparent: 00-{trace_id}-{span_id}-01`
- Extracted and injected at each component boundary

**Custom Format**:
- Headers: `trace_id`, `span_id`
- Payload: `trace_id` (fallback)
- Compatibility: `X-Trace-Id`, `X-Span-Id`

### Span Attributes

All spans include:
- `trace_id`: Current trace ID
- `assignment_id`: Assignment identifier (if available)
- `request_id`: Request identifier (if available)
- `tenant_id`: Tenant identifier
- `subject`: NATS subject
- `status`: Processing status (ok/error)

## Verification Points

### ✅ Assignment Publication
- [x] `trace_id` extracted from `RequestMap`
- [x] OpenTelemetry span started with parent context
- [x] Trace context injected into headers (W3C + custom)
- [x] Headers published with assignment

### ✅ Result Processing
- [x] `trace_id` extracted from headers (priority) or payload (fallback)
- [x] OpenTelemetry span started with parent context
- [x] Span linked to assignment publication span via `trace_id`
- [x] Usage event includes `trace_id`

### ✅ ACK Processing
- [x] `trace_id` extracted from headers (priority) or payload (fallback)
- [x] OpenTelemetry span started with parent context
- [x] Span linked to assignment publication span via `trace_id`

### ✅ Usage Emission
- [x] `trace_id` extracted from result context
- [x] OpenTelemetry span started with trace context
- [x] Usage event payload includes `trace_id`

## End-to-End Trace Flow

```
Router (DecideRequest)
  └─ trace_id: "tr-123"
     │
     ├─ Assignment Publication (span: beamline.router.publish.assignment)
     │  └─ Headers: trace_id, traceparent, span_id
     │     │
     │     └─→ CAF (ExecAssignment)
     │        │
     │        └─→ Result (ExecResult)
     │           └─ Headers: trace_id, traceparent, span_id
     │              │
     │              └─→ Router Result Processing (span: beamline.router.process.result)
     │                 └─ trace_id: "tr-123" (from headers)
     │                    │
     │                    └─→ Usage Emission (span: beamline.router.emit.usage)
     │                       └─ trace_id: "tr-123"
     │
     └─→ ACK (ExecAssignmentAck)
        └─ Headers: trace_id, traceparent, span_id
           │
           └─→ Router ACK Processing (span: beamline.router.process.ack)
              └─ trace_id: "tr-123" (from headers)
```

## Testing

### Manual Verification

1. **Start Router with OpenTelemetry**:
   ```erlang
   application:set_env(beamline_router, opentelemetry_enabled, true),
   application:start(beamline_router).
   ```

2. **Send DecideRequest with trace_id**:
   ```json
   {
     "version": "1",
     "request_id": "req-123",
     "trace_id": "tr-123",
     "tenant_id": "acme",
     "task": {"type": "text.generate", "payload": "test"},
     "push_assignment": true
   }
   ```

3. **Verify Spans in OpenTelemetry Collector**:
   - `beamline.router.publish.assignment` with `trace_id: "tr-123"`
   - `beamline.router.process.result` with `trace_id: "tr-123"` (from headers)
   - `beamline.router.emit.usage` with `trace_id: "tr-123"`

### E2E Test Coverage

- ✅ Headers extraction in consumers
- ✅ Trace context propagation
- ✅ Span creation with parent context
- ✅ Usage event includes trace_id

## Known Limitations

1. **OpenTelemetry SDK**: Currently using `opentelemetry_api` only. Full SDK integration may be needed for production.
2. **Trace Context Format**: Supports both W3C and custom formats, but W3C may not be fully validated.
3. **Span Links**: Spans are linked via `trace_id` but explicit span links may not be created.

## References

- `src/router_caf_adapter.erl`: Assignment publication with trace context
- `src/router_result_consumer.erl`: Result processing with trace context
- `src/router_ack_consumer.erl`: ACK processing with trace context
- `src/router_tracing.erl`: OpenTelemetry integration
- `docs/dev/OPENTELEMETRY_TRACING_IMPLEMENTATION_REPORT.md`: Initial implementation report

