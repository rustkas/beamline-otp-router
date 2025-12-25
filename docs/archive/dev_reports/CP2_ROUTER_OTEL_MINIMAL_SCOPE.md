# CP2 Router OTel Minimal Scope

**Version**: CP2 Minimal Increment  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

Minimal CP2 increment for Router OpenTelemetry spans without dashboards and alerts. Focuses on critical paths (decide + result handling) with CP1 correlation attributes.

## Scope

### Included

- ✅ **OTel spans for decide path**: `beamline.router.process.decide`
- ✅ **OTel spans for result handling path**: `beamline.router.process.result`
- ✅ **CP1 correlation attributes**: `trace_id`, `run_id`, `flow_id`, `step_id`, `tenant_id`, `subject`
- ✅ **Trace propagation**: Headers priority over payload (W3C Trace Context format)
- ✅ **Integration with router_tracing.erl**: Uses existing OpenTelemetry API

### Excluded (Deferred)

- ❌ **Dashboards**: Grafana dashboards (deferred to release infra)
- ❌ **Alerts**: Alertmanager rules (deferred to release infra)
- ❌ **Full Prometheus integration**: Prometheus metrics export (deferred to Wave 1)
- ❌ **OTLP export**: OpenTelemetry Protocol export (deferred to CP2 full)

## Implementation

### Module: `router_observability.erl`

**Functions**:
- `init/0`: Initialize observability module (verify router_tracing availability)
- `create_decide_span/2`: Create span for decide path with CP1 attributes
- `create_result_span/2`: Create span for result handling path with CP1 attributes
- `extract_cp1_attributes/1`: Extract CP1 correlation fields as span attributes
- `get_span_attributes/1`: Helper for tests

**CP1 Correlation Attributes**:
- `tenant_id`: Tenant identifier (from headers or payload)
- `trace_id`: Trace identifier (from headers or payload, priority: headers)
- `run_id`: Run identifier for workflow runs (from headers or payload)
- `flow_id`: Flow identifier for multi-step flows (from headers or payload)
- `step_id`: Step identifier within a flow (from headers or payload)
- `subject`: NATS subject (for decide: `beamline.router.v1.decide`, for result: `caf.exec.result.v1`)

**Additional Attributes** (decide path):
- `request_id`: Request identifier
- `policy_id`: Policy identifier

**Additional Attributes** (result path):
- `assignment_id`: Assignment identifier
- `request_id`: Request identifier
- `status`: Result status (`success`, `error`, `timeout`, `cancelled`)
- `provider_id`: Provider identifier

### Trace Propagation

**Priority**: Headers first, then payload (fallback)

**W3C Trace Context Format**:
- Supports `traceparent` header (W3C Trace Context)
- Supports custom format (`trace_id`, `X-Trace-Id`)
- Parent context extracted from headers for span creation

## Tests

### Test Suite: `router_observability_SUITE.erl`

**Test Group**: `otel_tests` (parallel execution)

**Test Cases**:
1. **`test_otel_decide_span/1`**: Verifies decide span creation with CP1 correlation attributes
2. **`test_otel_result_span/1`**: Verifies result span creation with CP1 correlation attributes
3. **`test_otel_cp1_attributes/1`**: Verifies CP1 correlation fields extraction
4. **`test_otel_trace_propagation/1`**: Verifies trace context propagation (headers priority)

**Test Coverage**:
- ✅ Span creation for critical paths
- ✅ CP1 correlation attributes present in spans
- ✅ Trace propagation from headers
- ✅ Headers priority over payload

## Integration Points

### Existing Modules

- **`router_tracing.erl`**: OpenTelemetry API integration (already exists)
- **`router_decide_consumer.erl`**: Uses `router_tracing:with_span/4` for decide processing
- **`router_result_consumer.erl`**: Uses `router_tracing:with_span/4` for result processing

### Future Integration

- **`router_observability.erl`**: Can be integrated into consumers for consistent span creation
- **OTLP Export**: Can be added in CP2 full (OTLP collector integration)

## Verification

### End-to-End Trace Collection

**Dev Environment Setup**:
1. Start Router with tracing enabled: `tracing_enabled=true`
2. Send decide request with CP1 correlation fields
3. Verify span created: `beamline.router.process.decide`
4. Send result with CP1 correlation fields
5. Verify span created: `beamline.router.process.result`
6. Verify trace_id propagated across spans

**Expected Trace Structure**:
```
beamline.router.process.decide (root span)
  ├─ trace_id: "trace-abc456"
  ├─ tenant_id: "tenant-123"
  ├─ run_id: "run-789"
  ├─ flow_id: "flow-456"
  └─ step_id: "step-123"

beamline.router.process.result (child span, same trace_id)
  ├─ trace_id: "trace-abc456" (propagated)
  ├─ tenant_id: "tenant-123"
  ├─ run_id: "run-789"
  ├─ flow_id: "flow-456"
  ├─ step_id: "step-123"
  ├─ status: "success"
  └─ provider_id: "openai"
```

## Status

**CP2 Minimal Increment**: ✅ **Complete**

- ✅ `router_observability.erl` created with minimal scope
- ✅ CP1 correlation attributes defined and extracted
- ✅ Tests added to `router_observability_SUITE.erl`
- ✅ Status updated in `CP2_CHECKLIST.md`

**Next Steps** (Deferred):
- Full Prometheus integration (Wave 1)
- OTLP export (CP2 full)
- Grafana dashboards (release infra)
- Alertmanager rules (release infra)

## References

- `apps/otp/router/src/router_observability.erl` - Implementation
- `apps/otp/router/test/router_observability_SUITE.erl` - Tests
- `apps/otp/router/src/router_tracing.erl` - OpenTelemetry API integration
- `docs/CP2_CHECKLIST.md` - Checklist with updated status
- `docs/OBSERVABILITY.md` - CP1 observability baseline

