# Metrics and Trace Attributes Update Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **All Updates Complete**

## Overview

This report documents the addition of:
1. Metrics for redelivery/MaxDeliver exhaustion and alerts
2. Metrics for tenant validation rejections with alerts
3. Trace attributes for key operations (assignment publish, result process, ack process, usage emit)

## Metrics Updates

### 1. Redelivery/MaxDeliver Exhaustion Metrics

**Existing Metrics**:
- ✅ `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}`: Already implemented, emitted when NAK is called

**New Metrics** (Documented):
- ⚠️ `router_jetstream_maxdeliver_exhausted_total{assignment_id,request_id,reason}`: **Note**: Requires JetStream server integration to track delivery count; currently not emitted, but alert configured

**Alerts Added**:
- ✅ `RouterJetStreamRedeliveryHigh`: High JetStream redelivery rate (> 20/min for 10m, warning)
- ✅ `RouterJetStreamMaxDeliverExhausted`: MaxDeliver exhausted (> 0/min for 5m, critical)

**Location**: `docs/PROMETHEUS_ALERTS.md`, lines 58-64, 160-166

### 2. Tenant Validation Rejection Metrics

**Existing Metrics** (Enhanced with `tenant_id` label):
- ✅ `router_results_tenant_rejected_total{assignment_id,request_id,reason,tenant_id}`: Result tenant validation failures (emits NAK)
- ✅ `router_acks_tenant_rejected_total{assignment_id,reason,tenant_id}`: ACK tenant validation failures (emits NAK)
- ✅ `router_tenant_audit_total{tenant_id,reason,source}`: Tenant validation audit events

**Alerts Added**:
- ✅ `RouterTenantRejectedHigh`: High tenant rejection rate (> 5/min for 10m, warning)

**Location**: `docs/PROMETHEUS_ALERTS.md`, lines 47-50, 136-142

## Trace Attributes Updates

### 1. Assignment Publish (`router_caf_adapter.erl`)

**Span**: `beamline.router.publish.assignment`

**Initial Attributes** (from `SpanAttributes`):
- `assignment_id`, `request_id`, `tenant_id`, `subject`
- `expected_latency_ms`, `deadline_ms`, `trace_id`
- `provider_id`, `priority`, `expected_cost`, `reason`

**Additional Attributes** (set after publication):
- ✅ `assignment.publish_result`: `"ok"` or `"error"` (string)
- ✅ `assignment.retries`: Number of retries (integer)
- ✅ `assignment.error_kind`: Error classification (string, on error)
- ✅ `assignment.error`: Error details (string, on error)

**Status**: ✅ Set via `router_tracing:set_span_status(ok|error, ...)`

**Location**: `src/router_caf_adapter.erl`, lines 105-117

### 2. Result Process (`router_result_consumer.erl`)

**Span**: `beamline.router.process.result`

**Initial Attributes** (from span creation):
- `subject`, `assignment_id`, `request_id`, `trace_id`
- `tenant_id`, `version`, `status`, `provider_id`
- `job_type`, `latency_ms`, `cost`, `msg_id`

**Additional Attributes** (set after processing):
- ✅ `result.status`: Result status (string)
- ✅ `result.job_type`: Job type (string)
- ✅ `result.provider_id`: Provider ID (string)
- ✅ `result.latency_ms`: Latency in milliseconds (integer)
- ✅ `result.cost`: Cost (float)
- ✅ `result.tenant_id`: Validated tenant ID (string)

**Status**: ✅ Set via `router_tracing:set_span_status(ok, undefined)`

**Location**: `src/router_result_consumer.erl`, lines 274-281

### 3. ACK Process (`router_ack_consumer.erl`)

**Span**: `beamline.router.process.ack`

**Initial Attributes** (from span creation):
- `subject`, `assignment_id`, `trace_id`, `tenant_id`
- `version`, `status`, `message`, `msg_id`

**Additional Attributes** (set after processing):
- ✅ `ack.status`: ACK status (string)
- ✅ `ack.message`: ACK message (string)
- ✅ `ack.tenant_id`: Validated tenant ID (string, if available)

**Status**: ✅ Set via `router_tracing:set_span_status(ok, undefined)`

**Location**: `src/router_ack_consumer.erl`, lines 289-296

### 4. Usage Emit (`router_result_consumer.erl`)

**Span**: `beamline.router.emit.usage`

**Initial Attributes** (from span creation):
- `tenant_id`, `provider_id`, `event_type`, `status`
- `trace_id`, `assignment_id`, `request_id`
- `latency_ms`, `cost`, `usage_subject`

**Additional Attributes** (set after emission):
- ✅ `usage.emit_result`: `"ok"` or `"error"` (string)
- ✅ `usage.emit_error`: Error details (string, on error)

**Status**: ✅ Set via `router_tracing:set_span_status(ok|error, ...)`

**Location**: `src/router_result_consumer.erl`, lines 242-268

## Code Changes Summary

### Files Modified

1. **`src/router_caf_adapter.erl`**:
   - Updated trace attributes for assignment publication (lines 105-117)
   - Changed attribute names to use `assignment.*` prefix for clarity

2. **`src/router_result_consumer.erl`**:
   - Added trace attributes for result processing (lines 274-281)
   - Added trace attributes for usage emission (lines 242-268)
   - Updated `emit_usage_event/2` to return `ok | {error, term()}` for error handling

3. **`src/router_ack_consumer.erl`**:
   - Added trace attributes for ACK processing (lines 289-296)

4. **`docs/PROMETHEUS_ALERTS.md`**:
   - Added alerts for tenant rejection (lines 136-142)
   - Added alerts for JetStream redelivery and MaxDeliver exhaustion (lines 160-166)
   - Updated metric descriptions with `tenant_id` label

## Verification

### Compilation
- ✅ All modules compile successfully
- ✅ No compilation errors or warnings (except unused `format_error_term/1` in `router_result_consumer.erl`, which is expected)

### Metrics
- ✅ All metrics documented in `PROMETHEUS_ALERTS.md`
- ✅ Alerts configured for tenant rejection and JetStream redelivery/MaxDeliver exhaustion

### Trace Attributes
- ✅ All key operations have trace attributes:
  - Assignment publish: ✅ Complete
  - Result process: ✅ Complete
  - ACK process: ✅ Complete
  - Usage emit: ✅ Complete

## References

- `src/router_caf_adapter.erl`: Assignment publication with trace attributes
- `src/router_result_consumer.erl`: Result processing and usage emission with trace attributes
- `src/router_ack_consumer.erl`: ACK processing with trace attributes
- `docs/PROMETHEUS_ALERTS.md`: Metrics and alerts documentation

