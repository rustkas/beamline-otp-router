# Metrics, Alerts, and Tests Implementation Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

Completed implementation of metrics, alerts, and E2E tests for JetStream forwarding, NAK redelivery, headers, and idempotency.

## Completed Tasks

### 1. ✅ Metrics Implementation

**Tenant Validation Metrics**:
- `router_results_tenant_rejected_total`: Emitted in `router_result_consumer.erl` when tenant validation fails (lines 352, 384)
- `router_acks_tenant_rejected_total`: Emitted in `router_ack_consumer.erl` when tenant validation fails (lines 219, 248)
- `router_tenant_audit_total`: Emitted in `router_tenant_validator.erl` for all validation attempts (line 94)

**Idempotency Metrics**:
- `router_results_duplicate_total`: Emitted in `router_result_consumer.erl` when duplicate result detected (line 333)
- `router_acks_duplicate_total`: Emitted in `router_ack_consumer.erl` when duplicate ACK detected (line 201)

**JetStream Metrics**:
- `router_jetstream_redelivery_total`: Emitted when NAK is called:
  - `router_result_consumer.erl` (lines 363, 394): On tenant validation failure
  - `router_ack_consumer.erl` (lines 229, 257): On tenant validation failure
- `router_jetstream_maxdeliver_exhausted_total`: ✅ **IMPLEMENTED** - Emitted when `delivery_count >= MaxDeliver`:
  - `router_result_consumer.erl` (line 698): ETS-based delivery count tracking, exhaustion detection before NAK
  - `router_ack_consumer.erl` (line 461): ETS-based delivery count tracking, exhaustion detection before NAK

### 2. ✅ Alerts Configuration

**Updated `PROMETHEUS_ALERTS.md`**:
- ✅ `RouterTenantRejectedHigh`: Alert for high tenant rejection rate
- ✅ `RouterIdempotencyDuplicatesHigh`: Alert for high duplicate message rate
- ✅ `RouterJetStreamRedeliveryHigh`: Alert for high JetStream redelivery rate
- ✅ `RouterJetStreamMaxDeliverExhausted`: Alert for MaxDeliver exhaustion (⚠️ Note: metric not yet emitted)

**Alert Thresholds**:
- Tenant rejection: > 5/min for 10m (warning)
- Duplicates: > 10/min for 10m (warning)
- Redelivery: > 20/min for 10m (warning)
- MaxDeliver exhausted: > 0/min for 5m (critical)

### 3. ✅ E2E Tests Implementation

**Test Suite**: `router_jetstream_e2e_SUITE.erl`

**Implemented Tests**:
1. ✅ `test_headers_in_assignment_publication/1` (lines 448-496):
   - Verifies headers (`trace_id`, `tenant_id`, `version`) are published with assignments
   - Mocks `publish_with_ack/3` to capture headers
   - Validates header content matches request context

2. ✅ `test_nak_redelivery_on_validator_error/1` (lines 552-638):
   - Verifies NAK is called when tenant validation fails
   - Tracks NAK calls via mock
   - Verifies `router_jetstream_redelivery_total` metric is emitted
   - Tests controlled redelivery behavior

3. ✅ `test_jetstream_forwarding_with_headers/1` (lines 499-550):
   - Verifies JetStream message forwarding with headers and msg_id
   - Tests consumer accepts `{nats_message, Subject, Payload, Headers, MsgId}` format
   - Validates headers extraction and usage

**Existing Tests** (already implemented):
- `test_durable_subscription_creation/1`
- `test_durable_subscription_reconnect/1`
- `test_jetstream_publish_with_ack/1`
- `test_message_acknowledgment/1`
- `test_message_nak_redelivery/1`
- `test_idempotency_result_processing/1`
- `test_idempotency_usage_emission/1`
- `test_idempotency_ack_processing/1`
- `test_durable_group_isolation/1`
- `test_message_redelivery_on_failure/1`

## Implementation Details

### Headers in Assignment Publication

**Flow**:
1. `router_caf_adapter:publish_assignment/2` extracts `trace_id`, `tenant_id` from request
2. Builds headers map: `#{<<"trace_id">>, <<"tenant_id">>, <<"version">>}`
3. Calls `router_nats:publish_with_ack(Subject, Json, Headers)`
4. `router_nats` formats headers as `NATS/1.0` block and includes in publication

**Test Coverage**:
- Headers are built correctly from request context
- Headers are passed to `publish_with_ack/3`
- Headers are formatted as `NATS/1.0` block

### NAK Redelivery on Validator Errors

**Flow**:
1. Consumer receives message with `msg_id`
2. Validates tenant via `router_tenant_validator:validate_tenant/2`
3. On validation failure:
   - Emits `router_results_tenant_rejected_total` or `router_acks_tenant_rejected_total`
   - Emits `router_jetstream_redelivery_total` with reason and source
   - Calls `router_nats:nak_message(MsgId)` for controlled redelivery
4. JetStream redelivers message (respects `MaxDeliver` and `Backoff`)

**Test Coverage**:
- NAK is called when tenant validation fails
- Redelivery metric is emitted with correct metadata
- Message format supports headers and msg_id

### JetStream Forwarding

**Flow**:
1. `router_nats` receives message from NATS server
2. Parses headers and extracts `msg_id`
3. `forward_to_subscribers/6` matches subject to JetStream consumers
4. Sends `{nats_message, Subject, Payload, Headers, MsgId}` to matching consumers

**Test Coverage**:
- Consumer accepts messages with headers and msg_id
- Headers are extracted and used for correlation
- Message processing works with headers

## Implementation Status

### MaxDeliver Exhaustion Tracking

**Status**: ✅ **IMPLEMENTED**

**Implementation Details**:
- Router uses ETS tables (`router_delivery_count` and `router_ack_delivery_count`) to track delivery count per message
- `track_delivery_count/1` increments count atomically when message is received
- `check_maxdeliver_exhaustion/3` or `/4` checks if `delivery_count >= MaxDeliver` before NAK
- If exhausted, emits `router_jetstream_maxdeliver_exhausted_total` with metadata: `assignment_id`, `request_id`, `msg_id`, `delivery_count`, `max_deliver`, `reason`
- Tracking is cleaned up after successful ACK via `cleanup_delivery_count/1`

**Files**:
- `router_result_consumer.erl`: Lines 653-716 (track_delivery_count, check_maxdeliver_exhaustion, cleanup_delivery_count)
- `router_ack_consumer.erl`: Lines 412-486 (track_delivery_count, check_maxdeliver_exhaustion, cleanup_delivery_count)

## Metrics Summary

### Emitted Metrics

| Metric | Location | When Emitted |
|--------|----------|--------------|
| `router_results_tenant_rejected_total` | `router_result_consumer.erl` | Tenant validation fails for ExecResult |
| `router_acks_tenant_rejected_total` | `router_ack_consumer.erl` | Tenant validation fails for ExecAssignmentAck |
| `router_tenant_audit_total` | `router_tenant_validator.erl` | All tenant validation attempts |
| `router_results_duplicate_total` | `router_result_consumer.erl` | Duplicate ExecResult detected (idempotency) |
| `router_acks_duplicate_total` | `router_ack_consumer.erl` | Duplicate ExecAssignmentAck detected (idempotency) |
| `router_jetstream_redelivery_total` | `router_result_consumer.erl`, `router_ack_consumer.erl` | NAK called for redelivery |

### All Metrics Emitted

All required metrics are now implemented and emitted:
- ✅ `router_jetstream_maxdeliver_exhausted_total`: ETS-based delivery count tracking in `router_result_consumer.erl` and `router_ack_consumer.erl`

## Test Coverage

### E2E Tests

- ✅ Headers in assignment publication
- ✅ NAK redelivery on validator errors
- ✅ JetStream forwarding with headers
- ✅ Durable subscription creation
- ✅ Message acknowledgment
- ✅ Idempotency checks
- ✅ Duplicate detection

### Test Execution

```bash
cd apps/otp/router
rebar3 ct --suite test/router_jetstream_e2e_SUITE
```

## References

- `src/router_result_consumer.erl`: Result processing with NAK and metrics
- `src/router_ack_consumer.erl`: ACK processing with NAK and metrics
- `src/router_tenant_validator.erl`: Tenant validation with audit metrics
- `src/router_idempotency.erl`: Idempotency checks
- `src/router_nats.erl`: JetStream forwarding and headers support
- `src/router_caf_adapter.erl`: Assignment publication with headers
- `test/router_jetstream_e2e_SUITE.erl`: E2E tests
- `docs/PROMETHEUS_ALERTS.md`: Alerting rules

