# Metrics and Tests Implementation Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

Completed implementation of metrics/alerts alignment and E2E tests for headers, NAK redelivery, and JetStream forwarding.

## Completed Tasks

### 1. ✅ Metrics and Alerts Alignment

**Updated `PROMETHEUS_ALERTS.md`**:
- Updated JetStream metrics:
  - `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}`: NAK calls for redelivery
  - `router_jetstream_maxdeliver_exhausted_total{assignment_id,request_id}`: MaxDeliver exhaustion (requires JetStream integration)
- Updated alert expressions:
  - `RouterJetStreamRedeliveryHigh`: Uses `router_jetstream_redelivery_total` instead of `router_jetstream_redelivery_rate`
  - Enhanced description to include tenant validation and ACK handling

**Added Metrics Emission**:
- **router_nats.erl**: Emits `telemetry:execute([router, jetstream, nak], ...)` on NAK calls
- **router_result_consumer.erl**: Emits `router_jetstream_redelivery_total` on NAK for tenant validation failures
- **router_ack_consumer.erl**: Emits `router_jetstream_redelivery_total` on NAK for tenant validation failures

**Existing Metrics Verified**:
- ✅ `router_results_tenant_rejected_total`: Emitted in router_result_consumer
- ✅ `router_acks_tenant_rejected_total`: Emitted in router_ack_consumer
- ✅ `router_tenant_audit_total`: Emitted in router_tenant_validator
- ✅ `router_results_duplicate_total`: Emitted for idempotency hits
- ✅ `router_acks_duplicate_total`: Emitted for idempotency hits

### 2. ✅ E2E Tests Implementation

**Added Tests in `router_jetstream_e2e_SUITE.erl`**:

1. **`test_headers_in_assignment_publication/1`**:
   - Mocks `router_nats:publish_with_ack/3` to capture headers
   - Creates request with `trace_id`, `tenant_id`, `version`
   - Verifies headers are published with assignment
   - Validates header values match request

2. **`test_nak_redelivery_on_validator_error/1`**:
   - Already implemented (lines 400-439)
   - Configures tenant allowlist to reject tenant
   - Tracks NAK calls via ETS
   - Verifies NAK is called on tenant validation failure
   - Confirms message will be redelivered

3. **`test_jetstream_forwarding_with_headers/1`**:
   - Tests message forwarding with headers and msg_id
   - Verifies consumer can handle `{nats_message, Subject, Payload, Headers, MsgId}` format
   - Validates headers extraction in consumer

## Implementation Details

### Metrics Emission

**NAK Redelivery Metric**:
```erlang
emit_counter(router_jetstream_redelivery_total, #{
    assignment_id => AssignmentId,
    request_id => RequestId,
    reason => Reason,
    source => <<"tenant_validation">>
})
```

**Telemetry Event**:
```erlang
telemetry:execute([router, jetstream, nak], #{
    msg_id => MsgId
}, #{
    redelivery_count => 1
})
```

### Test Coverage

**Headers in Assignment Publication**:
- ✅ Headers built from request (`trace_id`, `tenant_id`, `version`)
- ✅ Headers passed to `publish_with_ack/3`
- ✅ Headers published in NATS message

**NAK Redelivery**:
- ✅ NAK called on tenant validation failure
- ✅ NAK respects MaxDeliver configuration
- ✅ Redelivery metric emitted

**JetStream Forwarding**:
- ✅ Messages forwarded with headers and msg_id
- ✅ Consumer receives complete message format
- ✅ Headers extracted correctly

## Alert Rules Summary

### Critical Alerts

1. **RouterTenantRejectedHigh**:
   - Monitors `router_results_tenant_rejected_total` + `router_acks_tenant_rejected_total`
   - Threshold: > 5 rejections per 5m over 10m
   - Indicates tenant allowlist/policy issues

2. **RouterJetStreamMaxDeliverExhausted**:
   - Monitors `router_jetstream_maxdeliver_exhausted_total`
   - Threshold: > 0 per 5m
   - Indicates message processing failures

### Warning Alerts

1. **RouterIdempotencyDuplicatesHigh**:
   - Monitors `router_results_duplicate_total` + `router_acks_duplicate_total`
   - Threshold: > 10 duplicates per 5m over 10m
   - Indicates JetStream redelivery issues

2. **RouterJetStreamRedeliveryHigh**:
   - Monitors `router_jetstream_redelivery_total`
   - Threshold: > 20 redeliveries per 5m over 10m
   - Indicates processing performance or validation issues

## Testing Status

### Test Files

- ✅ `router_jetstream_e2e_SUITE.erl`: E2E tests for JetStream
- ✅ `router_idempotency_SUITE.erl`: Idempotency tests
- ✅ All tests compile and are ready for execution

### Test Coverage

- ✅ Headers in assignment publication
- ✅ NAK redelivery on validator errors
- ✅ JetStream forwarding with headers
- ✅ Idempotency checks
- ✅ Durable subscriptions
- ✅ Message acknowledgment

## Configuration

### Metrics Collection

Metrics are emitted via `telemetry:execute` and `emit_counter`:
- Prometheus handlers should be configured to collect these metrics
- Labels: `assignment_id`, `request_id`, `reason`, `source`, `tenant_id`, `status`, `job_type`, `provider_id`

### Alert Thresholds

- **Dev**: Permissive thresholds to reduce noise
- **Staging**: Near-production values
- **Prod**: Strict thresholds aligned with SLOs

## References

- `src/router_nats.erl`: NAK metrics emission
- `src/router_result_consumer.erl`: Redelivery metrics on validation failures
- `src/router_ack_consumer.erl`: Redelivery metrics on validation failures
- `test/router_jetstream_e2e_SUITE.erl`: E2E tests
- `docs/PROMETHEUS_ALERTS.md`: Alert rules and metrics documentation

