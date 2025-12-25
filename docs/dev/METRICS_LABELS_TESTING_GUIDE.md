# Metrics Labels Testing Guide

**Date**: 2025-11-30  
**Status**: Testing Guide Complete  
**Purpose**: Guide for testing metrics labels implementation and their relationship to JetStream fault injection scenarios

## Overview

This guide provides instructions for testing the metrics labels implementation, including unit tests, integration tests, performance tests, and validation scripts. It also describes how to use these tests to verify JetStream fault injection scenarios (S1, S2, S3) and their observability coverage.

**Related Documents**:
- **Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md` - Complete S1-S3 scenario descriptions
- **Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md` - Scenario → tests → metrics → alerts → dashboards mapping
- **OBS Coverage Analysis**: `apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md` - Detailed observability coverage analysis

## Test Suites

### 1. Unit Tests (`router_metrics_labels_unit_SUITE.erl`)

**Purpose**: Test individual helper functions in isolation.

**Coverage**:
- Label extraction functions (`extract_assignment_id`, `extract_tenant_id`, `extract_request_id`)
- Error normalization functions (`error_to_reason`, `extract_stream_from_subject`)
- Metric emission with labels

**Running**:
```bash
rebar3 ct --suite apps/otp/router/test/router_metrics_labels_unit_SUITE
```

**Expected Results**:
- All 20 tests should pass
- No errors or warnings

### 2. Integration Tests (`router_metrics_labels_integration_SUITE.erl`)

**Purpose**: Test metrics labels in real scenarios (MaxDeliver exhaustion, NATS failures).

**Coverage**:
- DLQ metric emission during MaxDeliver exhaustion
- NATS failure metrics emission during connection/publish/ACK failures
- Label cardinality verification

**Running**:
```bash
rebar3 ct --suite apps/otp/router/test/router_metrics_labels_integration_SUITE
```

**Expected Results**:
- All 5 tests should pass
- Label cardinality should be reasonable (< 1000 for test scenarios)

### 3. Performance Tests (`router_metrics_labels_performance_SUITE.erl`)

**Purpose**: Verify performance impact of labels is acceptable.

**Coverage**:
- Label extraction performance (< 10μs per extraction)
- Metric emission performance (< 50μs per emission)
- ETS table size growth verification

**Running**:
```bash
rebar3 ct --suite apps/otp/router/test/router_metrics_labels_performance_SUITE
```

**Expected Results**:
- All 3 tests should pass
- Performance overhead should be minimal

## Validation Script

### `validate_metrics_labels.sh`

**Purpose**: Quick validation of labels implementation without running full test suite.

**Checks**:
1. Helper functions are exported
2. Metric emission uses labels
3. Dashboard queries are valid PromQL
4. Test files exist
5. Documentation references are present

**Running**:
```bash
bash apps/otp/router/scripts/validate_metrics_labels.sh
```

**Expected Output**:
```
=== Metrics Labels Validation ===

1. Checking helper function exports...
✓ router_jetstream:extract_assignment_id/1 exported
✓ router_jetstream:extract_tenant_id/1 exported
✓ router_nats:error_to_reason/1 exported
✓ router_nats:extract_stream_from_subject/1 exported

2. Checking metric emission with labels...
✓ router_dlq_total uses emit_metric with labels
✓ router_nats_publish_failures_total uses emit_metric with labels
✓ router_nats_ack_failures_total uses emit_metric with labels

3. Validating dashboard queries...
Found X PromQL queries
✓ Valid PromQL: DLQ by reason
✓ Valid PromQL: DLQ by assignment
✓ Valid PromQL: Publish failures by reason

4. Checking test files...
✓ Unit tests exist
✓ Integration tests exist
✓ Performance tests exist

5. Checking documentation references...
✓ Dashboard documentation updated with labels status
✓ Implementation report exists

=== Validation Summary ===
✓ All checks passed
```

## Manual Testing

### 1. Test Label Extraction

**Test `extract_assignment_id/1`**:
```erlang
1> router_jetstream:extract_assignment_id(~"beamline.router.v1.decide").
~"decide"
```

**Test `extract_tenant_id/1`**:
```erlang
2> Msg = #{headers => #{~"tenant_id" => ~"tenant-123"}}.
3> router_jetstream:extract_tenant_id(Msg).
~"tenant-123"
```

**Test `error_to_reason/1`**:
```erlang
4> router_nats:error_to_reason(timeout).
~"timeout"
```

### 2. Test Metric Emission

**Test DLQ metric with labels**:
```erlang
5> router_metrics:emit_metric(router_dlq_total, #{count => 1}, #{
    assignment_id => ~"decide",
    reason => ~"maxdeliver_exhausted",
    tenant_id => ~"tenant-123"
}).
ok

6> ets:tab2list(router_metrics).
[{{router_dlq_total, [{assignment_id,~"decide"},
                      {reason,~"maxdeliver_exhausted"},
                      {tenant_id,~"tenant-123"}]}, 1}]
```

### 3. Test Dashboard Queries

**Test PromQL query** (requires Prometheus):
```promql
sum by (reason) (rate(router_dlq_total[5m]))
```

**Expected**: Time series with labels `reason=<<maxdeliver_exhausted>>`

## Troubleshooting

### Tests Fail: Helper Functions Not Found

**Error**: `undefined function router_jetstream:extract_assignment_id/1`

**Solution**: Ensure helper functions are exported in module:
```erlang
-export([extract_assignment_id/1, extract_tenant_id/1, extract_request_id/1]).
```

### Tests Fail: Metric Not Found in ETS

**Error**: `ets:lookup(router_metrics, Key)` returns empty list

**Solution**: 
1. Ensure `router_metrics:ensure()` is called before emission
2. Verify metric name matches exactly
3. Check that labels key matches (use `normalize_labels/1`)

### Performance Tests Fail: Too Slow

**Error**: Average time exceeds threshold

**Solution**:
1. Check for unnecessary operations in label extraction
2. Verify ETS table is not too large
3. Consider optimizing helper functions

### Cardinality Tests Fail: Too High

**Error**: Label cardinality > 1000

**Solution**:
1. Review use of high-cardinality labels (`msg_id`, `request_id`)
2. Consider excluding high-cardinality labels in production
3. Monitor cardinality in staging environment

## Testing JetStream Fault Injection Scenarios

### Scenario S1: Intermittent ACK/NAK Errors

**Scenario ID**: `S1`  
**Fault Injection Doc**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md#scenario-1-intermittent-acknak-errors`  
**Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#s1-intermittent-acknak-errors`

**Tests to Run**:
1. **E2E Test**: `test_intermittent_ack_failure_recovery/1`
   ```bash
   rebar3 ct --suite router_jetstream_e2e_SUITE --case test_intermittent_ack_failure_recovery
   ```

2. **Unit Test**: `test_delivery_count_tracking_under_ack_failures/1`
   ```bash
   rebar3 ct --suite router_delivery_count_tracking_SUITE --case test_delivery_count_tracking_under_ack_failures
   ```

3. **Integration Test**: `test_nats_ack_failure_labels/1`
   ```bash
   rebar3 ct --suite router_metrics_labels_integration_SUITE --case test_nats_ack_failure_labels
   ```

**Expected Metrics**:
- `router_nats_ack_failures_total` (with labels: `reason`, `subject`, `stream`, `consumer`)
- `router_nats_nak_failures_total` (with labels: `reason`, `subject`, `stream`)

**Expected Alerts**:
- `RouterNATSAckFailuresHigh` (⚠️ Partial - exists but not explicitly linked to S1)

**Dashboard Panels to Check**:
- "NATS ACK Failures by Reason" (Section 4.4, Panel 8)
- "NATS NAK Failures by Reason" (Section 4.4, Panel 9)
- "Correlation: NATS Failures vs Redeliveries" (Section 4.4, Panel 10)

### Scenario S2: Processing Delays → Redelivery Growth

**Scenario ID**: `S2`  
**Fault Injection Doc**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md#scenario-2-processing-delays-causing-redelivery-growth`  
**Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#s2-processing-delays--redelivery-growth`

**Tests to Run**:
1. **E2E Test**: `test_processing_delays_redelivery_with_delivery_count/1`
   ```bash
   rebar3 ct --suite router_jetstream_e2e_SUITE --case test_processing_delays_redelivery_with_delivery_count
   ```

2. **Fault Injection Test**: `test_redelivery_metric_labels/1`
   ```bash
   rebar3 ct --suite router_jetstream_fault_injection_SUITE --case test_redelivery_metric_labels
   ```

3. **Fault Injection Test**: `test_redelivery_tenant_validation_failed/1`
   ```bash
   rebar3 ct --suite router_jetstream_fault_injection_SUITE --case test_redelivery_tenant_validation_failed
   ```

**Expected Metrics**:
- `router_jetstream_redelivery_total` (with labels: `assignment_id`, `request_id`, `reason`, `source`, `msg_id`, `tenant_id`)
- `router_nats_pending_operations_count` (gauge, for queue depth)

**Expected Alerts**:
- `RouterJetStreamHighRedeliveryRate` (Scenario ID: JS-001)
- `RouterJetStreamHighRedeliveryFromSource` (Scenario ID: JS-005)
- `RouterJetStreamGrowingRedeliveryQueue` (Scenario ID: JS-004)

**Dashboard Panels to Check**:
- "Redelivery Rate by Assignment" (Section 4.2, Panel 1)
- "Redelivery Rate by Tenant" (Section 4.2, Panel 2)
- "Redeliveries by Reason" (Section 4.2, Panel 5)
- "Pending Operations Queue" (Section 4.4, Panel 9)

### Scenario S3: MaxDeliver Exhaustion

**Scenario ID**: `S3`  
**Fault Injection Doc**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md#scenario-3-maxdeliver-exhaustion-for-partial-messages`  
**Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md#s3-maxdeliver-exhaustion-partial-messages`

**Tests to Run**:
1. **E2E Test**: `test_maxdeliver_exhaustion_partial_messages_e2e/1`
   ```bash
   rebar3 ct --suite router_jetstream_e2e_SUITE --case test_maxdeliver_exhaustion_partial_messages_e2e
   ```

2. **Fault Injection Test**: `test_maxdeliver_exhausted_metric_labels/1`
   ```bash
   rebar3 ct --suite router_jetstream_fault_injection_SUITE --case test_maxdeliver_exhausted_metric_labels
   ```

3. **Integration Test**: `test_dlq_metric_emission_with_labels/1`
   ```bash
   rebar3 ct --suite router_metrics_labels_integration_SUITE --case test_dlq_metric_emission_with_labels
   ```

**Expected Metrics**:
- `router_jetstream_maxdeliver_exhausted_total` (with labels: `assignment_id`, `request_id`, `msg_id`, `delivery_count`, `max_deliver`, `reason`)
- `router_dlq_total` (with labels: `assignment_id`, `reason`, `tenant_id`, `source`, `msg_id`, `request_id`)

**Expected Alerts**:
- `RouterJetStreamMaxDeliverExhausted` (Scenario ID: JS-002)
- `RouterDLQHighRate` (Scenario ID: JS-003, warning)
- `RouterDLQHighRateCritical` (Scenario ID: JS-003, critical)

**Dashboard Panels to Check**:
- "MaxDeliver Exhausted Rate (Overall)" (Section 4.3, Panel 1)
- "MaxDeliver Exhausted by Assignment" (Section 4.3, Panel 2)
- "DLQ Inflow Rate (Total)" (Section 4.3, Panel 5)
- "DLQ by Reason" (Section 4.3, Panel 6)

## References

- **Unit Tests**: `apps/otp/router/test/router_metrics_labels_unit_SUITE.erl`
- **Integration Tests**: `apps/otp/router/test/router_metrics_labels_integration_SUITE.erl`
- **Performance Tests**: `apps/otp/router/test/router_metrics_labels_performance_SUITE.erl`
- **Validation Script**: `apps/otp/router/scripts/validate_metrics_labels.sh`
- **Implementation Report**: `apps/otp/router/docs/dev/LABELS_IMPLEMENTATION_COMPLETE.md`
- **Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **Coverage Matrix**: `apps/otp/router/docs/dev/JETSTREAM_OBS_COVERAGE_MATRIX.md`
- **OBS Coverage Analysis**: `apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md`

---

**Prepared by**: WORKER wrk-9 (Documentation & Developer Experience)  
**Status**: Testing Guide Complete  
**Date**: 2025-11-30

