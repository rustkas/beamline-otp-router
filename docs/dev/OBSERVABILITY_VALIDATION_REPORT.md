# Observability Documentation and Alerts Validation Report

**Version**: 1.0  
**Date**: 2025-11-30  
**Status**: ⚠️ **Issues Found**  
**Scope**: Validation of observability documentation and alerts for `router_jetstream`

## Executive Summary

This report validates the observability documentation (`docs/OBSERVABILITY_ROUTER_DASHBOARD.md`), alert rules (`docs/observability/router-alert-rules.yaml`), and related ADR documents against the actual implementation in `router_jetstream` and related consumers.

**Key Findings**:
- ⚠️ **Critical**: Metric name mismatch between code and documentation
- ⚠️ **Critical**: Missing labels in metric implementation
- ✅ **OK**: `router_jetstream_maxdeliver_exhausted_total` correctly implemented
- ⚠️ **Warning**: Some fault injection scenarios may not be fully covered by alerts

## 1. Metrics Inventory and Validation

### 1.1. Metrics from Documentation

#### From `OBSERVABILITY_ROUTER_DASHBOARD.md`

| Metric Name | Type | Labels (Documented) | Status |
|------------|------|---------------------|--------|
| `router_jetstream_redelivery_total` | Counter | `assignment_id`, `request_id`, `reason`, `source` | ❌ **MISMATCH** |
| `router_jetstream_maxdeliver_exhausted_total` | Counter | `assignment_id`, `request_id`, `msg_id`, `reason` | ✅ **OK** |
| `router_jetstream_ack_total` | Counter | None | ✅ **OK** |
| `router_dlq_total` | Counter | None | ✅ **OK** |
| `router_results_total` | Counter | `status`, `job_type`, `provider_id`, `latency_ms`, `cost` | ✅ **OK** |
| `router_nats_contract_violations_total` | Counter | `subject`, `violation_count`, `msg_id` | ✅ **OK** |

#### From `router-alert-rules.yaml`

| Metric Name | Used In Alerts | Status |
|------------|----------------|--------|
| `router_jetstream_redelivery_total` | `RouterJetStreamHighRedeliveryRate`, `RouterJetStreamHighRedeliveryFromSource`, `RouterJetStreamContractViolationsWithRedelivery`, `RouterJetStreamGrowingRedeliveryQueue` | ❌ **MISMATCH** |
| `router_jetstream_maxdeliver_exhausted_total` | `RouterJetStreamMaxDeliverExhausted` | ✅ **OK** |
| `router_results_total` | Multiple alerts | ✅ **OK** |
| `router_nats_contract_violations_total` | `RouterNATSContractViolations`, `RouterJetStreamContractViolationsWithRedelivery` | ✅ **OK** |

### 1.2. Metrics from Code Implementation

#### From `router_jetstream.erl`

```erlang
%% Line 61: ACK metric
router_metrics:inc(router_jetstream_ack_total)  % ✅ Correct name, no labels

%% Line 77: Redelivery metric
router_metrics:inc(router_redelivery_total)  % ❌ WRONG NAME: Should be router_jetstream_redelivery_total
% ❌ MISSING LABELS: Should have assignment_id, request_id, reason, source

%% Line 101: DLQ metric
router_metrics:inc(router_dlq_total)  % ✅ Correct name, no labels
```

**Issue**: The code uses `router_redelivery_total` instead of `router_jetstream_redelivery_total`, and the metric is created without labels.

#### From `router_result_consumer.erl`, `router_ack_consumer.erl`, `router_decide_consumer.erl`

```erlang
%% MaxDeliver exhausted metric (correctly implemented)
emit_counter(router_jetstream_maxdeliver_exhausted_total, #{
    assignment_id => AssignmentId,
    request_id => RequestId,
    msg_id => MsgId,
    delivery_count => DeliveryCount,
    max_deliver => MaxDeliver,
    reason => <<"maxdeliver_exhausted">>
})  % ✅ Correctly implemented with labels
```

### 1.3. Metric Name Mismatch Analysis

**Problem**: Documentation and alerts use `router_jetstream_redelivery_total`, but code uses `router_redelivery_total`.

**Evidence**:
- **Documentation**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` line 63: `router_jetstream_redelivery_total`
- **Alerts**: `docs/observability/router-alert-rules.yaml` lines 69, 103, 117, 152: `router_jetstream_redelivery_total`
- **Code**: `apps/otp/router/src/router_jetstream.erl` line 77: `router_redelivery_total`
- **Prometheus metadata**: `apps/otp/router/src/router_prometheus.erl` line 82: `router_redelivery_total`

**Impact**:
- Alerts will **never fire** because the metric name doesn't exist
- Dashboard queries will return **no data**
- Observability is **broken** for redelivery monitoring

### 1.4. Missing Labels Analysis

**Problem**: `router_redelivery_total` is created without labels, but documentation and alerts expect labels.

**Expected Labels** (from `METRICS_CONTRACT_SPECIFICATION.md`):
- Required: `assignment_id`, `request_id`, `reason`, `source`
- Optional: `msg_id`, `tenant_id`

**Current Implementation**:
- Code: `router_metrics:inc(router_redelivery_total)` - **no labels**
- Telemetry event: `[router, jetstream, nak]` with metadata `#{msg => Msg, reason => Reason, delivery_count => DeliveryCount}` - **labels in telemetry, not in Prometheus metric**

**Impact**:
- Alerts using `by (source)` or `by (reason)` will **not work**
- Dashboard panels showing breakdown by source/reason will show **no data**
- Cannot identify which consumer or reason is causing redeliveries

## 2. Label Name Validation

### 2.1. Labels in Documentation vs Code

| Metric | Label | Documentation | Code | Status |
|--------|-------|---------------|------|--------|
| `router_jetstream_redelivery_total` | `assignment_id` | ✅ Required | ❌ Missing | ❌ **MISMATCH** |
| `router_jetstream_redelivery_total` | `request_id` | ✅ Required | ❌ Missing | ❌ **MISMATCH** |
| `router_jetstream_redelivery_total` | `reason` | ✅ Required | ⚠️ In telemetry only | ⚠️ **PARTIAL** |
| `router_jetstream_redelivery_total` | `source` | ✅ Required | ❌ Missing | ❌ **MISMATCH** |
| `router_jetstream_maxdeliver_exhausted_total` | `assignment_id` | ✅ Required | ✅ Present | ✅ **OK** |
| `router_jetstream_maxdeliver_exhausted_total` | `request_id` | ✅ Required | ✅ Present | ✅ **OK** |
| `router_jetstream_maxdeliver_exhausted_total` | `msg_id` | ✅ Required | ✅ Present | ✅ **OK** |
| `router_jetstream_maxdeliver_exhausted_total` | `reason` | ✅ Required | ✅ Present | ✅ **OK** |

### 2.2. Dead Label Conditions

**Alert**: `RouterJetStreamHighRedeliveryFromSource`
```yaml
expr: sum(rate(router_jetstream_redelivery_total[5m])) by (source) > 0.01
```

**Problem**: This alert will **never fire** because:
1. Metric name is wrong (`router_jetstream_redelivery_total` doesn't exist)
2. Label `source` doesn't exist in the metric

**Status**: ❌ **DEAD ALERT** (will never fire)

## 3. Fault Injection Scenarios Coverage

### 3.1. Fault Injection Scenarios from Documentation

From `JETSTREAM_FAULT_INJECTION_TESTS.md`:

| Scenario | Description | Expected Metrics | Alert Coverage |
|----------|-------------|------------------|----------------|
| **S1**: Intermittent ACK/NAK Errors | ACK/NAK failures cause redeliveries | `router_jetstream_redelivery_total{reason="ack_failure"}` | ⚠️ **PARTIAL** |
| **S2**: Processing Delays → Redelivery Growth | Tenant validation delays cause redeliveries | `router_jetstream_redelivery_total{reason="tenant_validation_failed", source="tenant_validation"}` | ⚠️ **PARTIAL** |
| **S3**: MaxDeliver Exhaustion | Messages exceed MaxDeliver limit | `router_jetstream_maxdeliver_exhausted_total` | ✅ **COVERED** |

### 3.2. Alert Coverage Analysis

#### ✅ Covered Scenarios

**S3: MaxDeliver Exhaustion**
- **Alert**: `RouterJetStreamMaxDeliverExhausted`
- **Metric**: `router_jetstream_maxdeliver_exhausted_total` ✅ Correctly implemented
- **Status**: ✅ **FULLY COVERED**

#### ⚠️ Partially Covered Scenarios

**S1: Intermittent ACK/NAK Errors**
- **Expected Alert**: Should detect high redelivery rate from ACK failures
- **Current Alert**: `RouterJetStreamHighRedeliveryRate` (general, not ACK-specific)
- **Problem**: Cannot distinguish ACK failures from other redelivery reasons (missing `reason` label)
- **Status**: ⚠️ **PARTIALLY COVERED** (detects redeliveries, but not ACK-specific)

**S2: Processing Delays → Redelivery Growth**
- **Expected Alert**: Should detect high redelivery rate from tenant validation
- **Current Alert**: `RouterJetStreamHighRedeliveryFromSource` (uses `source` label)
- **Problem**: `source` label doesn't exist in metric
- **Status**: ⚠️ **NOT COVERED** (alert will never fire)

### 3.3. Missing Alert Coverage

**Scenario**: High redelivery rate from specific sources (tenant validation, parse errors, etc.)
- **Expected**: Alert `RouterJetStreamHighRedeliveryFromSource` with `source` label
- **Reality**: Alert exists but will never fire (metric name and labels wrong)
- **Status**: ❌ **NOT COVERED**

**Scenario**: Redelivery rate correlation with contract violations
- **Expected**: Alert `RouterJetStreamContractViolationsWithRedelivery`
- **Reality**: Alert exists but will never fire (metric name wrong)
- **Status**: ❌ **NOT COVERED**

## 4. ADR Compliance Check

### 4.1. ADR-014: Metrics and Distributed Tracing

**Reference**: `docs/ADR/ADR-014-metrics-tracing.md`

**ADR States**:
- JetStream Metrics: `router_jetstream_ack_total`, `router_redelivery_total`, `router_dlq_total`
- Note: ADR uses `router_redelivery_total` (without `router_jetstream_` prefix)

**Status**: ⚠️ **INCONSISTENT**
- ADR uses `router_redelivery_total` (matches code)
- Documentation uses `router_jetstream_redelivery_total` (doesn't match code)
- Alerts use `router_jetstream_redelivery_total` (doesn't match code)

**Recommendation**: Update ADR to use `router_jetstream_redelivery_total` for consistency, OR update code/documentation/alerts to use `router_redelivery_total`.

### 4.2. ADR-011: JetStream E2E

**Reference**: `docs/ADR/ADR-011-jetstream-e2e.md`

**ADR States**:
- MaxDeliver exhaustion metric: `router_jetstream_maxdeliver_exhausted_total` ✅
- Delivery count tracking for MaxDeliver detection ✅

**Status**: ✅ **COMPLIANT** (MaxDeliver exhaustion correctly implemented)

## 5. Summary of Issues

### 5.1. Critical Issues

| Issue | Location | Impact | Priority |
|-------|----------|--------|----------|
| **Metric name mismatch** | `router_jetstream.erl:77` | Alerts will never fire, dashboards show no data | 🔴 **CRITICAL** |
| **Missing labels in redelivery metric** | `router_jetstream.erl:77` | Cannot filter by source/reason, alerts broken | 🔴 **CRITICAL** |
| **Dead alert conditions** | `router-alert-rules.yaml:117` | Alert `RouterJetStreamHighRedeliveryFromSource` will never fire | 🔴 **CRITICAL** |

### 5.2. Warning Issues

| Issue | Location | Impact | Priority |
|-------|----------|--------|----------|
| **Inconsistent metric naming** | ADR vs Documentation vs Code | Confusion, maintenance burden | 🟡 **WARNING** |
| **Partial fault injection coverage** | Alerts don't distinguish ACK failures | Cannot identify specific failure modes | 🟡 **WARNING** |

### 5.3. Recommendations

#### Immediate Actions (Critical)

1. **Fix metric name in code**:
   ```erlang
   % Change from:
   router_metrics:inc(router_redelivery_total)
   
   % To:
   router_metrics:inc(router_jetstream_redelivery_total)
   ```

2. **Add labels to redelivery metric**:
   ```erlang
   % Change from:
   router_metrics:inc(router_redelivery_total)
   
   % To:
   emit_counter(router_jetstream_redelivery_total, #{
       assignment_id => AssignmentId,
       request_id => RequestId,
       reason => Reason,
       source => Source
   })
   ```

3. **Update Prometheus metadata**:
   ```erlang
   % In router_prometheus.erl, change:
   router_redelivery_total -> router_jetstream_redelivery_total
   ```

#### Documentation Updates

1. **Update ADR-014** to use `router_jetstream_redelivery_total` consistently
2. **Verify all documentation** uses correct metric name
3. **Update examples** in `OBSERVABILITY_ROUTER_DASHBOARD.md` to match implementation

#### Alert Validation

1. **Test all alerts** after metric name fix
2. **Verify label-based alerts** work correctly (e.g., `by (source)`, `by (reason)`)
3. **Add specific alerts** for ACK failure scenarios if needed

## 6. Detailed Metric Status Table

| Metric | Documentation | Alerts | Code | Labels (Doc) | Labels (Code) | Status |
|--------|---------------|--------|------|--------------|---------------|--------|
| `router_jetstream_redelivery_total` | ✅ Present | ✅ Used | ❌ Wrong name | `assignment_id`, `request_id`, `reason`, `source` | ❌ None | ❌ **BROKEN** |
| `router_jetstream_maxdeliver_exhausted_total` | ✅ Present | ✅ Used | ✅ Correct | `assignment_id`, `request_id`, `msg_id`, `reason` | ✅ All present | ✅ **OK** |
| `router_jetstream_ack_total` | ✅ Present | ❌ Not used | ✅ Correct | None | None | ✅ **OK** |
| `router_dlq_total` | ✅ Present | ❌ Not used | ✅ Correct | None | None | ✅ **OK** |
| `router_results_total` | ✅ Present | ✅ Used | ✅ Correct | `status`, `job_type`, `provider_id`, `latency_ms`, `cost` | ✅ Present | ✅ **OK** |
| `router_nats_contract_violations_total` | ✅ Present | ✅ Used | ✅ Correct | `subject`, `violation_count`, `msg_id` | ✅ Present | ✅ **OK** |

## 7. Fault Injection Coverage Matrix

| Fault Scenario | Metric Expected | Metric Implemented | Alert Exists | Alert Works | Status |
|----------------|-----------------|-------------------|--------------|-------------|--------|
| **S1**: ACK/NAK Errors | `router_jetstream_redelivery_total{reason="ack_failure"}` | ❌ Wrong name, no labels | ✅ Yes | ❌ No (metric wrong) | ❌ **NOT COVERED** |
| **S2**: Processing Delays | `router_jetstream_redelivery_total{source="tenant_validation"}` | ❌ Wrong name, no labels | ✅ Yes | ❌ No (metric wrong) | ❌ **NOT COVERED** |
| **S3**: MaxDeliver Exhaustion | `router_jetstream_maxdeliver_exhausted_total` | ✅ Correct | ✅ Yes | ✅ Yes | ✅ **COVERED** |
| **S2**: High Redelivery Rate | `router_jetstream_redelivery_total` (general) | ❌ Wrong name | ✅ Yes | ❌ No (metric wrong) | ❌ **NOT COVERED** |

## 8. Conclusion

**Overall Status**: ⚠️ **CRITICAL ISSUES FOUND**

The observability system for `router_jetstream` has **critical issues** that prevent alerts from firing and dashboards from showing data:

1. **Metric name mismatch**: Code uses `router_redelivery_total`, but documentation/alerts use `router_jetstream_redelivery_total`
2. **Missing labels**: Redelivery metric is created without required labels (`assignment_id`, `request_id`, `reason`, `source`)
3. **Broken alerts**: Multiple alerts will never fire due to wrong metric names and missing labels

**Positive Findings**:
- `router_jetstream_maxdeliver_exhausted_total` is correctly implemented
- MaxDeliver exhaustion scenario is fully covered by alerts
- Other metrics (`router_results_total`, `router_nats_contract_violations_total`) are correctly implemented

**Next Steps**:
1. Fix metric name and labels in `router_jetstream.erl`
2. Update Prometheus metadata in `router_prometheus.erl`
3. Test all alerts after fixes
4. Update ADR-014 for consistency
5. Re-validate after fixes

## References

- **Observability Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
- **Alert Rules**: `docs/observability/router-alert-rules.yaml`
- **Metrics Contract**: `apps/otp/router/docs/dev/METRICS_CONTRACT_SPECIFICATION.md`
- **Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **ADR-014**: `docs/ADR/ADR-014-metrics-tracing.md`
- **ADR-011**: `docs/ADR/ADR-011-jetstream-e2e.md`
- **Code**: `apps/otp/router/src/router_jetstream.erl`

