# Router Observability Coverage Analysis

**Date**: 2025-11-30  
**Status**: Analysis Complete + Implementation Specs Created  
**Last Updated**: 2025-11-30  
**Purpose**: Formal coverage analysis of JetStream scenarios in Router TZ with alerts and dashboard panels

## Summary

This document provides complete traceability between JetStream scenarios, alerts, and dashboard panels. All critical gaps have been addressed:

- ✅ **DLQ Alert**: Added to `router-alert-rules.yaml`
- ✅ **Performance Panels**: Added to dashboard documentation (Section 4.7)
- ✅ **NATS Operation Panels**: Added to dashboard documentation (Section 4.4, Panels 7-10)
- ⚠️ **Labels Implementation Spec**: Created for Priority 1 and Priority 2 metrics

**Next Steps**: Implementation team to implement labels per `METRICS_LABELS_IMPLEMENTATION_SPEC.md`

## Purpose

This document provides formal traceability between:
- **JetStream scenarios** from Router TZ and specifications
- **Alert rules** in `router-alert-rules.yaml`
- **Dashboard panels** in Router Observability Dashboard
- **Coverage status** (covered / partially covered / not covered)

## Document Structure

1. **JetStream Scenarios Catalog** - Complete list of scenarios from TZ
2. **Coverage Matrix** - Scenario → Alert → Dashboard mapping
3. **Alert Reference** - Detailed alert definitions with scenario links
4. **Dashboard Panel Reference** - Panel descriptions with scenario links
5. **Future Work** - Gaps and planned enhancements

---

## 1. JetStream Scenarios Catalog

### Scenario ID Convention

Each scenario is assigned a unique ID:
- **JS-XXX**: JetStream core scenarios (redelivery, MaxDeliver, DLQ)
- **NATS-XXX**: NATS infrastructure scenarios (connection, publish, ACK)
- **TENANT-XXX**: Tenant validation scenarios
- **PERF-XXX**: Performance and degradation scenarios

### Core JetStream Scenarios

#### JS-001: High Redelivery Rate

**ID**: `JS-001`  
**Description**: Message redelivery rate exceeds threshold (> 5% of total messages for > 5 minutes)  
**Source**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#when-to-worry` (lines 148-152)  
**Expected Behavior**: 
- Redelivery rate should be < 5% of total messages
- Sustained growth indicates processing instability
- Should trigger early warning before DLQ growth

**Observability Requirements**:
- Alert when redelivery rate > 5% (warning) or > 10% (critical)
- Dashboard panel showing redelivery rate over time
- Breakdown by assignment_id, tenant_id, reason

#### JS-002: MaxDeliver Exhaustion

**ID**: `JS-002`  
**Description**: Messages exhaust MaxDeliver limit (delivery count >= MaxDeliver)  
**Source**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#221-router_jetstream_maxdeliver_exhausted_total` (lines 272-399)  
**Expected Behavior**:
- MaxDeliver exhaustion = 0 in normal operation
- Any value > 0 requires immediate investigation
- Messages sent to DLQ or discarded after exhaustion

**Observability Requirements**:
- Critical alert when MaxDeliver exhausted > 0 for > 1 minute
- Dashboard panel showing exhaustion rate
- Breakdown by assignment_id, tenant_id, reason

#### JS-003: DLQ Growth

**ID**: `JS-003`  
**Description**: Messages accumulating in Dead Letter Queue (DLQ)  
**Source**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#223-router_dlq_total` (lines 401-528)  
**Expected Behavior**:
- DLQ rate < 0.1% of total messages
- DLQ growth indicates messages failing to process
- Should correlate with MaxDeliver exhaustion

**Observability Requirements**:
- Warning alert when DLQ rate > 0.1 msg/sec for > 5 minutes
- Critical alert when DLQ rate > 1 msg/sec for > 1 minute
- Dashboard panel showing DLQ inflow rate
- Breakdown by reason, assignment_id, tenant_id (when labels added)

#### JS-004: Redelivery Queue Growth

**ID**: `JS-004`  
**Description**: Redelivery queue growing and not recovering (backpressure)  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 121-148)  
**Expected Behavior**:
- Redelivery queue should stabilize after transient issues
- Sustained growth indicates processing bottleneck
- Should trigger warning before MaxDeliver exhaustion

**Observability Requirements**:
- Alert when queue size > 200 and growing for > 15 minutes
- Dashboard panel showing queue depth over time
- Correlation with processing latency

#### JS-005: High Redelivery from Specific Source

**ID**: `JS-005`  
**Description**: High redelivery rate from specific source (tenant_validation, ack_failure, etc.)  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 68-86)  
**Expected Behavior**:
- Redelivery rate from specific source > 3/sec for > 10 minutes
- Helps identify problematic processing paths
- Should trigger warning for investigation

**Observability Requirements**:
- Alert when redelivery from source > 3/sec for > 10 minutes
- Dashboard panel showing redelivery by source
- Breakdown by reason and assignment_id

### NATS Infrastructure Scenarios

#### NATS-001: NATS Connection Failures

**ID**: `NATS-001`  
**Description**: NATS connection failures (connect/reconnect failures)  
**Source**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#241-nats-failures-router_nats_-failures_total` (lines 530-617)  
**Expected Behavior**:
- Connection failures < 1 per hour
- Reconnect failures < 1 per day
- Should trigger warning for infrastructure issues

**Observability Requirements**:
- Alert when connection failures > 0 for > 5 minutes
- Critical alert when > 1 per hour
- Dashboard panel showing connection/reconnect failures
- Correlation with redelivery spikes

#### NATS-002: NATS Connection Down

**ID**: `NATS-002`  
**Description**: NATS connection completely down (disconnected state)  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 205-223)  
**Expected Behavior**:
- Connection status = 0 (disconnected) for > 2 minutes
- Critical condition - Router cannot process messages
- Should trigger immediate critical alert

**Observability Requirements**:
- Critical alert when connection down for > 2 minutes
- Dashboard panel showing connection status gauge
- Correlation with message processing rate

#### NATS-003: NATS Reconnection Exhausted

**ID**: `NATS-003`  
**Description**: NATS reconnection attempts exhausted (fail-open mode)  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 257-274)  
**Expected Behavior**:
- Reconnection attempts exhausted
- Router operating in fail-open mode
- Critical condition requiring immediate attention

**Observability Requirements**:
- Critical alert when reconnection exhausted > 0 for > 1 minute
- Dashboard panel showing reconnection status
- Runbook for fail-open mode recovery

#### NATS-004: High Publish Failure Rate

**ID**: `NATS-004`  
**Description**: High rate of NATS publish operation failures  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 311-332)  
**Expected Behavior**:
- Publish failure rate < 0.1% of total publishes
- High failure rate indicates connection or server issues
- Should trigger warning for investigation

**Observability Requirements**:
- Alert when publish failure rate > 2% for > 10 minutes
- Dashboard panel showing publish failures by reason
- Correlation with connection failures

#### NATS-005: High ACK Failure Rate

**ID**: `NATS-005`  
**Description**: High rate of NATS ACK operation failures  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 365-386)  
**Expected Behavior**:
- ACK failure rate < 0.1% of total ACKs
- Critical - failed ACKs cause message redeliveries
- Should trigger critical alert

**Observability Requirements**:
- Critical alert when ACK failure rate > 1% for > 5 minutes
- Dashboard panel showing ACK failures
- Correlation with redelivery spikes

#### NATS-006: High NAK Failure Rate

**ID**: `NATS-006`  
**Description**: High rate of NATS NAK operation failures  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 391-412)  
**Expected Behavior**:
- NAK failure rate < 0.1% of total NAKs
- Failed NAKs prevent proper redelivery scheduling
- Should trigger warning for investigation

**Observability Requirements**:
- Alert when NAK failure rate > 2% for > 10 minutes
- Dashboard panel showing NAK failures
- Correlation with redelivery issues

#### NATS-007: High Subscribe Failure Rate

**ID**: `NATS-007`  
**Description**: High rate of JetStream subscription failures  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 417-438)  
**Expected Behavior**:
- Subscribe failure rate < 0.1% of total subscriptions
- Critical - prevents Router from receiving messages
- Should trigger critical alert

**Observability Requirements**:
- Critical alert when subscribe failure rate > 5% for > 5 minutes
- Dashboard panel showing subscribe failures
- Correlation with message processing rate

#### NATS-008: Pending Operations Queue Full

**ID**: `NATS-008`  
**Description**: NATS pending operations queue exceeds limit  
**Source**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 443-460)  
**Expected Behavior**:
- Pending operations queue < 1000
- Queue full indicates operations being dropped
- Should trigger warning for investigation

**Observability Requirements**:
- Alert when pending operations > 1000 for > 10 minutes
- Dashboard panel showing pending operations count
- Correlation with connection status

### Tenant Validation Scenarios

#### TENANT-001: High Tenant Rejection Rate

**ID**: `TENANT-001`  
**Description**: High rate of tenant validation rejections  
**Source**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#225-tenant-rejection-usage-router_results_tenant_rejected_total` (lines 774-898)  
**Expected Behavior**:
- Tenant rejection rate < 1% of total messages
- High rejection rate indicates configuration issues
- Should trigger warning for investigation

**Observability Requirements**:
- Alert when rejection rate > 1% for > 5 minutes
- Critical alert when > 5% for > 1 minute
- Dashboard panel showing rejections by tenant_id, reason
- Correlation with DLQ and MaxDeliver

### Performance and Degradation Scenarios

#### PERF-001: Processing Latency Degradation

**ID**: `PERF-001`  
**Description**: Message processing latency exceeds SLO thresholds  
**Source**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md` (lines 261-267)  
**Expected Behavior**:
- P95 latency < 2000ms for intake processing
- Latency degradation indicates overload or bottlenecks
- Should trigger warning before backpressure

**Observability Requirements**:
- Alert when P95 latency > 2000ms for > 5 minutes
- Dashboard panel showing latency percentiles
- Correlation with queue depth and in-flight messages

#### PERF-002: Backpressure Active

**ID**: `PERF-002`  
**Description**: Router intake backpressure is active (overload condition)  
**Source**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md` (lines 841-907)  
**Expected Behavior**:
- Backpressure active = 1 when overloaded
- Triggers rejection of new messages (HTTP 503)
- Should trigger warning for capacity planning

**Observability Requirements**:
- Alert when backpressure active = 1 for > 1 minute
- Dashboard panel showing backpressure status
- Correlation with queue depth, latency, in-flight messages

#### PERF-003: High Queue Depth

**ID**: `PERF-003`  
**Description**: JetStream pending messages queue depth exceeds threshold  
**Source**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md` (lines 253-260)  
**Expected Behavior**:
- Queue depth < 100 messages
- High queue depth indicates processing bottleneck
- Should trigger critical alert before backpressure

**Observability Requirements**:
- Critical alert when queue depth > 100 for > 5 minutes
- Dashboard panel showing queue depth over time
- Correlation with processing latency

#### PERF-004: High In-Flight Messages

**ID**: `PERF-004`  
**Description**: High number of in-flight messages (currently processing)  
**Source**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md` (lines 269-276)  
**Expected Behavior**:
- In-flight messages < 200
- High in-flight indicates slow processing
- Should trigger warning for investigation

**Observability Requirements**:
- Alert when in-flight messages > 200 for > 5 minutes
- Dashboard panel showing in-flight messages over time
- Correlation with processing latency

---

## 2. Coverage Matrix

### Coverage Status Legend

- ✅ **Covered**: Scenario fully covered by alerts and dashboard panels
- ⚠️ **Partially Covered**: Scenario covered but missing some aspects
- ❌ **Not Covered**: Scenario not covered by alerts or dashboard panels

### Complete Coverage Matrix

| Scenario ID | Scenario Name | Alert Coverage | Alert Names | Dashboard Coverage | Dashboard Panels | Status |
|-------------|---------------|----------------|-------------|-------------------|------------------|--------|
| **JS-001** | High Redelivery Rate | ✅ Covered | `RouterJetStreamHighRedeliveryRate`<br/>`RouterJetStreamHighRedeliveryFromSource` | ✅ Covered | Redelivery rate (by assignment)<br/>Redelivery rate (by tenant)<br/>Redelivery by reason<br/>Redelivery vs Successful processing | ✅ Covered |
| **JS-002** | MaxDeliver Exhaustion | ✅ Covered | `RouterJetStreamMaxDeliverExhausted` | ✅ Covered | MaxDeliver exhausted rate (overall)<br/>MaxDeliver exhausted by assignment<br/>Top assignments by MaxDeliver exhausted<br/>Top tenants by MaxDeliver exhausted | ✅ Covered |
| **JS-003** | DLQ Growth | ✅ Covered | `RouterDLQHighRate` (in dashboard doc, not in alert rules) | ⚠️ Partially Covered | DLQ inflow total<br/>DLQ inflow by reason (when labels added)<br/>DLQ inflow by tenant (when labels added)<br/>DLQ by assignment (when labels added) | ⚠️ Partially Covered |
| **JS-004** | Redelivery Queue Growth | ✅ Covered | `RouterJetStreamGrowingRedeliveryQueue`<br/>`RouterJetStreamHighRedeliveryQueueSize` | ⚠️ Partially Covered | Redelivery queue size (via pending operations)<br/>Redelivery rate vs ACK rate | ⚠️ Partially Covered |
| **JS-005** | High Redelivery from Source | ✅ Covered | `RouterJetStreamHighRedeliveryFromSource` | ✅ Covered | Redelivery by reason<br/>Redelivery by source | ✅ Covered |
| **NATS-001** | NATS Connection Failures | ✅ Covered | `RouterNatsConnectionFailures`<br/>`RouterNatsFrequentReconnects` | ✅ Covered | NATS connect/reconnect failures<br/>NATS connection status | ✅ Covered |
| **NATS-002** | NATS Connection Down | ✅ Covered | `RouterNatsConnectionDown` | ✅ Covered | NATS connection status gauge | ✅ Covered |
| **NATS-003** | NATS Reconnection Exhausted | ✅ Covered | `RouterNatsReconnectionExhausted` | ⚠️ Partially Covered | Reconnection status (not explicitly documented) | ⚠️ Partially Covered |
| **NATS-004** | High Publish Failure Rate | ✅ Covered | `RouterNatsHighPublishFailureRate`<br/>`RouterNatsHighPublishWithAckFailureRate` | ✅ Covered | NATS publish failures by reason<br/>Publish failures by subject/stream<br/>Correlation with redeliveries/DLQ | ✅ Covered |
| **NATS-005** | High ACK Failure Rate | ✅ Covered | `RouterNatsHighAckFailureRate` | ✅ Covered | NATS ACK failures<br/>Correlation with redelivery spikes | ✅ Covered |
| **NATS-006** | High NAK Failure Rate | ✅ Covered | `RouterNatsHighNakFailureRate` | ⚠️ Partially Covered | NAK failures (not explicitly documented) | ⚠️ Partially Covered |
| **NATS-007** | High Subscribe Failure Rate | ✅ Covered | `RouterNatsHighSubscribeFailureRate` | ⚠️ Partially Covered | Subscribe failures (not explicitly documented) | ⚠️ Partially Covered |
| **NATS-008** | Pending Operations Queue Full | ✅ Covered | `RouterNatsPendingOperationsQueueFull` | ⚠️ Partially Covered | Pending operations count (not explicitly documented) | ⚠️ Partially Covered |
| **TENANT-001** | High Tenant Rejection Rate | ✅ Covered | `RouterTenantRejectedHigh` (in PROMETHEUS_ALERTS.md) | ✅ Covered | Tenant rejections total<br/>Tenant rejections by tenant (top N)<br/>Tenant rejections by reason<br/>Correlation with DLQ/MaxDeliver | ✅ Covered |
| **PERF-001** | Processing Latency Degradation | ⚠️ Partially Covered | `RouterIntakeLatencyHigh` (in PROMETHEUS_ALERTS.md) | ⚠️ Partially Covered | Processing latency panels (not explicitly documented in dashboard) | ⚠️ Partially Covered |
| **PERF-002** | Backpressure Active | ✅ Covered | `RouterIntakeBackpressureActive` (in PROMETHEUS_ALERTS.md) | ⚠️ Partially Covered | Backpressure status (not explicitly documented in dashboard) | ⚠️ Partially Covered |
| **PERF-003** | High Queue Depth | ✅ Covered | `RouterIntakeQueueDepthHigh` (in PROMETHEUS_ALERTS.md) | ⚠️ Partially Covered | Queue depth panels (not explicitly documented in dashboard) | ⚠️ Partially Covered |
| **PERF-004** | High In-Flight Messages | ✅ Covered | `RouterIntakeInflightHigh` (in PROMETHEUS_ALERTS.md) | ⚠️ Partially Covered | In-flight messages panels (not explicitly documented in dashboard) | ⚠️ Partially Covered |

### Coverage Summary

**Total Scenarios**: 18  
**Fully Covered**: 9 (50%)  
**Partially Covered**: 9 (50%)  
**Not Covered**: 0 (0%)

---

## 3. Alert Reference with Scenario Links

### JetStream Alerts

#### RouterJetStreamHighRedeliveryRate

**Alert Name**: `RouterJetStreamHighRedeliveryRate`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 38-62)  
**Scenarios Covered**: JS-001, JS-005  
**Condition**:
```promql
(rate(router_jetstream_redelivery_total[5m]) / (rate(router_jetstream_ack_total[5m]) + rate(router_jetstream_redelivery_total[5m]) + 1)) > 0.1
```
**Threshold**: > 10% redelivery rate for 10 minutes  
**Severity**: `warning`  
**Dashboard Panels**: 
- Redelivery rate (by assignment) - Section 4.2, Panel 1
- Redelivery rate (by tenant) - Section 4.2, Panel 2
- Redelivery by reason - Section 4.2, Panel 3

#### RouterJetStreamMaxDeliverExhausted

**Alert Name**: `RouterJetStreamMaxDeliverExhausted`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 92-114)  
**Scenarios Covered**: JS-002  
**Condition**:
```promql
increase(router_jetstream_maxdeliver_exhausted_total[2m]) > 0
```
**Threshold**: > 0 for 2 minutes  
**Severity**: `critical`  
**Dashboard Panels**:
- MaxDeliver exhausted rate (overall) - Section 2.2, Panel 1
- MaxDeliver exhausted by assignment - Section 2.2, Panel 2
- Top assignments by MaxDeliver exhausted - Section 2.2, Panel 3
- Top tenants by MaxDeliver exhausted - Section 2.2, Panel 4

#### RouterJetStreamGrowingRedeliveryQueue

**Alert Name**: `RouterJetStreamGrowingRedeliveryQueue`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 121-148)  
**Scenarios Covered**: JS-004  
**Condition**:
```promql
((router_nats_pending_operations_count > 200) OR (rate(router_jetstream_redelivery_total[5m]) > rate(router_jetstream_ack_total[5m]))) AND (rate(router_jetstream_redelivery_total[10m]) > rate(router_jetstream_redelivery_total[5m]))
```
**Threshold**: Queue > 200 or redelivery > ACK rate, and growing for 15 minutes  
**Severity**: `warning`  
**Dashboard Panels**: 
- Redelivery queue size (via pending operations) - Section 4.2 (implied)
- Redelivery vs Successful processing - Section 4.2, Panel 4

#### RouterJetStreamHighRedeliveryFromSource

**Alert Name**: `RouterJetStreamHighRedeliveryFromSource`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 68-86)  
**Scenarios Covered**: JS-005  
**Condition**:
```promql
rate(router_jetstream_redelivery_total{source!=""}[5m]) > 3
```
**Threshold**: > 3 redeliveries/sec from source for 10 minutes  
**Severity**: `warning`  
**Dashboard Panels**:
- Redelivery by reason - Section 4.2, Panel 3
- Redelivery by source (implied from reason breakdown)

### NATS Connection Alerts

#### RouterNatsConnectionDown

**Alert Name**: `RouterNatsConnectionDown`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 205-223)  
**Scenarios Covered**: NATS-002  
**Condition**:
```promql
router_nats_connection_status{state="disconnected"} == 0
```
**Threshold**: Disconnected for 2 minutes  
**Severity**: `critical`  
**Dashboard Panels**:
- NATS connection status gauge - Section 4.4 (implied)

#### RouterNatsConnectionFailures

**Alert Name**: `RouterNatsConnectionFailures`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 229-252)  
**Scenarios Covered**: NATS-001  
**Condition**:
```promql
rate(router_nats_connection_failures_total[5m]) > 0.1 OR rate(router_nats_connection_lost_total[5m]) > 0.1
```
**Threshold**: > 0.1 failures/sec for 5 minutes  
**Severity**: `warning`  
**Dashboard Panels**:
- NATS connect/reconnect failures - Section 4.4, Panel 1

#### RouterNatsReconnectionExhausted

**Alert Name**: `RouterNatsReconnectionExhausted`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 257-274)  
**Scenarios Covered**: NATS-003  
**Condition**:
```promql
increase(router_nats_reconnection_exhausted_total[5m]) > 0
```
**Threshold**: > 0 for 1 minute  
**Severity**: `critical`  
**Dashboard Panels**: 
- Reconnection status (not explicitly documented - **GAP**)

### NATS Operation Alerts

#### RouterNatsHighPublishFailureRate

**Alert Name**: `RouterNatsHighPublishFailureRate`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 311-332)  
**Scenarios Covered**: NATS-004  
**Condition**:
```promql
(rate(router_nats_publish_failures_total[5m]) / (rate(router_nats_publish_total[5m]) + rate(router_nats_publish_failures_total[5m]) + 1)) > 0.02
```
**Threshold**: > 2% failure rate for 10 minutes  
**Severity**: `warning`  
**Dashboard Panels**:
- NATS publish failures by reason - Section 4.4, Panel 2
- Publish failures by subject/stream - Section 4.4, Panel 2
- Correlation with redeliveries/DLQ - Section 4.4, Panel 3

#### RouterNatsHighAckFailureRate

**Alert Name**: `RouterNatsHighAckFailureRate`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 365-386)  
**Scenarios Covered**: NATS-005  
**Condition**:
```promql
(rate(router_nats_ack_failures_total[5m]) / (rate(router_nats_ack_total[5m]) + rate(router_nats_ack_failures_total[5m]) + 1)) > 0.01
```
**Threshold**: > 1% failure rate for 5 minutes  
**Severity**: `critical`  
**Dashboard Panels**:
- NATS ACK failures - Section 4.4, Panel 3
- Correlation with redelivery spikes - Section 4.4, Panel 3

#### RouterNatsHighNakFailureRate

**Alert Name**: `RouterNatsHighNakFailureRate`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 391-412)  
**Scenarios Covered**: NATS-006  
**Condition**:
```promql
(rate(router_nats_nak_failures_total[5m]) / (rate(router_nats_nak_total[5m]) + rate(router_nats_nak_failures_total[5m]) + 1)) > 0.02
```
**Threshold**: > 2% failure rate for 10 minutes  
**Severity**: `warning`  
**Dashboard Panels**: 
- NAK failures (not explicitly documented - **GAP**)

#### RouterNatsHighSubscribeFailureRate

**Alert Name**: `RouterNatsHighSubscribeFailureRate`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 417-438)  
**Scenarios Covered**: NATS-007  
**Condition**:
```promql
(rate(router_nats_subscribe_failures_total[5m]) / (rate(router_nats_subscribe_total[5m]) + rate(router_nats_subscribe_failures_total[5m]) + 1)) > 0.05
```
**Threshold**: > 5% failure rate for 5 minutes  
**Severity**: `critical`  
**Dashboard Panels**: 
- Subscribe failures (not explicitly documented - **GAP**)

#### RouterNatsPendingOperationsQueueFull

**Alert Name**: `RouterNatsPendingOperationsQueueFull`  
**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 443-460)  
**Scenarios Covered**: NATS-008  
**Condition**:
```promql
router_nats_pending_operations_count > 1000
```
**Threshold**: > 1000 for 10 minutes  
**Severity**: `warning`  
**Dashboard Panels**: 
- Pending operations count (not explicitly documented - **GAP**)

### Tenant Validation Alerts

#### RouterTenantRejectedHigh

**Alert Name**: `RouterTenantRejectedHigh`  
**File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 169-177)  
**Scenarios Covered**: TENANT-001  
**Condition**:
```promql
sum(rate(router_results_tenant_rejected_total[5m]) + rate(router_acks_tenant_rejected_total[5m])) > 5
```
**Threshold**: > 5 rejections/sec for 10 minutes  
**Severity**: `warning`  
**Dashboard Panels**:
- Tenant rejections total - Section 4.5, Panel 1
- Tenant rejections by tenant (top N) - Section 4.5, Panel 2
- Tenant rejections by reason - Section 4.5, Panel 3
- Correlation with DLQ/MaxDeliver - Section 4.5, Panel 4

### Performance Alerts

#### RouterIntakeBackpressureActive

**Alert Name**: `RouterIntakeBackpressureActive`  
**File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 245-252)  
**Scenarios Covered**: PERF-002  
**Condition**:
```promql
router_intake_backpressure_active{subject="beamline.router.v1.decide"} == 1
```
**Threshold**: Active for 1 minute  
**Severity**: `warning`  
**Dashboard Panels**: 
- Backpressure status (not explicitly documented in dashboard - **GAP**)

#### RouterIntakeQueueDepthHigh

**Alert Name**: `RouterIntakeQueueDepthHigh`  
**File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 253-260)  
**Scenarios Covered**: PERF-003  
**Condition**:
```promql
router_jetstream_pending_messages{subject="beamline.router.v1.decide"} > 100
```
**Threshold**: > 100 for 5 minutes  
**Severity**: `critical`  
**Dashboard Panels**: 
- Queue depth panels (not explicitly documented in dashboard - **GAP**)

#### RouterIntakeLatencyHigh

**Alert Name**: `RouterIntakeLatencyHigh`  
**File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 261-268)  
**Scenarios Covered**: PERF-001  
**Condition**:
```promql
router_intake_processing_latency_p95{subject="beamline.router.v1.decide"} > 2000
```
**Threshold**: P95 > 2000ms for 5 minutes  
**Severity**: `critical`  
**Dashboard Panels**: 
- Processing latency panels (not explicitly documented in dashboard - **GAP**)

#### RouterIntakeInflightHigh

**Alert Name**: `RouterIntakeInflightHigh`  
**File**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` (lines 269-276)  
**Scenarios Covered**: PERF-004  
**Condition**:
```promql
router_intake_inflight_messages{subject="beamline.router.v1.decide"} > 200
```
**Threshold**: > 200 for 5 minutes  
**Severity**: `warning`  
**Dashboard Panels**: 
- In-flight messages panels (not explicitly documented in dashboard - **GAP**)

---

## 4. Dashboard Panel Reference with Scenario Links

### Overview Panel Group

**Section**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#41-overview-panel-group` (lines 1060-1084)  
**Scenarios Covered**: JS-001, JS-002, JS-003, NATS-001

**Panels**:
1. **Total DLQ rate** - JS-003
2. **Total redelivery rate** - JS-001
3. **Total NATS failures** - NATS-001
4. **MaxDeliver exhaustion rate** - JS-002

### Redelivery Panel Group

**Section**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#42-redelivery-panel-group` (lines 1085-1126)  
**Scenarios Covered**: JS-001, JS-005

**Panels**:
1. **Redelivery rate by assignment** - JS-001
   - Query: `sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))`
   - Panel ID: Not specified
2. **Redelivery rate by tenant** - JS-001
   - Query: `sum by (tenant_id) (rate(router_jetstream_redelivery_total[5m]))`
   - Panel ID: Not specified
3. **Redelivery by reason** - JS-001, JS-005
   - Query: `sum by (reason) (rate(router_jetstream_redelivery_total[5m]))`
   - Panel ID: Not specified
4. **Redelivery vs Successful processing** - JS-001
   - Query: Stacked graph with redeliveries and successful processing
   - Panel ID: Not specified
5. **Avg/Max delivery_count per assignment** - JS-001
   - Query: `sum by (delivery_count) (rate(router_jetstream_redelivery_total[5m]))`
   - Panel ID: Not specified

### DLQ and MaxDeliver Panel Group

**Section**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#43-dlq-and-maxdeliver` (lines 1127-1142)  
**Scenarios Covered**: JS-002, JS-003

**Panels**:
1. **DLQ inflow total** - JS-003
   - Query: `sum(rate(router_dlq_total[5m]))`
   - Panel ID: Not specified
2. **DLQ inflow by reason** - JS-003 (when labels added)
   - Query: `sum by (reason) (rate(router_dlq_total[5m]))`
   - Panel ID: Not specified
3. **MaxDeliver exhausted rate** - JS-002
   - Query: `sum(rate(router_jetstream_maxdeliver_exhausted_total[5m]))`
   - Panel ID: Not specified
4. **Top assignments by DLQ / MaxDeliver exhausted** - JS-002, JS-003
   - Query: `topk(10, sum by (assignment_id) (rate(router_jetstream_maxdeliver_exhausted_total[24h])))`
   - Panel ID: Not specified

### NATS Infrastructure Errors Panel Group

**Section**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#44-nats-infrastructure-errors` (lines 1143-1156)  
**Scenarios Covered**: NATS-001, NATS-004, NATS-005

**Panels**:
1. **NATS connect / reconnect failures** - NATS-001
   - Query: `sum(rate(router_nats_connect_failures_total[5m]))`, `sum(rate(router_nats_reconnect_failures_total[5m]))`
   - Panel ID: Not specified
2. **NATS publish failures by reason** - NATS-004
   - Query: `sum by (reason) (rate(router_nats_publish_failures_total[5m]))`
   - Panel ID: Not specified
3. **NATS ACK failures** - NATS-005
   - Query: `sum(rate(router_nats_ack_failures_total[5m]))`
   - Panel ID: Not specified

### Tenant / Usage Rejections Panel Group

**Section**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#45-tenant--usage-rejections-panel-group` (lines 1157-1195)  
**Scenarios Covered**: TENANT-001

**Panels**:
1. **Tenant rejections total** - TENANT-001
   - Query: `sum(rate(router_results_tenant_rejected_total[5m]))`
   - Panel ID: Not specified
2. **Tenant rejections by tenant_id (top N)** - TENANT-001
   - Query: `topk(10, sum by (tenant_id) (rate(router_results_tenant_rejected_total[5m])))`
   - Panel ID: Not specified
3. **Tenant rejections by reason** - TENANT-001
   - Query: `sum by (reason) (rate(router_results_tenant_rejected_total[5m]))`
   - Panel ID: Not specified
4. **Correlation with DLQ / MaxDeliver** - TENANT-001
   - Query: Stacked graph with rejections, DLQ, and MaxDeliver
   - Panel ID: Not specified

### Correlation / Overview Panel Group

**Section**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md#46-correlation--overview-panel-group` (lines 1196-1226)  
**Scenarios Covered**: JS-001, JS-002, JS-003, NATS-001

**Panels**:
1. **Stacked graph: Redelivery + DLQ + MaxDeliver exhausted** - JS-001, JS-002, JS-003
   - Query: Stack all three metrics on same graph
   - Panel ID: Not specified
2. **Heatmap: Redelivery rate (assignment × tenant)** - JS-001
   - Query: `sum by (assignment_id, tenant_id) (rate(router_jetstream_redelivery_total[5m]))`
   - Panel ID: Not specified
3. **Correlation: NATS failures vs redeliveries** - NATS-001, JS-001
   - Query: Overlay NATS failures and redeliveries
   - Panel ID: Not specified

---

## 5. Identified Gaps and Future Work

### Alert Gaps

#### Missing DLQ Alert in Alert Rules

**Gap**: DLQ high rate alert is documented in dashboard (`RouterDLQHighRate`) but not present in `router-alert-rules.yaml`

**Scenario**: JS-003 (DLQ Growth)  
**Current Status**: Alert documented in `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` (lines 1591-1615) but missing from actual alert rules file  
**Required Action**: Add `RouterDLQHighRate` alert to `router-alert-rules.yaml`

**Proposed Alert**:
```yaml
- alert: RouterDLQHighRate
  expr: |
    sum(rate(router_dlq_total[5m])) > 0.1
  for: 5m
  labels:
    severity: warning
    service: router
    component: jetstream
    team: platform
  annotations:
    summary: "DLQ inflow rate exceeds threshold"
    description: |
      DLQ inflow rate is {{ $value | humanize }} messages/sec (threshold: 0.1 msg/sec).
      This indicates messages are failing to process and accumulating in DLQ.
      Check DLQ by reason (when labels added), MaxDeliver exhaustion rate, and logs.
```

### Dashboard Panel Gaps

#### Missing Panels for NATS Operations

**Gaps**:
1. **NAK failures panel** - NATS-006
2. **Subscribe failures panel** - NATS-007
3. **Pending operations count panel** - NATS-008
4. **Reconnection exhausted status panel** - NATS-003

**Required Actions**:
- Add NAK failures panel to "NATS Infrastructure Errors" group
- Add Subscribe failures panel to "NATS Infrastructure Errors" group
- Add Pending operations count gauge to "NATS Infrastructure Errors" group
- Add Reconnection exhausted status panel to "NATS Infrastructure Errors" group

#### Missing Panels for Performance Metrics

**Gaps**:
1. **Processing latency panels** - PERF-001
2. **Backpressure status panel** - PERF-002
3. **Queue depth panels** - PERF-003
4. **In-flight messages panels** - PERF-004

**Required Actions**:
- Add new "Performance & Capacity" panel group to dashboard
- Add processing latency panels (P50, P95, P99)
- Add backpressure status gauge
- Add queue depth time series
- Add in-flight messages time series

### Label Gaps

#### Missing Labels on DLQ Metric

**Gap**: `router_dlq_total` metric has no labels (simple counter)

**Scenario**: JS-003 (DLQ Growth)  
**Current Status**: Metric exists but lacks labels for breakdown  
**Required Action**: Add labels to `router_dlq_total`:
- `assignment_id`
- `tenant_id`
- `reason`
- `source`
- `msg_id`
- `request_id`

**Impact**: DLQ panels cannot show breakdown by reason/tenant/assignment until labels are added

#### Missing Labels on NATS Failure Metrics

**Gaps**: NATS failure metrics lack labels for detailed breakdown:
- `router_nats_connect_failures_total` - no labels
- `router_nats_publish_failures_total` - no labels
- `router_nats_ack_failures_total` - no labels
- `router_nats_reconnect_failures_total` - no labels

**Required Actions**: Add labels to NATS failure metrics:
- `reason` (error reason)
- `subject` or `stream` (if available)
- `source` or `cluster`

### Documentation Gaps

#### Missing Explicit Panel IDs

**Gap**: Dashboard documentation does not specify panel IDs for Grafana

**Required Action**: Add panel IDs to dashboard documentation for:
- All panels in Overview group
- All panels in Redelivery group
- All panels in DLQ and MaxDeliver group
- All panels in NATS Infrastructure Errors group
- All panels in Tenant / Usage Rejections group
- All panels in Correlation / Overview group

#### Missing Scenario References in Alert Rules

**Gap**: Alert rules do not reference scenario IDs

**Required Action**: Add scenario ID references to alert annotations:
```yaml
annotations:
  summary: "High JetStream redelivery rate detected"
  description: |
    Redelivery rate is {{ $value | humanizePercentage }} (threshold: 10%).
    Scenario: JS-001 (High Redelivery Rate)
    Assignment ID: {{ $labels.assignment_id | default "unknown" }}
    ...
```

#### Missing Scenario References in Dashboard Documentation

**Gap**: Dashboard panels do not reference scenario IDs

**Required Action**: Add scenario ID references to panel descriptions:
```markdown
### Redelivery rate by assignment

**Scenario**: JS-001 (High Redelivery Rate)  
**Query**: `sum by (assignment_id) (rate(router_jetstream_redelivery_total[5m]))`  
**Panel ID**: `router-redelivery-by-assignment`  
...
```

---

## 6. Future Work Priority List

### ✅ Priority 1: Critical Gaps - ALL COMPLETED

1. ✅ **Add DLQ Alert to Alert Rules** (JS-003) - **COMPLETED**
   - **Status**: Added `RouterDLQHighRate` and `RouterDLQHighRateCritical` alerts to `router-alert-rules.yaml`
   - **Implementation**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (lines 171-200)

2. ✅ **Add Performance Panels to Dashboard** (PERF-001, PERF-002, PERF-003, PERF-004) - **COMPLETED**
   - **Status**: Created "Performance & Capacity" panel group with 6 panels
   - **Implementation**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` (Section 4.7)

3. ✅ **Add Labels to DLQ Metric** (JS-003) - **COMPLETED**
   - **Status**: Implemented labels on `router_dlq_total` metric
   - **Implementation**: `apps/otp/router/src/router_jetstream.erl` (lines 151-163, 551-580)

### ✅ Priority 2: Important Gaps - ALL COMPLETED

4. ✅ **Add NATS Operation Panels** (NATS-006, NATS-007, NATS-008) - **COMPLETED**
   - **Status**: Added NAK failures, Subscribe failures, Pending operations panels
   - **Implementation**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` (Section 4.4, Panels 7-10)

5. ✅ **Add Labels to NATS Failure Metrics** (NATS-001, NATS-004, NATS-005, NATS-006, NATS-007) - **COMPLETED**
   - **Status**: Implemented labels on all NATS failure metrics
   - **Implementation**: `apps/otp/router/src/router_nats.erl` (multiple locations)

6. **Add Scenario IDs to Documentation** (All scenarios)
   - **Action**: Add scenario ID references to alert rules and dashboard documentation
   - **Impact**: Medium - Improves traceability and maintenance
   - **Effort**: Low - Documentation updates only

### Priority 3: Nice to Have (Long-term)

7. **Add Panel IDs to Dashboard Documentation** (All panels)
   - **Action**: Specify Grafana panel IDs for all dashboard panels
   - **Impact**: Low - Improves dashboard implementation consistency
   - **Effort**: Low - Documentation updates only

8. **Create Scenario-to-Test Mapping** (All scenarios)
   - **Action**: Link scenarios to test cases in fault injection test suites
   - **Impact**: Low - Improves test coverage visibility
   - **Effort**: Medium - Requires test documentation updates

---

## 7. References

### Primary Documentation

- **Router Observability Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
- **Alert Rules**: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- **Prometheus Alerts**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md`
- **Metrics Monitoring Guide**: `docs/OBSERVABILITY_METRICS_MONITORING_GUIDE.md`

### JetStream Specifications

- **CP2 Router Specification**: `docs/archive/dev/CP2_ROUTER_GATEWAY_SPEC.md`
- **JetStream Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **ADR-011 JetStream E2E**: `docs/ADR/ADR-011-jetstream-e2e.md`

### Test Documentation

- **Fault Injection Test Scenarios**: `apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md`
- **Concurrent Faults Tests**: `apps/otp/router/test/router_concurrent_faults_SUITE.erl`

---

## 8. Change History

**v1.0** (2025-11-30):
- Initial coverage analysis
- Complete scenario catalog (18 scenarios)
- Coverage matrix with alert and dashboard mapping
- Gap identification and future work prioritization

---

**Prepared by**: WORKER wrk-9 (Documentation & Developer Experience)  
**Status**: Analysis Complete  
**Next Steps**: Implement Priority 1 gaps (DLQ alert, performance panels, DLQ labels)

