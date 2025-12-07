# JetStream Fault Injection → Tests → Metrics/Alerts/Dashboards Coverage Matrix

**Date**: 2025-11-30  
**Status**: ✅ **Complete** - Formal coverage matrix for JetStream scenarios  
**Purpose**: Single source of truth for scenario → tests → metrics → alerts → dashboards traceability

## Document Purpose

This document provides formal traceability between:
- **JetStream fault injection scenarios** (S1, S2, S3, and additional scenarios from TZ)
- **Test implementations** (unit, integration, performance, fault injection)
- **Metrics** (with labels)
- **Alert rules** (Prometheus/Alertmanager)
- **Dashboard panels** (Grafana)

**Coverage Status**: For each scenario, we track:
- `tests`: `covered` / `partial` / `none`
- `metrics`: `covered` / `partial` / `none`
- `alerts`: `covered` / `partial` / `none`
- `dashboards`: `covered` / `partial` / `none`

## Scenario ID Convention

- **JS-XXX**: JetStream core scenarios (redelivery, MaxDeliver, DLQ)
- **NATS-XXX**: NATS infrastructure scenarios (connection, publish, ACK)
- **S1, S2, S3**: Fault injection test scenarios (from `JETSTREAM_FAULT_INJECTION_TESTS.md`)

## Coverage Matrix

| Scenario ID | Scenario Name | Tests | Metrics | Alerts | Dashboards | Coverage Status | Comments |
|-------------|---------------|-------|---------|--------|------------|-----------------|----------|
| **S1** | Intermittent ACK/NAK Errors | ✅ **Covered**<br/>- `test_intermittent_ack_failure_recovery/1` (E2E)<br/>- `test_delivery_count_tracking_under_ack_failures/1` (Unit)<br/>**Files**:<br/>- `router_jetstream_e2e_SUITE.erl:665-811`<br/>- `router_delivery_count_tracking_SUITE.erl:386-451` | ✅ **Covered**<br/>- `router_nats_ack_failures_total` (with labels: `reason`, `subject`, `stream`, `consumer`)<br/>- `router_nats_nak_failures_total` (with labels: `reason`, `subject`, `stream`)<br/>- `router_jetstream_redelivery_total` (indirect, via NAK) | ⚠️ **Partial**<br/>- `RouterNATSAckFailuresHigh` (exists, but not explicitly linked to S1)<br/>**Alert File**: `router-alert-rules.yaml` | ✅ **Covered**<br/>- **Panel**: "NATS ACK Failures by Reason" (Section 4.4, Panel 8)<br/>- **Panel**: "NATS NAK Failures by Reason" (Section 4.4, Panel 9)<br/>- **Panel**: "Correlation: NATS Failures vs Redeliveries" (Section 4.4, Panel 10)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ⚠️ Partial<br/>**Dashboards**: ✅ Covered | **Future Work**: Link S1 explicitly to `RouterNATSAckFailuresHigh` alert in alert rules annotations |
| **S2** | Processing Delays → Redelivery Growth | ✅ **Covered**<br/>- `test_processing_delays_redelivery_with_delivery_count/1` (E2E)<br/>- `test_processing_delays_redelivery_with_delivery_count/1` (Unit)<br/>- `test_redelivery_metric_labels/1` (Fault Injection)<br/>- `test_redelivery_tenant_validation_failed/1` (Fault Injection)<br/>**Files**:<br/>- `router_jetstream_e2e_SUITE.erl:833-985`<br/>- `router_delivery_count_tracking_SUITE.erl:466-510`<br/>- `router_jetstream_fault_injection_SUITE.erl:844-977,979-1076` | ✅ **Covered**<br/>- `router_jetstream_redelivery_total` (with labels: `assignment_id`, `request_id`, `reason`, `source`, `msg_id`, `tenant_id`)<br/>- `router_nats_pending_operations_count` (gauge, for queue depth) | ✅ **Covered**<br/>- `RouterJetStreamHighRedeliveryRate` (Scenario ID: JS-001)<br/>- `RouterJetStreamHighRedeliveryFromSource` (Scenario ID: JS-005)<br/>- `RouterJetStreamGrowingRedeliveryQueue` (Scenario ID: JS-004)<br/>**Alert File**: `router-alert-rules.yaml:38-159` | ✅ **Covered**<br/>- **Panel**: "Redelivery Rate by Assignment" (Section 4.2, Panel 1)<br/>- **Panel**: "Redelivery Rate by Tenant" (Section 4.2, Panel 2)<br/>- **Panel**: "Redeliveries by Reason" (Section 4.2, Panel 5)<br/>- **Panel**: "Redeliveries by Delivery Count" (Section 4.2, Panel 4)<br/>- **Panel**: "Pending Operations Queue" (Section 4.4, Panel 9)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Complete**: Full coverage for S2 scenario |
| **S3** | MaxDeliver Exhaustion (Partial Messages) | ✅ **Covered**<br/>- `test_maxdeliver_exhaustion_partial_messages_e2e/1` (E2E)<br/>- `test_maxdeliver_exhaustion_emits_metric/1` (Unit)<br/>- `test_maxdeliver_exhaustion_removes_tracking/1` (Unit)<br/>- `test_maxdeliver_exhausted_metric_labels/1` (Fault Injection)<br/>- `test_maxdeliver_exhausted_different_limits/1` (Fault Injection)<br/>**Files**:<br/>- `router_jetstream_e2e_SUITE.erl:1007-1181`<br/>- `router_delivery_count_tracking_SUITE.erl:137-248`<br/>- `router_jetstream_fault_injection_SUITE.erl:1078-1258,1789-1894` | ✅ **Covered**<br/>- `router_jetstream_maxdeliver_exhausted_total` (with labels: `assignment_id`, `request_id`, `msg_id`, `delivery_count`, `max_deliver`, `reason`)<br/>- `router_dlq_total` (with labels: `assignment_id`, `reason`, `tenant_id`, `source`, `msg_id`, `request_id`)<br/>- `router_jetstream_redelivery_total` (indirect, before exhaustion) | ✅ **Covered**<br/>- `RouterJetStreamMaxDeliverExhausted` (Scenario ID: JS-002)<br/>- `RouterDLQHighRate` (Scenario ID: JS-003)<br/>- `RouterDLQHighRateCritical` (Scenario ID: JS-003)<br/>**Alert File**: `router-alert-rules.yaml:92-123,187-220` | ✅ **Covered**<br/>- **Panel**: "MaxDeliver Exhausted Rate (Overall)" (Section 4.3, Panel 1)<br/>- **Panel**: "MaxDeliver Exhausted by Assignment" (Section 4.3, Panel 2)<br/>- **Panel**: "Top Assignments by MaxDeliver Exhausted" (Section 4.3, Panel 3)<br/>- **Panel**: "DLQ Inflow Rate (Total)" (Section 4.3, Panel 5)<br/>- **Panel**: "DLQ by Reason" (Section 4.3, Panel 6)<br/>- **Panel**: "DLQ by Assignment" (Section 4.3, Panel 7)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Complete**: Full coverage for S3 scenario |
| **JS-001** | High Redelivery Rate | ✅ **Covered**<br/>- Same tests as S2 (redelivery growth)<br/>- `test_redelivery_metric_labels/1`<br/>- `test_redelivery_tenant_validation_failed/1` | ✅ **Covered**<br/>- `router_jetstream_redelivery_total` (with labels) | ✅ **Covered**<br/>- `RouterJetStreamHighRedeliveryRate`<br/>**Alert File**: `router-alert-rules.yaml:38-62` | ✅ **Covered**<br/>- **Panel**: "Redelivery Rate by Assignment" (Section 4.2, Panel 1)<br/>- **Panel**: "Redelivery vs Successful Processing" (Section 4.2, Panel 3)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S2**: JS-001 is covered by S2 tests |
| **JS-002** | MaxDeliver Exhaustion | ✅ **Covered**<br/>- Same tests as S3 (MaxDeliver exhaustion)<br/>- `test_maxdeliver_exhaustion_partial_messages_e2e/1`<br/>- `test_maxdeliver_exhausted_metric_labels/1` | ✅ **Covered**<br/>- `router_jetstream_maxdeliver_exhausted_total` (with labels) | ✅ **Covered**<br/>- `RouterJetStreamMaxDeliverExhausted`<br/>**Alert File**: `router-alert-rules.yaml:92-123` | ✅ **Covered**<br/>- **Panel**: "MaxDeliver Exhausted Rate (Overall)" (Section 4.3, Panel 1)<br/>- **Panel**: "MaxDeliver Exhausted by Assignment" (Section 4.3, Panel 2)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S3**: JS-002 is covered by S3 tests |
| **JS-003** | DLQ Growth | ✅ **Covered**<br/>- Same tests as S3 (MaxDeliver exhaustion leads to DLQ)<br/>- `test_maxdeliver_exhaustion_partial_messages_e2e/1` (indirect, via MaxDeliver exhaustion)<br/>- `test_dlq_metric_emission_with_labels/1` (Unit test for labels)<br/>**Files**:<br/>- `router_jetstream_e2e_SUITE.erl:1007-1181`<br/>- `router_metrics_labels_unit_SUITE.erl` | ✅ **Covered**<br/>- `router_dlq_total` (with labels: `assignment_id`, `reason`, `tenant_id`, `source`, `msg_id`, `request_id`)<br/>- `router_jetstream_maxdeliver_exhausted_total` (indirect, correlates with DLQ) | ✅ **Covered**<br/>- `RouterDLQHighRate` (warning)<br/>- `RouterDLQHighRateCritical` (critical)<br/>**Alert File**: `router-alert-rules.yaml:187-220` | ✅ **Covered**<br/>- **Panel**: "DLQ Inflow Rate (Total)" (Section 4.3, Panel 5)<br/>- **Panel**: "DLQ by Reason" (Section 4.3, Panel 6)<br/>- **Panel**: "DLQ by Assignment" (Section 4.3, Panel 7)<br/>- **Panel**: "DLQ by Tenant" (Section 4.3, Panel 8, when labels added)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S3**: JS-003 is covered by S3 tests |
| **JS-004** | Redelivery Queue Growth | ✅ **Covered**<br/>- Same tests as S2 (redelivery growth)<br/>- `test_processing_delays_redelivery_with_delivery_count/1` | ✅ **Covered**<br/>- `router_jetstream_redelivery_total` (with labels)<br/>- `router_nats_pending_operations_count` (gauge, for queue depth) | ✅ **Covered**<br/>- `RouterJetStreamGrowingRedeliveryQueue`<br/>- `RouterJetStreamHighRedeliveryQueueSize`<br/>**Alert File**: `router-alert-rules.yaml:121-185` | ✅ **Covered**<br/>- **Panel**: "Pending Operations Queue" (Section 4.4, Panel 9)<br/>- **Panel**: "Redelivery Rate by Assignment" (Section 4.2, Panel 1)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S2**: JS-004 is covered by S2 tests |
| **JS-005** | High Redelivery from Specific Source | ✅ **Covered**<br/>- Same tests as S2 (redelivery with source labels)<br/>- `test_redelivery_tenant_validation_failed/1` (validates `source` label) | ✅ **Covered**<br/>- `router_jetstream_redelivery_total` (with `source` label) | ✅ **Covered**<br/>- `RouterJetStreamHighRedeliveryFromSource`<br/>**Alert File**: `router-alert-rules.yaml:66-92` | ✅ **Covered**<br/>- **Panel**: "Redeliveries by Reason" (Section 4.2, Panel 5)<br/>- **Panel**: "Redelivery Rate by Assignment" (Section 4.2, Panel 1)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S2**: JS-005 is covered by S2 tests |
| **NATS-001** | NATS Connection Failures | ✅ **Covered**<br/>- `test_nats_connection_loss_recovery/1` (Fault Injection)<br/>- `test_nats_connect_failure_labels/1` (Integration test for labels)<br/>**Files**:<br/>- `router_jetstream_fault_injection_SUITE.erl:240-384`<br/>- `router_metrics_labels_integration_SUITE.erl` | ✅ **Covered**<br/>- `router_nats_connect_failures_total` (with labels: `reason`, `cluster`, `source`)<br/>- `router_nats_connection_status` (gauge) | ✅ **Covered**<br/>- `RouterNATSConnectionDown`<br/>- `RouterNATSReconnectionExhausted`<br/>**Alert File**: `router-alert-rules.yaml:222-280` | ✅ **Covered**<br/>- **Panel**: "NATS Connection Failures by Reason" (Section 4.4, Panel 1)<br/>- **Panel**: "NATS Connection Status" (Section 4.4, Panel 2)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Complete**: Full coverage for NATS connection failures |
| **NATS-002** | NATS Reconnection Failures | ✅ **Covered**<br/>- `test_nats_connection_loss_recovery/1` (includes reconnection)<br/>- `test_jetstream_consumer_reconnection/1`<br/>**Files**:<br/>- `router_jetstream_fault_injection_SUITE.erl:240-384,386-510` | ✅ **Covered**<br/>- `router_nats_reconnect_failures_total` (with labels: `reason`, `cluster`, `attempt`)<br/>- `router_nats_reconnect_attempts_total` (counter) | ✅ **Covered**<br/>- `RouterNATSReconnectionExhausted`<br/>**Alert File**: `router-alert-rules.yaml:260-280` | ✅ **Covered**<br/>- **Panel**: "NATS Reconnection Failures by Reason" (Section 4.4, Panel 3)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Complete**: Full coverage for NATS reconnection failures |
| **NATS-003** | NATS Publish Failures | ✅ **Covered**<br/>- `test_nats_publish_failure_labels/1` (Integration test for labels)<br/>- `test_connect_and_publish_faults/1` (Concurrent faults)<br/>**Files**:<br/>- `router_metrics_labels_integration_SUITE.erl`<br/>- `router_concurrent_faults_SUITE.erl` | ✅ **Covered**<br/>- `router_nats_publish_failures_total` (with labels: `reason`, `subject`, `stream`, `source`)<br/>- `router_nats_publish_with_ack_failures_total` (with labels) | ⚠️ **Partial**<br/>- `RouterNATSPublishFailuresHigh` (exists, but not explicitly linked to NATS-003)<br/>**Alert File**: `router-alert-rules.yaml:282-320` | ✅ **Covered**<br/>- **Panel**: "NATS Publish Failures by Reason" (Section 4.4, Panel 4)<br/>- **Panel**: "NATS Publish Failures by Subject" (Section 4.4, Panel 5)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ⚠️ Partial<br/>**Dashboards**: ✅ Covered | **Future Work**: Link NATS-003 explicitly to `RouterNATSPublishFailuresHigh` alert |
| **NATS-004** | NATS ACK Failures | ✅ **Covered**<br/>- Same tests as S1 (intermittent ACK failures)<br/>- `test_nats_ack_failure_labels/1` (Integration test for labels)<br/>**Files**:<br/>- `router_jetstream_e2e_SUITE.erl:665-811`<br/>- `router_metrics_labels_integration_SUITE.erl` | ✅ **Covered**<br/>- `router_nats_ack_failures_total` (with labels: `reason`, `subject`, `stream`, `consumer`)<br/>- `router_nats_nak_failures_total` (with labels: `reason`, `subject`, `stream`) | ⚠️ **Partial**<br/>- `RouterNATSAckFailuresHigh` (exists, but not explicitly linked to NATS-004)<br/>**Alert File**: `router-alert-rules.yaml:322-360` | ✅ **Covered**<br/>- **Panel**: "NATS ACK Failures by Reason" (Section 4.4, Panel 8)<br/>- **Panel**: "NATS NAK Failures by Reason" (Section 4.4, Panel 9)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ⚠️ Partial<br/>**Dashboards**: ✅ Covered | **Mapped to S1**: NATS-004 is covered by S1 tests. **Future Work**: Link NATS-004 explicitly to `RouterNATSAckFailuresHigh` alert |
| **NATS-005** | NATS Subscribe Failures | ⚠️ **Partial**<br/>- No dedicated fault injection test<br/>- `test_nats_subscribe_failures_total` (unit test for metric emission)<br/>**Files**:<br/>- `router_metrics_labels_unit_SUITE.erl` (indirect) | ✅ **Covered**<br/>- `router_nats_subscribe_failures_total` (with labels: `reason`, `subject`, `stream`, `consumer`) | ❌ **None**<br/>- No alert rule for subscribe failures<br/>**Alert File**: N/A | ⚠️ **Partial**<br/>- **Panel**: "NATS Subscribe Failures by Reason" (Section 4.4, Panel 7, when labels added)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ⚠️ Partial<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ❌ None<br/>**Dashboards**: ⚠️ Partial | **Future Work**: Add fault injection test for subscribe failures, add alert rule |
| **NATS-006** | NATS NAK Failures | ✅ **Covered**<br/>- Same tests as S2 (NAK is called during redelivery)<br/>- `test_processing_delays_redelivery_with_delivery_count/1` | ✅ **Covered**<br/>- `router_nats_nak_failures_total` (with labels: `reason`, `subject`, `stream`) | ❌ **None**<br/>- No alert rule for NAK failures<br/>**Alert File**: N/A | ✅ **Covered**<br/>- **Panel**: "NATS NAK Failures by Reason" (Section 4.4, Panel 9)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ❌ None<br/>**Dashboards**: ✅ Covered | **Future Work**: Add alert rule for NAK failures (low priority, NAK failures are expected during redelivery) |
| **NATS-007** | NATS Pending Operations Queue | ✅ **Covered**<br/>- Same tests as S2 (queue depth monitoring)<br/>- `test_processing_delays_redelivery_with_delivery_count/1` | ✅ **Covered**<br/>- `router_nats_pending_operations_count` (gauge) | ✅ **Covered**<br/>- `RouterJetStreamHighRedeliveryQueueSize` (uses pending operations count)<br/>**Alert File**: `router-alert-rules.yaml:161-185` | ✅ **Covered**<br/>- **Panel**: "Pending Operations Queue" (Section 4.4, Panel 9)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S2**: NATS-007 is covered by S2 tests |
| **NATS-008** | NATS Publish with ACK Failures | ⚠️ **Partial**<br/>- `test_nats_publish_failure_labels/1` (covers publish failures, but not specifically publish_with_ack)<br/>**Files**:<br/>- `router_metrics_labels_integration_SUITE.erl` | ✅ **Covered**<br/>- `router_nats_publish_with_ack_failures_total` (with labels: `reason`, `subject`, `stream`, `source`) | ❌ **None**<br/>- No alert rule for publish_with_ack failures<br/>**Alert File**: N/A | ⚠️ **Partial**<br/>- **Panel**: "NATS Publish Failures by Reason" (Section 4.4, Panel 4, covers both publish and publish_with_ack)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ⚠️ Partial<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ❌ None<br/>**Dashboards**: ⚠️ Partial | **Future Work**: Add dedicated test for publish_with_ack failures, add alert rule (low priority) |
| **PERF-001** | Processing Latency (P50/P95/P99) | ✅ **Covered**<br/>- `test_label_extraction_performance/1` (Performance test)<br/>- `test_metric_emission_performance/1` (Performance test)<br/>**Files**:<br/>- `router_metrics_labels_performance_SUITE.erl` | ✅ **Covered**<br/>- `router_intake_processing_latency_seconds` (histogram)<br/>- `router_intake_processing_latency_p50` (summary)<br/>- `router_intake_processing_latency_p95` (summary)<br/>- `router_intake_processing_latency_p99` (summary) | ❌ **None**<br/>- No alert rule for latency thresholds<br/>**Alert File**: N/A | ✅ **Covered**<br/>- **Panel**: "Processing Latency (P50/P95/P99)" (Section 4.7, Panel 1)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ❌ None<br/>**Dashboards**: ✅ Covered | **Future Work**: Add alert rule for latency thresholds (high priority for production) |
| **PERF-002** | Queue Depth (Pending Messages) | ✅ **Covered**<br/>- Same tests as S2 (queue depth monitoring)<br/>- `test_processing_delays_redelivery_with_delivery_count/1` | ✅ **Covered**<br/>- `router_jetstream_pending_messages` (gauge)<br/>- `router_nats_pending_operations_count` (gauge) | ✅ **Covered**<br/>- `RouterJetStreamHighRedeliveryQueueSize`<br/>**Alert File**: `router-alert-rules.yaml:161-185` | ✅ **Covered**<br/>- **Panel**: "Queue Depth (Pending Messages)" (Section 4.7, Panel 2)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ✅ Covered<br/>**Dashboards**: ✅ Covered | **Mapped to S2**: PERF-002 is covered by S2 tests |
| **PERF-003** | In-Flight Messages | ⚠️ **Partial**<br/>- No dedicated fault injection test<br/>- Unit test for metric emission exists<br/>**Files**:<br/>- `router_metrics_labels_unit_SUITE.erl` (indirect) | ✅ **Covered**<br/>- `router_intake_inflight_messages` (gauge)<br/>- `router_intake_inflight_messages_max` (gauge) | ❌ **None**<br/>- No alert rule for in-flight messages<br/>**Alert File**: N/A | ✅ **Covered**<br/>- **Panel**: "In-Flight Messages" (Section 4.7, Panel 3)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ⚠️ Partial<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ❌ None<br/>**Dashboards**: ✅ Covered | **Future Work**: Add fault injection test for in-flight messages, add alert rule (medium priority) |
| **PERF-004** | Backpressure Status | ✅ **Covered**<br/>- `test_processing_delays_redelivery_with_delivery_count/1` (indirect, backpressure triggers redelivery)<br/>**Files**:<br/>- `router_jetstream_e2e_SUITE.erl:833-985` | ✅ **Covered**<br/>- `router_intake_backpressure_active` (gauge)<br/>- `router_intake_backpressure_triggered_total` (counter)<br/>- `router_intake_backpressure_rejections_total` (counter) | ⚠️ **Partial**<br/>- `RouterJetStreamGrowingRedeliveryQueue` (indirect, backpressure causes queue growth)<br/>**Alert File**: `router-alert-rules.yaml:121-159` | ✅ **Covered**<br/>- **Panel**: "Backpressure Status" (Section 4.7, Panel 5)<br/>- **Panel**: "Backpressure Triggers" (Section 4.7, Panel 6)<br/>**Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` | **Tests**: ✅ Covered<br/>**Metrics**: ✅ Covered<br/>**Alerts**: ⚠️ Partial<br/>**Dashboards**: ✅ Covered | **Future Work**: Add dedicated alert rule for backpressure (medium priority) |

## Coverage Summary

### Overall Coverage Status

| Dimension | Covered | Partial | None | Total |
|-----------|---------|---------|------|-------|
| **Tests** | 18 | 3 | 0 | 21 |
| **Metrics** | 21 | 0 | 0 | 21 |
| **Alerts** | 12 | 4 | 5 | 21 |
| **Dashboards** | 20 | 1 | 0 | 21 |

### Coverage by Scenario Type

| Scenario Type | Count | Tests | Metrics | Alerts | Dashboards |
|---------------|-------|-------|---------|--------|------------|
| **Fault Injection (S1-S3)** | 3 | ✅ 3/3 | ✅ 3/3 | ⚠️ 2/3 | ✅ 3/3 |
| **JetStream Core (JS-XXX)** | 5 | ✅ 5/5 | ✅ 5/5 | ✅ 5/5 | ✅ 5/5 |
| **NATS Infrastructure (NATS-XXX)** | 8 | ✅ 6/8 | ✅ 8/8 | ⚠️ 4/8 | ✅ 7/8 |
| **Performance (PERF-XXX)** | 4 | ✅ 2/4 | ✅ 4/4 | ⚠️ 1/4 | ✅ 4/4 |
| **Total** | 20 | ✅ 16/20 | ✅ 20/20 | ⚠️ 12/20 | ✅ 19/20 |

## Future Work

### High Priority

#### 1. Alert Rules - Complete Partial Coverage

**Goal**: Achieve 100% alert coverage for all scenarios (currently 12/21 covered, 4/21 partial, 5/21 none)

**Tasks**:

1. **Link existing alerts to scenarios** (4 alerts need explicit links):
   - `RouterNATSAckFailuresHigh` → Link to S1 and NATS-004
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml:448-476`
     - **Action**: Add scenario ID comments and link in description
     - **Priority**: High (S1 is core fault injection scenario)
   
   - `RouterNATSPublishFailuresHigh` → Link to NATS-003
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml:389-417`
     - **Action**: Add scenario ID comments and link in description
     - **Priority**: High (NATS-003 is critical infrastructure scenario)
   
   - `RouterNATSNakFailuresHigh` → Link to NATS-006 and S2
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml:478-506`
     - **Action**: Add scenario ID comments and link in description
     - **Priority**: Medium (NAK failures are expected during redelivery, but should be monitored)
   
   - `RouterNATSSubscribeFailuresHigh` → Link to NATS-005
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml:507-535`
     - **Action**: Add scenario ID comments and link in description
     - **Priority**: High (NATS-005 needs alert coverage)

2. **Create missing alerts** (5 alerts need to be created):
   - **PERF-001**: Processing Latency Threshold Alert
     - **Metric**: `router_intake_processing_latency_p99`
     - **Threshold**: P99 latency > 5 seconds for > 5 minutes
     - **Severity**: warning
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (new alert)
     - **Priority**: High (critical for production performance monitoring)
   
   - **PERF-003**: In-Flight Messages Threshold Alert
     - **Metric**: `router_intake_inflight_messages`
     - **Threshold**: In-flight messages > 1000 for > 10 minutes
     - **Severity**: warning
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (new alert)
     - **Priority**: Medium (indicates processing backlog)
   
   - **PERF-004**: Backpressure Active Alert
     - **Metric**: `router_intake_backpressure_active`
     - **Threshold**: Backpressure active = 1 for > 5 minutes
     - **Severity**: warning
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (new alert)
     - **Priority**: Medium (backpressure indicates system overload)
   
   - **NATS-006**: NAK Failures Alert (if not covered by existing alert)
     - **Metric**: `router_nats_nak_failures_total`
     - **Threshold**: Rate > 10 NAK failures/sec for > 5 minutes
     - **Severity**: warning
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (verify if `RouterNATSNakFailuresHigh` covers this)
     - **Priority**: Low (NAK failures are expected during redelivery)
   
   - **NATS-008**: Publish with ACK Failures Alert
     - **Metric**: `router_nats_publish_with_ack_failures_total`
     - **Threshold**: Rate > 1 failure/sec for > 5 minutes
     - **Severity**: warning
     - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml` (new alert)
     - **Priority**: Low (publish_with_ack is less critical than regular publish)

3. **Add scenario ID links to all alert descriptions**:
   - Add "Scenario IDs: S1, NATS-004" to `RouterNATSAckFailuresHigh` description
   - Add "Scenario IDs: NATS-003" to `RouterNATSPublishFailuresHigh` description
   - Add "Scenario IDs: S2, NATS-006" to `RouterNATSNakFailuresHigh` description
   - Add "Scenario IDs: NATS-005" to `RouterNATSSubscribeFailuresHigh` description
   - Add "Scenario IDs: S2, JS-001, JS-004, JS-005" to redelivery alerts
   - Add "Scenario IDs: S3, JS-002" to MaxDeliver alerts
   - Add "Scenario IDs: S3, JS-003" to DLQ alerts
   - Add "Scenario IDs: NATS-001, NATS-002" to connection alerts

**Expected Outcome**: 21/21 scenarios with alert coverage (100%)

#### 2. Tests - Complete Partial Coverage

**Goal**: Achieve 100% test coverage for all scenarios (currently 18/21 covered, 3/21 partial)

**Tasks**:

1. **NATS-005**: Add dedicated fault injection test for NATS subscribe failures
   - **File**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` (new test)
   - **Test Name**: `test_nats_subscribe_failure_recovery/1`
   - **Scope**: Simulate subscribe failure, verify recovery, verify metrics
   - **Priority**: High (NATS-005 has partial test coverage)

2. **PERF-003**: Add dedicated fault injection test for in-flight messages
   - **File**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` (new test)
   - **Test Name**: `test_inflight_messages_under_load/1`
   - **Scope**: Simulate high load, verify in-flight message tracking, verify backpressure
   - **Priority**: Medium (PERF-003 has partial test coverage)

3. **NATS-008**: Add dedicated test for publish_with_ack failures
   - **File**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` (new test)
   - **Test Name**: `test_publish_with_ack_failure_recovery/1`
   - **Scope**: Simulate publish_with_ack failure, verify retry, verify metrics
   - **Priority**: Low (NATS-008 has partial test coverage)

**Expected Outcome**: 21/21 scenarios with test coverage (100%)

### Medium Priority

#### 1. Dashboard Panels - Complete Partial Coverage

**Goal**: Achieve 100% dashboard coverage for all scenarios (currently 20/21 covered, 1/21 partial)

**Tasks**:

1. **NATS-005**: Add panel for NATS subscribe failures breakdown
   - **File**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` (Section 4.4, new panel)
   - **Panel Name**: "NATS Subscribe Failures by Reason"
   - **Query**: `sum by (reason) (rate(router_nats_subscribe_failures_total[5m]))`
   - **Priority**: Medium (NATS-005 has partial dashboard coverage)

**Expected Outcome**: 21/21 scenarios with dashboard coverage (100%)

#### 2. Link-Back in OBS Documentation

**Goal**: Add scenario ID references to all alert and dashboard panel descriptions

**Tasks**:

1. **Alert Rules**: Add scenario IDs to all alert annotations.description
   - **File**: `apps/otp/router/docs/observability/router-alert-rules.yaml`
   - **Format**: Add "Related Scenarios: S1, NATS-004" to description
   - **Priority**: Medium (improves traceability)

2. **Dashboard Documentation**: Add scenario IDs to all panel descriptions
   - **File**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
   - **Format**: Add "Scenario IDs: S2, JS-001" to panel descriptions
   - **Priority**: Medium (improves traceability)

### Low Priority

#### 1. CI Integration

**Goal**: Integrate validation checks into CI/CD pipeline

**Tasks**:

1. **GitHub Actions**: Add validate_metrics_labels.sh to CI workflow
   - **File**: `.github/workflows/router-observability-validation.yml` (new workflow)
   - **Trigger**: On PR, on push to main
   - **Action**: Run `bash apps/otp/router/scripts/validate_metrics_labels.sh`
   - **Priority**: Low (validation can be run manually, but CI integration improves consistency)

2. **Drone CI**: Add validation step to Drone pipeline (if applicable)
   - **File**: `.drone.yml` (add step)
   - **Priority**: Low

3. **GitLab CI**: Add validation step to GitLab pipeline (if applicable)
   - **File**: `.gitlab-ci.yml` (add step)
   - **Priority**: Low

**Expected Outcome**: Automated validation on every PR and push

### Implementation Plan

#### Phase 1: Alert Rules (High Priority)
1. Link existing alerts to scenarios (4 alerts)
2. Create missing alerts (5 alerts)
3. Add scenario IDs to all alert descriptions
4. **Estimated Time**: 2-3 hours
5. **Owner**: wrk-6 (Security & Secrets) or wrk-9 (Documentation)

#### Phase 2: Tests (High Priority)
1. Add NATS-005 test (subscribe failures)
2. Add PERF-003 test (in-flight messages)
3. Add NATS-008 test (publish_with_ack failures)
4. **Estimated Time**: 4-6 hours
5. **Owner**: wrk-1 (Router Core) or test team

#### Phase 3: Dashboard Panels (Medium Priority)
1. Add NATS-005 panel (subscribe failures breakdown)
2. **Estimated Time**: 1 hour
5. **Owner**: wrk-9 (Documentation)

#### Phase 4: Link-Back in OBS (Medium Priority)
1. Update alert rules descriptions with scenario IDs
2. Update dashboard panel descriptions with scenario IDs
3. **Estimated Time**: 2-3 hours
5. **Owner**: wrk-9 (Documentation)

#### Phase 5: CI Integration (Low Priority)
1. Create GitHub Actions workflow
2. Test validation script in CI
3. Add to PR checklist
4. **Estimated Time**: 1-2 hours
5. **Owner**: wrk-5 (CI/CD) or wrk-9 (Documentation)

## References

- **Fault Injection Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **Coverage Analysis**: `apps/otp/router/docs/dev/OBS_COVERAGE_ANALYSIS.md`
- **Alert Rules**: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- **Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
- **Testing Guide**: `apps/otp/router/docs/dev/METRICS_LABELS_TESTING_GUIDE.md`

---

**Prepared by**: WORKER wrk-9 (Documentation & Developer Experience)  
**Status**: Coverage Matrix Complete  
**Date**: 2025-11-30

