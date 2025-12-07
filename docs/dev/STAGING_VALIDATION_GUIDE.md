# Staging Validation Guide: router_jetstream_redelivery_total

**Version**: 1.0  
**Date**: 2025-11-30  
**Purpose**: Step-by-step guide for validating redelivery metrics in staging environment

## Prerequisites

- Router deployed to staging environment
- Metrics endpoint accessible (default: `http://localhost:9000/metrics`)
- Prometheus configured and scraping router metrics
- Alertmanager configured (optional)
- Grafana dashboards configured (optional)

## Validation Steps

### 1. Check Metrics Endpoint

**Quick Check**:
```bash
curl http://localhost:9000/metrics | grep router_jetstream_redelivery_total
```

**Expected Output**:
```prometheus
# HELP router_jetstream_redelivery_total Total number of JetStream message redeliveries (NAK operations)
# TYPE router_jetstream_redelivery_total counter
router_jetstream_redelivery_total{assignment_id="test",reason="backoff",request_id="test",source="backoff"} 1
```

**Automated Check**:
```bash
./scripts/check_metrics_endpoint.sh http://localhost:9000/metrics
```

### 2. Trigger Redelivery Scenarios

#### Scenario 1: Tenant Validation Failure

**Action**: Send a result message with invalid tenant

**Expected**:
- Metric incremented with `reason="tenant_validation_failed"`, `source="tenant_validation"`
- Log entry: `"Message redelivery requested"` with context

**Verify**:
```bash
# Check metrics
curl http://localhost:9000/metrics | grep 'router_jetstream_redelivery_total{.*reason="tenant_validation_failed"'

# Check logs
grep "Message redelivery requested" .windsurf/reports/router_*.jsonl | grep tenant_validation
```

#### Scenario 2: Backpressure

**Action**: Trigger backpressure condition in decide consumer

**Expected**:
- Metric incremented with `reason="backpressure"`, `source="backpressure"`
- Log entry with backpressure context

**Verify**:
```bash
curl http://localhost:9000/metrics | grep 'router_jetstream_redelivery_total{.*reason="backpressure"'
```

#### Scenario 3: ACK/NAK Errors

**Action**: Simulate NATS ACK/NAK errors

**Expected**:
- Metric incremented with `reason="ack_error"` or `reason="nak_error"`
- Log entry with error context

**Verify**:
```bash
curl http://localhost:9000/metrics | grep 'router_jetstream_redelivery_total{.*reason="ack_error\|nak_error"'
```

### 3. Verify Prometheus Queries

**Query 1: Total redelivery rate**:
```promql
sum(rate(router_jetstream_redelivery_total[5m]))
```

**Query 2: Redelivery rate by source**:
```promql
sum by (source) (rate(router_jetstream_redelivery_total[5m]))
```

**Query 3: Redelivery rate by reason**:
```promql
sum by (reason) (rate(router_jetstream_redelivery_total[5m]))
```

**Query 4: Redelivery rate by source and reason**:
```promql
sum by (source, reason) (rate(router_jetstream_redelivery_total[5m]))
```

**Expected**: All queries return data with correct labels

### 4. Verify Alert Rules

**Check Alert Status**:
```bash
# If Alertmanager is accessible
curl http://localhost:9093/api/v2/alerts | jq '.[] | select(.labels.alertname | contains("RouterJetStream"))'
```

**Expected Alerts**:
- `RouterJetStreamHighRedeliveryRate` - fires when redelivery rate > threshold
- `RouterJetStreamHighRedeliveryFromSource` - fires when redelivery from specific source > threshold
- `RouterJetStreamMaxDeliverExhausted` - fires when MaxDeliver exhausted

**Trigger Alert**:
1. Generate high redelivery rate (e.g., multiple tenant validation failures)
2. Wait for alert evaluation period (default: 10m)
3. Check Alertmanager UI or API for `FIRING` status

### 5. Verify Grafana Dashboards

**Dashboard Checks**:
1. Open Router observability dashboard
2. Verify `router_jetstream_redelivery_total` appears in panel queries
3. Check that panels show data grouped by `source` and `reason`
4. Verify time series graphs display correctly

**Expected Panels**:
- Redelivery rate over time
- Redelivery rate by source (pie chart or bar chart)
- Redelivery rate by reason (bar chart)
- Redelivery rate by source and reason (table)

### 6. Verify Log Correlation

**Check Log Entries**:
```bash
# Find redelivery log entries
grep "Message redelivery requested" .windsurf/reports/router_*.jsonl | jq '.'

# Expected log format:
# {
#   "timestamp": "2025-11-30T12:00:00Z",
#   "level": "INFO",
#   "component": "router",
#   "message": "Message redelivery requested",
#   "context": {
#     "assignment_id": "test-assignment",
#     "request_id": "test-request",
#     "reason": "backoff",
#     "source": "backoff",
#     "delivery_count": 1,
#     "msg_id": "msg-123"
#   }
# }
```

**Correlate with Metrics**:
1. Note `assignment_id` and `request_id` from log entry
2. Query Prometheus for metric with same labels:
   ```promql
   router_jetstream_redelivery_total{assignment_id="test-assignment",request_id="test-request"}
   ```
3. Verify metric value matches expected count

### 7. Comprehensive Validation

**Run Automated Script**:
```bash
./scripts/validate_staging_observability.sh
```

**With Custom URLs**:
```bash
METRICS_URL=http://router-staging:9000/metrics \
PROMETHEUS_URL=http://prometheus-staging:9090 \
GRAFANA_URL=http://grafana-staging:3000 \
./scripts/validate_staging_observability.sh
```

**Expected Output**:
```
=== Staging Observability Validation ===
...
✓ Metrics endpoint accessible
✓ router_jetstream_redelivery_total found
✓ Metric has labels
✓ Prometheus accessible
✓ Metric queryable in Prometheus
✓ Alert rules reference router_jetstream_redelivery_total
...
=== Validation Summary ===
Errors: 0
Warnings: 0
✓ Validation PASSED
```

## Troubleshooting

### Metric Not Appearing

**Possible Causes**:
1. No redeliveries have occurred yet
2. Metrics endpoint not accessible
3. Prometheus not scraping router

**Solutions**:
1. Trigger fault injection scenarios
2. Check router logs for errors
3. Verify Prometheus scrape config

### Labels Missing

**Possible Causes**:
1. Old code version (before labels support)
2. Metrics emitted without context

**Solutions**:
1. Verify router version includes labels support
2. Check that `nak/3` is called with context

### Alerts Not Firing

**Possible Causes**:
1. Threshold too high
2. Evaluation period too long
3. Alert rules not loaded in Alertmanager

**Solutions**:
1. Lower threshold temporarily for testing
2. Reduce evaluation period
3. Verify alert rules in Alertmanager config

## Success Criteria

✅ **Metrics endpoint** returns `router_jetstream_redelivery_total` with labels  
✅ **Prometheus queries** return data with correct label filtering  
✅ **Alerts fire** under expected conditions  
✅ **Grafana dashboards** display data grouped by source/reason  
✅ **Log entries** correlate with metric values  
✅ **Automated validation** passes with 0 errors

## Next Steps

After successful validation:
1. Monitor metrics in production
2. Calibrate alert thresholds based on real traffic
3. Update dashboards based on operational insights
4. Document any operational learnings

