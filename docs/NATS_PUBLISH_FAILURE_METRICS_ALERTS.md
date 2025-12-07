# NATS Publish Failure Metrics and Alerts - SRE Recommendations

## Purpose

This document provides recommendations for SRE/operators on monitoring and alerting for `router_nats` publish and publish_with_ack failures. It complements the behavior specification in `NATS_PUBLISH_FAILURE_BEHAVIOR.md` with operational guidance.

## Current Metrics

### Primary Failure Metrics

**`router_nats_publish_with_ack_failures_total`** (counter)
- **Incremented when**: All types of `publish_with_ack` failures
  - `{error, Reason}` during connected state
  - `timeout` during connected state
  - `close_connection` during operation
  - Not connected state (operation queued)
- **Current labels**: None (counter only)
- **Recommended labels** (for future enhancement):
  - `reason`: Error reason (`nats_unavailable`, `timeout`, `connection_closed`, `not_connected`)
  - `error_type`: Error category (`operation_error`, `timeout`, `connection_lost`)
  - `mode`: Operation mode (`fail_open`, `queueing`)

**`router_nats_publish_failures_total`** (counter)
- **Incremented when**: All types of `publish` failures
- **Same failure scenarios as `publish_with_ack`**
- **Current labels**: None

### Queue Metrics

**`router_nats_pending_operations_count`** (gauge)
- **Current value**: Number of operations in retry queue
- **Max value**: Configurable (default: 1000)
- **Alert threshold**: Recommend warning at 80% capacity (800), critical at 95% (950)

**`router_nats_pending_operations_dropped_total`** (counter)
- **Incremented when**: Queue is full and oldest operation is dropped
- **Alert threshold**: Any increment should trigger warning (queue overflow)

### Connection Metrics

**`router_nats_connection_status`** (gauge)
- **Values**: `0.0` (disconnected), `0.5` (reconnecting), `1.0` (connected)
- **Alert threshold**: `0.0` for > 1 minute (critical)

## Recommended Alerts

### Critical Alerts

#### 1. High Publish_with_ack Failure Rate

```yaml
- alert: RouterNATSPublishWithAckHighFailureRate
  expr: |
    rate(router_nats_publish_with_ack_failures_total[5m]) 
    / 
    rate(router_nats_publish_with_ack_total[5m]) > 0.1
  for: 5m
  labels:
    severity: critical
    component: router
    error_type: publish_with_ack_failure
  annotations:
    summary: "Router NATS publish_with_ack failure rate is high"
    description: |
      Router NATS publish_with_ack failure rate is {{ $value | humanizePercentage }}.
      This indicates potential NATS connectivity or JetStream issues.
      Check NATS connection status and JetStream availability.
```

**Rationale**: 
- 10% failure rate indicates significant degradation
- `publish_with_ack` is critical for guaranteed delivery
- Failures may indicate NATS/JetStream unavailability

**Action Items**:
1. Check `router_nats_connection_status` gauge
2. Review NATS server logs for errors
3. Check JetStream stream/consumer status
4. Verify network connectivity to NATS

#### 2. Pending Operations Queue Full

```yaml
- alert: RouterNATSPendingQueueFull
  expr: router_nats_pending_operations_count >= 1000
  for: 2m
  labels:
    severity: critical
    component: router
    error_type: queue_overflow
  annotations:
    summary: "Router NATS pending operations queue is full"
    description: |
      Router NATS pending operations queue is at {{ $value }} (max: 1000).
      Operations are being dropped. Check NATS connectivity immediately.
```

**Rationale**:
- Queue full indicates prolonged NATS unavailability
- Operations are being dropped (message loss)
- Requires immediate investigation

**Action Items**:
1. Check NATS connection status
2. Verify NATS server availability
3. Check network connectivity
4. Review `router_nats_pending_operations_dropped_total` counter

#### 3. Connection Down for Extended Period

```yaml
- alert: RouterNATSConnectionDown
  expr: router_nats_connection_status == 0
  for: 1m
  labels:
    severity: critical
    component: router
    error_type: connection_lost
  annotations:
    summary: "Router NATS connection is down"
    description: |
      Router has been disconnected from NATS for {{ $for }}.
      Check NATS server status and network connectivity.
```

**Rationale**:
- Connection down prevents all NATS operations
- System may be in fail-open mode (message loss)
- Requires immediate attention

### Warning Alerts

#### 1. Moderate Publish_with_ack Failure Rate

```yaml
- alert: RouterNATSPublishWithAckModerateFailureRate
  expr: |
    rate(router_nats_publish_with_ack_failures_total[5m]) 
    / 
    rate(router_nats_publish_with_ack_total[5m]) > 0.05
  for: 10m
  labels:
    severity: warning
    component: router
    error_type: publish_with_ack_failure
  annotations:
    summary: "Router NATS publish_with_ack failure rate is elevated"
    description: |
      Router NATS publish_with_ack failure rate is {{ $value | humanizePercentage }}.
      Monitor for trends and check NATS connection status.
```

**Rationale**:
- 5% failure rate indicates potential issues
- May be transient (network hiccups)
- Monitor for trends

#### 2. Pending Operations Queue High

```yaml
- alert: RouterNATSPendingQueueHigh
  expr: router_nats_pending_operations_count >= 800
  for: 5m
  labels:
    severity: warning
    component: router
    error_type: queue_high
  annotations:
    summary: "Router NATS pending operations queue is high"
    description: |
      Router NATS pending operations queue is at {{ $value }} (80% of max: 1000).
      Monitor for queue growth and check NATS connectivity.
```

**Rationale**:
- 80% capacity indicates potential issues
- May indicate intermittent NATS problems
- Monitor for trends

#### 3. Operations Dropped

```yaml
- alert: RouterNATSOperationsDropped
  expr: increase(router_nats_pending_operations_dropped_total[5m]) > 0
  for: 1m
  labels:
    severity: warning
    component: router
    error_type: queue_overflow
  annotations:
    summary: "Router NATS operations dropped due to queue full"
    description: |
      Router NATS dropped {{ $value }} operations in the last 5 minutes.
      Queue overflow indicates prolonged NATS unavailability.
```

**Rationale**:
- Any dropped operations indicate message loss
- Queue overflow requires investigation
- May indicate configuration issues (queue too small)

## Recommended Dashboards

### Dashboard: NATS Publish Failure Overview

**Panels**:

1. **Publish_with_ack Failure Rate** (graph)
   ```promql
   rate(router_nats_publish_with_ack_failures_total[5m]) 
   / 
   rate(router_nats_publish_with_ack_total[5m])
   ```

2. **Publish Failure Rate** (graph)
   ```promql
   rate(router_nats_publish_failures_total[5m]) 
   / 
   rate(router_nats_publish_total[5m])
   ```

3. **Connection Status** (gauge)
   ```promql
   router_nats_connection_status
   ```

4. **Pending Operations Count** (gauge)
   ```promql
   router_nats_pending_operations_count
   ```

5. **Operations Dropped** (counter)
   ```promql
   rate(router_nats_pending_operations_dropped_total[5m])
   ```

6. **Retry Success Rate** (graph)
   ```promql
   rate(router_nats_pending_operations_retry_success[5m])
   /
   rate(router_nats_pending_operations_retry[5m])
   ```

## Operational Procedures

### When High Failure Rate Alert Fires

1. **Check Connection Status**:
   ```bash
   curl http://localhost:9001/metrics | grep router_nats_connection_status
   ```

2. **Check Failure Rate**:
   ```bash
   curl http://localhost:9001/metrics | grep router_nats_publish_with_ack_failures_total
   ```

3. **Review Logs**:
   ```bash
   cat router_*.jsonl | jq 'select(.error_code == "NATS_PUBLISH_WITH_ACK_ERROR")' | tail -20
   ```

4. **Check NATS Server**:
   - Verify NATS server is running
   - Check NATS server logs
   - Verify network connectivity

5. **Check Queue Status**:
   ```bash
   curl http://localhost:9001/metrics | grep router_nats_pending_operations_count
   ```

### When Queue Full Alert Fires

1. **Immediate Actions**:
   - Check NATS connection status
   - Verify NATS server availability
   - Check network connectivity

2. **Investigation**:
   - Review `router_nats_pending_operations_dropped_total` counter
   - Check logs for connection loss events
   - Verify reconnection attempts

3. **Mitigation**:
   - If NATS is down: Wait for automatic reconnection
   - If queue size too small: Consider increasing `nats_max_pending_operations`
   - If fail-open mode: Consider disabling for guaranteed delivery

## Future Enhancements

### Metric Labels

**Recommended labels for `router_nats_publish_with_ack_failures_total`**:

- `reason`: Error reason (for filtering by error type)
- `error_type`: Error category (for grouping)
- `mode`: Operation mode (for fail-open vs queueing analysis)

**Implementation**: Requires code changes to add labels to metric calls.

### Additional Metrics

**Recommended additional metrics**:

1. **`router_nats_publish_with_ack_failure_rate_by_reason`** (histogram)
   - Breakdown of failures by reason
   - Helps identify most common failure types

2. **`router_nats_operation_latency`** (histogram)
   - Latency of publish/publish_with_ack operations
   - Helps identify performance issues

3. **`router_nats_retry_count`** (histogram)
   - Number of retries per operation
   - Helps identify persistent issues

## References

- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Complete behavior specification
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Connection resilience documentation
- `apps/otp/router/docs/NATS_METRICS_COMPLIANCE.md` - Metrics compliance documentation
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Prometheus alerts documentation

