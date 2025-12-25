# NATS Metrics and Alerts

## Overview

This document provides Prometheus metrics and alerting rules for NATS connection resilience monitoring.

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete NATS resilience documentation.

## Prometheus Metrics

### Connection Metrics

```promql
# Connection status (gauge: 0.0 = disconnected, 0.5 = reconnecting, 1.0 = connected)
router_nats_connection_status

# Connection events (counters)
router_nats_connection_established_total
router_nats_connection_lost_total
router_nats_connection_restored_total
router_nats_connection_failures_total
```

### Reconnection Metrics

```promql
# Reconnection attempts and failures (counters)
router_nats_reconnect_attempts_total
router_nats_reconnect_failures_total
router_nats_reconnection_exhausted_total
```

### Operation Metrics

```promql
# Publish operations (counters)
router_nats_publish_total
router_nats_publish_failures_total
router_nats_publish_with_ack_total
router_nats_publish_with_ack_failures_total

# ACK/NAK operations (counters)
router_nats_ack_total
router_nats_ack_failures_total
router_nats_nak_total
router_nats_nak_failures_total

# Subscribe operations (counters)
router_nats_subscribe_total
router_nats_subscribe_failures_total
```

### Queue Metrics

```promql
# Pending operations (gauge)
router_nats_pending_operations_count

# Retry operations (counters)
router_nats_pending_operations_retry
router_nats_pending_operations_retry_success
router_nats_pending_operations_retry_failed
router_nats_pending_operations_dropped_total
```

## Prometheus Alerts

### Critical Alerts

#### NATS Connection Down

```yaml
- alert: RouterNATSConnectionDown
  expr: router_nats_connection_status == 0
  for: 1m
  labels:
    severity: critical
    component: router
  annotations:
    summary: "Router NATS connection is down"
    description: "Router has been disconnected from NATS for {{ $for }}"
```

#### Reconnection Exhausted

```yaml
- alert: RouterNATSReconnectionExhausted
  expr: rate(router_nats_reconnection_exhausted_total[5m]) > 0
  for: 1m
  labels:
    severity: critical
    component: router
  annotations:
    summary: "Router NATS reconnection attempts exhausted"
    description: "Router has exhausted NATS reconnection attempts"
```

### Warning Alerts

#### High Publish Failure Rate

```yaml
- alert: RouterNATSPublishFailureRate
  expr: |
    (
      sum(rate(router_nats_publish_failures_total[5m])) /
      sum(rate(router_nats_publish_total[5m]))
    ) > 0.1
  for: 5m
  labels:
    severity: warning
    component: router
  annotations:
    summary: "Router NATS publish failure rate is high"
    description: "Router NATS publish failure rate is {{ $value | humanizePercentage }} (threshold: 10%)"
```

#### Pending Operations Queue Full

```yaml
- alert: RouterNATSPendingQueueFull
  expr: router_nats_pending_operations_count >= 1000
  for: 2m
  labels:
    severity: warning
    component: router
  annotations:
    summary: "Router NATS pending operations queue is full"
    description: "Router NATS pending operations queue is at {{ $value }} (max: 1000)"
```

#### High Reconnect Failure Rate

```yaml
- alert: RouterNATSReconnectFailureRate
  expr: |
    (
      sum(rate(router_nats_reconnect_failures_total[5m])) /
      sum(rate(router_nats_reconnect_attempts_total[5m]))
    ) > 0.5
  for: 5m
  labels:
    severity: warning
    component: router
  annotations:
    summary: "Router NATS reconnect failure rate is high"
    description: "Router NATS reconnect failure rate is {{ $value | humanizePercentage }} (threshold: 50%)"
```

## Grafana Dashboard Queries

### Connection Status Panel

```promql
# Connection status gauge
router_nats_connection_status
```

### Connection Events Panel

```promql
# Connection events over time
rate(router_nats_connection_established_total[5m])
rate(router_nats_connection_lost_total[5m])
rate(router_nats_connection_restored_total[5m])
```

### Operation Success Rate Panel

```promql
# Publish success rate
(
  sum(rate(router_nats_publish_total[5m])) -
  sum(rate(router_nats_publish_failures_total[5m]))
) / sum(rate(router_nats_publish_total[5m]))

# ACK success rate
(
  sum(rate(router_nats_ack_total[5m])) -
  sum(rate(router_nats_ack_failures_total[5m]))
) / sum(rate(router_nats_ack_total[5m]))
```

### Queue Metrics Panel

```promql
# Pending operations count
router_nats_pending_operations_count

# Retry success rate
sum(rate(router_nats_pending_operations_retry_success[5m])) /
sum(rate(router_nats_pending_operations_retry[5m]))
```

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete NATS resilience documentation
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - General Prometheus alerts documentation

