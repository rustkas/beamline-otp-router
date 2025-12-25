# NATS Connection Resilience - Production Monitoring Guide

## Overview

This document provides production-ready monitoring and alerting configuration for NATS connection resilience. Use this guide to set up Prometheus alerts and Grafana dashboards in production environments.

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete NATS resilience documentation.

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
    team: platform
  annotations:
    summary: "Router NATS connection is down"
    description: "Router has been disconnected from NATS for {{ $for }}. Check NATS server availability and network connectivity."
    runbook_url: "https://docs.example.com/runbooks/router-nats-connection-down"
```

**Action**: 
1. Check NATS server status
2. Verify network connectivity
3. Check Router logs for connection errors
4. Review `router_nats_connection_failures_total` metric

#### Reconnection Exhausted

```yaml
- alert: RouterNATSReconnectionExhausted
  expr: rate(router_nats_reconnection_exhausted_total[5m]) > 0
  for: 1m
  labels:
    severity: critical
    component: router
    team: platform
  annotations:
    summary: "Router NATS reconnection attempts exhausted"
    description: "Router has exhausted NATS reconnection attempts. Router is operating in fail-open mode (messages may be lost)."
    runbook_url: "https://docs.example.com/runbooks/router-nats-reconnection-exhausted"
```

**Action**:
1. Check NATS server availability
2. Verify Router configuration (`nats_reconnect_attempts`)
3. Review Router logs for reconnection failures
4. Consider enabling fail-open mode if appropriate

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
    team: platform
  annotations:
    summary: "Router NATS publish failure rate is high"
    description: "Router NATS publish failure rate is {{ $value | humanizePercentage }} (threshold: 10%)"
    runbook_url: "https://docs.example.com/runbooks/router-nats-publish-failures"
```

**Action**:
1. Check NATS server load
2. Review Router logs for publish errors
3. Verify message payload sizes
4. Check network latency

#### Pending Operations Queue Full

```yaml
- alert: RouterNATSPendingQueueFull
  expr: router_nats_pending_operations_count >= 1000
  for: 2m
  labels:
    severity: warning
    component: router
    team: platform
  annotations:
    summary: "Router NATS pending operations queue is full"
    description: "Router NATS pending operations queue is at {{ $value }} (max: 1000). Operations are being dropped."
    runbook_url: "https://docs.example.com/runbooks/router-nats-queue-full"
```

**Action**:
1. Check NATS connection status
2. Review Router logs for queue full warnings
3. Consider increasing `nats_max_pending_operations` if appropriate
4. Monitor queue retry success rate

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
    team: platform
  annotations:
    summary: "Router NATS reconnect failure rate is high"
    description: "Router NATS reconnect failure rate is {{ $value | humanizePercentage }} (threshold: 50%)"
    runbook_url: "https://docs.example.com/runbooks/router-nats-reconnect-failures"
```

**Action**:
1. Check NATS server availability
2. Verify network connectivity
3. Review Router logs for reconnection errors
4. Check NATS server connection limits

## Grafana Dashboard

### Connection Status Panel

**Query**:
```promql
router_nats_connection_status
```

**Visualization**: Gauge
- **Thresholds**: 
  - Green: `1.0` (connected)
  - Yellow: `0.5` (reconnecting)
  - Red: `0.0` (disconnected)

### Connection Events Panel

**Query**:
```promql
rate(router_nats_connection_established_total[5m])
rate(router_nats_connection_lost_total[5m])
rate(router_nats_connection_restored_total[5m])
```

**Visualization**: Time series graph
- **Legend**: `Established`, `Lost`, `Restored`

### Operation Success Rate Panel

**Query**:
```promql
(
  sum(rate(router_nats_publish_total[5m])) -
  sum(rate(router_nats_publish_failures_total[5m]))
) / sum(rate(router_nats_publish_total[5m]))
```

**Visualization**: Stat panel
- **Unit**: Percent (0-100)
- **Thresholds**: 
  - Green: `> 0.95` (95%+ success)
  - Yellow: `0.90 - 0.95` (90-95% success)
  - Red: `< 0.90` (<90% success)

### Queue Metrics Panel

**Query**:
```promql
router_nats_pending_operations_count
```

**Visualization**: Time series graph
- **Thresholds**: 
  - Warning: `>= 500` (50% of max)
  - Critical: `>= 1000` (max queue size)

### Retry Success Rate Panel

**Query**:
```promql
sum(rate(router_nats_pending_operations_retry_success[5m])) /
sum(rate(router_nats_pending_operations_retry[5m]))
```

**Visualization**: Stat panel
- **Unit**: Percent (0-100)
- **Thresholds**: 
  - Green: `> 0.90` (90%+ success)
  - Yellow: `0.80 - 0.90` (80-90% success)
  - Red: `< 0.80` (<80% success)

## Production Checklist

### Pre-Deployment

- [ ] Prometheus alerts configured and tested
- [ ] Grafana dashboard created and accessible
- [ ] Alert routing configured (PagerDuty, Slack, etc.)
- [ ] Runbook URLs updated with actual documentation links
- [ ] Metrics endpoint accessible (`/metrics` on port 9001)

### Post-Deployment

- [ ] Verify metrics are being scraped by Prometheus
- [ ] Verify alerts are firing correctly (test with fault injection)
- [ ] Verify Grafana dashboard shows correct data
- [ ] Monitor connection status for 24-48 hours
- [ ] Review alert frequency and adjust thresholds if needed

## Metric Verification

### Verify Metrics Are Exported

```bash
# Check metrics endpoint
curl http://localhost:9001/metrics | grep router_nats

# Expected metrics:
# router_nats_connection_status
# router_nats_connection_established_total
# router_nats_connection_lost_total
# router_nats_publish_total
# router_nats_publish_failures_total
# etc.
```

### Verify Metric Names Match Documentation

All metric names must match exactly:
- ✅ `router_nats_connection_status` (gauge)
- ✅ `router_nats_connection_established_total` (counter)
- ✅ `router_nats_connection_lost_total` (counter)
- ✅ `router_nats_publish_total` (counter)
- ✅ `router_nats_publish_failures_total` (counter)
- ✅ `router_nats_pending_operations_count` (gauge)

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete metrics list.

## Troubleshooting

### Alerts Not Firing

**Check**:
1. Prometheus is scraping Router metrics endpoint
2. Metric names match exactly (case-sensitive)
3. Alert expressions are correct
4. Alert thresholds are appropriate

### Metrics Not Appearing

**Check**:
1. Router is running and metrics endpoint is accessible
2. Prometheus scrape configuration includes Router
3. Router metrics module is loaded (`router_metrics_http:start/0`)
4. No firewall blocking port 9001

### Dashboard Not Showing Data

**Check**:
1. Prometheus data source is configured correctly
2. Time range is appropriate
3. Query syntax is correct
4. Metrics exist in Prometheus (query directly)

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete NATS resilience documentation
- `apps/otp/router/docs/NATS_METRICS_ALERTS.md` - Metrics and alerts reference
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - Operational procedures

