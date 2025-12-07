# Alert Thresholds Summary

**Quick Reference**: Current thresholds for all Router alerts.

## JetStream Alerts

| Alert | Threshold | Duration | Severity | Rationale |
|-------|-----------|----------|----------|-----------|
| `RouterJetStreamHighRedeliveryRate` | 10% of total | 10m | warning | Redelivery rate > 10% indicates processing issues |
| `RouterJetStreamHighRedeliveryFromSource` | 3/sec | 10m | warning | Persistent redelivery from specific source |
| `RouterJetStreamMaxDeliverExhausted` | > 0/sec | 2m | **critical** | Messages lost or sent to DLQ |
| `RouterJetStreamGrowingRedeliveryQueue` | 200 ops OR redelivery > ACK | 15m | warning | Queue growing, not recovering |

## NATS Connection Alerts

| Alert | Threshold | Duration | Severity | Rationale |
|-------|-----------|----------|----------|-----------|
| `RouterNatsFrequentReconnects` | 0.2/sec | 10m | warning | Connection instability |
| `RouterNatsConnectionFailures` | 0.05/sec AND disconnected | 1m | **critical** | Cannot connect to NATS |
| `RouterNatsReconnectionExhausted` | > 0 events | 1m | **critical** | Fail-open mode activated |
| `RouterNatsHighReconnectFailureRate` | 30% of attempts | 10m | warning | Persistent connection issues |

## NATS Operation Alerts

| Alert | Threshold | Duration | Severity | Rationale |
|-------|-----------|----------|----------|-----------|
| `RouterNatsHighPublishFailureRate` | 2% of operations | 10m | warning | Publish operations failing |
| `RouterNatsHighPublishWithAckFailureRate` | 2% of operations | 10m | warning | Acknowledged publishes failing |
| `RouterNatsHighAckFailureRate` | 1% of operations | 5m | **critical** | ACK failures cause redeliveries |
| `RouterNatsHighNakFailureRate` | 2% of operations | 10m | warning | NAK failures prevent redelivery |
| `RouterNatsHighSubscribeFailureRate` | 5% of operations | 5m | **critical** | Cannot receive messages |
| `RouterNatsPendingOperationsQueueFull` | 1000 operations | 10m | warning | Queue full, operations dropped |

## Threshold Tuning Guidelines

### If Alerts Fire Too Frequently

**Increase threshold by 50-100%**:
- Example: 10% → 15-20%
- Example: 2% → 3-4%

**Increase duration by 50%**:
- Example: 10m → 15m
- Example: 5m → 7-8m

### If Alerts Don't Fire When They Should

**Decrease threshold by 25-50%**:
- Example: 10% → 5-7%
- Example: 2% → 1%

**Decrease duration (for critical only)**:
- Example: 10m → 5m (warning)
- Example: 5m → 2m (critical)

### Critical vs Warning

**Critical alerts** (immediate action required):
- MaxDeliver exhausted
- NATS connection failures
- ACK failures
- Subscribe failures

**Warning alerts** (attention needed):
- High redelivery rate
- Frequent reconnects
- Publish failures
- Queue growth

## Rate Calculation

All rate calculations use **5-minute windows**:
```promql
rate(metric_name[5m])
```

This provides:
- Enough data points for reliable calculation
- Balance between responsiveness and stability
- Standard Prometheus practice

## Production Recommendations

### Initial Deployment

1. **Start with current thresholds** - They are tuned for production
2. **Monitor for 24-48 hours** - Track alert frequency
3. **Adjust based on feedback** - Tune thresholds if needed

### Ongoing Tuning

1. **Monthly review** - Review alert effectiveness
2. **SLO-based adjustment** - Align thresholds with SLOs
3. **Remove unused alerts** - Clean up alerts that never fire

## Quick Reference Commands

### Check Current Thresholds

```bash
# View alert rules
cat apps/otp/router/docs/observability/router-alert-rules.yaml | \
  grep -A 5 "expr:"

# Check specific alert
grep -A 10 "RouterJetStreamHighRedeliveryRate" \
  apps/otp/router/docs/observability/router-alert-rules.yaml
```

### Test Thresholds

```bash
# Test alert expression in Prometheus
curl 'http://localhost:9090/api/v1/query?query=rate(router_jetstream_redelivery_total[5m])'

# Check if alert would fire
curl 'http://localhost:9090/api/v1/query?query=rate(router_jetstream_redelivery_total[5m]) / (rate(router_jetstream_ack_total[5m]) + rate(router_jetstream_redelivery_total[5m]) + 1)'
```

## References

- Full thresholds review: `ALERT_THRESHOLDS_REVIEW.md`
- Alert rules: `../observability/router-alert-rules.yaml`
- Setup guide: `../observability/PROMETHEUS_SETUP.md`

