# Alert Thresholds Review and Production Tuning

**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Review and adjust alert thresholds for production stability

## Summary

Alert thresholds and durations have been adjusted to reduce false positives while maintaining effective monitoring. All alerts now include routing labels (`team: platform`) for proper Alertmanager routing.

## Changes Made

### 1. Threshold Adjustments

#### JetStream Alerts

| Alert | Old Threshold | New Threshold | Change | Reason |
|-------|--------------|---------------|--------|--------|
| `RouterJetStreamHighRedeliveryRate` | 10% for 5m | 10% for 10m | Duration increased | Reduce false positives from temporary spikes |
| `RouterJetStreamHighRedeliveryFromSource` | 5/sec for 5m | 3/sec for 10m | Threshold lowered, duration increased | More sensitive to persistent issues |
| `RouterJetStreamMaxDeliverExhausted` | > 0 for 5m | > 0 for 2m | Duration decreased | Critical alert should fire faster |
| `RouterJetStreamGrowingRedeliveryQueue` | 100 ops for 10m | 200 ops for 15m | Threshold increased, duration increased | Reduce false positives, allow natural recovery |

#### NATS Connection Alerts

| Alert | Old Threshold | New Threshold | Change | Reason |
|-------|--------------|---------------|--------|--------|
| `RouterNatsFrequentReconnects` | 0.5/sec for 5m | 0.2/sec for 10m | Threshold lowered, duration increased | More sensitive to connection instability |
| `RouterNatsConnectionFailures` | 0.1/sec for 2m | 0.05/sec for 1m | Threshold lowered, duration decreased | Critical alert should fire faster |
| `RouterNatsReconnectionExhausted` | > 0 for 1m | > 0 for 1m | No change | Already optimal |
| `RouterNatsHighReconnectFailureRate` | 50% for 5m | 30% for 10m | Threshold lowered, duration increased | More sensitive to persistent issues |

#### NATS Operation Alerts

| Alert | Old Threshold | New Threshold | Change | Reason |
|-------|--------------|---------------|--------|--------|
| `RouterNatsHighPublishFailureRate` | 5% for 5m | 2% for 10m | Threshold lowered, duration increased | More sensitive to persistent issues |
| `RouterNatsHighPublishWithAckFailureRate` | 5% for 5m | 2% for 10m | Threshold lowered, duration increased | More sensitive to persistent issues |
| `RouterNatsHighAckFailureRate` | 5% for 5m | 1% for 5m | Threshold lowered | Critical alert, should be very sensitive |
| `RouterNatsHighNakFailureRate` | 5% for 5m | 2% for 10m | Threshold lowered, duration increased | More sensitive to persistent issues |
| `RouterNatsHighSubscribeFailureRate` | 10% for 5m | 5% for 5m | Threshold lowered | Critical alert, should be sensitive |
| `RouterNatsPendingOperationsQueueFull` | 500 ops for 5m | 1000 ops for 10m | Threshold increased, duration increased | Reduce false positives, allow natural recovery |

### 2. Duration Adjustments

**General Pattern**:
- **Warning alerts**: 10-15 minutes (reduced false positives)
- **Critical alerts**: 1-5 minutes (faster response to critical issues)

**Rationale**:
- Warning alerts can wait longer to confirm persistent issues
- Critical alerts need faster response time
- Longer durations reduce noise from temporary spikes

### 3. Rate Calculation Windows

All rate calculations use **5-minute windows** (`rate(...[5m])`), which is:
- Standard practice for Prometheus alerts
- Balances responsiveness with stability
- Provides enough data points for reliable rate calculation

### 4. Routing Labels Added

All alerts now include:
- `team: platform` - For Alertmanager routing to Platform team
- `service: router` - Service identifier
- `component: jetstream | nats` - Component identifier

**External Labels** (added by Prometheus `external_labels`):
- `env: staging | production` - Environment identifier
- `cluster: cluster-name` - Cluster identifier

These labels enable proper routing in Alertmanager to:
- Slack channels (by team)
- Email groups (by team)
- PagerDuty (by severity and team)
- Different channels for staging vs production

## Production Recommendations

### Initial Deployment

1. **Start with current thresholds** - They are tuned for production stability
2. **Monitor alert frequency** - Track how often each alert fires
3. **Adjust based on SLOs** - Tune thresholds to match your SLO requirements

### Threshold Tuning Guidelines

**If alerts fire too frequently**:
- Increase threshold (e.g., 10% → 15%)
- Increase duration (e.g., 10m → 15m)
- Check if underlying issue needs fixing

**If alerts don't fire when they should**:
- Decrease threshold (e.g., 10% → 5%)
- Decrease duration (e.g., 10m → 5m)
- Verify metrics are being collected correctly

### SLO-Based Tuning

**Example SLO**: 99.9% message processing success rate

**Redelivery Rate Alert**:
- If SLO allows 0.1% failures, redelivery rate should be < 0.1%
- Current threshold: 10% (100x SLO) - this is for detecting degradation, not SLO violation
- Consider adding separate SLO violation alert if needed

**MaxDeliver Exhaustion**:
- Should be 0 for healthy system
- Current threshold: > 0 for 2m - appropriate for critical alert

## Testing

### Smoke Test Script

Use `scripts/smoke_test_alerts.sh` to verify alerts fire correctly:

```bash
# Run smoke test in staging
./scripts/smoke_test_alerts.sh \
  --router-url http://router-staging:9001 \
  --prometheus-url http://prometheus-staging:9090 \
  --alertmanager-url http://alertmanager-staging:9093 \
  --wait-time 120
```

### Test Scenarios

1. **MaxDeliver Exhaustion**:
   - Enable fault injection: `router_nats_fault_injection:enable_fault(ack, {error, timeout})`
   - Send messages that will fail repeatedly
   - Verify `RouterJetStreamMaxDeliverExhausted` fires after 2 minutes

2. **NATS Connection Failure**:
   - Stop NATS server or enable connection fault
   - Verify `RouterNatsConnectionFailures` fires after 1 minute

3. **High Redelivery Rate**:
   - Enable ACK fault injection
   - Send messages that will fail processing
   - Verify `RouterJetStreamHighRedeliveryRate` fires after 10 minutes

## Monitoring Alert Health

### Key Metrics to Monitor

1. **Alert Frequency**:
   ```promql
   # Alert firing rate
   rate(alertmanager_alerts_received_total[5m])
   ```

2. **Alert Resolution Time**:
   ```promql
   # Time from firing to resolution
   alertmanager_alerts{state="firing"} - alertmanager_alerts{state="resolved"}
   ```

3. **False Positive Rate**:
   - Track alerts that fire but resolve without action
   - Adjust thresholds based on false positive rate

### Alert Dashboard

Create Grafana dashboard with:
- Alert firing frequency by alert name
- Alert duration (time to resolution)
- Alert by severity (warning vs critical)
- Alert by component (jetstream vs nats)

## References

- Alert rules file: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- Smoke test script: `apps/otp/router/scripts/smoke_test_alerts.sh`
- Alert validation script: `apps/otp/router/scripts/check_alert_rules.sh`
- Operational runbook: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`

