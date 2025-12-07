# Alert Routing Verification Guide

**Purpose**: Verify that Router alerts are properly routed to Alertmanager, Slack, email, and PagerDuty.

## Quick Verification Checklist

- [ ] Prometheus is scraping Router metrics
- [ ] Alert rules are loaded in Prometheus
- [ ] Alerts are firing in Prometheus
- [ ] Alerts are received by Alertmanager
- [ ] Alert labels are correct (service, component, team, env, cluster)
- [ ] Alerts are routed to correct channels (Slack/email/PagerDuty)
- [ ] Alert notifications are received

## Step-by-Step Verification

### 1. Check Prometheus Rules

```bash
# Check if Router alert rules are loaded
curl -s http://localhost:9090/api/v1/rules | \
  jq '.data.groups[] | select(.name | contains("router")) | {name: .name, rules: [.rules[] | .name]}'

# Expected output:
# {
#   "name": "router-jetstream.rules",
#   "rules": [
#     "RouterJetStreamHighRedeliveryRate",
#     "RouterJetStreamMaxDeliverExhausted",
#     ...
#   ]
# }
```

### 2. Check Alert Labels

```bash
# Get all Router alerts with their labels
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.service == "router") | {
    alertname: .labels.alertname,
    service: .labels.service,
    component: .labels.component,
    team: .labels.team,
    severity: .labels.severity,
    env: .labels.env,
    cluster: .labels.cluster
  }'

# Expected labels for each alert:
# {
#   "alertname": "RouterJetStreamHighRedeliveryRate",
#   "service": "router",
#   "component": "jetstream",
#   "team": "platform",
#   "severity": "warning",
#   "env": "production",  # or "staging"
#   "cluster": "production-cluster"  # or your cluster name
# }
```

### 3. Verify Alert Routing

#### Check Alertmanager Routes

```bash
# Check Alertmanager configuration
curl -s http://localhost:9093/api/v2/status | jq '.data.configYAML' | grep -A 10 routes

# Or check Alertmanager UI:
# http://localhost:9093/#/status
```

#### Test Alert Routing

1. **Trigger a test alert** (in staging only!):
   ```bash
   # Use smoke test script
   ./scripts/smoke_test_alerts.sh \
     --router-url http://router-staging:9001 \
     --prometheus-url http://prometheus-staging:9090 \
     --alertmanager-url http://alertmanager-staging:9093
   ```

2. **Check if alert is received**:
   ```bash
   # Check Alertmanager
   curl -s http://localhost:9093/api/v2/alerts | \
     jq '.[] | select(.labels.alertname == "RouterJetStreamMaxDeliverExhausted")'
   ```

3. **Verify notification was sent**:
   - Check Slack channel: `#platform-alerts` (or configured channel)
   - Check email inbox (if configured)
   - Check PagerDuty (for critical alerts)

### 4. Verify External Labels

External labels (`env`, `cluster`) are added by Prometheus, not in alert rules.

```bash
# Check Prometheus external labels
curl -s http://localhost:9090/api/v1/status/config | \
  jq -r '.data.yaml' | \
  grep -A 3 external_labels

# Expected:
# external_labels:
#   cluster: 'production-cluster'
#   env: 'production'
```

**If labels are missing**:
1. Update `prometheus.yml`:
   ```yaml
   global:
     external_labels:
       cluster: 'your-cluster-name'
       env: 'production'  # or 'staging'
   ```
2. Reload Prometheus: `curl -X POST http://localhost:9090/-/reload`

### 5. Check Alert Grouping

Alertmanager groups alerts by `alertname`, `cluster`, `env`:

```bash
# Check active alert groups
curl -s http://localhost:9093/api/v2/alerts | \
  jq 'group_by(.labels.alertname) | map({
    alertname: .[0].labels.alertname,
    count: length,
    labels: .[0].labels
  })'
```

### 6. Verify Severity-Based Routing

#### Critical Alerts

Should go to:
- PagerDuty (if configured)
- Slack `#platform-alerts-critical`
- Email (if configured)

```bash
# Check critical alerts
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.severity == "critical" and .labels.service == "router")'
```

#### Warning Alerts

Should go to:
- Slack `#platform-alerts`
- Email (if configured)
- NOT to PagerDuty

```bash
# Check warning alerts
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.severity == "warning" and .labels.service == "router")'
```

### 7. Environment-Specific Routing

#### Production Alerts

```bash
# Production critical alerts should go to PagerDuty
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.env == "production" and .labels.severity == "critical")'
```

#### Staging Alerts

```bash
# Staging alerts should go to Slack only
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.env == "staging")'
```

## Common Issues

### Issue: Alerts not firing

**Check**:
1. Prometheus is scraping Router metrics:
   ```bash
   curl 'http://localhost:9090/api/v1/query?query=router_jetstream_ack_total'
   ```
2. Alert rules are loaded:
   ```bash
   curl http://localhost:9090/api/v1/rules | jq '.data.groups[] | select(.name | contains("router"))'
   ```
3. Alert expression evaluates to true:
   ```bash
   curl 'http://localhost:9090/api/v1/query?query=rate(router_jetstream_redelivery_total[5m])'
   ```

### Issue: Alerts not reaching Alertmanager

**Check**:
1. Prometheus is configured to send to Alertmanager:
   ```bash
   curl -s http://localhost:9090/api/v1/status/config | jq '.data.yaml' | grep alertmanager
   ```
2. Alertmanager is accessible:
   ```bash
   curl http://localhost:9093/-/healthy
   ```

### Issue: Wrong labels on alerts

**Check**:
1. External labels in Prometheus config
2. Alert rule labels (should have `team: platform`)
3. Reload Prometheus after changes

### Issue: Alerts not routed correctly

**Check**:
1. Alertmanager routing configuration
2. Label matching in routes
3. Receiver configuration (Slack/PagerDuty/email)

## Testing in Staging

### Manual Test

1. **Trigger MaxDeliver exhaustion**:
   ```erlang
   % In Router Erlang console
   router_nats_fault_injection:enable_fault(ack, {error, timeout}).
   ```

2. **Wait for alert** (2 minutes for critical):
   ```bash
   watch -n 5 'curl -s http://localhost:9093/api/v2/alerts | jq ".[] | select(.labels.alertname == \"RouterJetStreamMaxDeliverExhausted\")"'
   ```

3. **Verify notification**:
   - Check Slack channel
   - Check PagerDuty (if critical)
   - Check email (if configured)

4. **Clean up**:
   ```erlang
   router_nats_fault_injection:clear_all_faults().
   ```

### Automated Test

Use smoke test script:

```bash
./scripts/smoke_test_alerts.sh \
  --router-url http://router-staging:9001 \
  --prometheus-url http://prometheus-staging:9090 \
  --alertmanager-url http://alertmanager-staging:9093 \
  --wait-time 120
```

## References

- Prometheus setup: `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`
- Alert rules: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- Smoke test: `apps/otp/router/scripts/smoke_test_alerts.sh`

