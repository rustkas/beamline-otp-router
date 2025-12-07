# Quick Start: Router Alerts Setup

**Purpose**: Quick reference for setting up and verifying Router alerts.

## 1. Verify Alert Rules

```bash
# Check alert rules syntax and structure
cd apps/otp/router
./scripts/check_alert_rules.sh --verbose

# Or validate with promtool (if available)
promtool check rules docs/observability/router-alert-rules.yaml
```

## 2. Configure Prometheus

Edit `prometheus.yml`:

```yaml
global:
  external_labels:
    cluster: 'your-cluster-name'
    env: 'production'  # or 'staging'

rule_files:
  - "apps/otp/router/docs/observability/router-alert-rules.yaml"
```

Reload Prometheus:
```bash
curl -X POST http://localhost:9090/-/reload
```

## 3. Verify Alerts in Prometheus

```bash
# Check if rules are loaded
curl http://localhost:9090/api/v1/rules | \
  jq '.data.groups[] | select(.name | contains("router"))'

# Check if alerts are firing
curl http://localhost:9090/api/v1/alerts | \
  jq '.data.alerts[] | select(.labels.service == "router")'
```

## 4. Verify Alerts in Alertmanager

```bash
# Check active alerts
curl http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.service == "router")'

# Check alert labels
curl http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.service == "router") | .labels'
```

## 5. Test in Staging

```bash
# Run smoke test
cd apps/otp/router
./scripts/smoke_test_alerts.sh \
  --router-url http://router-staging:9001 \
  --prometheus-url http://prometheus-staging:9090 \
  --alertmanager-url http://alertmanager-staging:9093 \
  --wait-time 120
```

## 6. Verify Routing

Check that alerts are routed correctly:
- **Critical alerts** → PagerDuty (if configured)
- **Warning alerts** → Slack `#platform-alerts`
- **Staging alerts** → Slack `#platform-alerts-staging`

## Common Commands

### Check Alert Status

```bash
# All Router alerts
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.service == "router") | {
    alertname: .labels.alertname,
    severity: .labels.severity,
    state: .status.state
  }'

# Critical alerts only
curl -s http://localhost:9093/api/v2/alerts | \
  jq '.[] | select(.labels.service == "router" and .labels.severity == "critical")'
```

### Check Metrics

```bash
# Redelivery rate
curl 'http://localhost:9090/api/v1/query?query=rate(router_jetstream_redelivery_total[5m])'

# NATS connection status
curl 'http://localhost:9090/api/v1/query?query=router_nats_connection_status'

# MaxDeliver exhausted
curl 'http://localhost:9090/api/v1/query?query=rate(router_jetstream_maxdeliver_exhausted_total[5m])'
```

## Troubleshooting

### Alerts Not Firing

1. Check Prometheus is scraping Router:
   ```bash
   curl 'http://localhost:9090/api/v1/query?query=router_jetstream_ack_total'
   ```

2. Check alert expression:
   ```bash
   curl 'http://localhost:9090/api/v1/query?query=rate(router_jetstream_redelivery_total[5m])'
   ```

3. Check rules are loaded:
   ```bash
   curl http://localhost:9090/api/v1/rules | jq '.data.groups[] | select(.name | contains("router"))'
   ```

### Alerts Not Reaching Alertmanager

1. Check Prometheus alertmanager config:
   ```bash
   curl -s http://localhost:9090/api/v1/status/config | jq '.data.yaml' | grep alertmanager
   ```

2. Check Alertmanager is accessible:
   ```bash
   curl http://localhost:9093/-/healthy
   ```

### Wrong Labels

1. Check external labels in Prometheus:
   ```bash
   curl -s http://localhost:9090/api/v1/status/config | \
     jq -r '.data.yaml' | grep -A 3 external_labels
   ```

2. Reload Prometheus after changes:
   ```bash
   curl -X POST http://localhost:9090/-/reload
   ```

## References

- Full setup guide: `PROMETHEUS_SETUP.md`
- Routing verification: `ALERT_ROUTING_VERIFICATION.md`
- Thresholds review: `../dev/ALERT_THRESHOLDS_REVIEW.md`
- Alert rules: `router-alert-rules.yaml`

