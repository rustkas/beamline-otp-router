# Prometheus Setup for Router Alerts

**Purpose**: Configure Prometheus to use Router alert rules with proper external labels for Alertmanager routing.

## 1. Prometheus Configuration

### Add Alert Rules to Prometheus

Edit your `prometheus.yml` (or `prometheus.yaml`):

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s
  external_labels:
    cluster: 'production'  # or 'staging', 'development'
    env: 'production'      # or 'staging', 'development'
    # team is added in alert rules themselves

# Rule files
rule_files:
  - "apps/otp/router/docs/observability/router-alert-rules.yaml"
  # Or use absolute path:
  # - "/path/to/router-alert-rules.yaml"

scrape_configs:
  # Router metrics
  - job_name: 'router'
    static_configs:
      - targets: ['router:9001']  # Router metrics endpoint
        labels:
          component: 'router'
          service: 'router'
```

### External Labels

**Required external labels** for proper Alertmanager routing:

```yaml
global:
  external_labels:
    cluster: 'production'    # Cluster identifier (e.g., 'us-east-1', 'eu-west-1')
    env: 'production'        # Environment (staging, production, development)
```

**Note**: `team` label is already included in alert rules (`team: platform`).

### Environment-Specific Configuration

**Staging**:
```yaml
global:
  external_labels:
    cluster: 'staging-cluster'
    env: 'staging'
```

**Production**:
```yaml
global:
  external_labels:
    cluster: 'production-cluster'
    env: 'production'
```

## 2. Alertmanager Configuration

### Routing Configuration

Configure Alertmanager to route alerts based on labels:

```yaml
route:
  receiver: 'default'
  group_by: ['alertname', 'cluster', 'env']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h
  
  routes:
    # Critical alerts to PagerDuty
    - match:
        severity: critical
        service: router
      receiver: 'pagerduty-router-critical'
      continue: true
    
    # Warning alerts to Slack
    - match:
        severity: warning
        service: router
        team: platform
      receiver: 'slack-platform'
    
    # Production critical alerts
    - match:
        env: production
        severity: critical
        service: router
      receiver: 'pagerduty-router-critical'
    
    # Staging alerts to Slack only
    - match:
        env: staging
        service: router
      receiver: 'slack-platform-staging'

receivers:
  - name: 'default'
    webhook_configs:
      - url: 'http://localhost:5001/webhook'
  
  - name: 'pagerduty-router-critical'
    pagerduty_configs:
      - service_key: 'YOUR_PAGERDUTY_SERVICE_KEY'
        severity: 'critical'
  
  - name: 'slack-platform'
    slack_configs:
      - api_url: 'YOUR_SLACK_WEBHOOK_URL'
        channel: '#platform-alerts'
        title: 'Router Alert: {{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'
  
  - name: 'slack-platform-staging'
    slack_configs:
      - api_url: 'YOUR_SLACK_WEBHOOK_URL'
        channel: '#platform-alerts-staging'
        title: '[STAGING] Router Alert: {{ .GroupLabels.alertname }}'
```

## 3. Verify Configuration

### Check Prometheus Rules

```bash
# Check if rules are loaded
curl http://localhost:9090/api/v1/rules | jq '.data.groups[] | select(.name | contains("router"))'

# Check specific rule group
curl http://localhost:9090/api/v1/rules | jq '.data.groups[] | select(.name == "router-jetstream.rules")'
```

### Check Alertmanager Alerts

```bash
# List all Router alerts
curl http://localhost:9093/api/v2/alerts | jq '.[] | select(.labels.service == "router")'

# Check alert labels
curl http://localhost:9093/api/v2/alerts | jq '.[] | select(.labels.service == "router") | .labels'
```

### Verify External Labels

```bash
# Check Prometheus configuration
curl http://localhost:9090/api/v1/status/config | jq '.data.yaml' | grep -A 5 external_labels

# Check labels on metrics
curl 'http://localhost:9090/api/v1/query?query=router_jetstream_ack_total' | jq '.data.result[0].metric'
```

## 4. Reload Prometheus

After configuration changes:

```bash
# Reload Prometheus configuration
curl -X POST http://localhost:9090/-/reload

# Or restart Prometheus
systemctl reload prometheus
# or
docker-compose restart prometheus
```

## 5. Test Alert Routing

### Check Alert Labels

All Router alerts should have these labels:
- `service: router`
- `component: jetstream | nats`
- `team: platform`
- `severity: warning | critical`
- `env: staging | production` (from external_labels)
- `cluster: cluster-name` (from external_labels)

### Verify in Alertmanager UI

1. Open Alertmanager UI: `http://localhost:9093`
2. Check "Alerts" tab
3. Verify Router alerts have correct labels
4. Check "Silences" tab to ensure alerts are not silenced

## 6. Integration with Existing Setup

### Local Development

For local development, use `tools/observability/prometheus.yml`:

```yaml
global:
  external_labels:
    cluster: 'local'
    env: 'development'  # Changed from 'environment' to 'env' for consistency
```

### Staging/Production

Update Prometheus configuration with:
- Correct `cluster` name
- Correct `env` value (staging/production)
- Alert rules file path
- Router metrics endpoint

## References

- Alert rules: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- Smoke test: `apps/otp/router/scripts/smoke_test_alerts.sh`
- Alert validation: `apps/otp/router/scripts/check_alert_rules.sh`
- Thresholds review: `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md`

