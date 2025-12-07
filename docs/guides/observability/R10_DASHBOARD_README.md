# R10 Circuit Breaker Dashboard

This directory contains observability artifacts for R10 (Retry Logic + Circuit Breaker).

## Files

- **`r10_dashboard.json`** - Grafana dashboard JSON (ready to import)
- **`r10_alerts.yaml`** - Prometheus/Alertmanager alert rules
- **`prometheus_r10_config_example.yml`** - Example Prometheus configuration

## Dashboard Import

### Step 1: Import Dashboard

1. Open Grafana → Dashboards → Import
2. Upload `r10_dashboard.json` or paste JSON content
3. Configure Prometheus datasource (set `${DS_PROMETHEUS}` variable)
4. Save dashboard

### Step 2: Configure Variables

The dashboard includes two template variables:
- **`tenant_id`**: Filter by tenant (auto-populated from metrics)
- **`provider_id`**: Filter by provider (auto-populated from metrics)

Both variables support "All" option to view aggregated metrics.

### Step 3: Verify Panels

After import, verify all panels display correctly:
- Circuit State (timeseries)
- Open Circuits (stat)
- Sliding Error Rate (timeseries)
- Trigger Reasons (bargauge)
- State Transitions (timeseries)
- Timeout Remaining (timeseries)

## Alert Rules Deployment

### Prometheus Configuration

Add to your `prometheus.yml`:

```yaml
rule_files:
  - "apps/otp/router/observability/r10_alerts.yaml"
```

Or copy `r10_alerts.yaml` to your Prometheus rules directory and reference it.

### Alertmanager Configuration

If using Alertmanager, ensure it's configured in Prometheus:

```yaml
alerting:
  alertmanagers:
    - static_configs:
        - targets:
            - alertmanager:9093
```

### Verify Alerts

After deployment:
1. Check Prometheus → Alerts page
2. Verify all 4 R10 alerts appear:
   - `R10CircuitOpenTooLong`
   - `R10HighErrorRate`
   - `R10CircuitFlapping`
   - `R10LatencyTriggerDominating`
3. Verify alert descriptions include runbook URLs

## Runbook Links

All alerts include runbook URLs pointing to:
```
https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

Update these URLs if your repository structure differs.

## Metrics Required

The dashboard and alerts require these metrics to be exported:

- `router_circuit_breaker_state` (gauge)
- `router_circuit_breaker_state_transitions_total` (counter)
- `router_circuit_breaker_trigger_reason` (gauge/info)
- `router_circuit_breaker_error_rate` (gauge)
- `router_circuit_breaker_timeout_remaining_ms` (gauge)
- `router_nats_publish_attempts_total` (counter)
- `router_nats_publish_errors_total` (counter)

Ensure your router metrics endpoint exports these metrics in Prometheus format.

## Troubleshooting

### Dashboard Shows "No Data"

1. Verify Prometheus datasource is configured correctly
2. Check that router metrics are being scraped
3. Verify metric names match exactly (case-sensitive)
4. Check time range selector (try "Last 5 minutes")

### Alerts Not Firing

1. Verify alert rules are loaded: `curl http://prometheus:9090/api/v1/rules`
2. Check alert expressions in Prometheus UI
3. Verify metrics exist: `curl http://prometheus:9090/api/v1/query?query=router_circuit_breaker_state`
4. Check alert evaluation interval matches your scrape interval

### Runbook Links Not Working

1. Verify GitHub repository URL is correct
2. Check file path matches your repository structure
3. Ensure file exists in the specified branch (main/master)
