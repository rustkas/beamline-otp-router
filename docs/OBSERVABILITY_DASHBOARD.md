# Router Observability Metrics Dashboard (CP2 Planning)

**Date**: 2025-11-30  
**Component**: Router (`apps/otp/router/`)  
**Status**: 📋 **PLANNING DOCUMENT** (CP2)  
**Current Phase**: CP1 (Complete) → CP2 (Planned)

---

## Overview

This document defines the planned Grafana dashboard for Router observability metrics (CP2). The dashboard will visualize Prometheus metrics exported by Router when CP2 observability features are implemented.

**Note**: This is a **planning document** for CP2. Metrics are not yet implemented in CP1. See `docs/OBSERVABILITY_CP2_PLANNING.md` for CP2 implementation plan.

---

## Planned Metrics

### Request Metrics

**Counter**: `router_requests_total`
- **Description**: Total number of requests processed by Router
- **Labels**: `status` (success|error), `provider` (openai|anthropic|...)
- **Query**: `sum(rate(router_requests_total[5m]))`

**Histogram**: `router_request_duration_seconds`
- **Description**: Request processing latency in seconds
- **Labels**: `status` (success|error), `provider` (openai|anthropic|...)
- **Buckets**: `0.1, 0.5, 1.0, 2.5, 5.0, 10.0`
- **Query (p95)**: `histogram_quantile(0.95, rate(router_request_duration_seconds_bucket[5m]))`

**Counter**: `router_requests_by_status`
- **Description**: Requests by status code
- **Labels**: `status` (success|error|timeout)
- **Query**: `sum by (status) (rate(router_requests_by_status[5m]))`

**Counter**: `router_requests_by_provider`
- **Description**: Requests by provider
- **Labels**: `provider` (openai|anthropic|...)
- **Query**: `sum by (provider) (rate(router_requests_by_provider[5m]))`

### Error Metrics

**Counter**: `router_errors_total`
- **Description**: Total number of errors
- **Labels**: `error_type` (nats|routing|policy|provider)
- **Query**: `sum(rate(router_errors_total[5m]))`

**Counter**: `router_errors_by_type`
- **Description**: Errors by type
- **Labels**: `error_type` (nats|routing|policy|provider)
- **Query**: `sum by (error_type) (rate(router_errors_by_type[5m]))`

**Counter**: `router_nats_errors_total`
- **Description**: NATS-specific errors
- **Labels**: `subject` (beamline.router.v1.decide|...)
- **Query**: `sum(rate(router_nats_errors_total[5m]))`

**Counter**: `router_routing_errors_total`
- **Description**: Routing decision errors
- **Labels**: `reason` (policy_not_found|provider_unavailable|...)
- **Query**: `sum(rate(router_routing_errors_total[5m]))`

### Connection Metrics

**Gauge**: `router_active_connections`
- **Description**: Active NATS connections
- **Labels**: None
- **Query**: `router_active_connections`

**Counter**: `router_nats_messages_total`
- **Description**: Total NATS messages processed
- **Labels**: `subject` (beamline.router.v1.decide|...)
- **Query**: `sum(rate(router_nats_messages_total[5m]))`

**Counter**: `router_nats_messages_by_subject`
- **Description**: NATS messages by subject
- **Labels**: `subject` (beamline.router.v1.decide|...)
- **Query**: `sum by (subject) (rate(router_nats_messages_by_subject[5m]))`

### Performance Metrics

**Histogram**: `router_policy_load_duration_seconds`
- **Description**: Policy loading time
- **Labels**: `source` (cache|database)
- **Query (p95)**: `histogram_quantile(0.95, rate(router_policy_load_duration_seconds_bucket[5m]))`

**Histogram**: `router_provider_selection_duration_seconds`
- **Description**: Provider selection time
- **Labels**: `method` (weighted|round_robin|...)
- **Query (p95)**: `histogram_quantile(0.95, rate(router_provider_selection_duration_seconds_bucket[5m]))`

**Counter**: `router_cache_hits_total`
- **Description**: Cache hits
- **Labels**: `cache_type` (policy|provider|...)
- **Query**: `sum(rate(router_cache_hits_total[5m]))`

**Counter**: `router_cache_misses_total`
- **Description**: Cache misses
- **Labels**: `cache_type` (policy|provider|...)
- **Query**: `sum(rate(router_cache_misses_total[5m]))`

---

## Grafana Dashboard Structure

### Dashboard Panels

#### 1. Request Rate Panel

**Title**: Request Rate  
**Type**: Time Series  
**Query**: `sum(rate(router_requests_total[5m]))`  
**Y-Axis**: Requests per second  
**Legend**: `{{status}} - {{provider}}`

**Description**: Shows the rate of requests processed by Router over time, broken down by status and provider.

#### 2. Request Latency Panel (p50, p95, p99)

**Title**: Request Latency  
**Type**: Time Series  
**Queries**:
- p50: `histogram_quantile(0.50, rate(router_request_duration_seconds_bucket[5m]))`
- p95: `histogram_quantile(0.95, rate(router_request_duration_seconds_bucket[5m]))`
- p99: `histogram_quantile(0.99, rate(router_request_duration_seconds_bucket[5m]))`

**Y-Axis**: Seconds  
**Legend**: `p{{quantile}}`

**Description**: Shows request latency percentiles (p50, p95, p99) over time.

#### 3. Error Rate Panel

**Title**: Error Rate  
**Type**: Time Series  
**Query**: `sum(rate(router_errors_total[5m]))`  
**Y-Axis**: Errors per second  
**Legend**: `{{error_type}}`

**Description**: Shows the rate of errors over time, broken down by error type.

#### 4. Requests by Provider Panel

**Title**: Requests by Provider  
**Type**: Pie Chart  
**Query**: `sum by (provider) (rate(router_requests_by_provider[5m]))`  
**Legend**: `{{provider}}`

**Description**: Shows the distribution of requests across different providers.

#### 5. Active Connections Panel

**Title**: Active NATS Connections  
**Type**: Stat  
**Query**: `router_active_connections`  
**Unit**: None

**Description**: Shows the current number of active NATS connections.

#### 6. NATS Messages Panel

**Title**: NATS Messages Rate  
**Type**: Time Series  
**Query**: `sum(rate(router_nats_messages_total[5m]))`  
**Y-Axis**: Messages per second  
**Legend**: `{{subject}}`

**Description**: Shows the rate of NATS messages processed over time, broken down by subject.

#### 7. Cache Hit Rate Panel

**Title**: Cache Hit Rate  
**Type**: Time Series  
**Query**: `sum(rate(router_cache_hits_total[5m])) / (sum(rate(router_cache_hits_total[5m])) + sum(rate(router_cache_misses_total[5m])))`  
**Y-Axis**: Hit rate (0-1)  
**Unit**: Percent (0-100)

**Description**: Shows the cache hit rate over time.

#### 8. Policy Load Duration Panel

**Title**: Policy Load Duration  
**Type**: Time Series  
**Query**: `histogram_quantile(0.95, rate(router_policy_load_duration_seconds_bucket[5m]))`  
**Y-Axis**: Seconds  
**Legend**: `{{source}}`

**Description**: Shows policy loading duration (p95) over time, broken down by source (cache|database).

---

## Dashboard Panels for Load/Chaos/Rate-Limit Scenarios

### Load Test Scenarios

#### Panel: Intake Message Processing Rate
**Title**: Intake Message Processing Rate  
**Type**: Time Series  
**Query**: `rate(router_intake_messages_total[5m])`  
**Legend**: `{{status}} - {{subject}}`  
**What to Look For**:
- **High-Volume Success**: Steady rate matching sent message count, `status="ok"` ≈ N
- **High-Volume Error**: Rate matches error count, `status="failed"` ≈ N
- **Mixed Stream**: Rate reflects distribution (70% ok, 30% failed)

#### Panel: Validation Errors Over Time
**Title**: Validation Errors  
**Type**: Time Series  
**Query**: `rate(router_intake_validation_errors_total[5m])`  
**Legend**: `{{error_code}}`  
**What to Look For**:
- **Success Flood**: Should be 0 throughout
- **Error Flood**: Should match error message rate
- **Mixed Stream**: Should reflect error distribution

#### Panel: DLQ Status
**Title**: DLQ Messages and Failures  
**Type**: Stacked Area Chart  
**Queries**:
- `rate(router_intake_dlq_messages_total[5m])` (DLQ Messages)
- `rate(router_intake_dlq_publish_failed_total[5m])` (DLQ Failures)
**What to Look For**:
- **Success Flood**: Both metrics = 0
- **Error Flood**: `dlq_messages_total` ≈ error count, `dlq_publish_failed_total` = 0
- **DLQ Failures**: If `dlq_publish_failed_total` > 0, investigate NATS/DLQ issues

#### Panel: Idempotency Metrics
**Title**: Idempotency Hits vs Misses  
**Type**: Stacked Bar Chart  
**Queries**:
- `rate(router_idempotency_hit_total[5m])` (Hits)
- `rate(router_idempotency_miss_total[5m])` (Misses)
**What to Look For**:
- **Idempotency Stress**: `hit_total` ≈ duplicates, `miss_total` ≈ unique messages
- **Ratio**: `hit_total / (hit_total + miss_total)` should match duplicate ratio

### Chaos Test Scenarios

#### Panel: NATS Connection Status
**Title**: NATS Connection Status and Reconnects  
**Type**: Time Series (Status) + Counter (Reconnects)  
**Queries**:
- `router_nats_connection_status` (Status: 1=connected, 0=disconnected)
- `rate(router_nats_reconnect_total[5m])` (Reconnects)
**What to Look For**:
- **Single Restart**: Status drops to 0, then returns to 1; `reconnect_total` = 1
- **Multiple Restarts**: Status oscillates; `reconnect_total` = N_restarts
- **Recovery**: Status returns to 1 after each restart

#### Panel: NATS Connection Errors
**Title**: NATS Connection Errors  
**Type**: Time Series  
**Query**: `rate(router_nats_connection_errors_total[5m])`  
**What to Look For**:
- **During Restart**: Errors spike during NATS downtime
- **After Recovery**: Errors return to 0
- **No Infinite Errors**: Errors don't continue after recovery

#### Panel: Message Processing During Chaos
**Title**: Message Processing During Chaos  
**Type**: Time Series  
**Query**: `rate(router_intake_messages_total[5m])`  
**What to Look For**:
- **During Restart**: May drop to 0 or decrease
- **After Recovery**: Resumes normal rate
- **No Message Loss**: Total messages processed = messages sent (after recovery)

### Abuse Detection Scenarios (CP2+)

#### Panel: Router Abuse Events by Type
**Title**: Router Abuse Events by Type  
**Type**: Stacked Area Chart  
**Query**: `rate(router_abuse_heavy_payload_total[5m])`  
**Y-axis**: Events/second  
**Legend**: `{{abuse_type}}`  
**What to Look For**:
- **Heavy Payload Pattern**: High rate indicates resource exhaustion attempts
- **Correlation with Gateway**: Router abuse should correlate with Gateway abuse events
- **Tenant Analysis**: Which tenants trigger heavy payload abuse

#### Panel: Payload Size Distribution
**Title**: Payload Size Distribution  
**Type**: Histogram  
**Query**: `rate(router_payload_size_bytes_bucket[5m])`  
**Y-axis**: Request count  
**X-axis**: Payload size buckets  
**What to Look For**:
- **Large Payload Anomalies**: Sudden increase in large payloads (>500KB)
- **Distribution Changes**: Shift in payload size distribution over time
- **Abuse Indicators**: High ratio of large payloads (>80%) indicates abuse

#### Panel: Abuse Events by Tenant
**Title**: Abuse Events by Tenant  
**Type**: Table  
**Query**: `sum by (tenant_id) (rate(router_abuse_heavy_payload_total[5m]))`  
**Columns**: Tenant ID, Abuse Event Rate  
**What to Look For**:
- **Top Abusers**: Tenants with highest abuse event rates
- **Pattern Analysis**: Which tenants trigger heavy payload abuse
- **Response Actions**: Effectiveness of blocking/rate limiting

#### Panel: Abuse Events vs Processing Rate
**Title**: Abuse Events vs Processing Rate  
**Type**: Dual Y-axis Time Series  
**Query**: `rate(router_abuse_heavy_payload_total[5m])` (Abuse Events)  
**Query**: `rate(router_intake_messages_total[5m])` (Processing Rate)  
**Y-axis**: Events/second (left), Messages/second (right)  
**What to Look For**:
- **Correlation**: Abuse events should correlate with processing rate
- **Impact**: High abuse rate may impact processing performance
- **Response Effectiveness**: Reduction in abuse events after response actions

## Grafana Dashboard JSON Template

**Note**: This is a placeholder template for CP2. Actual metrics must be implemented before this dashboard can be used.

```json
{
  "dashboard": {
    "title": "Router Observability Dashboard",
    "tags": ["router", "observability", "cp2"],
    "timezone": "browser",
    "schemaVersion": 30,
    "version": 1,
    "refresh": "30s",
    "panels": [
      {
        "id": 1,
        "title": "Request Rate",
        "type": "timeseries",
        "targets": [
          {
            "expr": "sum(rate(router_requests_total[5m]))",
            "legendFormat": "{{status}} - {{provider}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 0, "y": 0}
      },
      {
        "id": 2,
        "title": "Request Latency (p50, p95, p99)",
        "type": "timeseries",
        "targets": [
          {
            "expr": "histogram_quantile(0.50, rate(router_request_duration_seconds_bucket[5m]))",
            "legendFormat": "p50"
          },
          {
            "expr": "histogram_quantile(0.95, rate(router_request_duration_seconds_bucket[5m]))",
            "legendFormat": "p95"
          },
          {
            "expr": "histogram_quantile(0.99, rate(router_request_duration_seconds_bucket[5m]))",
            "legendFormat": "p99"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 12, "y": 0}
      },
      {
        "id": 3,
        "title": "Error Rate",
        "type": "timeseries",
        "targets": [
          {
            "expr": "sum(rate(router_errors_total[5m]))",
            "legendFormat": "{{error_type}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 0, "y": 8}
      },
      {
        "id": 4,
        "title": "Requests by Provider",
        "type": "piechart",
        "targets": [
          {
            "expr": "sum by (provider) (rate(router_requests_by_provider[5m]))",
            "legendFormat": "{{provider}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 12, "y": 8}
      },
      {
        "id": 5,
        "title": "Active NATS Connections",
        "type": "stat",
        "targets": [
          {
            "expr": "router_active_connections"
          }
        ],
        "gridPos": {"h": 4, "w": 6, "x": 0, "y": 16}
      },
      {
        "id": 6,
        "title": "NATS Messages Rate",
        "type": "timeseries",
        "targets": [
          {
            "expr": "sum(rate(router_nats_messages_total[5m]))",
            "legendFormat": "{{subject}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 6, "y": 16}
      },
      {
        "id": 7,
        "title": "Cache Hit Rate",
        "type": "timeseries",
        "targets": [
          {
            "expr": "sum(rate(router_cache_hits_total[5m])) / (sum(rate(router_cache_hits_total[5m])) + sum(rate(router_cache_misses_total[5m])))",
            "legendFormat": "Hit Rate"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 0, "y": 24}
      },
      {
        "id": 8,
        "title": "Policy Load Duration (p95)",
        "type": "timeseries",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(router_policy_load_duration_seconds_bucket[5m]))",
            "legendFormat": "{{source}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 12, "y": 24}
      }
    ]
  }
}
```

---

## Alerting Rules (CP2 Planning)

### High Error Rate Alert

```yaml
groups:
  - name: router_alerts
    interval: 30s
    rules:
      - alert: RouterHighErrorRate
        expr: rate(router_errors_total[5m]) > 0.1
        for: 5m
        labels:
          severity: warning
          component: router
        annotations:
          summary: "Router error rate is high"
          description: "Router error rate is {{ $value }} errors/second (threshold: 0.1)"
```

### High Latency Alert

```yaml
      - alert: RouterHighLatency
        expr: histogram_quantile(0.95, rate(router_request_duration_seconds_bucket[5m])) > 1.0
        for: 5m
        labels:
          severity: warning
          component: router
        annotations:
          summary: "Router latency is high"
          description: "Router p95 latency is {{ $value }} seconds (threshold: 1.0)"
```

### NATS Connection Issues Alert

```yaml
      - alert: RouterNATSConnectionIssues
        expr: router_active_connections < 1
        for: 1m
        labels:
          severity: critical
          component: router
        annotations:
          summary: "Router NATS connection is down"
          description: "Router has no active NATS connections"
```

### Low Cache Hit Rate Alert

```yaml
      - alert: RouterLowCacheHitRate
        expr: sum(rate(router_cache_hits_total[5m])) / (sum(rate(router_cache_hits_total[5m])) + sum(rate(router_cache_misses_total[5m]))) < 0.5
        for: 10m
        labels:
          severity: warning
          component: router
        annotations:
          summary: "Router cache hit rate is low"
          description: "Router cache hit rate is {{ $value | humanizePercentage }} (threshold: 50%)"
```

---

## Metric Naming Conventions

### Prometheus Naming Standards

All Router metrics follow Prometheus naming conventions:

- **Counters**: `router_*_total` (e.g., `router_requests_total`)
- **Histograms**: `router_*_duration_seconds` (e.g., `router_request_duration_seconds`)
- **Gauges**: `router_*` (e.g., `router_active_connections`)

### Label Naming

- **Status labels**: `status` (success|error|timeout)
- **Provider labels**: `provider` (openai|anthropic|...)
- **Error type labels**: `error_type` (nats|routing|policy|provider)
- **Subject labels**: `subject` (beamline.router.v1.decide|...)
- **Source labels**: `source` (cache|database)

### Cardinality Considerations

- **Low cardinality labels**: `status`, `provider`, `error_type`
- **Medium cardinality labels**: `subject` (limited number of subjects)
- **Avoid high cardinality**: Do not include `tenant_id`, `run_id`, `trace_id` in metrics (use logs for correlation)

---

## Dashboard Usage

### Importing Dashboard

1. **Grafana UI**:
   - Navigate to Dashboards → Import
   - Paste dashboard JSON template
   - Select Prometheus data source
   - Click "Import"

2. **Grafana API**:
   ```bash
   curl -X POST http://grafana:3000/api/dashboards/db \
     -H "Content-Type: application/json" \
     -d @router-dashboard.json
   ```

### Customization

**Variables**:
- `$provider`: Provider filter (openai|anthropic|...)
- `$error_type`: Error type filter (nats|routing|policy|provider)
- `$time_range`: Time range selector (5m|15m|1h|6h|24h)

**Refresh Interval**:
- Default: 30 seconds
- Recommended: 30s-1m for production
- Real-time: 10s for debugging

---

## Current Limitations

### CP1 Limitations

1. **No Metrics**: Prometheus metrics are not yet implemented (planned for CP2)
2. **No Dashboard**: Grafana dashboard cannot be used until metrics are available
3. **No Alerting**: Alertmanager integration is not available (planned for CP2)

### Workarounds

- **Metrics**: Use log analysis to extract metrics (not recommended for production)
- **Dashboards**: Use log analysis tools (limited functionality)
- **Alerting**: Use log monitoring tools (limited functionality)

---

## References

- `apps/otp/router/docs/OBSERVABILITY_CP2_PLANNING.md` - CP2 observability planning
- `apps/otp/router/docs/OBSERVABILITY.md` - CP1 observability documentation
- `docs/OBSERVABILITY.md` - Unified observability requirements
- Prometheus documentation: https://prometheus.io/docs/
- Grafana documentation: https://grafana.com/docs/
- Alertmanager documentation: https://prometheus.io/docs/alerting/latest/alertmanager/

---

## Next Steps

1. **CP2 Implementation**: Implement Prometheus metrics in Router (see `OBSERVABILITY_CP2_PLANNING.md`)
2. **Dashboard Creation**: Create Grafana dashboard using this template
3. **Alerting Setup**: Configure Alertmanager with alert rules
4. **Testing**: Validate dashboard and alerts in staging environment
5. **Documentation**: Update documentation with CP2 dashboard usage

**Status**: ✅ **CP1 Complete** → 📋 **CP2 Planned**

