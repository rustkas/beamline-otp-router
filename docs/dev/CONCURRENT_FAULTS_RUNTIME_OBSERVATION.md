# Concurrent Faults Test Suite - Runtime Observation Guide

**Date**: 2025-11-30  
**Purpose**: Guide for observing `router_concurrent_faults_SUITE` behavior in production-like environments with monitoring

## Overview

This guide explains how to run `router_concurrent_faults_SUITE` and observe its behavior through Prometheus/Grafana monitoring systems.

## Prerequisites

1. **Router running** with Prometheus metrics enabled
2. **Prometheus** scraping router metrics
3. **Grafana** (optional) for visualization
4. **Test suite** compiled and ready

## Running Tests with Monitoring

### Step 1: Start Monitoring

Ensure Prometheus is scraping router metrics:

```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets

# Verify router metrics endpoint
curl http://localhost:9091/metrics | grep router_nats
```

### Step 2: Run Test Suite

```bash
cd apps/otp/router
rebar3 ct --suite test/router_concurrent_faults_SUITE
```

### Step 3: Observe Metrics

During test execution, monitor these metrics in Prometheus/Grafana:

#### Connection Metrics

**During `test_connect_and_publish_faults`**:
- `router_nats_connection_lost_total` - Should increase
- `router_nats_connection_status` - Should drop to 0, then recover to 1
- `router_nats_reconnect_attempts_total` - Should increase

**PromQL Query**:
```promql
rate(router_nats_connection_lost_total[1m])
```

#### Publish Metrics

**During `test_publish_and_ack_nak_faults`**:
- `router_nats_publish_failures_total` - Should increase
- `router_nats_publish_with_ack_failures_total` - Should increase
- `router_nats_publish_total` - Should NOT increase (operations failed)

**PromQL Query**:
```promql
rate(router_nats_publish_failures_total[1m])
```

#### ACK/NAK Metrics

**During `test_connect_and_ack_nak_faults`**:
- `router_nats_ack_failures_total` - Should increase
- `router_jetstream_redelivery_total` - Should increase
- `router_nats_nak_total` - Should increase (if NAK faults injected)

**PromQL Query**:
```promql
rate(router_jetstream_redelivery_total[1m])
```

#### Validation Metrics

**During `test_validation_and_publish_faults`**:
- `router_tenant_audit_total` - Should increase (for invalid tenants)
- `router_nats_publish_failures_total` - Should increase (for infrastructure errors)

**PromQL Query**:
```promql
rate(router_tenant_audit_total[1m])
```

## Grafana Dashboard Setup

### Recommended Panels

1. **Connection Status Panel**:
   - **Query**: `router_nats_connection_status`
   - **Type**: Gauge
   - **Expected**: 1.0 (connected) → 0.0 (disconnected) → 1.0 (recovered)

2. **Error Rate Panel**:
   - **Query**: `rate(router_nats_connection_lost_total[1m]) + rate(router_nats_publish_failures_total[1m])`
   - **Type**: Graph
   - **Expected**: Spikes during fault injection, returns to baseline after recovery

3. **Redelivery Rate Panel**:
   - **Query**: `rate(router_jetstream_redelivery_total[1m])`
   - **Type**: Graph
   - **Expected**: Increases during ACK/NAK faults, stabilizes after recovery

4. **Validation Errors Panel**:
   - **Query**: `rate(router_tenant_audit_total[1m])`
   - **Type**: Graph
   - **Expected**: Increases for invalid tenants, separate from infrastructure errors

### Dashboard JSON

```json
{
  "dashboard": {
    "title": "Router Concurrent Faults Test",
    "panels": [
      {
        "title": "Connection Status",
        "targets": [{"expr": "router_nats_connection_status"}],
        "type": "gauge"
      },
      {
        "title": "Error Rate",
        "targets": [{"expr": "rate(router_nats_connection_lost_total[1m]) + rate(router_nats_publish_failures_total[1m])"}],
        "type": "graph"
      },
      {
        "title": "Redelivery Rate",
        "targets": [{"expr": "rate(router_jetstream_redelivery_total[1m])"}],
        "type": "graph"
      }
    ]
  }
}
```

## Verification Checklist

After running tests, verify:

- [ ] **Connection Metrics**: `router_nats_connection_lost_total` increased during connect faults
- [ ] **Publish Metrics**: `router_nats_publish_failures_total` increased during publish faults
- [ ] **ACK/NAK Metrics**: `router_jetstream_redelivery_total` increased during ACK/NAK faults
- [ ] **Recovery**: `router_nats_connection_status` returned to 1.0 after fault removal
- [ ] **No Leaks**: Process count stable, no memory leaks observed
- [ ] **Metrics Stabilize**: Error rates return to baseline after recovery

## Troubleshooting

### Metrics Not Appearing

1. **Check Prometheus Configuration**:
   ```yaml
   scrape_configs:
     - job_name: 'router'
       static_configs:
         - targets: ['localhost:9091']
   ```

2. **Check Router Metrics Endpoint**:
   ```bash
   curl http://localhost:9091/metrics | grep router_nats
   ```

3. **Check Test Execution**:
   ```bash
   # Verify tests actually ran
   tail -f apps/otp/router/ct_logs/*/suite.log
   ```

### Metrics Not Changing

1. **Verify Fault Injection**:
   - Check that `router_nats_fault_injection:enable_fault/2` is called
   - Verify faults are not immediately disabled

2. **Check Timing**:
   - Ensure sufficient wait time for faults to manifest
   - Check recovery wait time (default: 2000ms)

3. **Verify Metrics Collection**:
   - Check that `router_metrics:inc/1` is called during faults
   - Verify ETS table `router_metrics` exists

## Periodic Execution

### Recommended Schedule

- **Development**: Run before each commit
- **CI/CD**: Run in every CI pipeline
- **Production-like**: Run weekly in staging environment

### Automation Script

```bash
#!/bin/bash
# scripts/run_concurrent_faults_with_monitoring.sh

set -euo pipefail

echo "Starting concurrent faults test with monitoring..."

# Check Prometheus is available
if ! curl -fsS http://localhost:9090/api/v1/status/config > /dev/null; then
    echo "Warning: Prometheus not available, metrics won't be observed"
fi

# Run tests
cd apps/otp/router
rebar3 ct --suite test/router_concurrent_faults_SUITE

# Wait for metrics to stabilize
sleep 5

# Query Prometheus for verification
echo "Verifying metrics in Prometheus..."
curl -s "http://localhost:9090/api/v1/query?query=router_nats_connection_lost_total" | jq '.data.result[0].value[1]'

echo "Test complete. Check Grafana dashboard for detailed metrics."
```

## Integration with CI/CD

### GitHub Actions

Add monitoring verification step:

```yaml
- name: Verify Concurrent Faults Metrics
  if: always()
  run: |
    # Wait for metrics to be scraped
    sleep 10
    # Query Prometheus API
    curl -s "http://localhost:9090/api/v1/query?query=router_nats_connection_lost_total" | jq '.'
```

### GitLab CI

Add metrics verification:

```yaml
- echo "Verifying metrics after concurrent faults test..."
- curl -s "http://localhost:9090/api/v1/query?query=rate(router_nats_publish_failures_total[1m])" | jq '.data.result'
```

## Best Practices

1. **Baseline Metrics**: Record baseline metrics before test execution
2. **Compare Patterns**: Compare test metrics with production metrics
3. **Document Anomalies**: Document any unexpected metric behavior
4. **Regular Review**: Review metrics weekly to catch regressions

## References

- `CONCURRENT_FAULTS_TEST_COVERAGE_REPORT.md` - Test coverage details
- `NATS_CONNECTION_RESILIENCE.md` - Connection metrics documentation
- `NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Publish failure semantics
- `PROMETHEUS_ALERTS.md` - Prometheus alerting rules

