# Extension Registry & Extensions Operations Runbook

**Version**: CP2-LC  
**Date**: 2025-11-30  
**Status**: Production Ready  
**Target Audience**: SRE/Ops teams

---

## Overview

This runbook provides operational procedures for managing Extension Registry and Extensions in production environments. It covers health checks, alerts, standard operations, dashboards, and rollout/rollback procedures.

**Key Components**:
- Extension Registry (PostgreSQL + ETS/Mnesia cache)
- Extension Pipeline (pre → validators → provider → post)
- Circuit Breaker
- Health Metrics
- Versioning and Load Balancing

---

## Table of Contents

1. [Health Checks](#health-checks)
2. [Standard Operations](#standard-operations)
3. [Typical Alerts](#typical-alerts)
4. [Dashboards and Metrics](#dashboards-and-metrics)
5. [Alert Rules](#alert-rules)
6. [Rollout/Rollback Procedures](#rolloutrollback-procedures)
7. [Troubleshooting](#troubleshooting)

---

## Health Checks

### 1. Extension Registry Health

#### Check Registry Status

**Via Erlang Shell**:
```erlang
%% Check registry is running
whereis(router_extension_registry).

%% Check loaded extensions count
ets:info(extension_registry, size).

%% Check registry source mode
application:get_env(beamline_router, extension_registry).
```

**Expected Output**:
```erlang
%% Registry running
<0.123.0>

%% Extensions loaded
4

%% Configuration
[{source, database}, {db_enabled, true}, ...]
```

#### Check Database Connection

**Via Erlang Shell**:
```erlang
%% Test database connection
router_extension_registry_db:test_connection().

%% Check connection pool
router_extension_registry_db:get_db_connection().
```

**Expected Output**:
```erlang
%% Connection successful
{ok, connected}

%% Connection PID
<0.456.0>
```

#### Check Extension Availability

**Via Erlang Shell**:
```erlang
%% Lookup specific extension
router_extension_registry:lookup(<<"normalize_text">>).

%% List all extensions by type
router_extension_registry:lookup_by_type(<<"pre">>).
```

**Expected Output**:
```erlang
%% Extension found
{ok, #extension{
    id = <<"normalize_text">>,
    type = <<"pre">>,
    subject = <<"beamline.ext.pre.normalize_text.v1">>,
    timeout_ms = 100,
    retry = 0,
    enabled = true
}}

%% Extensions by type
{ok, [#extension{...}, #extension{...}]}
```

### 2. Extension Health Metrics

#### Get Health for Single Extension

**Via Erlang Shell**:
```erlang
%% Get health metrics
router_extension_health:get_health(<<"normalize_text">>).
```

**Expected Output**:
```erlang
{ok, #{
    extension_id => <<"normalize_text">>,
    last_success => {{2025,1,27},{12,0,0}},
    last_failure => null,
    success_count => 1000,
    failure_count => 5,
    success_rate => 0.995,
    avg_latency_ms => 15.5,
    p50_latency_ms => 12.0,
    p95_latency_ms => 25.0,
    p99_latency_ms => 35.0,
    circuit_breaker_state => <<"closed">>,
    circuit_breaker_opened_at => null
}}
```

#### Get Health Summary

**Via Erlang Shell**:
```erlang
%% Get aggregated health summary
router_extension_health:get_health_summary().
```

**Expected Output**:
```erlang
{ok, #{
    total => 4,
    healthy => 3,
    unhealthy => 1,
    half_open => 0,
    avg_success_rate => 0.98,
    avg_latency_ms => 18.5,
    avg_p95_latency_ms => 30.0
}}
```

#### Get All Health Metrics

**Via Erlang Shell**:
```erlang
%% Get all extension health
router_extension_health:get_all_health().
```

**Expected Output**:
```erlang
{ok, [
    #{extension_id => <<"normalize_text">>, ...},
    #{extension_id => <<"pii_guard">>, ...},
    ...
]}
```

### 3. Router Health Endpoint

**Via gRPC Health Check**:
```bash
grpc_health_probe -addr=localhost:9000
```

**Via Admin API**:
```bash
grpcurl -plaintext -H "x-api-key: YOUR_API_KEY" \
  -d '{}' \
  localhost:9000 beamline.flow.v1.RouterAdmin/GetValidatorsHealth
```

**Expected Response**:
```json
{
  "validators": [
    {
      "name": "grpc_health",
      "status": "serving",
      "timestamp_ms": 1706356800000
    }
  ]
}
```

---

## Standard Operations

### 1. Hot Reload Extensions

#### Reload from Current Source

**Via Erlang Shell**:
```erlang
%% Reload extensions (uses current source mode)
router_extension_registry:reload().
```

**Behavior**:
- **Fixtures mode**: Reloads from `priv/fixtures/extensions/*.json`
- **Database mode**: Reloads from PostgreSQL
- **Auto mode**: Tries database first, falls back to fixtures

**Expected Output**:
```erlang
ok
```

**Verification**:
```erlang
%% Check extensions were reloaded
ets:info(extension_registry, size).

%% Verify specific extension
router_extension_registry:lookup(<<"normalize_text">>).
```

### 2. Switch Registry Source Mode

#### Switch to Fixtures Mode

**Steps**:
1. Update configuration:
```erlang
application:set_env(beamline_router, extension_registry, [
    {source, fixtures},
    {db_enabled, false}
]).
```

2. Restart registry:
```erlang
gen_server:stop(router_extension_registry),
timer:sleep(100),
router_extension_registry:start_link().
```

3. Verify:
```erlang
application:get_env(beamline_router, extension_registry).
router_extension_registry:lookup(<<"normalize_text">>).
```

#### Switch to Database Mode

**Steps**:
1. Update configuration:
```erlang
application:set_env(beamline_router, extension_registry, [
    {source, database},
    {db_enabled, true},
    {db_host, "localhost"},
    {db_port, 5432},
    {db_name, "beamline"},
    {db_user, "beamline"},
    {db_password, "..."}
]).
```

2. Restart registry:
```erlang
gen_server:stop(router_extension_registry),
timer:sleep(100),
router_extension_registry:start_link().
```

3. Verify:
```erlang
application:get_env(beamline_router, extension_registry).
router_extension_registry:lookup(<<"normalize_text">>).
```

#### Switch to Auto Mode

**Steps**:
1. Update configuration:
```erlang
application:set_env(beamline_router, extension_registry, [
    {source, auto},
    {db_enabled, true},
    {db_host, "localhost"},
    {db_port, 5432},
    {db_name, "beamline"},
    {db_user, "beamline"},
    {db_password, "..."}
]).
```

2. Restart registry:
```erlang
gen_server:stop(router_extension_registry),
timer:sleep(100),
router_extension_registry:start_link().
```

3. Verify:
```erlang
application:get_env(beamline_router, extension_registry).
router_extension_registry:lookup(<<"normalize_text">>).
```

**Behavior**:
- Tries database first
- Falls back to fixtures if database unavailable
- Logs fallback warning

### 3. Enable/Disable Extension

#### Via Database (Production)

**SQL**:
```sql
-- Disable extension
UPDATE extensions SET enabled = FALSE WHERE id = 'normalize_text';

-- Enable extension
UPDATE extensions SET enabled = TRUE WHERE id = 'normalize_text';
```

**Reload Registry**:
```erlang
router_extension_registry:reload().
```

**Verify**:
```erlang
{ok, Extension} = router_extension_registry:lookup(<<"normalize_text">>),
Extension#extension.enabled.  % Should be false/true
```

#### Via Fixtures (Development)

**Edit File**:
```bash
vim apps/otp/router/priv/fixtures/extensions/normalize_text.json
```

**Update**:
```json
{
  "id": "normalize_text",
  "enabled": false,
  ...
}
```

**Reload**:
```erlang
router_extension_registry:reload().
```

### 4. Update Extension Configuration

#### Via Database

**SQL**:
```sql
-- Update timeout
UPDATE extensions SET timeout_ms = 200 WHERE id = 'normalize_text';

-- Update retry count
UPDATE extensions SET retry = 1 WHERE id = 'normalize_text';

-- Update config
UPDATE extensions SET config = '{"max_length": 1000}'::jsonb WHERE id = 'normalize_text';
```

**Reload**:
```erlang
router_extension_registry:reload().
```

### 5. Check Sync Status

**Via Erlang Shell**:
```erlang
%% Check sync timer
process_info(whereis(router_extension_registry), messages).

%% Check last sync time (from logs)
%% Look for: "Extension Registry synced" in logs
```

**Logs**:
```bash
grep "Extension Registry synced" /var/log/router/router.log
```

---

## Typical Alerts

### 1. High Error Rate

**Alert**: `ExtensionErrorRateHigh`

**Symptoms**:
- Error rate > 30% for extension
- Circuit breaker may open
- Requests failing frequently

**Diagnosis**:
```erlang
%% Check extension health
{ok, Health} = router_extension_health:get_health(<<"normalize_text">>),
ErrorRate = 1.0 - maps:get(success_rate, Health, 0.0),
ErrorRate.  % Should be < 0.3
```

**Actions**:
1. Check extension service logs
2. Verify NATS connectivity
3. Check extension service health
4. Review recent changes
5. Consider disabling extension if critical

**Resolution**:
- Fix extension service issue
- Restart extension service
- Disable extension if not critical
- Update extension configuration

### 2. Circuit Breaker Opened

**Alert**: `ExtensionCircuitBreakerOpened`

**Symptoms**:
- Circuit breaker state = "open"
- Requests failing fast (no retries)
- Extension unavailable

**Diagnosis**:
```erlang
%% Check circuit breaker state
{ok, Health} = router_extension_health:get_health(<<"normalize_text">>),
maps:get(circuit_breaker_state, Health).  % Should be "closed"
```

**Actions**:
1. Check extension service status
2. Verify NATS connectivity
3. Review error logs
4. Wait for recovery (60 seconds default)
5. Manually close circuit if needed

**Resolution**:
- Fix extension service
- Wait for automatic recovery (half-open testing)
- Manually reset circuit breaker (if supported)

### 3. High Latency

**Alert**: `ExtensionLatencyHigh`

**Symptoms**:
- P95 latency > 100ms
- P99 latency > 200ms
- Requests timing out

**Diagnosis**:
```erlang
%% Check latency metrics
{ok, Health} = router_extension_health:get_health(<<"normalize_text">>),
maps:get(p95_latency_ms, Health).  % Should be < 100
maps:get(p99_latency_ms, Health).  % Should be < 200
```

**Actions**:
1. Check extension service performance
2. Review NATS latency
3. Check network connectivity
4. Review extension processing logic
5. Consider scaling extension service

**Resolution**:
- Optimize extension service
- Scale extension instances
- Reduce processing time
- Increase timeout if acceptable

### 4. Extension Not Found

**Alert**: `ExtensionNotFound`

**Symptoms**:
- Extension lookup returns `{error, not_found}`
- Pipeline fails with extension_not_found error

**Diagnosis**:
```erlang
%% Check extension exists
router_extension_registry:lookup(<<"normalize_text">>).
```

**Actions**:
1. Verify extension is registered
2. Check registry source mode
3. Reload registry
4. Check database/fixtures

**Resolution**:
- Register extension
- Reload registry
- Fix configuration
- Verify source mode

### 5. Registry Sync Failed

**Alert**: `ExtensionRegistrySyncFailed`

**Symptoms**:
- Sync errors in logs
- Extensions not updating
- Database connection issues

**Diagnosis**:
```erlang
%% Check database connection
router_extension_registry_db:test_connection().

%% Check sync status
application:get_env(beamline_router, extension_registry).
```

**Actions**:
1. Check database connectivity
2. Verify database credentials
3. Check database permissions
4. Review sync interval
5. Manually trigger sync

**Resolution**:
- Fix database connection
- Update credentials
- Fix permissions
- Adjust sync interval
- Manual reload

---

## Dashboards and Metrics

### Prometheus Metrics

#### Extension Invocation Metrics

**Metric**: `router_extension_invocation_total`

**Labels**:
- `extension_id` - Extension identifier
- `type` - Extension type (pre, validator, post, provider)
- `status` - success | error | timeout
- `tenant_id` - Tenant identifier (optional)

**Example**:
```promql
# Total invocations per extension
sum(rate(router_extension_invocation_total[5m])) by (extension_id)

# Error rate per extension
sum(rate(router_extension_invocation_total{status="error"}[5m])) by (extension_id) /
sum(rate(router_extension_invocation_total[5m])) by (extension_id)

# Success rate per extension
sum(rate(router_extension_invocation_total{status="success"}[5m])) by (extension_id) /
sum(rate(router_extension_invocation_total[5m])) by (extension_id)
```

**Metric**: `router_extension_invocation_latency_ms`

**Type**: Histogram

**Labels**:
- `extension_id` - Extension identifier
- `type` - Extension type

**Buckets**: `[10, 25, 50, 100, 200, 500, 1000]` (milliseconds)

**Example**:
```promql
# P95 latency per extension
histogram_quantile(0.95, 
  sum(rate(router_extension_invocation_latency_ms_bucket[5m])) by (extension_id, le)
)

# P99 latency per extension
histogram_quantile(0.99, 
  sum(rate(router_extension_invocation_latency_ms_bucket[5m])) by (extension_id, le)
)

# Average latency per extension
sum(rate(router_extension_invocation_latency_ms_sum[5m])) by (extension_id) /
sum(rate(router_extension_invocation_latency_ms_count[5m])) by (extension_id)
```

**Metric**: `router_extension_retries_total`

**Labels**:
- `extension_id` - Extension identifier
- `retry_count` - Number of retries (0, 1, 2, ...)

**Example**:
```promql
# Retry rate per extension
sum(rate(router_extension_retries_total{retry_count!="0"}[5m])) by (extension_id) /
sum(rate(router_extension_retries_total[5m])) by (extension_id)
```

#### Circuit Breaker Metrics

**Metric**: `router_extension_circuit_breaker_state`

**Type**: Gauge

**Labels**:
- `extension_id` - Extension identifier
- `state` - closed | open | half_open

**Example**:
```promql
# Circuit breaker state per extension
router_extension_circuit_breaker_state

# Number of open circuits
count(router_extension_circuit_breaker_state{state="open"})

# Number of half-open circuits
count(router_extension_circuit_breaker_state{state="half_open"})
```

**Metric**: `router_extension_circuit_breaker_opened_total`

**Type**: Counter

**Labels**:
- `extension_id` - Extension identifier

**Example**:
```promql
# Circuit breaker openings per extension
sum(rate(router_extension_circuit_breaker_opened_total[5m])) by (extension_id)
```

#### Health Metrics

**Metric**: `router_extension_health_success_rate`

**Type**: Gauge

**Labels**:
- `extension_id` - Extension identifier

**Example**:
```promql
# Success rate per extension
router_extension_health_success_rate

# Extensions with low success rate (< 0.9)
router_extension_health_success_rate < 0.9
```

**Metric**: `router_extension_health_latency_p95_ms`

**Type**: Gauge

**Labels**:
- `extension_id` - Extension identifier

**Example**:
```promql
# P95 latency per extension
router_extension_health_latency_p95_ms

# Extensions with high latency (> 100ms)
router_extension_health_latency_p95_ms > 100
```

#### Registry Metrics

**Metric**: `router_extension_registry_extensions_total`

**Type**: Gauge

**Labels**:
- `source` - database | fixtures | auto
- `type` - Extension type

**Example**:
```promql
# Total extensions in registry
sum(router_extension_registry_extensions_total)

# Extensions by type
sum(router_extension_registry_extensions_total) by (type)

# Extensions by source
sum(router_extension_registry_extensions_total) by (source)
```

**Metric**: `router_extension_registry_sync_total`

**Type**: Counter

**Labels**:
- `status` - success | error

**Example**:
```promql
# Sync success rate
sum(rate(router_extension_registry_sync_total{status="success"}[5m])) /
sum(rate(router_extension_registry_sync_total[5m]))
```

### Grafana Dashboard Panels

#### Panel 1: Extension Invocation Rate

**Query**:
```promql
sum(rate(router_extension_invocation_total[5m])) by (extension_id)
```

**Visualization**: Time series graph

**Y-Axis**: Requests per second

**Legend**: `{{extension_id}}`

#### Panel 2: Extension Error Rate

**Query**:
```promql
sum(rate(router_extension_invocation_total{status="error"}[5m])) by (extension_id) /
sum(rate(router_extension_invocation_total[5m])) by (extension_id) * 100
```

**Visualization**: Time series graph

**Y-Axis**: Percentage (0-100%)

**Legend**: `{{extension_id}}`

**Alert Threshold**: > 30%

#### Panel 3: Extension Latency (P95)

**Query**:
```promql
histogram_quantile(0.95, 
  sum(rate(router_extension_invocation_latency_ms_bucket[5m])) by (extension_id, le)
)
```

**Visualization**: Time series graph

**Y-Axis**: Milliseconds

**Legend**: `{{extension_id}}`

**Alert Threshold**: > 100ms

#### Panel 4: Extension Latency (P99)

**Query**:
```promql
histogram_quantile(0.99, 
  sum(rate(router_extension_invocation_latency_ms_bucket[5m])) by (extension_id, le)
)
```

**Visualization**: Time series graph

**Y-Axis**: Milliseconds

**Legend**: `{{extension_id}}`

**Alert Threshold**: > 200ms

#### Panel 5: Circuit Breaker State

**Query**:
```promql
router_extension_circuit_breaker_state
```

**Visualization**: State timeline

**States**:
- `closed` = 0 (green)
- `half_open` = 1 (yellow)
- `open` = 2 (red)

**Legend**: `{{extension_id}}`

#### Panel 6: Circuit Breaker Openings

**Query**:
```promql
sum(rate(router_extension_circuit_breaker_opened_total[5m])) by (extension_id)
```

**Visualization**: Time series graph

**Y-Axis**: Openings per second

**Legend**: `{{extension_id}}`

#### Panel 7: Extension Success Rate

**Query**:
```promql
router_extension_health_success_rate * 100
```

**Visualization**: Gauge

**Y-Axis**: Percentage (0-100%)

**Thresholds**:
- Green: > 95%
- Yellow: 90-95%
- Red: < 90%

**Legend**: `{{extension_id}}`

#### Panel 8: Registry Extensions Count

**Query**:
```promql
sum(router_extension_registry_extensions_total) by (type)
```

**Visualization**: Pie chart

**Legend**: `{{type}}`

#### Panel 9: Registry Sync Status

**Query**:
```promql
sum(rate(router_extension_registry_sync_total{status="success"}[5m])) /
sum(rate(router_extension_registry_sync_total[5m])) * 100
```

**Visualization**: Single stat

**Y-Axis**: Percentage (0-100%)

**Thresholds**:
- Green: > 99%
- Yellow: 95-99%
- Red: < 95%

---

## Alert Rules

### 1. High Error Rate

**Alert**: `ExtensionErrorRateHigh`

**Rule**:
```yaml
- alert: ExtensionErrorRateHigh
  expr: |
    sum(rate(router_extension_invocation_total{status="error"}[5m])) by (extension_id) /
    sum(rate(router_extension_invocation_total[5m])) by (extension_id) > 0.3
  for: 5m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension {{ $labels.extension_id }} error rate is high"
    description: "Extension {{ $labels.extension_id }} has error rate of {{ $value | humanizePercentage }} over the last 5 minutes. Threshold: 30%"
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#high-error-rate"
```

**Severity**: Warning

**Threshold**: Error rate > 30% for 5 minutes

**Actions**:
1. Check extension service logs
2. Verify NATS connectivity
3. Review extension health metrics
4. Consider disabling extension if critical

### 2. Circuit Breaker Opened

**Alert**: `ExtensionCircuitBreakerOpened`

**Rule**:
```yaml
- alert: ExtensionCircuitBreakerOpened
  expr: router_extension_circuit_breaker_state{state="open"} == 2
  for: 1m
  labels:
    severity: critical
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension {{ $labels.extension_id }} circuit breaker is open"
    description: "Extension {{ $labels.extension_id }} circuit breaker has been open for at least 1 minute. Requests are failing fast."
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#circuit-breaker-opened"
```

**Severity**: Critical

**Threshold**: Circuit breaker state = "open" for 1 minute

**Actions**:
1. Check extension service status
2. Verify NATS connectivity
3. Review error logs
4. Wait for automatic recovery (60 seconds)
5. Manually investigate if persists

### 3. High Latency (P95)

**Alert**: `ExtensionLatencyHighP95`

**Rule**:
```yaml
- alert: ExtensionLatencyHighP95
  expr: |
    histogram_quantile(0.95, 
      sum(rate(router_extension_invocation_latency_ms_bucket[5m])) by (extension_id, le)
    ) > 100
  for: 10m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension {{ $labels.extension_id }} P95 latency is high"
    description: "Extension {{ $labels.extension_id }} has P95 latency of {{ $value }}ms over the last 10 minutes. Threshold: 100ms"
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#high-latency"
```

**Severity**: Warning

**Threshold**: P95 latency > 100ms for 10 minutes

**Actions**:
1. Check extension service performance
2. Review NATS latency
3. Check network connectivity
4. Consider scaling extension service

### 4. High Latency (P99)

**Alert**: `ExtensionLatencyHighP99`

**Rule**:
```yaml
- alert: ExtensionLatencyHighP99
  expr: |
    histogram_quantile(0.99, 
      sum(rate(router_extension_invocation_latency_ms_bucket[5m])) by (extension_id, le)
    ) > 200
  for: 5m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension {{ $labels.extension_id }} P99 latency is high"
    description: "Extension {{ $labels.extension_id }} has P99 latency of {{ $value }}ms over the last 5 minutes. Threshold: 200ms"
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#high-latency"
```

**Severity**: Warning

**Threshold**: P99 latency > 200ms for 5 minutes

**Actions**:
1. Check extension service performance
2. Review tail latency patterns
3. Check for degraded instances
4. Consider load balancing

### 5. Frequent Circuit Breaker Openings

**Alert**: `ExtensionCircuitBreakerFrequentOpenings`

**Rule**:
```yaml
- alert: ExtensionCircuitBreakerFrequentOpenings
  expr: |
    sum(rate(router_extension_circuit_breaker_opened_total[10m])) by (extension_id) > 0.1
  for: 10m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension {{ $labels.extension_id }} circuit breaker opens frequently"
    description: "Extension {{ $labels.extension_id }} circuit breaker has opened {{ $value }} times per second over the last 10 minutes. This indicates unstable extension service."
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#circuit-breaker-opened"
```

**Severity**: Warning

**Threshold**: > 0.1 openings/second for 10 minutes

**Actions**:
1. Investigate extension service stability
2. Check for intermittent failures
3. Review extension service health
4. Consider disabling extension

### 6. Registry Sync Failed

**Alert**: `ExtensionRegistrySyncFailed`

**Rule**:
```yaml
- alert: ExtensionRegistrySyncFailed
  expr: |
    sum(rate(router_extension_registry_sync_total{status="error"}[5m])) /
    sum(rate(router_extension_registry_sync_total[5m])) > 0.1
  for: 5m
  labels:
    severity: warning
    component: router
    subsystem: extension_registry
  annotations:
    summary: "Extension Registry sync is failing"
    description: "Extension Registry sync has error rate of {{ $value | humanizePercentage }} over the last 5 minutes. Extensions may not be updating."
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#registry-sync-failed"
```

**Severity**: Warning

**Threshold**: Sync error rate > 10% for 5 minutes

**Actions**:
1. Check database connectivity
2. Verify database credentials
3. Check database permissions
4. Review sync logs

### 7. Low Success Rate

**Alert**: `ExtensionSuccessRateLow`

**Rule**:
```yaml
- alert: ExtensionSuccessRateLow
  expr: router_extension_health_success_rate < 0.9
  for: 10m
  labels:
    severity: warning
    component: router
    subsystem: extensions
  annotations:
    summary: "Extension {{ $labels.extension_id }} success rate is low"
    description: "Extension {{ $labels.extension_id }} has success rate of {{ $value | humanizePercentage }} over the last 10 minutes. Threshold: 90%"
    runbook_url: "apps/otp/router/docs/EXTENSIONS_RUNBOOK.md#high-error-rate"
```

**Severity**: Warning

**Threshold**: Success rate < 90% for 10 minutes

**Actions**:
1. Check extension service health
2. Review error patterns
3. Check circuit breaker state
4. Consider disabling extension

---

## Rollout/Rollback Procedures

### 1. Deploy New Extension Version

#### Step 1: Prepare New Version in Database

**SQL**:
```sql
-- Insert new version
INSERT INTO extension_versions (
    id, version, subject, timeout_ms, retry, config, metadata, routing_rules
) VALUES (
    'normalize_text',
    'v2',
    'beamline.ext.pre.normalize_text.v2',
    150,  -- Increased timeout
    1,    -- Added retry
    '{"max_length": 2000}'::jsonb,
    '{"description": "Normalize text v2 with improved handling"}'::jsonb,
    '{"tenant_id": ["premium_tenant"]}'::jsonb  -- Routing rules
);
```

#### Step 2: Test New Version

**Via Erlang Shell**:
```erlang
%% Test version selection
Context = #{<<"tenant_id">> => <<"premium_tenant">>},
router_extension_versioning:lookup_with_version(<<"normalize_text">>, Context).
```

**Expected Output**:
```erlang
{ok, #extension{
    id = <<"normalize_text">>,
    version = <<"v2">>,
    subject = <<"beamline.ext.pre.normalize_text.v2">>,
    ...
}}
```

#### Step 3: Enable New Version Gradually

**SQL**:
```sql
-- Update routing rules to include more tenants
UPDATE extension_versions
SET routing_rules = '{"tenant_id": ["premium_tenant", "standard_tenant"]}'::jsonb
WHERE id = 'normalize_text' AND version = 'v2';
```

#### Step 4: Monitor Metrics

**Check**:
- Error rate for new version
- Latency for new version
- Circuit breaker state
- Success rate

**Via Erlang Shell**:
```erlang
%% Monitor health
router_extension_health:get_health(<<"normalize_text">>).
```

#### Step 5: Rollout to All Tenants

**SQL**:
```sql
-- Make v2 default (remove routing rules or set to all tenants)
UPDATE extension_versions
SET routing_rules = '{}'::jsonb  -- Empty = default for all
WHERE id = 'normalize_text' AND version = 'v2';
```

#### Step 6: Reload Registry

**Via Erlang Shell**:
```erlang
router_extension_registry:reload().
```

### 2. Rollback Extension Version

#### Step 1: Identify Issue

**Check Metrics**:
- Error rate increased
- Latency increased
- Circuit breaker opened

#### Step 2: Revert Routing Rules

**SQL**:
```sql
-- Remove new version from routing (fallback to v1)
UPDATE extension_versions
SET routing_rules = '{"tenant_id": []}'::jsonb  -- Empty list = no match
WHERE id = 'normalize_text' AND version = 'v2';
```

#### Step 3: Reload Registry

**Via Erlang Shell**:
```erlang
router_extension_registry:reload().
```

#### Step 4: Verify Rollback

**Via Erlang Shell**:
```erlang
%% Should return v1
Context = #{<<"tenant_id">> => <<"premium_tenant">>},
router_extension_versioning:lookup_with_version(<<"normalize_text">>, Context).
```

#### Step 5: Disable Problematic Version

**SQL**:
```sql
-- Disable extension version (if needed)
UPDATE extensions
SET enabled = FALSE
WHERE id = 'normalize_text' AND version = 'v2';
```

### 3. Safely Disable Extension

#### Step 1: Check Extension Usage

**Via Erlang Shell**:
```erlang
%% Check which policies use this extension
%% (Requires policy store query)
```

**Via Logs**:
```bash
grep "normalize_text" /var/log/router/router.log | grep "invocation"
```

#### Step 2: Update Policies (If Needed)

**Remove Extension from Policies**:
- Update policies to remove extension reference
- Or set extension mode to "optional" (fail-open)

#### Step 3: Disable Extension

**Via Database**:
```sql
UPDATE extensions SET enabled = FALSE WHERE id = 'normalize_text';
```

**Via Fixtures**:
```bash
# Edit fixture file
vim apps/otp/router/priv/fixtures/extensions/normalize_text.json
# Set "enabled": false
```

#### Step 4: Reload Registry

**Via Erlang Shell**:
```erlang
router_extension_registry:reload().
```

#### Step 5: Verify Disabled

**Via Erlang Shell**:
```erlang
{ok, Extension} = router_extension_registry:lookup(<<"normalize_text">>),
Extension#extension.enabled.  % Should be false
```

#### Step 6: Monitor Impact

**Check**:
- Pipeline errors (if extension was required)
- Pipeline continues (if extension was optional)
- No new invocations

### 4. Re-enable Extension

#### Step 1: Verify Extension Service

**Check**:
- Extension service is running
- NATS connectivity
- Extension responds to test requests

#### Step 2: Enable Extension

**Via Database**:
```sql
UPDATE extensions SET enabled = TRUE WHERE id = 'normalize_text';
```

**Via Fixtures**:
```bash
# Edit fixture file
vim apps/otp/router/priv/fixtures/extensions/normalize_text.json
# Set "enabled": true
```

#### Step 3: Reload Registry

**Via Erlang Shell**:
```erlang
router_extension_registry:reload().
```

#### Step 4: Verify Enabled

**Via Erlang Shell**:
```erlang
{ok, Extension} = router_extension_registry:lookup(<<"normalize_text">>),
Extension#extension.enabled.  % Should be true
```

#### Step 5: Monitor Recovery

**Check**:
- Success rate improves
- Latency returns to normal
- Circuit breaker closes (if was open)

---

## Troubleshooting

### Issue: Extension Not Found

**Symptoms**:
- `{error, not_found}` when looking up extension
- Pipeline fails with extension_not_found error

**Diagnosis**:
```erlang
%% Check extension exists
router_extension_registry:lookup(<<"normalize_text">>).

%% Check registry source
application:get_env(beamline_router, extension_registry).
```

**Solutions**:
1. **Verify Extension Registered**:
   - Check database: `SELECT * FROM extensions WHERE id = 'normalize_text';`
   - Check fixtures: `ls apps/otp/router/priv/fixtures/extensions/`

2. **Reload Registry**:
   ```erlang
   router_extension_registry:reload().
   ```

3. **Check Source Mode**:
   - Verify `source` matches where extension is stored
   - Switch to correct mode if needed

### Issue: Extension Timeout

**Symptoms**:
- High timeout rate
- Requests timing out
- Circuit breaker may open

**Diagnosis**:
```erlang
%% Check extension health
{ok, Health} = router_extension_health:get_health(<<"normalize_text">>),
maps:get(avg_latency_ms, Health).
```

**Solutions**:
1. **Increase Timeout**:
   ```sql
   UPDATE extensions SET timeout_ms = 200 WHERE id = 'normalize_text';
   ```
   ```erlang
   router_extension_registry:reload().
   ```

2. **Check Extension Service**:
   - Verify extension service is running
   - Check extension service logs
   - Review extension processing time

3. **Check NATS Latency**:
   - Verify NATS connectivity
   - Check NATS server health
   - Review network latency

### Issue: Circuit Breaker Stuck Open

**Symptoms**:
- Circuit breaker state = "open" for extended period
- No automatic recovery
- All requests failing fast

**Diagnosis**:
```erlang
%% Check circuit breaker state
{ok, Health} = router_extension_health:get_health(<<"normalize_text">>),
maps:get(circuit_breaker_state, Health).
```

**Solutions**:
1. **Wait for Recovery**:
   - Circuit breaker attempts recovery after timeout (default: 60 seconds)
   - Half-open state allows limited requests for testing

2. **Fix Extension Service**:
   - Resolve extension service issues
   - Verify extension service health
   - Test extension manually

3. **Manual Reset** (if supported):
   ```sql
   UPDATE extension_health
   SET circuit_breaker_state = 'closed',
       circuit_breaker_opened_at = NULL
   WHERE extension_id = 'normalize_text';
   ```

### Issue: Registry Sync Not Working

**Symptoms**:
- Extensions not updating
- Sync errors in logs
- Stale extension data

**Diagnosis**:
```erlang
%% Check database connection
router_extension_registry_db:test_connection().

%% Check sync configuration
application:get_env(beamline_router, extension_registry).
```

**Solutions**:
1. **Check Database Connection**:
   - Verify database is running
   - Check credentials
   - Test connectivity

2. **Check Sync Interval**:
   - Verify `sync_interval_seconds` is set
   - Reduce interval for faster updates

3. **Manual Reload**:
   ```erlang
   router_extension_registry:reload().
   ```

4. **Check Permissions**:
   - Verify database user has SELECT permissions
   - Check for database errors in logs

### Issue: High Latency

**Symptoms**:
- P95/P99 latency > thresholds
- Requests slow
- Timeouts increasing

**Diagnosis**:
```erlang
%% Check latency metrics
{ok, Health} = router_extension_health:get_health(<<"normalize_text">>),
maps:get(p95_latency_ms, Health),
maps:get(p99_latency_ms, Health).
```

**Solutions**:
1. **Check Extension Service Performance**:
   - Review extension service logs
   - Check extension processing time
   - Optimize extension logic

2. **Scale Extension Service**:
   - Add more extension instances
   - Enable load balancing
   - Distribute load

3. **Check Network**:
   - Verify NATS latency
   - Check network connectivity
   - Review routing

4. **Increase Timeout** (if acceptable):
   ```sql
   UPDATE extensions SET timeout_ms = 300 WHERE id = 'normalize_text';
   ```

---

## Quick Reference

### Health Check Commands

```erlang
%% Registry status
whereis(router_extension_registry).
ets:info(extension_registry, size).

%% Extension lookup
router_extension_registry:lookup(<<"normalize_text">>).

%% Health metrics
router_extension_health:get_health(<<"normalize_text">>).
router_extension_health:get_health_summary().

%% Database connection
router_extension_registry_db:test_connection().
```

### Operational Commands

```erlang
%% Reload registry
router_extension_registry:reload().

%% Switch source mode (requires restart)
application:set_env(beamline_router, extension_registry, [
    {source, database},  % or fixtures, auto
    {db_enabled, true}
]).
gen_server:stop(router_extension_registry),
router_extension_registry:start_link().
```

### SQL Commands

```sql
-- Disable extension
UPDATE extensions SET enabled = FALSE WHERE id = 'normalize_text';

-- Update timeout
UPDATE extensions SET timeout_ms = 200 WHERE id = 'normalize_text';

-- Add new version
INSERT INTO extension_versions (id, version, subject, ...) VALUES (...);

-- Check extension health
SELECT * FROM extension_health WHERE extension_id = 'normalize_text';
```

---

## References

- `docs/dev/EXTENSION_REGISTRY_PRODUCTION_DESIGN.md` - Registry design
- `docs/dev/EXTENSION_ADVANCED_FEATURES_REPORT.md` - Advanced features
- `docs/dev/EXTENSIONS_PIPELINE_PERF_REPORT.md` - Performance analysis
- `docs/dev/EXTENSION_REGISTRY_CONFIG_EXAMPLES.md` - Configuration examples
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md` - General operational guide
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert rules
- `apps/otp/router/docs/CONFIG.md` - Configuration reference
- `docs/observability/router-alert-rules.yaml` - Prometheus alert rules (includes extension alerts)
- `docs/observability/router-extensions-dashboard-grafana.json` - Grafana dashboard for extensions
- `apps/otp/router/docs/EXTENSIONS_SECURITY_GUIDE.md` - Extensions security guide

---

## Appendix: Pre-Release Metrics (CP3+)

### Additional Metrics (Not Yet Implemented)

The following metrics are planned for CP3+ and should be included in dashboards when available:

#### Extension Instance Metrics

**Metric**: `router_extension_instance_health`

**Labels**:
- `extension_id` - Extension identifier
- `instance_id` - Instance identifier
- `health_status` - healthy | unhealthy | degraded

**Use Case**: Load balancing health checks

#### Extension Version Metrics

**Metric**: `router_extension_version_invocation_total`

**Labels**:
- `extension_id` - Extension identifier
- `version` - Extension version (v1, v2, ...)

**Use Case**: Version rollout monitoring

#### Extension Load Balancing Metrics

**Metric**: `router_extension_load_balancer_selection_total`

**Labels**:
- `extension_id` - Extension identifier
- `instance_id` - Selected instance
- `algorithm` - weighted_round_robin | round_robin

**Use Case**: Load balancing effectiveness

---

## Change History

**v1.0 (2025-11-30)**:
- Initial runbook creation
- Health checks, standard operations, alerts
- Dashboards and metrics (Pre-Release)
- Rollout/rollback procedures
- Alert rules added to `docs/observability/router-alert-rules.yaml`
- Grafana dashboard created: `docs/observability/router-extensions-dashboard-grafana.json`
- References added to `OPERATIONAL_GUIDE.md`, `PROMETHEUS_ALERTS.md`, `FULL_DOCS.md`

