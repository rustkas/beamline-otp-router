# R10 Observability Requirements

This document specifies the observability requirements for R10 (Retry Logic + Circuit Breaker) in production environments.

## Dashboard: "Circuit Breakers (R10)"

### Required Panels

#### 1. Circuit Breaker State (Gauge)

**Metrics**:
- `router_circuit_breaker_state{tenant_id="...", provider_id="...", state="..."}`

**Visualization**:
- Per tenant/provider: Individual gauge showing current state (0.0=closed, 0.5=half_open, 1.0=open)
- Aggregated: Stacked gauge showing distribution of states across all tenant/provider pairs

**Purpose**: Quick overview of circuit breaker health across the system

#### 2. State Transitions (Stacked Area Chart)

**Metrics**:
- `router_circuit_breaker_state_transitions_total{from="...", to="..."}`

**Visualization**:
- Stacked area chart showing transition rates over time
- Grouped by `from` and `to` labels
- Key transitions: `closed→open`, `open→half_open`, `half_open→closed`, `half_open→open`

**Purpose**: Identify flapping patterns and transition frequency

#### 3. Trigger Reasons (Pie Chart / Bar Chart)

**Metrics**:
- `router_circuit_breaker_trigger_reason{tenant_id="...", provider_id="...", reason="..."}`

**Visualization**:
- Pie chart showing distribution of trigger reasons
- Bar chart showing trigger reason frequency over time
- Grouped by `reason` label

**Purpose**: Understand why circuit breakers are opening (latency vs failures vs error rate)

#### 4. Publish Attempts vs Errors (Time Series)

**Metrics**:
- `router_nats_publish_attempts_total{tenant_id="...", provider_id="..."}`
- `router_nats_publish_errors_total{tenant_id="...", provider_id="...", reason="circuit_open"}`

**Visualization**:
- Dual-axis time series chart
- Left axis: Attempts (line)
- Right axis: Errors with `reason="circuit_open"` (line, different color)

**Purpose**: Correlate circuit breaker behavior with actual publish activity

#### 5. Error Rate (Time Series)

**Metrics**:
- Calculated: `rate(router_nats_publish_errors_total[5m]) / rate(router_nats_publish_attempts_total[5m])`

**Visualization**:
- Time series showing error rate percentage
- Horizontal line showing configured `error_rate_threshold`

**Purpose**: Visualize error rate against threshold to understand trigger conditions

### Dashboard Filters

- **Tenant ID**: Filter by specific tenant
- **Provider ID**: Filter by specific provider
- **State**: Filter by current state (closed/open/half_open)
- **Time Range**: Standard Grafana time range selector

## Alerts

### Alert 1: Breaker Stuck Open

**Name**: `r10_circuit_breaker_stuck_open`

**Condition**:
```
router_circuit_breaker_state{state="open"} == 1.0
AND
time() - last_change_time > 5 minutes
```

**Severity**: Warning

**Description**: Circuit breaker has been in `open` state for more than 5 minutes. This may indicate:
- Provider is genuinely down
- `open_timeout_ms` configuration issue
- Circuit breaker process health issue

**Actions**:
- Link to `R10_RUNBOOK.md#scenario-a-breaker-stuck-in-open-state`
- Check `router_circuit_breaker_trigger_reason` to understand why it opened
- Verify `open_timeout_ms` configuration

### Alert 2: Breaker Flapping

**Name**: `r10_circuit_breaker_flapping`

**Condition**:
```
rate(router_circuit_breaker_state_transitions_total[5m]) > 10
```

**Severity**: Warning

**Description**: Circuit breaker is transitioning states too frequently (flapping). This may indicate:
- Thresholds are too sensitive
- Provider is unstable
- Configuration mismatch

**Actions**:
- Link to `R10_RUNBOOK.md#scenario-b-breaker-flapping-rapid-state-transitions`
- Review transition pattern in dashboard
- Check threshold configurations

### Alert 3: High Circuit Open Errors

**Name**: `r10_high_circuit_open_errors`

**Condition**:
```
rate(router_nats_publish_errors_total{reason="circuit_open"}[5m]) > 100
```

**Severity**: Critical

**Description**: High rate of publish errors due to circuit breaker being open. This indicates:
- Multiple circuit breakers are open
- Significant impact on publish operations
- Potential provider outage

**Actions**:
- Link to `R10_RUNBOOK.md` for full diagnostics
- Check dashboard for affected tenant/provider pairs
- Review trigger reasons to identify root cause

### Alert 4: Dominant Trigger Reason

**Name**: `r10_dominant_trigger_reason`

**Condition**:
```
count(router_circuit_breaker_trigger_reason{reason="latency_threshold_exceeded"}) > 
count(router_circuit_breaker_trigger_reason{reason="failure_threshold_exceeded"}) * 2
```

**Severity**: Info

**Description**: Latency-based triggers are dominating over failure-based triggers. This may indicate:
- Provider latency issues
- Network problems
- `latency_threshold_ms` may be too low

**Actions**:
- Review latency metrics for affected providers
- Consider adjusting `latency_threshold_ms` if threshold is too aggressive
- Link to `R10_RUNBOOK.md#scenario-c-latency-based-trigger-dominating`

## Integration Points

### Incident Management

- **Ticket Templates**: Include link to `R10_RUNBOOK.md` in incident ticket templates
- **Alert Descriptions**: Include `R10_RUNBOOK.md` link in all R10 alert descriptions
- **On-Call Runbooks**: Reference `R10_RUNBOOK.md` in on-call documentation

### Monitoring

- **Dashboard Links**: Include dashboard link in `R10_RUNBOOK.md`
- **Alert Links**: Include alert definitions in monitoring system
- **Metric Documentation**: Reference `OBSERVABILITY_CONVENTIONS.md` for metric details

## Implementation Status

- [ ] Dashboard "Circuit Breakers (R10)" created in Grafana
- [ ] All required panels added to dashboard
- [ ] Dashboard filters configured
- [ ] Alert `r10_circuit_breaker_stuck_open` configured
- [ ] Alert `r10_circuit_breaker_flapping` configured
- [ ] Alert `r10_high_circuit_open_errors` configured
- [ ] Alert `r10_dominant_trigger_reason` configured
- [ ] Alert descriptions include `R10_RUNBOOK.md` links
- [ ] Dashboard link added to `R10_RUNBOOK.md`
- [ ] Incident ticket templates updated with runbook link

