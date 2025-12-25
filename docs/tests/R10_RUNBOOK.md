# R10 Operational Runbook: Circuit Breaker Incident Diagnostics

This runbook provides step-by-step guidance for diagnosing and reproducing incidents related to the R10 (Retry Logic + Circuit Breaker) functionality in the Router.

## 1. Incident Detection and Initial Triage

**Symptoms**:
- Increased `router_nats_publish_errors_total` (especially `circuit_open` reason)
- Alerts on `router_circuit_breaker_state` (e.g., `state=open` for a critical provider)
- Customer reports of "provider unavailable" or "service degraded" for specific tenants/providers

**Initial Checks (Grafana/Monitoring Dashboard)**:

**Dashboard**: "Circuit Breakers (R10)" (see Observability section below)

1. **`router_circuit_breaker_state`**: Check the current state for the affected `tenant_id`/`provider_id`
   - `1.0` = `open`
   - `0.5` = `half_open`
   - `0.0` = `closed`
   - Check both per-tenant/provider view and aggregated view

2. **`router_circuit_breaker_trigger_reason`**: Identify the `reason` label for the latest state transition
   - `failure_threshold_exceeded` - Consecutive failures exceeded threshold
   - `error_rate_threshold_exceeded` - Error rate exceeded threshold
   - `latency_threshold_exceeded` - Latency consistently exceeded threshold
   - `half_open_failure` - Failure occurred during half-open probe
   - `timeout_elapsed` - Timeout expired while in open state

3. **`router_nats_publish_attempts_total` / `router_nats_publish_errors_total`**: Look for spikes in errors or drops in attempts for the affected provider

4. **`router_circuit_breaker_state_transitions_total`**: Check transition rate (high rate = flapping)

5. **`router_circuit_breaker_error_rate`**: Check the calculated error rate for the provider (if available)

## 2. Live System Diagnostics

**How to check breaker state in live system** (without tests):

1. **Via Metrics API** (if available):
   ```erlang
   %% In Erlang shell or via admin API
   router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
       tenant_id => ~"affected_tenant",
       provider_id => ~"affected_provider"
   }).
   ```

2. **Via Circuit Breaker API**:
   ```erlang
   %% Get current state
   router_circuit_breaker:get_state(~"tenant_id", ~"provider_id").
   
   %% Check if requests are allowed
   router_circuit_breaker:should_allow(~"tenant_id", ~"provider_id").
   ```

3. **Via Monitoring Dashboard**:
   - Navigate to "Circuit Breakers (R10)" dashboard
   - Filter by `tenant_id` and `provider_id`
   - Review state history and transition timeline

## 3. Local Reproduction (using R10 E2E Test Suite)

The `router_publish_failure_e2e_SUITE` can be used to quickly reproduce and understand the circuit breaker's behavior under controlled conditions.

**Steps**:

1. **Identify Affected Parameters**: From monitoring, note down:
   - `tenant_id`, `provider_id`
   - Observed `trigger_reason`
   - Approximate `failure_threshold`, `error_rate_threshold`, `latency_threshold_ms` (from config or logs)

2. **Select Relevant Scenario**: Choose an E2E scenario that best matches the observed `trigger_reason`:
   - `scenario_mass_failure_opens_breaker`: For `failure_threshold_exceeded` or `error_rate_threshold_exceeded` due to high failure volume
   - `scenario_latency_based_trigger`: For `latency_threshold_exceeded`
   - `scenario_error_rate_partial_failure`: For `error_rate_threshold_exceeded` with mixed success/failure
   - `scenario_recovery_after_failure`: To test recovery path

3. **Run the Scenario Locally**:
   ```bash
   # Navigate to router app directory
   cd apps/otp/router
   
   # Run the chosen scenario
   rebar3 ct --suite test/router_publish_failure_e2e_SUITE --case scenario_mass_failure_opens_breaker
   
   # Or with heavy profile for more realistic load
   R10_PROFILE=heavy rebar3 ct --suite test/router_publish_failure_e2e_SUITE --case scenario_mass_failure_opens_breaker
   ```

4. **Analyze Test Logs**: Pay attention to:
   - `router_logger` messages (especially `event="circuit_breaker_state_changed"`)
   - `router_r10_metrics:dump_metrics()` output on test failures to see the exact state of metrics

## 4. Scenario-Based Troubleshooting

### Scenario A: Breaker Stuck in Open State

**Symptoms**: Breaker has been `open` for extended period (> `open_timeout_ms`)

**Actions**:
1. Check `router_circuit_breaker_trigger_reason` to understand why it opened
2. Verify `open_timeout_ms` configuration (should transition to `half_open` after timeout)
3. Check for `timeout_elapsed` trigger reason (indicates timeout-based transition)
4. If timeout not working, check `router_circuit_breaker` process health
5. Reproduce locally: `scenario_recovery_after_failure`

### Scenario B: Breaker Flapping (Rapid State Transitions)

**Symptoms**: High `router_circuit_breaker_state_transitions_total` rate

**Actions**:
1. Check transition pattern: `closed → open → half_open → closed` repeating
2. Review `trigger_reason` history to identify root cause
3. Check if thresholds are too sensitive for current load
4. Consider adjusting `failure_threshold`, `error_rate_threshold`, or `success_threshold`
5. Reproduce locally: Run E2E suite with `heavy` profile to simulate load

### Scenario C: Latency-Based Trigger Dominating

**Symptoms**: `trigger_reason=latency_threshold_exceeded` is most common

**Actions**:
1. Verify `latency_threshold_ms` configuration
2. Check actual latency metrics for the provider
3. Determine if threshold is too low or provider is genuinely slow
4. Review recent changes to provider or network configuration
5. Reproduce locally: `scenario_latency_based_trigger`

### Scenario D: Error Rate Trigger with Mixed Success/Failure

**Symptoms**: `trigger_reason=error_rate_threshold_exceeded` but some requests succeed

**Actions**:
1. Check `error_rate_threshold` and `error_rate_window_seconds` configuration
2. Review error rate calculation (should be percentage over window)
3. Verify error classification (what counts as "error" vs "success")
4. Reproduce locally: `scenario_error_rate_partial_failure`

## 5. Remediation and Configuration Tuning

Based on the reproduction and analysis:

1. **Adjust Thresholds**:
   - `failure_threshold`: Number of consecutive failures to open
   - `error_rate_threshold`: Percentage of errors over `error_rate_window_seconds`
   - `latency_threshold_ms`: Latency in milliseconds to trigger opening
   - `open_timeout_ms`: Time in `open` state before transitioning to `half_open`
   - `success_threshold`: Consecutive successes in `half_open` to `closed`

2. **Configuration Location**: These parameters are typically configured via application environment variables or a configuration management system. Refer to `router_circuit_breaker.erl` for default values and `router_r10_client_utils:get_r10_config/0` for how test profiles override them.

3. **Deployment**: Apply configuration changes to a staging environment first, then monitor closely before rolling out to production.

## 6. Observability Integration

### Grafana Dashboard

**Location**: `apps/otp/router/observability/r10_dashboard.json`

**Import Instructions**:
1. Open Grafana → Dashboards → Import
2. Upload `r10_dashboard.json` or paste JSON content
3. Configure Prometheus datasource
4. Dashboard will display:
   - Circuit breaker state (per tenant/provider + aggregated)
   - Trigger reasons distribution
   - State transitions timeline
   - Sliding error rate
   - Timeout remaining for open circuits

**Dashboard Link**: Configure after import in your Grafana instance

### Alert Rules

**Location**: `apps/otp/router/observability/r10_alerts.yaml`

**Deployment Instructions**:
1. Add to Prometheus configuration:
   ```yaml
   rule_files:
     - "apps/otp/router/observability/r10_alerts.yaml"
   ```
2. Or add to Alertmanager rules directory
3. Reload Prometheus/Alertmanager configuration
4. Verify alerts appear in Alertmanager UI

**Alert Rules**:
- `R10CircuitOpenTooLong` - Circuit stuck in open state
- `R10HighErrorRate` - High error rate detected
- `R10CircuitFlapping` - Excessive state transitions
- `R10LatencyTriggerDominating` - Latency-based triggers dominating

All alerts include runbook URLs pointing to this document.

### CLI Diagnostic Tool

**Command**: `./bin/router_ctl r10 status <tenant> <provider>`

**Usage**:
```bash
# Check circuit breaker status
./bin/router_ctl r10 status tenant1 provider1

# Output includes:
# - Current state
# - Latest trigger reason
# - Error rate
# - Timeout remaining
# - Should allow check
# - Runbook URL
```

**Module**: `router_ctl_r10.erl` (integrate into `router_ctl` escript)

## 7. Observability Integration (Legacy Section)

### Grafana Dashboard: "Circuit Breakers (R10)"

**Location**: Monitoring dashboards → "Circuit Breakers (R10)"

**Key Panels**:
1. **Circuit Breaker State** (Gauge):
   - Per tenant/provider: `router_circuit_breaker_state{tenant_id="...", provider_id="..."}`
   - Aggregated: `sum(router_circuit_breaker_state) by (state)`

2. **State Transitions** (Stacked Area):
   - `router_circuit_breaker_state_transitions_total` grouped by `from` and `to` labels

3. **Trigger Reasons** (Pie Chart / Bar Chart):
   - `router_circuit_breaker_trigger_reason` grouped by `reason` label

4. **Publish Attempts vs Errors** (Time Series):
   - `router_nats_publish_attempts_total` vs `router_nats_publish_errors_total{reason="circuit_open"}`

### Alerts

**Alert 1: Breaker Stuck Open**
- **Condition**: `router_circuit_breaker_state{state="open"} == 1.0` for > 5 minutes
- **Severity**: Warning
- **Action**: Link to `R10_RUNBOOK.md#scenario-a-breaker-stuck-in-open-state`

**Alert 2: Breaker Flapping**
- **Condition**: `rate(router_circuit_breaker_state_transitions_total[5m]) > 10`
- **Severity**: Warning
- **Action**: Link to `R10_RUNBOOK.md#scenario-b-breaker-flapping-rapid-state-transitions`

**Alert 3: High Circuit Open Errors**
- **Condition**: `rate(router_nats_publish_errors_total{reason="circuit_open"}[5m]) > 100`
- **Severity**: Critical
- **Action**: Link to `R10_RUNBOOK.md` for diagnostics

## 7. Long-Term Prevention

- **Monitoring**: Ensure robust alerting is in place for critical R10 metrics (see Observability section above)
- **Regular Review**: Periodically review `R10_MAINTENANCE_CHECKLIST.md` when making changes to R10 or related components
- **Documentation**: Keep `QA_TEST_PLAN.md` and `OBSERVABILITY_CONVENTIONS.md` updated with any new R10 features or metric changes
- **Incident Integration**: Include `R10_RUNBOOK.md` link in incident ticket templates and alert descriptions

