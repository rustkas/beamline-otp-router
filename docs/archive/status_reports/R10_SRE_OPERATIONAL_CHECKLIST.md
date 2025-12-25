# R10 Unified SRE Operational Checklist

**Purpose**: Single source of truth for R10 operational procedures  
**Audience**: SRE, DevOps, On-Call Engineers  
**Last Updated**: 2025-01-27

---

## A. Pre-Production (One-time setup before go-live)

### Dashboard Setup

- [ ] Dashboard `r10_dashboard.json` imported into Grafana
- [ ] All 6 panels display data correctly:
  - Circuit Breaker State (timeseries)
  - Open Circuits (stat)
  - Trigger Reasons (bargauge)
  - State Transitions (timeseries)
  - Timeout Remaining (timeseries)
  - Sliding Error Rate (timeseries)
- [ ] Variables `tenant_id` / `provider_id` auto-populate from metrics
- [ ] Datasource configured correctly (Prometheus)

### Alert Rules Setup

- [ ] Alert rules (`r10_alerts.yaml`) applied in Prometheus configuration
- [ ] All 4 alerts visible in Prometheus UI → Alerts:
  - `R10CircuitOpenTooLong`
  - `R10HighErrorRate`
  - `R10CircuitFlapping`
  - `R10LatencyTriggerDominating`
- [ ] Alert annotations include `runbook_url` pointing to GitHub
- [ ] Alert labels configured (severity, team, component)

### CLI Integration

- [ ] `router_ctl_r10.erl` integrated into `router_ctl` escript
- [ ] Command works: `./router_ctl r10 status <tenant> <provider>`
- [ ] Output includes all required sections:
  - State with visual indicators (✓ ✗ ⚠)
  - Trigger reason
  - Error rate with threshold indicator
  - Timeout remaining (formatted)
  - Configuration
  - Runbook URL

### CI Jobs Setup

- [ ] Required jobs enabled:
  - `router-r10-unit` (runs on MR changes)
  - `router-r10-e2e-ci` (runs on MR changes)
  - `router-r10-protective-rails` (required, blocks merge)
- [ ] Nightly jobs configured:
  - `router-r10-e2e-heavy` (scheduled/nightly)
  - `router-r10-property` (scheduled/nightly, non-blocking)
- [ ] Artifacts configured (CT reports, logs, retention policies)

---

## B. Daily Ops (Regular operational cycle)

### Morning Checks

- [ ] **Grafana Dashboard Review**:
  - [ ] No "stuck" open circuits (> 5 minutes)
  - [ ] No sudden error-rate spikes
  - [ ] No excessive flapping (> 5 transitions per minute)
  - [ ] Trigger reasons distribution looks normal

- [ ] **CLI Health Check** (for critical providers):
  ```bash
  ./router_ctl r10 status tenantX providerY
  ```
  Verify:
  - [ ] `State: CLOSED ✓` (or expected state)
  - [ ] `Last trigger reason: none` (or acceptable reason)
  - [ ] `Error rate: ✓ < threshold%` (green indicator)
  - [ ] No unexpected timeouts

- [ ] **Alert Status**:
  - [ ] No firing alerts in Prometheus/Alertmanager
  - [ ] All R10 alerts in `INACTIVE` or `PENDING` state
  - [ ] No alert notification noise

### During Business Hours

- [ ] Monitor dashboard for unusual patterns
- [ ] Respond to alert notifications within SLA
- [ ] Document any manual interventions

---

## C. Weekly Ops (Calibration and housekeeping)

### Threshold Review

- [ ] **Review alert firing frequency**:
  - [ ] `R10HighErrorRate` - adjust `for` duration or threshold if too noisy
  - [ ] `R10CircuitFlapping` - adjust transition threshold if false positives
  - [ ] `R10LatencyTriggerDominating` - verify latency thresholds are appropriate

- [ ] **Adjust thresholds if needed**:
  - [ ] Error rate: `> 0.5` → `> 0.6` or `> 0.7` (if too sensitive)
  - [ ] Transitions: `> 10/5m` → `> 20/5m` (if too sensitive)
  - [ ] `for` duration: `5m` → `10m` or `15m` (for less noise)

### CI Health Check

- [ ] Review `router-r10-e2e-ci` job results:
  - [ ] No flaky tests
  - [ ] Execution time within expected range
  - [ ] Artifacts accessible

- [ ] Review nightly `router-r10-e2e-heavy` job:
  - [ ] Catches load-related regressions
  - [ ] Execution time acceptable
  - [ ] No false positives

### Manual Provider Health Check

- [ ] Run CLI on 10-20 critical providers:
  ```bash
  for tenant in tenant1 tenant2 ...; do
    for provider in provider1 provider2 ...; do
      ./router_ctl r10 status "$tenant" "$provider"
    done
  done
  ```
- [ ] Compare latency vs open frequency
- [ ] Document any anomalies

---

## D. Incident Checklist (When alert fires)

### Step 1: Identify Alert Type

Determine which alert fired:
- [ ] `R10CircuitOpenTooLong` - Circuit stuck in open state
- [ ] `R10HighErrorRate` - High error rate detected
- [ ] `R10CircuitFlapping` - Excessive state transitions
- [ ] `R10LatencyTriggerDominating` - Latency-based triggers dominating

### Step 2: Execute CLI Diagnosis

```bash
./router_ctl r10 status <tenant> <provider>
```

Verify output:
- [ ] Current state (closed/open/half_open)
- [ ] Last trigger reason
- [ ] Error rate percentage
- [ ] Timeout remaining (if open)
- [ ] Configuration values

### Step 3: Follow Runbook

- [ ] Open runbook URL from alert annotation or CLI output
- [ ] Follow scenario-based troubleshooting section
- [ ] Execute recommended diagnostic steps

### Step 4: Mitigation Actions

Based on trigger type:

**For `failure_threshold_exceeded` or `error_rate_threshold_exceeded`**:
- [ ] Check provider health/status
- [ ] Temporarily disable provider if unhealthy
- [ ] Shift traffic to backup provider if available
- [ ] Increase `failure_threshold` or `error_rate_threshold` if justified

**For `latency_threshold_exceeded`**:
- [ ] Check provider latency in Grafana
- [ ] Verify no concurrent deploys/config changes
- [ ] Consider traffic offload or load balancing
- [ ] Increase `latency_threshold_ms` if provider SLA allows

**For `R10CircuitFlapping`**:
- [ ] Review recent configuration changes
- [ ] Increase `open_timeout_ms` to reduce sensitivity
- [ ] Increase `success_threshold` for half-open → closed transition

### Step 5: Verification

- [ ] Circuit breaker transitions: `open → half_open → closed`
- [ ] Metrics return to baseline in Grafana
- [ ] Alerts clear automatically within 10-15 minutes
- [ ] No recurrence within 1 hour

### Step 6: Post-Incident

- [ ] File incident in incident tracker with full timeline
- [ ] Review R10 thresholds and provider SLA
- [ ] Update runbook with lessons learned
- [ ] Document any configuration changes made

---

## E. Do/Don't Safety List (Protective Rails)

### ✅ DO

- **Change only parameters** within `Config` in ETS (via `record_state_with_config/3`)
- **Use only public API**:
  - `should_allow/2`
  - `record_success/2`
  - `record_failure/2`
  - `get_state/2`
  - `get_status/2`
- **Check state** via CLI (`router_ctl r10 status`)
- **Read metrics** via `router_r10_metrics` module (never direct ETS)
- **Use constants** for trigger reasons from `router_r10_metrics:trigger_reason_*()`

### ❌ DO NOT

- ⛔ **Direct ETS access** in production or tests (`ets:lookup(router_provider_circuit_breaker, ...)`)
- ⛔ **Hardcode trigger reason strings** (use constants from `router_r10_metrics`)
- ⛔ **Stop `router_circuit_breaker` process** manually
- ⛔ **Call `application:stop(beamline_router)`** under load
- ⛔ **Modify ETS table structure** without coordination
- ⛔ **Bypass circuit breaker** by calling provider directly

---

## F. Emergency Procedures

### Circuit Breaker Stuck Open (> 10 minutes)

1. **Verify provider health**:
   ```bash
   ./router_ctl r10 status <tenant> <provider>
   ```

2. **Check if provider is actually down**:
   - Review provider status page
   - Check provider logs
   - Test provider endpoint directly

3. **If provider is healthy**:
   - Manually reset circuit breaker (if API available)
   - Or restart router application (last resort)

4. **If provider is down**:
   - Disable provider in router configuration
   - Route traffic to backup provider
   - Document incident

### Excessive Flapping (> 20 transitions in 5 minutes)

1. **Immediate action**: Increase `open_timeout_ms` to reduce sensitivity
2. **Review configuration**: Check if thresholds are too aggressive
3. **Monitor**: Watch for stabilization after config change
4. **Document**: Record flapping pattern for post-incident review

### All Providers for Tenant Affected

1. **Check tenant configuration**: Verify no tenant-level issues
2. **Review recent changes**: Check for config/deploy changes
3. **Escalate**: This may indicate broader system issue
4. **Document**: Full incident timeline required

---

## References

- **Runbook**: `test/R10_RUNBOOK.md` - Detailed incident diagnostics
- **CLI Examples**: `docs/R10_CLI_EXAMPLES.md` - Real-world CLI output examples
- **Maintenance Checklist**: `test/R10_MAINTENANCE_CHECKLIST.md` - Code change guidelines
- **Observability Spec**: `test/R10_OBSERVABILITY_REQUIREMENTS.md` - Metrics and alerts specification
- **CI Profiles**: `test/R10_CI_PROFILES.md` - Test execution profiles

