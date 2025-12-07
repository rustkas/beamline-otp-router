# R10 — How to Operate in Production

**Purpose**: Onboarding guide for SRE/DevOps/Backend engineers supporting R10  
**Audience**: New engineers joining R10 operations  
**Last Updated**: 2025-01-27

---

## 1. What is R10 (Quick Overview)

R10 is the **Circuit Breaker Layer** between Router → Provider.

**Core Functions**:
- Detects mass failures and prevents cascade
- Fail-fast without overloading unhealthy providers
- Protects against latency cascade
- Safe recovery mechanism (half_open → closed)
- Metrics exported to Prometheus
- Alerts configured in Grafana

**Why it exists**: Prevents a single failing provider from degrading the entire Router service.

---

## 2. Where to Check Status

### 2.1 Grafana Dashboard

**Location**: Imported from `apps/otp/router/observability/r10_dashboard.json`

**Key Panels**:
1. **Circuit State** - Current state per tenant/provider (closed/open/half_open)
2. **Open Circuits** - Count of currently open circuits
3. **Trigger Reasons** - Distribution of why circuits opened
4. **State Transitions** - Transition rate over time
5. **Timeout Remaining** - Time until half-open transition (for open circuits)
6. **Sliding Error Rate** - Error rate over sliding window

**Variables**: Filter by `tenant_id` and `provider_id`

**When to use**: Historical analysis, multi-tenant overview, trend analysis

---

### 2.2 CLI: router_ctl

**Command**:
```bash
./router_ctl r10 status <tenant> <provider>
```

**Output includes**:
- Current state with visual indicators (✓ closed, ✗ open, ⚠ half_open)
- Last state change timestamp
- Consecutive failure/success counts
- Half-open probe attempts (if in half_open state)
- Latest trigger reason
- Error rate percentage (with threshold indicator)
- Timeout remaining (if in open state)
- Configuration values (thresholds, timeouts)
- Should allow check (yes/no)
- Runbook URL

**When to use**: Quick diagnosis during incidents, verifying dashboard data, understanding current configuration

**Example**:
```bash
$ ./router_ctl r10 status tenant_prod_1 provider_openai

╔══════════════════════════════════════════════════════════════╗
║  R10 Circuit Breaker Status                                    ║
╚══════════════════════════════════════════════════════════════╝

Tenant ID:  tenant_prod_1
Provider ID: provider_openai

┌─ State ─────────────────────────────────────────────────────┐
│ State: CLOSED ✓ (normal operation, requests flow through)
│ Last state change: 2025-01-27 14:30:15 UTC (125.3 seconds ago)
└──────────────────────────────────────────────────────────────┘
...
```

See `docs/R10_CLI_EXAMPLES.md` for more examples.

---

## 3. How Alerts Work (Prometheus)

**4 Alert Rules**:

1. **`R10CircuitOpenTooLong`**
   - **Trigger**: Circuit in open state > 5 minutes
   - **Severity**: Warning
   - **Meaning**: Provider may be unavailable, requests failing fast

2. **`R10HighErrorRate`**
   - **Trigger**: Error rate > 50% for > 5 minutes
   - **Severity**: Warning
   - **Meaning**: Provider is unstable, high failure rate

3. **`R10CircuitFlapping`**
   - **Trigger**: > 10 state transitions in 5 minutes
   - **Severity**: Warning
   - **Meaning**: Circuit opening/closing rapidly, threshold may be too sensitive

4. **`R10LatencyTriggerDominating`**
   - **Trigger**: Latency-based triggers > 70% of all openings
   - **Severity**: Info
   - **Meaning**: Provider is consistently slow, latency threshold may be too aggressive

**All alerts include**:
- `runbook_url` annotation pointing to `R10_RUNBOOK.md`
- Proper severity labels
- Team and component labels

---

## 4. How to Respond to Incidents

### Step 1: Check State

```bash
./router_ctl r10 status <tenant> <provider>
```

**Interpret output**:
- `State: CLOSED ✓` → Normal operation
- `State: OPEN ✗` → Circuit is open, requests blocked
- `State: HALF_OPEN ⚠` → Probing for recovery

### Step 2: Check Trigger Reason

**Common trigger reasons**:

- **`failure_threshold_exceeded`**
  - Meaning: Too many consecutive failures
  - Action: Check provider error logs, verify provider health

- **`error_rate_threshold_exceeded`**
  - Meaning: Error rate exceeded threshold over sliding window
  - Action: Check provider stability, review error patterns

- **`latency_threshold_exceeded`**
  - Meaning: Provider latency consistently exceeds threshold
  - Action: Check provider latency, verify no network issues

- **`half_open_failure`**
  - Meaning: Recovery probe failed, circuit reopened
  - Action: Provider may still be unhealthy, wait for next recovery attempt

- **`timeout_elapsed`**
  - Meaning: Open timeout expired, transitioning to half_open
  - Action: Normal recovery process, monitor half_open attempts

### Step 3: Take Action

**For mass failures**:
- Temporarily disable provider in router configuration
- Shift traffic to backup provider if available
- Review provider status page/logs

**For latency issues**:
- Check provider latency in Grafana
- Verify no concurrent deploys or config changes
- Consider traffic offload or load balancing
- Increase `latency_threshold_ms` if provider SLA allows

**For flapping**:
- Increase `open_timeout_ms` to reduce sensitivity
- Increase `success_threshold` for half-open → closed transition
- Review recent configuration changes

### Step 4: Verify in Grafana

- Check error rate trends
- Verify state transitions timeline
- Monitor timeout remaining (should decrease for open circuits)
- Review trigger reasons distribution

---

## 5. Weekly Tasks

### Threshold Calibration

- Review alert firing frequency
- Adjust thresholds if too noisy or too insensitive
- Document threshold changes

### CI Health Check

- Review `router-r10-e2e-ci` job results
- Check nightly `router-r10-e2e-heavy` job
- Verify no flaky tests

### Manual Provider Health Check

Run CLI on critical providers:
```bash
./router_ctl r10 status tenant1 provider1
./router_ctl r10 status tenant1 provider2
# ... etc
```

Compare:
- Latency vs open frequency
- Error rate trends
- State stability

### Review Provider Changes

- Check for API errors from providers
- Review provider SLA changes
- Document any provider-specific configuration adjustments

---

## 6. Documentation Locations

**Core Documentation**:
- **Runbook**: `test/R10_RUNBOOK.md` - Detailed incident diagnostics
- **Operational Checklist**: `docs/R10_SRE_OPERATIONAL_CHECKLIST.md` - This document's companion checklist
- **CLI Examples**: `docs/R10_CLI_EXAMPLES.md` - Real-world CLI output examples
- **Maintenance Checklist**: `test/R10_MAINTENANCE_CHECKLIST.md` - Guidelines for code changes

**Observability**:
- **Observability Spec**: `test/R10_OBSERVABILITY_REQUIREMENTS.md` - Metrics and alerts specification
- **Dashboard README**: `observability/R10_DASHBOARD_README.md` - Dashboard setup guide

**Testing**:
- **CI Profiles**: `test/R10_CI_PROFILES.md` - Test execution profiles
- **Test Plan**: `docs/dev/QA_TEST_PLAN.md` - R10 E2E test suite details

**Integration**:
- **CLI Integration**: `docs/R10_CLI_INTEGRATION.md` - How to integrate CLI into router_ctl
- **Final Status**: `test/R10_FINAL_STATUS.md` - Production deployment checklist

---

## 7. Quick Reference

### Common Commands

```bash
# Check circuit breaker status
./router_ctl r10 status <tenant> <provider>

# Check help
./router_ctl r10 help
```

### Common States

- **CLOSED ✓**: Normal operation, requests flow through
- **OPEN ✗**: Circuit is open, requests fail immediately
- **HALF_OPEN ⚠**: Probing for recovery, limited requests allowed

### Common Trigger Reasons

- `failure_threshold_exceeded` - Too many consecutive failures
- `error_rate_threshold_exceeded` - Error rate exceeded threshold
- `latency_threshold_exceeded` - Latency consistently exceeds threshold
- `half_open_failure` - Recovery probe failed
- `timeout_elapsed` - Open timeout expired, transitioning to half_open

### Alert Response Time

- **Warning alerts**: Respond within 15 minutes
- **Critical alerts**: Respond within 5 minutes
- **Info alerts**: Review during next daily check

---

## 8. Getting Help

**During incidents**:
1. Check `R10_RUNBOOK.md` for scenario-based troubleshooting
2. Use CLI to get current state: `./router_ctl r10 status <tenant> <provider>`
3. Check Grafana dashboard for historical context
4. Review alert annotations for runbook URL

**For questions**:
- Review documentation in `test/` and `docs/` directories
- Check `R10_MAINTENANCE_CHECKLIST.md` for code change guidelines
- Consult team lead or R10 maintainer

