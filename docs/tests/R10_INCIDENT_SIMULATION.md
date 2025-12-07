# R10 Incident Simulation: "Provider X Latency Outage"

**Purpose**: Training scenario for SRE/Backend/On-Call engineers  
**Goal**: Practice R10 incident response procedures  
**Duration**: ~20 minutes  
**Last Updated**: 2025-01-27

---

## 🎯 Learning Objectives

After completing this simulation, you should be able to:

- Identify trigger type from alert and CLI output
- Assess impact using dashboard and CLI
- Apply CLI for real-time diagnostics
- Use dashboard for historical analysis
- Make informed mitigation decisions
- Execute post-incident steps

---

## 📍 Initial Conditions

**Environment**:
- Cluster operating normally
- Provider X is healthy
- R10 circuit breaker active in production
- Alerts enabled and configured
- Dashboard accessible in Grafana
- CLI available: `./router_ctl r10 status`

**Test Tenant/Provider**:
- Tenant: `tenant_sim_1`
- Provider: `provider_sim_1`

---

## 🔥 Incident Start (T+0m)

### Fault Injection

**Simulate latency outage** using fault injection script:

```bash
ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret \
  scripts/r10_simulate_latency_outage.sh
```

**Fault**: Delay 6000ms on `publish` operations for provider X

**Expected Impact**: Latency exceeds `latency_threshold_ms` (5000ms), triggering circuit breaker

**Goal**: Open circuit breaker via latency-based trigger

---

## 🔔 Alert Trigger (T+1m)

### Prometheus Alert Fires

**Alert**: `R10LatencyTriggerDominating`

**Alert Details**:
- **Name**: `R10LatencyTriggerDominating`
- **Severity**: Info
- **Message**: "Circuit breaker opened by latency threshold for > 1m"
- **Runbook URL**: `https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md`

**On-Call receives**: Alert notification via PagerDuty/Slack/etc.

---

## 🧭 On-Call Actions (T+1..5m)

### Step 1: Execute CLI Diagnosis

```bash
./router_ctl r10 status tenant_sim_1 provider_sim_1
```

**Expected Output**:
```
╔══════════════════════════════════════════════════════════════╗
║  R10 Circuit Breaker Status                                    ║
╚══════════════════════════════════════════════════════════════╝

Tenant ID:  tenant_sim_1
Provider ID: provider_sim_1

┌─ State ─────────────────────────────────────────────────────┐
│ State: OPEN ✗ (circuit is open, requests fail immediately)
│ Last state change: 2025-01-27 15:01:30 UTC (45.8 seconds ago)
└──────────────────────────────────────────────────────────────┘

┌─ Counters ──────────────────────────────────────────────────┐
│ Consecutive failures: 0
│ Consecutive successes: 0
└──────────────────────────────────────────────────────────────┘

┌─ Metrics ───────────────────────────────────────────────────┐
│ Last trigger reason: latency_threshold_exceeded
│ Error rate: ✓ 0.00% (0.0000)
│ Timeout remaining: 0.9 minutes (54.2 seconds)
└──────────────────────────────────────────────────────────────┘

┌─ Configuration ──────────────────────────────────────────────┐
│ Failure threshold: 5 consecutive failures
│ Error rate threshold: 50.0% (0.50)
│ Latency threshold: 5000 ms
│ Open timeout: 60000 ms (60.0 seconds)
└──────────────────────────────────────────────────────────────┘

┌─ Request Handling ───────────────────────────────────────────┐
│ Should allow: NO ✗ (circuit is open, requests blocked)
└──────────────────────────────────────────────────────────────┘

📖 Runbook: https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

**Key Observations**:
- ✅ State: `OPEN ✗` (circuit is open)
- ✅ Trigger reason: `latency_threshold_exceeded` (latency-based trigger)
- ✅ Error rate: Low (0.00%) - confirms latency issue, not error issue
- ✅ Timeout remaining: ~54 seconds (circuit will attempt recovery soon)

### Step 2: Check Grafana Dashboard

**Navigate to**: Grafana → "Circuit Breakers (R10)" dashboard

**Verify**:
- [ ] Circuit state panel shows `OPEN` for tenant_sim_1/provider_sim_1
- [ ] Trigger reasons panel shows `latency_threshold_exceeded` as dominant
- [ ] Error rate panel shows low error rate (confirms latency issue)
- [ ] Timeout remaining panel shows decreasing countdown
- [ ] State transitions panel shows recent `closed → open` transition

**Key Observations**:
- Latency spike visible in latency metrics (if available)
- No corresponding error rate spike
- Trigger reason distribution shows latency trigger

### Step 3: Diagnose Provider

**Check Provider Health**:
- [ ] Review provider status page (if available)
- [ ] Check provider logs for latency issues
- [ ] Verify no concurrent deploys or config changes
- [ ] Test provider endpoint directly (if accessible)

**Check Router Logs**:
- [ ] Look for `event="circuit_breaker_state_changed"` entries
- [ ] Verify `reason="latency_threshold_exceeded"`
- [ ] Check for any error messages related to provider X

---

## 🛠 Mitigation (T+5..10m)

### Decision Point

**Based on diagnosis**:

**If provider X is actually unhealthy**:
- [ ] Temporarily disable provider X in router configuration
- [ ] Shift traffic to backup provider (if available)
- [ ] Document incident and provider status

**If provider X is healthy but slow**:
- [ ] Verify no network issues
- [ ] Consider traffic offload or load balancing
- [ ] Increase `latency_threshold_ms` for tenant_sim_1/provider_sim_1 if provider SLA allows
- [ ] Monitor for stabilization

**If fault injection is still active**:
- [ ] Clear fault injection (see Step 4 below)
- [ ] Monitor circuit recovery

### Recommended Actions

**For this simulation** (fault injection scenario):
1. Clear fault injection to allow recovery
2. Monitor circuit transition: `open → half_open → closed`
3. Verify metrics return to baseline

---

## 🧹 Post-Incident (T+10..20m)

### Step 1: Clear Fault Injection

```bash
ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret \
  scripts/r10_clear_faults.sh
```

**Expected**: All fault injections cleared, provider X returns to normal latency

### Step 2: Verify Recovery

**Monitor circuit transition**:

```bash
# Check every 10 seconds
watch -n 10 './router_ctl r10 status tenant_sim_1 provider_sim_1'
```

**Expected sequence**:
1. `State: OPEN ✗` (initially)
2. `State: HALF_OPEN ⚠` (after timeout expires)
3. `State: CLOSED ✓` (after successful probes)

**Verify in Grafana**:
- [ ] Circuit state transitions: `open → half_open → closed`
- [ ] Latency returns to baseline
- [ ] No error rate spike
- [ ] Alerts clear automatically

### Step 3: Final CLI Check

```bash
./router_ctl r10 status tenant_sim_1 provider_sim_1
```

**Expected Output**:
```
┌─ State ─────────────────────────────────────────────────────┐
│ State: CLOSED ✓ (normal operation, requests flow through)
│ Last state change: 2025-01-27 15:12:45 UTC (30.2 seconds ago)
└──────────────────────────────────────────────────────────────┘

┌─ Metrics ───────────────────────────────────────────────────┐
│ Last trigger reason: latency_threshold_exceeded
│ Error rate: ✓ 0.00% (0.0000)
└──────────────────────────────────────────────────────────────┘

┌─ Request Handling ───────────────────────────────────────────┐
│ Should allow: YES ✓ (requests will be processed)
└──────────────────────────────────────────────────────────────┘
```

### Step 4: Document Incident

**File incident in incident tracker**:
- [ ] Incident title: "R10 Circuit Breaker: Provider X Latency Outage (Simulation)"
- [ ] Timeline: T+0m (fault injection) → T+1m (alert) → T+5m (diagnosis) → T+10m (mitigation) → T+15m (recovery)
- [ ] Root cause: Latency fault injection (simulated)
- [ ] Actions taken: Cleared fault injection, monitored recovery
- [ ] Resolution: Circuit recovered successfully

### Step 5: Review and Adjust

**Review R10 thresholds**:
- [ ] Verify `latency_threshold_ms` is appropriate for provider X SLA
- [ ] Check if `open_timeout_ms` allows sufficient recovery time
- [ ] Document any threshold adjustments made

**Update runbook** (if needed):
- [ ] Add any lessons learned
- [ ] Document any unclear steps
- [ ] Update with provider-specific notes

---

## 📊 Success Criteria

**Simulation is successful if**:
- ✅ Alert identified correctly (`R10LatencyTriggerDominating`)
- ✅ CLI executed and output interpreted correctly
- ✅ Dashboard used for historical context
- ✅ Trigger reason identified (`latency_threshold_exceeded`)
- ✅ Mitigation action taken (clear fault injection)
- ✅ Recovery verified (circuit transitions to closed)
- ✅ Incident documented

---

## 🔄 Variations

### Variation 1: Error Burst

**Fault**: Error injection instead of latency
```bash
ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret \
  scripts/r10_simulate_error_burst.sh
```

**Expected**: Trigger reason = `failure_threshold_exceeded` or `error_rate_threshold_exceeded`

### Variation 2: Mixed Latency + Errors

**Fault**: Both latency and errors
```bash
# Enable both
ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret \
  scripts/r10_simulate_latency_outage.sh
ROUTER_NODE=router@127.0.0.1 ROUTER_COOKIE=secret \
  scripts/r10_simulate_error_burst.sh
```

**Expected**: Multiple trigger reasons possible, error rate > 0%

### Variation 3: Circuit Flapping

**Fault**: Rapid enable/disable of faults
```bash
# Rapidly toggle fault injection
for i in {1..5}; do
  scripts/r10_simulate_error_burst.sh
  sleep 10
  scripts/r10_clear_faults.sh
  sleep 10
done
```

**Expected**: `R10CircuitFlapping` alert fires

---

## 📚 References

- **Runbook**: `test/R10_RUNBOOK.md` - Detailed incident diagnostics
- **Operational Checklist**: `docs/R10_SRE_OPERATIONAL_CHECKLIST.md` - Daily/weekly/incident procedures
- **CLI Examples**: `docs/R10_CLI_EXAMPLES.md` - Real-world CLI output examples
- **Fault Injection Scripts**: `scripts/r10_simulate_*.sh` - Simulation tools

---

## 🎓 Post-Simulation Discussion

**Questions to consider**:
1. How quickly did you identify the trigger type?
2. Was the CLI output clear and actionable?
3. Did the dashboard provide sufficient context?
4. What would you do differently in a real incident?
5. Are there any gaps in the runbook or documentation?

**Feedback**: Document any improvements needed in runbook or procedures.

