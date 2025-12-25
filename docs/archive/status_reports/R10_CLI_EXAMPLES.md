# R10 CLI Command Examples

This document provides real-world examples of `router_ctl r10 status` command output for SRE operations.

## Command

```bash
./router_ctl r10 status <tenant> <provider>
```

## Example Outputs

### Example 1: Healthy Circuit (Closed State)

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

┌─ Counters ──────────────────────────────────────────────────┐
│ Consecutive failures: 0
│ Consecutive successes: 5
└──────────────────────────────────────────────────────────────┘

┌─ Metrics ───────────────────────────────────────────────────┐
│ Last trigger reason: none (circuit never opened)
│ Error rate: ✓ 0.00% (0.0000)
└──────────────────────────────────────────────────────────────┘

┌─ Configuration ──────────────────────────────────────────────┐
│ Failure threshold: 5 consecutive failures
│ Error rate threshold: 50.0% (0.50)
│ Latency threshold: 5000 ms
│ Open timeout: 60000 ms (60.0 seconds)
└──────────────────────────────────────────────────────────────┘

┌─ Request Handling ───────────────────────────────────────────┐
│ Should allow: YES ✓ (requests will be processed)
└──────────────────────────────────────────────────────────────┘

📖 Runbook: https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

### Example 2: Open Circuit (After Failures)

```bash
$ ./router_ctl r10 status tenant_prod_1 provider_openai

╔══════════════════════════════════════════════════════════════╗
║  R10 Circuit Breaker Status                                    ║
╚══════════════════════════════════════════════════════════════╝

Tenant ID:  tenant_prod_1
Provider ID: provider_openai

┌─ State ─────────────────────────────────────────────────────┐
│ State: OPEN ✗ (circuit is open, requests fail immediately)
│ Last state change: 2025-01-27 14:35:22 UTC (45.8 seconds ago)
└──────────────────────────────────────────────────────────────┘

┌─ Counters ──────────────────────────────────────────────────┐
│ Consecutive failures: 5
│ Consecutive successes: 0
└──────────────────────────────────────────────────────────────┘

┌─ Metrics ───────────────────────────────────────────────────┐
│ Last trigger reason: failure_threshold_exceeded
│ Error rate: ⚠ 85.50% (0.8550)
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

### Example 3: Half-Open State (Recovery Probe)

```bash
$ ./router_ctl r10 status tenant_prod_1 provider_openai

╔══════════════════════════════════════════════════════════════╗
║  R10 Circuit Breaker Status                                    ║
╚══════════════════════════════════════════════════════════════╝

Tenant ID:  tenant_prod_1
Provider ID: provider_openai

┌─ State ─────────────────────────────────────────────────────┐
│ State: HALF_OPEN ⚠ (probing for recovery, limited requests)
│ Last state change: 2025-01-27 14:36:22 UTC (12.5 seconds ago)
└──────────────────────────────────────────────────────────────┘

┌─ Counters ──────────────────────────────────────────────────┐
│ Consecutive failures: 0
│ Consecutive successes: 1
│ Half-open probe attempts: 1
│ Half-open max attempts: 3
└──────────────────────────────────────────────────────────────┘

┌─ Metrics ───────────────────────────────────────────────────┐
│ Last trigger reason: timeout_elapsed
│ Error rate: ✓ 15.20% (0.1520)
└──────────────────────────────────────────────────────────────┘

┌─ Configuration ──────────────────────────────────────────────┐
│ Failure threshold: 5 consecutive failures
│ Error rate threshold: 50.0% (0.50)
│ Latency threshold: 5000 ms
│ Open timeout: 60000 ms (60.0 seconds)
└──────────────────────────────────────────────────────────────┘

┌─ Request Handling ───────────────────────────────────────────┐
│ Should allow: YES ✓ (requests will be processed)
└──────────────────────────────────────────────────────────────┘

📖 Runbook: https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

### Example 4: Not Initialized (First Request)

```bash
$ ./router_ctl r10 status tenant_new provider_new

╔══════════════════════════════════════════════════════════════╗
║  R10 Circuit Breaker Status                                    ║
╚══════════════════════════════════════════════════════════════╝

Tenant ID:  tenant_new
Provider ID: provider_new

Status: not_found (no circuit state yet, treated as closed)

This circuit breaker has not been initialized yet.
It will be created automatically on first request.

📖 Runbook: https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md
```

## Common Scenarios

### Scenario: Circuit Stuck Open

**Symptom**: State is `OPEN` for > 5 minutes

**CLI Output**: Shows `State: OPEN ✗` with `Timeout remaining: 0.0 minutes`

**Action**: 
1. Check `Last trigger reason` to understand why it opened
2. Check `Error rate` - if still high, provider may still be failing
3. Check `Timeout remaining` - if 0, circuit should transition to half-open soon
4. Follow `R10_RUNBOOK.md` → "Scenario A: Breaker Stuck in Open State"

### Scenario: Circuit Flapping

**Symptom**: Frequent state transitions

**CLI Output**: `Last state change` timestamp changes frequently

**Action**:
1. Check `Error rate` - may be hovering around threshold
2. Check `Configuration` - thresholds may be too sensitive
3. Follow `R10_RUNBOOK.md` → "Scenario B: Breaker Flapping"

### Scenario: High Error Rate

**Symptom**: Error rate > 50% but circuit still closed

**CLI Output**: `Error rate: ⚠ 65.50%` with `State: CLOSED ✓`

**Action**:
1. Circuit may be about to open (check `Consecutive failures`)
2. Error rate may be in sliding window but not yet exceeded threshold
3. Monitor for state transition
4. Follow `R10_RUNBOOK.md` → "Scenario D: Error Rate Trigger"

## Integration with Monitoring

The CLI output complements Grafana dashboard:

- **Dashboard**: Historical trends, multiple tenants/providers
- **CLI**: Current state snapshot, detailed counters, configuration

Use CLI for:
- Quick diagnosis during incidents
- Verifying dashboard data
- Understanding current configuration
- Getting runbook link for copy-paste

Use Dashboard for:
- Historical analysis
- Multi-tenant/provider overview
- Alert investigation
- Trend analysis

