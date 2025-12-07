# R10: Provider X latency outage

**ID:** r10_latency_outage  
**Severity:** high  
**Tenant:** tenantX  
**Provider:** providerX  

---

## 1. Context

**Trigger type:** latency  
**Alert name:** R10LatencyTriggerDominating  

**Description:**  

Latency-based circuit breaker trigger for provider X. Circuit opens when provider latency consistently exceeds configured threshold (default: 5000ms).

---

## 2. Impact

- **Technical impact:** Increased latency and fail-fast for provider X, partial unavailability. Requests to provider X fail immediately when circuit is open.

- **Blast radius:** tenantX, providerX, R10 circuit breaker

- **Business impact:** Degraded response times, partial request failures, potential user-facing errors

---

## 3. Detection

### 3.1 Alerts

- R10LatencyTriggerDominating firing in Prometheus
- Alert annotation includes runbook_url

### 3.2 Dashboards

- Grafana → R10 Circuit Breakers → Panel: Circuit State (shows OPEN for tenantX/providerX)
- Grafana → R10 Circuit Breakers → Panel: Trigger Reasons (shows latency_threshold_exceeded)
- Grafana → R10 Circuit Breakers → Panel: Timeout Remaining (shows countdown for open circuit)

### 3.3 CLI

- ./router_ctl r10 status tenantX providerX

---

## 4. Diagnosis

1. Confirm circuit state is 'open' with latency trigger_reason
2. Check CLI output: State should be OPEN ✗, trigger_reason should be latency_threshold_exceeded
3. Verify error_rate is low (confirms latency issue, not error issue)
4. Check upstream provider X latency and error logs
5. Verify no concurrent deploys or config changes for provider X
6. Review router logs for 'event=circuit_breaker_state_changed' with 'reason=latency_threshold_exceeded'

---

## 5. Mitigation

1. If provider X is unhealthy, disable it in router config or reduce traffic
2. Shift traffic to backup provider if available
3. If provider X is healthy but slow, verify no network issues
4. Consider traffic offload or load balancing
5. Increase latency_threshold_ms for tenantX/providerX only if provider SLA allows
6. Monitor circuit recovery: open → half_open → closed

---

## 6. Verification

1. Circuit breaker transitions from open → half_open → closed
2. Latency returns to baseline in Grafana
3. Alerts clear automatically within 10–15 minutes
4. CLI shows State: CLOSED ✓ after recovery
5. No recurrence within 1 hour

---

## 7. Postmortem

1. File incident in incident tracker with full timeline
2. Review R10 thresholds and provider X SLA
3. Update runbook with lessons learned
4. Document any configuration changes made
5. Review provider X performance metrics for root cause
