# Priorities — T-CGW-ROUTER-01

## P0 — MUST (Release Blocking)

**Cannot proceed to CP1 production without these validated**

### 1. HTTP → NATS Contract
- Which headers are actually passed through
- Where trace_id/request_id gets lost (if at all)
- Tenant isolation at gateway level
- Subject used (confirm `beamline.router.v1.decide`)

**Risk if missing**: Silent data loss, broken correlation, security issues

---

### 2. Backpressure Propagation
- Who initiates HTTP 429 (gateway or router signal)
- Where `Retry-After` is transformed
- False HTTP 200 when backpressure active
- Caching behavior of backpressure state

**Risk if missing**: Client gets wrong signals, retry storms, system collapse

---

### 3. Failure Semantics
- Timeout → HTTP status code mapping
- NATS unavailable → client experience
- Router crash → retry vs fail-fast
- Partial response → error handling

**Risk if missing**: Unpredictable client behavior, cascading failures

---

## P1 — SHOULD (Architecture Input)

**Informs CP2 design decisions**

### 4. Observability Continuity
- Trace broken or continuous (HTTP → NATS → Router → back)
- Span lifecycle matches expectations
- Correlation Router ↔ Gateway exists

**Impact**: Debugging difficulty, incident response time

---

### 5. Load Amplification
- Request retry behavior (gateway-initiated)
- Fan-out on retry (single request → multiple NATS calls)
- Self-DDoS risk to Router

**Impact**: Capacity planning, stability under load

---

## P2 — NICE (Documented but Non-Blocking)

**Quality of life improvements**

### 6. Config Drift
- Environment variables vs documentation
- Defaults vs expectations
- Hard-coded values

**Impact**: Operational friction, onboarding difficulty

---

### 7. Operational UX
- How to debug prod incidents
- Log sufficiency without debug mode
- Metrics availability

**Impact**: Mean time to resolution (MTTR)

---

## Architectural Rule (CRITICAL)

> **If c-gateway and Router disagree on error interpretation or backpressure —
> this is NOT a bug. It's an architectural fact to be addressed in CP2, not patched in CP1.**

**Implication**:
- Document the gap
- Assess production impact
- Design CP2 solution
- Do NOT quick-fix CP1 (it's frozen)

---

## Decision Matrix

| Finding | Production Impact | Action |
|---------|------------------|--------|
| P0 gap (critical) | High | BLOCKER - Must fix before prod OR document risk acceptance |
| P1 gap (important) | Medium | Document + CP2 task |
| P2 gap (nice to have) | Low | Document only |

---

**Last Updated**: 2025-12-22  
**Source**: User validation of task priorities
