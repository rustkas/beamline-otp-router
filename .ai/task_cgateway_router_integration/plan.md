# Plan — T-CGW-ROUTER-01

## Phase 1: Contract Documentation (Read-Only)

### Step 1: Request Mapping Audit
**Goal**: Document HTTP → NATS transformation

**Actions**:
1. Review c-gateway code (HTTP handler → NATS publisher)
2. Identify actual subject used
3. Document header mapping (HTTP → NATS headers)
4. Document body transformation
5. Check timeout configuration

**Output**: `docs/c-gateway/REQUEST_MAPPING.md`

**Estimated**: 1-2 hours

---

### Step 2: Response Mapping Audit
**Goal**: Document NATS → HTTP transformation

**Actions**:
1. Review c-gateway NATS reply handler
2. Document success/error interpretation
3. Document backpressure signal handling
4. Check HTTP status code mapping
5. Verify Retry-After header generation

**Output**: `docs/c-gateway/RESPONSE_MAPPING.md`

**Estimated**: 1-2 hours

---

## Phase 2: Backpressure Validation (Critical Path)

### Step 3: Backpressure End-to-End
**Goal**: Validate backpressure propagates correctly

**Actions**:
1. Trigger Router backpressure (simulate or real load)
2. Observe NATS reply (`status=active`, `retry_after`)
3. Observe c-gateway HTTP response
4. Verify: HTTP 429 + Retry-After header
5. Check for local throttling/caching

**Output**: `docs/c-gateway/BACKPRESSURE_FLOW.md`

**Estimated**: 2-3 hours (includes testing)

---

## Phase 3: Observability Audit

### Step 4: Trace & Metrics Validation
**Goal**: Confirm end-to-end observability

**Actions**:
1. Send request with trace_id
2. Follow trace_id: HTTP → gateway → NATS → Router → CAF → back
3. Check logs for correlation
4. Review gateway metrics catalog
5. Compare with Router SLO metrics

**Output**: `docs/c-gateway/OBSERVABILITY.md`

**Estimated**: 1-2 hours

---

## Phase 4: Failure Mode Catalog

### Step 5: Failure Scenarios
**Goal**: Document behavior under failures

**Test Scenarios**:
1. Router down (stop Router, send request)
2. NATS timeout (slow Router, observe timeout)
3. Backpressure active (already covered in Step 3)
4. Partial reply (malformed NATS response)
5. Slow Router (latency > timeout)

**For Each**:
- Expected behavior (spec)
- Actual behavior (observed)
- Risk level (low/medium/high)

**Output**: `docs/c-gateway/FAILURE_MODES.md`

**Estimated**: 2-3 hours (includes chaos testing)

---

## Phase 5: Risk Assessment & Decision

### Step 6: Compile Findings
**Goal**: Production readiness decision

**Actions**:
1. Review all documentation
2. Identify gaps/risks
3. Categorize: blockers vs caveats vs OK
4. Make recommendation: Ready / Ready with caveats / Not ready

**Output**: `progress.md` decision section

**Estimated**: 1 hour

---

## Total Estimated Effort

- Phase 1: 2-4 hours
- Phase 2: 2-3 hours
- Phase 3: 1-2 hours
- Phase 4: 2-3 hours
- Phase 5: 1 hour

**Total**: 8-13 hours

**Can be parallelized**: Some steps independent (docs vs testing)

---

## Prerequisites

- c-gateway code access (for audit)
- Running c-gateway instance (for testing)
- CP1 Router deployed and accessible
- NATS accessible from c-gateway
- Permission to trigger backpressure (load or simulation)

---

## Risks

- c-gateway code may be undocumented (will document from scratch)
- Backpressure may be hard to trigger (will need simulation)
- Observability gaps may require instrumentation (document as finding, don't fix)
