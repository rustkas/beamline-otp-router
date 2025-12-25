# Acceptance — T-CGW-ROUTER-01

## Must Have

### 1. Contract Documentation Complete
- [x] `docs/c-gateway/REQUEST_MAPPING.md` exists
  - HTTP endpoint → NATS subject mapping
  - Header transformation rules
  - Body/payload mapping
  - Timeout configuration
- [x] `docs/c-gateway/RESPONSE_MAPPING.md` exists
  - NATS reply → HTTP response mapping
  - Success/error interpretation
  - Backpressure signal handling
  - Status code mapping

### 2. Backpressure Validated
- [x] `docs/c-gateway/BACKPRESSURE_FLOW.md` exists
  - End-to-end flow documented
  - HTTP status codes confirmed (429 expected)
  - Retry-After header presence verified
  - Caching behavior documented

### 3. Observability Confirmed
- [x] `docs/c-gateway/OBSERVABILITY.md` exists
  - Trace ID propagation verified
  - Log correlation confirmed
  - Metrics catalog aligned with Router SLO

### 4. Failure Modes Cataloged
- [x] `docs/c-gateway/FAILURE_MODES.md` exists
  - All 5+ failure scenarios documented
  - Expected behavior vs actual behavior
  - Risk assessment for each mode

### 5. No Hidden Issues
- [x] No undocumented contract mismatches found
- [x] Timeouts aligned (no double-waiting)
- [x] Retry policy documented (if exists)
- [x] All findings captured

## Verification

```bash
# All documentation exists
ls -lh docs/c-gateway/*.md
# Expected: 5 files (REQUEST_MAPPING, RESPONSE_MAPPING, BACKPRESSURE_FLOW, OBSERVABILITY, FAILURE_MODES)

# No Router code changes (CP1 frozen)
git diff cp1-freeze-1.0.0-rc1 -- src/
# Expected: No changes
```

## Decision Criteria

Task is **COMPLETE** when:
- All documentation delivered
- Backpressure flow validated end-to-end
- Observability gaps identified (if any)
- Failure modes cataloged
- **Production readiness decision** can be made

**Possible Outcomes**:
- ✅ Ready for production (no blockers)
- ⚠️ Ready with caveats (known limitations documented)
- ❌ Not ready (blockers identified, separate fix task needed)
