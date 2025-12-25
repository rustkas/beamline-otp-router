# CP2 Phase 2 — Contracts Enforcement & Migration

**Entry State**:
- CP1: 🔒 FROZEN (`cp1-freeze-1.0.0-rc1`)
- CP2 Phase 1: ✅ DONE (contracts registry + validator)

**Goal**: Transform contracts from "spec" to **operational system rule**, without touching code.

**Duration**: 2-3 hours  
**Risk**: Minimal  
**ROI**: Maximum

---

## 📦 Deliverables (Canonical Set)

### 1️⃣ Ownership Mapping (Contracts Governance)

**Files to Update**:
- `contracts/cp2_contracts.json`
- `docs/contracts/CP2_CONTRACTS.md`

**Add to JSON**:
```json
"ownership": {
  "beamline.router.v1.decide": {
    "module": "router_gateway",
    "handler": "handle_decide/2",
    "team": "router"
  },
  "beamline.router.v2.decide": {
    "module": "router_gateway_v2",
    "handler": "handle_decide_v2/2", 
    "team": "router",
    "status": "planned"
  },
  "caf.exec.assign.v1": {
    "module": "caf_worker",
    "handler": "execute_assignment/1",
    "team": "caf"
  },
  "caf.exec.result.v1": {
    "module": "router_result_consumer",
    "handler": "handle_result/2",
    "team": "router"
  },
  "beamline.usage.v1.metered": {
    "module": "router_result_consumer",
    "handler": "emit_usage/2",
    "team": "router"
  },
  "beamline.router.dlq.v2": {
    "module": "router_dlq_handler",
    "handler": "TBD",
    "team": "router",
    "status": "planned"
  },
  "beamline.router.v2.status.backpressure": {
    "module": "router_backpressure",
    "handler": "get_status/0",
    "team": "router",
    "status": "planned"
  }
}
```

**Acceptance**:
- ✅ Every subject has owner
- ✅ Owner = existing module/team
- ✅ No "orphan subjects"
- ✅ TBD allowed only for planned subjects with evidence

**Effort**: ~45 minutes

---

### 2️⃣ CI Enforcement (Hard Rules)

**Files to Update**:
- `scripts/contract_check.py` (extend)
- `.gitlab-ci.yml` (ensure job exists)

**New Rules (MUST enforce)**:

**Forbidden**:
- ❌ Delete CP1 subjects (status=frozen)
- ❌ Change CP1 headers (frozen contract)
- ❌ V2 subject without required headers
- ❌ Subject without owner
- ❌ Version regression (v2 → v1)

**Allowed**:
- ✅ Add new v2 subjects (parallel)
- ✅ Add optional headers to v2
- ✅ Enhance v2 payload (additive)

**Implementation**:
```python
# Add to contract_check.py

def _check_cp1_freeze(self, subjects):
    """Enforce CP1 freeze - no changes to frozen subjects"""
    for subj in subjects:
        if subj.get("status") == "frozen":
            version = subj.get("version")
            if version != "v1":
                self.error(f"Frozen subject must be v1: {subj['name']}")
            
            # Store frozen subjects for deletion check
            self.frozen_subjects.add(subj["name"])

def _check_ownership(self, subjects, ownership):
    """Ensure all subjects have owners"""
    for subj in subjects:
        name = subj.get("name")
        if name and name not in ownership:
            self.error(f"Subject missing ownership: {name}")
```

**Acceptance**:
- ✅ Any breaking change → CI FAIL
- ✅ Error message specifies exact subject
- ✅ CI job runs on every contract change

**Effort**: ~45 minutes

---

### 3️⃣ Migration Guide (Human Contract)

**File to Create**:
- `docs/contracts/CP1_TO_CP2_MIGRATION.md`

**Minimum Sections**:

```markdown
# CP1 to CP2 Migration Guide

## When to Use v2
- New deployments (greenfield)
- Opt-in for enhanced observability
- When strict headers needed
- DLQ infrastructure required

## NOT Ready for v2
- Production stability critical
- No bandwidth for migration
- Happy with v1 semantics

## Dual-Publish Pattern (v1 + v2)
```erlang
% Publish to both v1 and v2 during migration
publish_dual(Message) ->
    V1 = convert_to_v1(Message),
    V2 = enhance_to_v2(Message),
    ok = nats:publish("subject.v1", V1),
    ok = nats:publish("subject.v2", V2).
```

## Header Requirements
| Header | v1 | v2 | Migration Note |
|--------|----|----|----------------|
| x_trace_id | optional | **required** | Add before v2 |
| x_tenant_id | optional | **required** | Add before v2 |
| x_contract_version | optional | **required** | Set to "v2" |
| x_msg_id | optional | **required** | Critical for ack/nak |

## Rollback Strategy (v2 → v1)
1. Stop publishing to v2 subjects
2. Wait for v2 queue drain (monitor pending)
3. Switch consumers back to v1 
4. Verify v1 traffic resuming
5. Keep v2 infrastructure dormant (don't delete)

## Observability Deltas
- v2 adds: trace propagation end-to-end
- v2 adds: DLQ metrics
- v2 adds: strict correlation validation
- v2 changes: error codes (stricter taxonomy)

## Known Incompatibilities
- v1 tolerates missing headers; v2 does not
- v2 enforces idempotency keys; v1 best-effort
- DLQ subjects only exist in v2

## Rollout Strategy
1. **Canary**: 1% traffic to v2 (single tenant)
2. **Validation**: Monitor for 24-48h
3. **Expand**: 10% → 25% → 50%
4. **Full**: 100% after proven stable
5. **Deprecate v1**: Only after 90+ days v2 stability

## Feature Flags
```erlang
% Config-driven v2 adoption
{cp2_enabled, true},
{cp2_subjects_allowlist, [
  "beamline.router.v2.decide"
]},
{cp2_tenant_allowlist, [
  <<"tenant_alpha">>,
  <<"tenant_beta">>
]}
```
```

**Acceptance**:
- ✅ Document readable without code knowledge
- ✅ No TODO/TBD
- ✅ Rollback steps clear
- ✅ Examples valid

**Effort**: ~45-60 minutes

---

## ⏱️ Time Estimate

| Step | Effort |
|------|--------|
| Ownership mapping | 45 min |
| CI enforcement | 45 min |
| Migration guide | 45-60 min |
| **Total** | **2-3 hours** |

---

## 🚫 Explicit Non-Goals (Phase 2)

**Forbidden in this phase**:
- ❌ Implement CP2 in code
- ❌ Refactor Router/CAF
- ❌ Performance tuning for v2
- ❌ Delete v1 logic

**Principle**: *spec → governance → data → code*

Wait for production data before implementing.

---

## 🏁 Exit Criteria (Phase 2 Complete)

Phase 2 is **COMPLETE** when:

- [x] Contracts = law (CI enforced)
- [x] Ownership transparent (every subject mapped)
- [x] Migration documented (clear rollout + rollback)
- [x] CP1 remains untouched (no code changes)

**Validation**:
```bash
# All must pass
python3 scripts/contract_check.py
grep -q "ownership" contracts/cp2_contracts.json
ls docs/contracts/CP1_TO_CP2_MIGRATION.md
git diff cp1-freeze-1.0.0-rc1 -- src/  # Should be empty
```

---

## 🎯 Success Metrics

**After Phase 2**:
- Contracts enforceable by CI ✅
- Breaking changes blocked automatically ✅
- Migration path documented ✅
- Zero CP1 code changes ✅

**Value**:
- Governance without bureaucracy
- Safety without rigidity
- Evolution without rewrites

---

## 🧭 Current Position

You are at a **rare position**:
- CP1 truly frozen (not "almost")
- CP2 starts correctly (spec-first)
- No architectural debt
- No rush to code

**Two Valid Paths Forward**:

**Path A**: Start Phase 2 now (recommended)
- 2-3 hours investment
- Complete governance layer
- Ready for real implementation

**Path B**: Deploy CP1 first
- Let system breathe in production
- Collect real-world data
- Start Phase 2 with evidence

Both are valid. Path A recommended for momentum.

---

## 📋 Next Action

**If starting Phase 2**:
```bash
# Step 1: Begin ownership mapping
# Update contracts/cp2_contracts.json with ownership section
# Search codebase for actual modules
# Map subjects to handlers
```

**Ready to proceed on command** 🚀

---

**Roadmap Status**: READY  
**Phase 1**: DONE ✅  
**Phase 2**: READY TO START  
**Estimated Completion**: 2-3 hours from start

**This is the path to production-grade CP2** 🎯
