# Progress — T-CP2-CONTRACTS-01

## Status
IN_PROGRESS (Spec Phase)

**Note**: CP2 contracts started in SPEC-FIRST mode.  
CP1 remains frozen (tag: cp1-freeze-1.0.0-rc1).

## Work log
- [x] Create contracts/cp2_contracts.json v0.1
  - v1 subjects inventoried and marked `frozen`
  - v2 subjects defined as `parallel/additive`
  - Header sets for v1 (optional) and v2 (required)
  - DLQ contract defined
  - Breaking change rules formalized
- [x] Create scripts/contract_check.py (enhanced validator)
  - Schema validation ✅
  - CP1 freeze enforcement ✅
  - Breaking change detection ✅
  - v1/v2 parallel validation ✅
- [ ] Create docs/contracts/CP2_CONTRACTS.md (canonical documentation)
- [ ] Add CP2 contracts CI job to .gitlab-ci.yml
- [ ] Verify contracts match existing Router/CAF code

## Evidence

**Created**:
- ✅ contracts/cp2_contracts.json (7 subjects: 4 v1 frozen, 3 v2 planned)
- ✅ scripts/contract_check.py (executable, passing)

**Validation Output**:
```
INFO: 3 v2 subjects planned (parallel to v1)
✅ Validation PASSED: contracts are valid
```

**v1 Subjects (Frozen)**:
1. beamline.router.v1.decide
2. caf.exec.assign.v1
3. caf.exec.result.v1
4. beamline.usage.v1.metered

**v2 Subjects (Planned, Parallel)**:
1. beamline.router.v2.decide (enhanced with strict headers)
2. beamline.router.dlq.v2 (NEW - dead letter queue)
3. beamline.router.v2.status.backpressure (NEW - status query)

## Key Decisions

**Backward Compatibility Strategy**:
- v1 headers remain optional (CP1 compat)
- v2 headers are required (strict contract)
- v2 subjects are parallel, not replacements
- No silent semantic changes to v1

**Contract Enforcement**:
- `status: frozen` required for all v1 subjects
- Breaking change rules in JSON
- CI validation prevents violations

## Dependencies Met
- ✅ CP1 freeze complete (baseline for v1)
- ✅ Existing Router NATS modules (inventory source)

## Next Steps
1. Document contracts in docs/contracts/CP2_CONTRACTS.md
2. Add CI job (ready, just needs wiring)
3. Map contracts to existing code modules
4. Define migration path for v1→v2 (separate doc)

## Notes

**Spec-First Approach**:
- Contracts defined WITHOUT code changes
- This is intentional - freeze spec first
- Implementation comes later (separate tasks)

**v2 Philosophy**:
- Additive, not destructive
- Opt-in, not forced
- Parallel, not replacement

---

**Status**: Contracts v0.1 validated and ready for documentation ✅
