# SUMMARY — T-CP2-CONTRACTS-COMPLETE

## Goal
Complete CP2 contracts operational readiness:
- Documentation (human-readable)
- CI enforcement (automated validation)
- Ownership mapping (subject → module)
- Migration guide (v1→v2 adoption)

## Foundation (Already Done ✅)
- contracts/cp2_contracts.json v0.1
- scripts/contract_check.py (validator)
- 7 subjects: 4 v1 frozen, 3 v2 planned

## Deliverables
1. **docs/contracts/CP2_CONTRACTS.md**
   - Subject registry table
   - Header policy
   - Breaking-change rules
   - JSON examples
   - Migration guide

2. **CI Integration**
   - .gitlab-ci.yml job
   - Automated validation
   - Fail on violations

3. **Ownership Mapping**
   - Subject → module/handler
   - Evidence-based
   - TBD when unclear

4. **Migration Guide**
   - Parallel v1/v2 operation
   - Opt-in v2 steps
   - Rollout strategy

## Workflow
Spec-first → Docs → CI → Mapping → Migration

## Status
NOT_STARTED (ready to begin)

## Estimated Effort
2-3 hours (mostly documentation)

## Success Criteria
- Documentation matches JSON registry
- CI job passing locally
- Ownership mapping complete (with TBD OK)
- Migration guide clear and actionable
