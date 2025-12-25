# Prompts — T-CP2-CONTRACTS-COMPLETE

## Operating Instructions

**Source of Truth**: `contracts/cp2_contracts.json`
- All documentation MUST match this file
- Any discrepancy = error

**v1 Freeze Rules**:
- Do NOT modify v1 semantics
- Do NOT relax frozen rules
- Do NOT guess v1→v2 changes

**Ownership Mapping Rules**:
- Base on actual code search (grep/find)
- No speculation (use TBD when unclear)
- Provide evidence for each mapping
- OK to have TBD entries with reason

**Documentation Hygiene**:
- Keep JSON examples valid JSON (no comments)
- Use plain language for breaking-change rules
- Cross-reference sections clearly
- Avoid stale information

**Progress Updates**:
- Update `progress.md` after EACH completed step
- Include factual evidence (command output, file paths)
- Mark checklist items with [x] when done

## Micro-Prompts for Agent/IDE

- "Extract all subjects from contracts/cp2_contracts.json"
- "Search codebase for subject string: 'beamline.router.v1.decide'"
- "Map subject to handler module and function"
- "Validate JSON example syntax"
- "Check documentation matches registry"
