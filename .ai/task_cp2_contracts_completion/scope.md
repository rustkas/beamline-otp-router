# Scope — T-CP2-CONTRACTS-COMPLETE

## In Scope

### 1. Documentation (`docs/contracts/CP2_CONTRACTS.md`)
- Registry overview (v1 frozen, v2 planned/parallel)
- Breaking change rules (plain language)
- Header policy: v1 optional vs v2 required
- Valid JSON examples for request/reply and events

### 2. CI Enforcement (`.gitlab-ci.yml`)
- Add contract validation job
- Run: `python3 scripts/contract_check.py`
- Fail pipeline on violations
- Fast and deterministic

### 3. Ownership Mapping
- Subject → Router module/handler path
- Function/callback when clear
- Mark unknown as TBD (no guessing)
- Evidence-based only

### 4. Migration Guide
- Parallel v1/v2 adoption approach
- v1 remains compatible unchanged
- Steps to opt-in to v2 (subjects + headers + rollout)

## Out of Scope
- Implementing v2 subjects in code
- Modifying existing v1 behavior/semantics
- Refactoring Router architecture
- Performance testing CP2 (separate task)

## Constraints
- **v1 subjects are FROZEN** (cannot change)
- **v2 subjects are PARALLEL** (not replacements)
- **Documentation MUST match** `contracts/cp2_contracts.json`
- **No speculation** in ownership mapping
