# Acceptance — T-CP2-CONTRACTS-COMPLETE

## Must Have

### 1. Documentation Complete
- [x] `docs/contracts/CP2_CONTRACTS.md` exists
- [x] v1 frozen subjects listed (4 subjects)
- [x] v2 planned subjects listed (3 subjects)
- [x] Header rules: v1 optional, v2 required
- [x] Breaking-change rules (plain language)
- [x] Valid JSON examples:
  - request/reply message
  - event message

### 2. CI Enforcement
- [x] `.gitlab-ci.yml` has contract validation job
- [x] Job runs: `python3 scripts/contract_check.py`
- [x] Job fails on validation errors
- [x] Job is fast (<30s)

### 3. Ownership Mapping
- [x] Each subject mapped to module/handler OR marked TBD
- [x] Evidence provided for TBD items
- [x] No speculation (only proven mappings)

### 4. Migration Guide
- [x] Explains parallel v1/v2 operation
- [x] Explains client opt-in to v2
- [x] Confirms v1 clients remain unchanged
- [x] Includes rollout strategy (canary/gradual)

### 5. Evidence
- [x] `progress.md` contains:
  - Validator PASS output
  - CI job run evidence (or local simulation)
  - Ownership mapping table

## Verification Commands

```bash
# Validate contracts
python3 scripts/contract_check.py

# Check documentation exists
ls -lh docs/contracts/CP2_CONTRACTS.md

# Verify CI job syntax
grep -A 10 "contract_check" .gitlab-ci.yml
```
