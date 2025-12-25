# Plan — T-CP2-CONTRACTS-COMPLETE

## Step 1: Extract Registry Facts
**Input**: `contracts/cp2_contracts.json`

**Extract**:
- All subjects (v1 + v2)
- Versions, statuses (frozen/planned)
- Header sets
- Correlation/idempotency rules

**Output**: Summary for documentation

---

## Step 2: Write Documentation
**Create**: `docs/contracts/CP2_CONTRACTS.md`

**Sections**:
1. Overview (purpose, v1 frozen, v2 parallel)
2. Subject Registry Table
3. Header Policy (v1 optional, v2 required)
4. Correlation & Idempotency Rules
5. Breaking Change Rules (plain language)
6. JSON Examples (valid, no comments)
7. Migration Guide

**Quality**: Match JSON registry exactly

---

## Step 3: Add CI Job
**Edit**: `.gitlab-ci.yml`

**Add**:
```yaml
cp2_contracts_check:
  stage: test
  image: python:3.11-slim
  script:
    - python3 scripts/contract_check.py
  rules:
    - changes:
        - contracts/cp2_contracts.json
        - scripts/contract_check.py
  allow_failure: false
```

**Verify**: Syntax valid, job runs fast

---

## Step 4: Build Ownership Mapping
**Method**:
- Search codebase for subject strings
- Grep for: `beamline.router`, `caf.exec`, `beamline.usage`
- Map to modules/handlers
- Mark TBD when unclear

**Output**: Table in docs or separate file

**Example**:
| Subject | Owner Module | Handler | Status |
|---------|--------------|---------|--------|
| beamline.router.v1.decide | router_nats.erl | handle_decide/2 | Verified |
| caf.exec.result.v1 | router_result_consumer.erl | handle_result/2 | Verified |
| beamline.usage.v1.metered | TBD | TBD | Not found |

---

## Step 5: Write Migration Guide
**Include**:
- v1 remains stable (no code changes)
- v2 subjects are parallel (new namespace)
- Client opt-in steps:
  1. Update to support v2 headers
  2. Switch to v2 subjects gradually
  3. Monitor both paths
  4. Deprecate v1 only after proven stability

**Rollout**:
- Canary: 1% → 10% → 50% → 100%
- Feature flags per tenant/subject

---

## Step 6: Record Evidence
**Update**: `progress.md`

**Include**:
- Validator PASS output
- CI job simulation or actual run
- Ownership mapping table
- Links to created docs
