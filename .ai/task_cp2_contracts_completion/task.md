# T-CP2-CONTRACTS-COMPLETE: Documentation, CI, Mapping, Migration

## Problem
CP2 contract registry (`contracts/cp2_contracts.json` v0.1) and validator (`scripts/contract_check.py`) exist and validate successfully.

**Spec-first phase complete**, but not yet operational:
- ❌ No human-readable documentation for developers/consumers
- ❌ No CI enforcement to protect frozen v1 subjects
- ❌ No subject→module ownership mapping
- ❌ No v1→v2 migration guide

## Goal
Make CP2 contracts an enforced, documented interface:
- Publish clear CP2 contracts documentation
- Enforce validation in CI
- Map each subject to owner module/handler
- Document parallel v1/v2 adoption and migration steps

## Expected Outcome
- `docs/contracts/CP2_CONTRACTS.md` exists and matches JSON registry
- CI fails on contract violations (especially frozen v1 breaking changes)
- Each subject has explicit owner mapping (or TBD with evidence)
- Migration guide explains opt-in v2 while keeping v1 stable

## Foundation (Already Complete)
- ✅ contracts/cp2_contracts.json v0.1 (7 subjects: 4 v1 frozen, 3 v2 planned)
- ✅ scripts/contract_check.py (validation passing)
- ✅ Breaking-change rules defined
