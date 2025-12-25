# Progress

## Step 1 – Document invariants
- `docs/dev/CT_SUITE_CONVENTIONS.md` records the required lifecycle callbacks, export ordering, and no-op semantics; it also references the new guard script for quick validation.

## Step 2 – Fast guard
- Command: `scripts/lint/check_ct_suite_structure.sh` → exit 0; printed `ct suite structure guard: all suites export/define lifecycle callbacks`.

## Step 3 – Guard + full-tier validation
- Command: `./scripts/ct-full.sh` → exit 0; guard ran (`ct suite structure guard: ...`), suite linter passed, all 55 suites/332 tests executed, quality gates 0-3 green.
