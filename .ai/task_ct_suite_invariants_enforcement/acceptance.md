Acceptance:
- `docs/dev/CT_SUITE_CONVENTIONS.md` explains the mandatory callbacks, exports, and no-op semantics for new suites.
- `scripts/lint/check_ct_suite_structure.sh` runs quickly and exits non-zero if a suite lacks the required lifecycle definitions/exports.
- `./scripts/ct-full.sh` runs the guard before the suite linter and finishes with all suites passing in the current baseline.
- Progress tracked inside this task folder notes the guard execution and full-tier validation.
