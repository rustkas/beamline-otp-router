Acceptance:
- Every CT runner invokes `scripts/lint/check_ct_suite_structure.sh` before running suites.
- `docs/dev/CT_SUITE_CONVENTIONS.md` states the explicit guard scope (apps/otp/router/test) and links to the guard.
- Guard tests run deterministically (positive + negative cases) via a helper script.
- `./scripts/ct-full.sh` still succeeds with guard and quality gates.
- `.ai/task_ct_guard_ci_hardening/progress.md` tracks each verification step.
