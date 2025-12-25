1. Update docs/dev/CT_SUITE_CONVENTIONS.md to record the guard scope and mention the guard's purpose.
2. Extend scripts/lint/check_ct_suite_structure.sh to support explicit roots, and create deterministic tests for the guard.
3. Hook the guard into rebar3 (via provider hooks) and remaining CT scripts so it cannot be bypassed; ensure ./scripts/ct-full.sh still passes.
4. Record each step/verification in .ai/task_ct_guard_ci_hardening/progress.md.
