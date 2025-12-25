1. Document the lifecycle invariants and expected exports in a developer doc under `docs/dev/CT_SUITE_CONVENTIONS.md`.
2. Create a lightweight guard script (`scripts/lint/check_ct_suite_structure.sh`) that ensures every `test/*_SUITE.erl` defines and exports the required callbacks.
3. Hook the guard into `scripts/ct-full.sh` so it runs before the suite linter and verify `scripts/ct-full.sh` still passes.
4. Record each verification command and result in `progress.md`.
