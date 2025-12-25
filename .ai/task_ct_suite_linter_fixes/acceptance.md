Acceptance:
- `erl -noshell -pa test_support -s router_suite_linter run -s init stop` exits with 0.
- `./scripts/ct-full.sh` passes suite linter stage without any new warnings.
- All modified suites declare the required callbacks with minimal logic.
