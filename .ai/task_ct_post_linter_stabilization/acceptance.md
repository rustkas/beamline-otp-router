Acceptance:
- `erl -noshell -pa test_support -s router_suite_linter run -s init stop` returns 0.
- `./scripts/ct-full.sh` completes with all quality gates green.
- Targeted `router_idem_core_SUITE` run passes.
- Progress file documents each verification with command, result, summary.
