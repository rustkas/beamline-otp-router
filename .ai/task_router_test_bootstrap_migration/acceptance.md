## Acceptance Criteria

- `./scripts/ct-full.sh` проходит (без ухудшения времени и стабильности относительно текущего состояния репо).
- `router_suite_linter` проходит без “quality gate blocked”:
  - запуск: `erl -noshell -pa test_support -s router_suite_linter run -s init stop`
- Все CT suites используют `router_test_bootstrap` для общих hooks (без дублирования start/stop логики).
- Suite-specific логика сохраняется:
  - meck/mocks не ломаются,
  - fault injection/metrics resets выполняются в тех же местах (suite/testcase).
- Никаких “тяжёлых” side-effects в bootstrap (например `ct:pal`) — чтобы не влиять на timing.

