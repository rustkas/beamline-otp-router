## Plan (incremental, group-by-group)

1. Найти оставшиеся suites, которые стартуют роутер вручную (ripgrep по `start_router_suite`/`ensure_all_started`/`application:set_env`).
2. Выбрать “группу” suites с похожим bootstrap-паттерном и мигрировать их на `router_test_bootstrap`.
3. На каждый suite:
   - перенести общее в `router_test_bootstrap:init_per_suite/2` и `end_per_suite/2`,
   - сохранить suite-specific часть до/после вызова bootstrap (meck, fault injection, seed данных, resets),
   - при необходимости подключить testcase-opts (metrics/faults/mocks).
4. Валидация группы:
   - `./bin/rebar3 ct --suite test/<suite_name>_SUITE`
   - при наличии — сверка логики запуска с предыдущим поведением (порядок env/start).
5. Повторять, пока `rg` больше не находит `router_suite_helpers:start_router_suite()` в `test/*_SUITE.erl`.
6. Финальная проверка:
   - `erl -noshell -pa test_support -s router_suite_linter run -s init stop`
   - `./scripts/ct-full.sh`

