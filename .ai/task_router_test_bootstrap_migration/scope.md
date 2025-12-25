## Scope

**In scope**
- Единый helper `router_test_bootstrap` для:
  - suite lifecycle: старт/стоп приложений/инфры, конфиг env, ожидание app start.
  - testcase lifecycle: очистка метрик/фолтов, metrics_test_helper, валидация/cleanup моков.
  - режим инфраструктуры: `mock` / `docker` / `auto` (авто-детект + skip при несовместимости).
- Миграция всех `test/*_SUITE.erl`, где есть:
  - `router_suite_helpers:start_router_suite/0` + `stop_router_suite/0`,
  - ручные `application:load/stop/unload`, `ensure_all_started`, общие `application:set_env`,
  - повторяющиеся очистки метрик/фолтов/моков в hooks.
- Инкрементальная валидация: после каждой “группы” suites прогонять `./bin/rebar3 ct --suite ...`.

**Out of scope (если явно не требуется)**
- Переписывание/починка самих тест-кейсов (кроме минимально нужного для сохранения поведения).
- Переработка инфраструктурных модулей (`router_suite_helpers`, `router_test_utils`, и т.п.) сверх того, что нужно для интеграции.
- Оптимизация производительности тестов, если текущие тайминги не ухудшаются.

**Риски**
- Непреднамеренное изменение порядка: suite-start vs dependency-start vs env-set.
- Suite-specific meck/mocks должны остаться до/после bootstrap в нужных местах.
- Условные тесты (через `ROUTER_TEST_LEVEL` / `ROUTER_ENABLE_META`) часто дают “All 0 tests passed”; поэтому важно ориентироваться на `ct-full.sh` как на финальную проверку.

