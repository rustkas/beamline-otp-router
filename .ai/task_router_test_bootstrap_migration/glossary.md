## Glossary

- **CT / Common Test**: Erlang test framework used by `rebar3 ct`.
- **Suite**: `*_SUITE.erl` module; defines tests and lifecycle hooks.
- **bootstrap**: общая логика `init_per_suite`/`end_per_suite`/testcase hooks.
- **infra_mode**: режим инфраструктуры тестов (`mock` vs `docker`), влияет на доступность реальных сервисов.
- **quality gate**: проверка `router_suite_linter`, блокирующая запуск полного набора тестов.
- **start strategy**:
  - `router_suite`: `router_suite_helpers:start_router_suite/0`
  - `ensure_all_started`: `application:ensure_all_started/1`
  - `router_app`: `router_test_utils:start_router_app/0`
  - `none`: suite сам управляет запуском/остановкой

