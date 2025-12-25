## Task: router_test_bootstrap migration

Вынести общий Common Test bootstrap в единый helper и мигрировать все CT suites на него без регрессий по времени/стабильности.

**Драйверы задачи**
- Убрать копипасту `init_per_suite` / `end_per_suite` / `init_per_testcase` / `end_per_testcase`.
- Сделать старт/стоп и общую конфигурацию тестовой инфраструктуры декларативными (через opts).
- Сохранить поведение тестов: тайминги, порядок действий, стабильность в `./scripts/ct-full.sh`.

**Ключевой артефакт**
- `test/router_test_bootstrap.erl` — общий helper (start/stop, mock/docker режим, общая конфигурация, хуки на уровне testcase).

