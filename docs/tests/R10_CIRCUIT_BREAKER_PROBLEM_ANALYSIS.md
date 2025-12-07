# Анализ проблемы с тестами Circuit Breaker

## Проблема

Все тесты `router_circuit_breaker_SUITE` падают с ошибкой:
```
{noproc, {gen_server, call, [router_circuit_breaker, {record_state, ...}, 5000]}}
```

Это означает, что процесс `router_circuit_breaker` **не запущен** в момент вызова `gen_server:call`.

## Текущая реализация

### `init_per_suite`:
```erlang
init_per_suite(Config) ->
    %% Загружаем конфигурацию приложения
    _ = application:load(beamline_router),
    ok = application:set_env(...),
    
    %% Пытаемся запустить circuit breaker
    case whereis(router_circuit_breaker) of
        undefined ->
            case router_circuit_breaker:start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                Error -> ct:fail(...)
            end;
        _ -> ok
    end,
    Config.
```

### `init_per_testcase`:
```erlang
init_per_testcase(_TestCase, Config) ->
    %% Проверяем, запущен ли процесс
    case whereis(router_circuit_breaker) of
        undefined ->
            {ok, _} = router_circuit_breaker:start_link();
        _ -> ok
    end,
    %% Очищаем состояние
    ...
    Config.
```

## Возможные причины

### 1. Процесс завершается между `init_per_suite` и `init_per_testcase`

**Гипотеза**: Процесс запускается в `init_per_suite`, но затем завершается (возможно, из-за ошибки инициализации или supervisor'а).

**Проверка**: Нужно добавить логирование и проверку, что процесс жив после запуска.

### 2. Процесс не запускается в `init_per_testcase`

**Гипотеза**: `whereis(router_circuit_breaker)` возвращает `undefined`, но `start_link()` не запускает процесс (возможно, из-за того, что имя уже занято другим процессом или есть проблема с регистрацией).

**Проверка**: Нужно проверить, что `start_link()` действительно запускает процесс и регистрирует его.

### 3. Проблема с supervisor'ом

**Гипотеза**: `router_circuit_breaker` должен запускаться через supervisor (`beamline_router_sup`), а не напрямую через `start_link()`. Если supervisor не запущен, процесс может завершаться.

**Проверка**: В других тестах (`router_circuit_breaker_integration_SUITE`) используется `application:ensure_all_started(beamline_router)`, который запускает supervisor.

## Сравнение с рабочими тестами

### `router_circuit_breaker_integration_SUITE` (работает):
```erlang
init_per_suite(Config) ->
    _ = application:load(beamline_router),
    ok = application:set_env(...),
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Проверяем, запущен ли процесс (запускается через supervisor)
            case whereis(router_circuit_breaker) of
                undefined ->
                    {ok, _} = router_circuit_breaker:start_link();
                _Pid -> ok
            end,
            Config;
        Error -> ct:fail(...)
    end.
```

**Ключевое отличие**: Используется `application:ensure_all_started(beamline_router)`, который запускает supervisor и все зависимости.

### `router_circuit_breaker_load_SUITE` (работает):
```erlang
init_per_suite(Config) ->
    case application:ensure_all_started(beamline_router) of
        {ok, _} ->
            %% Проверяем, что процесс запущен через supervisor
            case whereis(router_circuit_breaker) of
                undefined ->
                    ct:fail("Circuit breaker should be started via supervisor");
                Pid ->
                    case is_process_alive(Pid) of
                        true -> Config;
                        false -> ct:fail("Circuit breaker is not alive")
                    end
            end;
        Error -> ct:fail(...)
    end.
```

**Ключевое отличие**: Ожидается, что процесс запущен через supervisor, и проверяется, что он жив.

## Решение

### Вариант 1: Использовать `application:ensure_all_started` (рекомендуется)

Это правильный способ, так как:
- Запускает supervisor и все зависимости
- Обеспечивает правильную инициализацию
- Соответствует тому, как работает приложение в production

### Вариант 2: Убедиться, что процесс жив в `init_per_testcase`

Добавить проверку `is_process_alive(Pid)` и перезапуск, если процесс завершился.

### Вариант 3: Использовать `meck` для мокирования

Если circuit breaker не нужен для тестов, можно замокировать его.

## Рекомендация

**Использовать `application:ensure_all_started(beamline_router)`** в `init_per_suite`, как в других рабочих тестах. Это обеспечит:
1. Правильную инициализацию всех зависимостей
2. Запуск supervisor'а
3. Правильную работу circuit breaker через supervisor

## Дополнительные проблемы

### Метрики тесты

Тесты `test_circuit_breaker_state_metrics_labels` и `test_circuit_breaker_transitions_metrics_labels` также падают, вероятно, по той же причине - circuit breaker не запущен.

### E2E тесты

E2E тесты также могут иметь проблемы с инициализацией, но нужно проверить отдельно.

