# Mock Discipline Guidelines

## 1. Правила использования meck

### 1.1 Обязательная очистка

```erlang
%% ПРАВИЛЬНО: Очистка в end_per_testcase
end_per_testcase(_TestCase, Config) ->
    catch meck:unload(router_nats),
    Config.

%% НЕПРАВИЛЬНО: Забыть очистить mock
%% Результат: flaky тесты, утечки состояния между тестами
```

### 1.2 Проверка доступности meck

```erlang
%% ПРАВИЛЬНО: Проверка перед использованием
case code:which(meck) of
    non_existing ->
        {skip, "meck not available"};
    _ ->
        meck:new(router_nats, [passthrough]),
        %% ... test logic
        meck:unload(router_nats),
        ok
end.

%% НЕПРАВИЛЬНО: Использовать meck без проверки
meck:new(router_nats, [passthrough]),  %% Упадёт если meck не установлен
```

### 1.3 Passthrough для частичного мокирования

```erlang
%% ПРАВИЛЬНО: passthrough сохраняет оригинальное поведение
meck:new(router_nats, [passthrough]),
meck:expect(router_nats, publish_with_ack, fun(_, _, _) -> {ok, <<"ack">>} end),

%% НЕПРАВИЛЬНО: Без passthrough все функции undefined
meck:new(router_nats),  %% router_nats:other_function() будет падать
```

---

## 2. Mock Helper Pattern

Используйте `router_caf_test_helper.erl` как референс:

```erlang
%% Использование helper
case router_caf_test_helper:setup_router_nats_mock() of
    {skip, Reason} -> {skip, Reason};
    ok ->
        %% ... test logic ...
        router_caf_test_helper:teardown_router_nats_mock(),
        ok
end.
```

---

## 3. STRICT_MOCK_DISCIPLINE Mode

При запуске с `STRICT_MOCK_DISCIPLINE=true`:

```bash
make test-discipline
```

Тесты **ДОЛЖНЫ**:
- Использовать mock helpers вместо прямого meck:new/expect
- Очищать все mocks в end_per_testcase
- Не оставлять глобальное состояние

---

## 4. Chaos Mode vs Mock Mode

### 4.1 Когда использовать Mock Mode

| Сценарий | Mock | Real (Docker) |
|----------|------|---------------|
| Локальная разработка | ✅ | Optional |
| CI без Docker | ✅ (degraded) | ❌ |
| CI с Docker | ❌ | ✅ |
| Integration tests | ❌ | ✅ |
| Unit tests | ✅ | ❌ |

### 4.2 CI Enforcement

```bash
# Требует Docker, падает если NATS недоступен
make test-chaos-ci

# Явно разрешает mock mode (degraded coverage)
make test-chaos-ci-degraded
```

---

## 5. CI Интеграция

### 5.1 CI Targets

```bash
# Mock discipline enforcement
make test-discipline

# Prometheus exporter validation (TYPE/HELP/labels)
make ci-validate-prometheus-export

# Full Circuit Breaker pipeline
make ci-circuit-breaker-pipeline

# Complete CI pipeline
make ci-full-pipeline
```

### 5.2 CI Pipeline Flow

```
make ci-circuit-breaker-pipeline
  │
  ├── Step 1: make test-circuit-breaker
  │     └── router_circuit_breaker_SUITE
  │
  ├── Step 2: make test-circuit-breaker-integration
  │     └── router_circuit_breaker_integration_SUITE
  │
  ├── Step 3: make test-circuit-breaker-invariants
  │     └── router_circuit_breaker_invariants_SUITE
  │
  ├── Step 4: make ci-validate-prometheus-export
  │     └── Validates: TYPE/HELP + labels + trigger_reason
  │
  └── Step 5: make test-discipline
        └── STRICT_MOCK_DISCIPLINE=true
```

### 5.3 GitHub Actions (пример)

```yaml
# .github/workflows/ci.yml
jobs:
  circuit-breaker:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Setup Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
          rebar3-version: '3.22.1'
      - name: Run CB pipeline
        run: make ci-circuit-breaker-pipeline
```

---

## 6. Чек-лист для PR

- [ ] Все meck mocks очищаются в end_per_testcase
- [ ] Используется `code:which(meck)` для проверки доступности
- [ ] passthrough используется при частичном мокировании
- [ ] `make test-discipline` проходит
- [ ] `make ci-validate-prometheus-export` проходит (если менялись метрики)
- [ ] `make ci-circuit-breaker-pipeline` проходит (если менялся CB)
- [ ] Chaos тесты не используют mock для NATS в CI

---

## 6. Файлы и модули

| Файл | Назначение |
|------|------------|
| `router_caf_test_helper.erl` | Helper для mock router_nats |
| `router_nats_test_helper.erl` | Helper для NATS mocking |
| `scripts/ci_check_chaos_mode.sh` | CI скрипт для проверки mock/docker mode |

---

## 7. Примеры ошибок

### 7.1 Mock не очищен

```
ERROR: Module router_nats is already mocked
```

**Решение**: Добавить `catch meck:unload(router_nats)` в end_per_testcase

### 7.2 Flaky тест из-за состояния

```
Test passes in isolation, fails in suite
```

**Решение**: Проверить что init_per_testcase сбрасывает состояние

### 7.3 meck not found

```
** exception error: undefined function meck:new/2
```

**Решение**: Добавить meck в test dependencies в rebar.config
