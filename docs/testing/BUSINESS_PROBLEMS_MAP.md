# Карта бизнес-проблем: Circuit Breaker / Chaos тесты

## Сводка

| Категория | Статус до исправления | После исправления |
|-----------|----------------------|-------------------|
| Ослабленные проверки | 5 тестов | 0 |
| Stub-тесты без логики | 4 теста | 0 |
| Бизнес-баги в prod-коде | 1 | Исправлен |
| Alert rules в Erlang | 3 правила | 7 правил (+4 новых) |
| Prometheus конфигурация | 0 | 1 файл (готов к деплою) |
| **Test Infrastructure** | **Flaky, no enforcement** | **CI hard-fail + deterministic seeds** |

---

## 0. Приоритизация по риску и стоимости

| # | Проблема | Риск | Стоимость исправления | Статус |
|---|----------|------|----------------------|--------|
| 1 | Latency threshold CB не работал | **CRITICAL** - CB не защищал от slow providers | Низкая (2 строки prod-кода) | ✅ Исправлено |
| 2 | Тесты принимали оба состояния | HIGH - регрессии не обнаруживались | Низкая (изменение ассертов) | ✅ Исправлено |
| 3 | Stub-тесты без логики | MEDIUM - нулевое покрытие переходов | Средняя (реализация логики) | ✅ Исправлено |
| 4 | Нет Prometheus алертов для trigger_reason | HIGH - ops не узнают о причинах CB | Низкая (YAML конфиг) | ✅ Добавлено |
| 5 | Нет recording rules для dashboards | MEDIUM - нет агрегаций для Grafana | Низкая (YAML конфиг) | ✅ Добавлено |
| 6 | Chaos тесты требуют инфраструктуры | LOW - ручное тестирование | Высокая (Docker + CI) | ✅ Mock fallback + CI enforcement |
| 7 | Flaky тесты (unseeded rand) | MEDIUM - невоспроизводимые сбои | Низкая (добавить seed) | ✅ Детерминированные seeds |
| 8 | Mock-тесты в CI без проверки | MEDIUM - silent coverage gaps | Низкая (CI script) | ✅ Hard-fail enforcement |

---

## 1. Выявленные проблемы (до исправления)

### 1.1 router_caf_adapter_enhanced_SUITE

**test_telemetry_span_attributes**
- **Проблема**: Тест проверял только наличие `assignment_id`, игнорируя остальные критичные атрибуты
- **Влияние**: Регрессии в telemetry metadata не отлавливались
- **Исправлено**: Добавлены STRICT проверки всех ключевых атрибутов: `request_id`, `tenant_id`, `subject`, `retries`, `pub_ack_id`

### 1.2 router_circuit_breaker_SUITE

**test_circuit_breaker_opens_on_latency_threshold**
- **Проблема**: `?assert(lists:member(State, [open, closed]))` — тест принимал ОБА состояния
- **Корневая причина бизнес-бага**: `get_recent_latency()` возвращал `{error, not_available}` для integer-значений метрик
- **Влияние**: Latency-based circuit breaker НЕ РАБОТАЛ в продакшене
- **Исправлено**: 
  1. Усилен тест до `?assertEqual(open, State)`
  2. Исправлен prod-код: `get_recent_latency()` теперь обрабатывает integer-значения

### 1.3 router_circuit_breaker_invariants_SUITE

**test_trigger_reason_correctness_failure**
- **Проблема**: Тест принимал состояние `closed` как валидное после 6 failures (threshold=5)
- **Влияние**: Failure threshold мог не срабатывать без обнаружения
- **Исправлено**: STRICT проверка `?assertEqual(open, State)`

### 1.4 router_circuit_breaker_integration_SUITE

**Stub-тесты без логики:**
- `test_circuit_breaker_half_open_to_closed` — был пустой `ok`
- `test_circuit_breaker_half_open_to_open` — был пустой `ok`  
- `test_circuit_breaker_with_retry` — был пустой `ok`
- `test_circuit_breaker_error_rate_threshold` — был пустой `ok`

**Влияние**: Нулевое покрытие критичных переходов состояний
**Исправлено**: Все тесты заполнены реальной логикой с STRICT проверками

**Мусорный экспорт:**
- `process_validated_result/12` — экспортировался, но функция не существовала
- **Исправлено**: Удален

---

## 2. Бизнес-логика: исправленные баги в prod-коде

### 2.1 router_circuit_breaker.erl — get_recent_latency/4

**Было:**
```erlang
[{router_nats_publish_latency_seconds, LatencySeconds}] when is_float(LatencySeconds) ->
    LatencyMs = trunc(LatencySeconds * 1000),
    {ok, LatencyMs};
_ ->
    {error, not_available}
```

**Проблема**: Guard `is_float` не пропускал integer-значения метрик

**Стало:**
```erlang
[{router_nats_publish_latency_seconds, LatencySeconds}] when is_float(LatencySeconds) ->
    LatencyMs = trunc(LatencySeconds * 1000),
    {ok, LatencyMs};
[{router_nats_publish_latency_seconds, LatencySeconds}] when is_integer(LatencySeconds) ->
    LatencyMs = LatencySeconds * 1000,
    {ok, LatencyMs};
[{router_nats_publish_latency_seconds, LatencyMs}] when is_number(LatencyMs), LatencyMs > 100 ->
    {ok, trunc(LatencyMs)};
_ ->
    {error, not_available}
```

---

## 3. Chaos Engineering тесты (router_chaos_engineering_SUITE)

### Текущий статус
- **Требуют**: `RUN_CHAOS_TESTS=true` и реальную инфраструктуру (NATS, network tools)
- **Тесты**: 8 (все требуют внешней среды)

### Рекомендации для запуска
```bash
RUN_CHAOS_TESTS=true rebar3 ct --suite=router_chaos_engineering_SUITE
```

### Известные ограничения
- `test_chaos_network_partition_*` требуют `router_network_partition` gen_server
- `test_chaos_service_degradation_*` требуют `router_nats_fault_injection` модуль
- Не запускаются в стандартном CI без специальной настройки

---

## 4. Стратегия закрытия бизнес-ошибок

### Фаза 1: Немедленные действия (выполнено)
1. ✅ Усилены ослабленные проверки в 3 SUITE-файлах
2. ✅ Заполнены 4 stub-теста реальной логикой
3. ✅ Исправлен баг в `get_recent_latency()` (latency threshold не работал)
4. ✅ Удален мусорный экспорт `process_validated_result/12`

### Фаза 2: Верификация (требуется)
```bash
# Запустить исправленные тесты
rebar3 ct --suite=router_circuit_breaker_SUITE
rebar3 ct --suite=router_circuit_breaker_integration_SUITE
rebar3 ct --suite=router_circuit_breaker_invariants_SUITE
rebar3 ct --suite=router_caf_adapter_enhanced_SUITE
```


### Фаза 3: Мониторинг в продакшене
1. Добавить алерт на `router_circuit_breaker_trigger_reason{reason="latency_threshold_exceeded"}`
2. Проверить метрики `router_circuit_breaker_state_transitions_total`
3. Валидировать, что latency threshold теперь срабатывает

### Фаза 4: Расширение Chaos тестов (ВЫПОЛНЕНО)
1. ✅ Настроен mock fallback mode (chaos тесты работают без Docker)
2. ✅ Добавлен `CHAOS_REQUIRE_DOCKER=true` для CI enforcement
3. ✅ Chaos тесты: `make test-chaos-ci` / `make ci-chaos-pipeline`

### Фаза 5: Test Infrastructure Improvements (ВЫПОЛНЕНО)

**Проблема 1: Flaky тесты (unseeded rand:uniform)**
| Suite | Решение |
|-------|---------|
| `router_concurrent_faults_stress_SUITE` | ✅ Deterministic seed `{1234, 5678, 91011}` |
| `router_intake_chaos_SUITE` | ✅ Logged seed + `CHAOS_RAND_SEED` env override |
| `router_metrics_under_faults_SUITE` | ✅ Deterministic seed |
| `router_publish_failure_e2e_randomized_SUITE` | ✅ Deterministic seed + `RANDOMIZED_TEST_SEED` env override |

**Проблема 2: Mock-тесты проходят по-тихому в CI**
| Механизм | Описание |
|----------|----------|
| `CHAOS_REQUIRE_DOCKER=true` | CI fails если Docker/NATS недоступен |
| `CHAOS_MOCK_ALLOWED=true` | Явное разрешение mock mode (degraded) |
| `scripts/ci_check_chaos_mode.sh` | Проверяет лог на "MOCK MODE" + fail |
| `make test-chaos-ci` | Запуск с enforcement |
| `make test-chaos-ci-degraded` | Запуск с explicit mock allowed |

**Проблема 3: Базовые моки без corner-case проверок**
| Механизм | Описание |
|----------|----------|
| `STRICT_MOCK_DISCIPLINE=true` | ct:fail() если только basic mocks |
| `check_corner_case_coverage/0` | Hint (default) или fail (strict) |
| `check_corner_case_coverage_strict/0` | Всегда fail на basic_only |
| `verify_publish_called_with/1` | Strict проверка аргументов |
| `make test-discipline` | CI target с strict mode |

**Проблема 4: Централизация router_nats mocking**
| Модуль | Функция |
|--------|---------|
| `router_nats_test_helper.erl` | Централизованный setup/teardown |
| `expect_publish_with_args/1` | Corner-case mocks |
| `expect_custom/2` | Любая функция router_nats |
| `get_call_history/1` | Для detailed debugging |

---

## 5. Метрики для мониторинга здоровья Circuit Breaker

```promql
# Переходы состояний CB
rate(router_circuit_breaker_state_transitions_total[5m])

# Причины открытия CB
router_circuit_breaker_trigger_reason{reason=~".*"}

# Текущее состояние CB по tenant/provider
router_circuit_breaker_state{state="open"}
```

---

## 6. Интеграция с Prometheus/Grafana

### 6.1 Деплой Alert Rules

```bash
# 1. Скопировать файл в Prometheus
cp priv/prometheus_circuit_breaker_alerts.yml /etc/prometheus/rules/

# 2. Валидировать конфигурацию
promtool check rules /etc/prometheus/rules/circuit_breaker_alerts.yml

# 3. Перезагрузить Prometheus
curl -X POST http://localhost:9090/-/reload

# 4. Проверить загрузку правил
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].name'
```

### 6.2 Добавление в prometheus.yml

```yaml
# prometheus.yml
rule_files:
  - "rules/*.yml"
  - "circuit_breaker_alerts.yml"  # Добавить эту строку

# Или конкретный путь:
rule_files:
  - "/etc/prometheus/rules/circuit_breaker_alerts.yml"
```

### 6.3 Grafana Dashboard Variables

```promql
# Переменная $tenant_id
label_values(router_circuit_breaker_state, tenant_id)

# Переменная $provider_id  
label_values(router_circuit_breaker_state{tenant_id="$tenant_id"}, provider_id)

# Переменная $trigger_reason
label_values(router_circuit_breaker_trigger_reason, reason)
```

### 6.4 Grafana Panels (готовые запросы)

**Panel: Circuit Breaker State Timeline**
```promql
router:circuit_breaker_state:current{tenant_id="$tenant_id", provider_id="$provider_id"}
```

**Panel: Trigger Reasons Distribution**
```promql
sum by (reason) (
  increase(router_circuit_breaker_trigger_reason{tenant_id="$tenant_id"}[1h])
)
```

**Panel: State Transitions Rate**
```promql
router:circuit_breaker_transitions:rate1m{tenant_id="$tenant_id"}
```

**Panel: Provider Health Score**
```promql
router:provider_health_score{tenant_id="$tenant_id"}
```

### 6.5 Alertmanager Routing (пример)

```yaml
# alertmanager.yml
route:
  routes:
    - match:
        component: circuit_breaker
        severity: critical
      receiver: pagerduty-critical
      continue: true
    - match:
        component: circuit_breaker
        severity: warning
      receiver: slack-platform
      group_wait: 5m
      group_interval: 10m

receivers:
  - name: pagerduty-critical
    pagerduty_configs:
      - service_key: ${PAGERDUTY_KEY}
        severity: critical
  - name: slack-platform
    slack_configs:
      - api_url: ${SLACK_WEBHOOK}
        channel: '#platform-alerts'
        title: 'Circuit Breaker Alert'
        text: '{{ .CommonAnnotations.description }}'
```

---

## 7. Тестирование алертов

### 7.1 Smoke Test

```bash
# Симулировать trigger_reason метрику
curl -X POST http://localhost:9091/metrics/job/test -d '
router_circuit_breaker_trigger_reason{tenant_id="test",provider_id="openai",reason="latency_threshold_exceeded"} 1
'

# Проверить, что алерт появился
curl -s http://localhost:9090/api/v1/alerts | jq '.data.alerts[] | select(.labels.alertname | contains("Latency"))'
```

### 7.2 Интеграционный тест с Erlang

```erlang
%% В тесте:
%% 1. Эмитировать метрику
router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{count => 1}, #{
    tenant_id => ~"test",
    provider_id => ~"openai",
    reason => ~"latency_threshold_exceeded"
}),

%% 2. Вызвать evaluate_all_rules
{ok, Results} = router_alerts:evaluate_all_rules(),

%% 3. Проверить, что алерт сработал
FiringAlerts = [R || R <- Results, maps:get(firing, R, false) =:= true],
?assert(length(FiringAlerts) > 0).
```

---

## 8. Привязка Alert Rules к экспортеру метрик

### 8.1 Архитектура потока метрик

```
┌─────────────────────────┐
│ router_circuit_breaker  │
│ .erl                    │
│                         │
│ emit_metric(            │
│   router_circuit_breaker│
│   _trigger_reason,      │
│   #{count => 1},        │
│   #{tenant_id => ...,   │
│     reason => ...})     │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ router_metrics.erl      │
│ (ETS: router_metrics)   │
│                         │
│ Key: {MetricName,       │
│       LabelsKey}        │
│ Val: count/value        │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ router_prometheus.erl   │
│ render() -> binary      │
│                         │
│ get_metric_metadata/1   │  ← Добавлены метаданные CB
│ format_labels/1         │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ HTTP endpoint           │
│ /metrics                │
│                         │
│ # HELP router_circuit...│
│ # TYPE router_circuit...│
│ router_circuit_breaker_ │
│ trigger_reason{...} 1   │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Prometheus scrape       │
│ prometheus.yml:         │
│   scrape_configs:       │
│     - job: router       │
│       static_configs:   │
│         - targets:      │
│           ['host:8080'] │
└───────────┬─────────────┘
            │
            ▼
┌─────────────────────────┐
│ Prometheus rules        │
│ circuit_breaker_alerts  │
│ .yml                    │
│                         │
│ alert: CircuitBreaker   │
│   LatencyThreshold...   │
│ expr: router_circuit_   │
│   breaker_trigger_reason│
│   {reason="latency..."}│
└─────────────────────────┘
```

### 8.2 Метрики и где они эмитируются

| Метрика | Эмитируется в | Файл | Строки |
|---------|---------------|------|--------|
| `router_circuit_breaker_state` | `record_state`, `update_on_failure`, `update_on_success` | `router_circuit_breaker.erl` | 421, 476, 598, 680, 810, 949 |
| `router_circuit_breaker_state_transitions_total` | `update_on_failure`, `update_on_success` | `router_circuit_breaker.erl` | 590, 672, 802, 941 |
| `router_circuit_breaker_trigger_reason` | `update_on_failure` (при открытии CB) | `router_circuit_breaker.erl` | 605, 817, 956 |
| `router_nats_publish_latency_seconds` | `publish_with_ack` | `router_nats.erl` | (emit после publish) |

### 8.3 Метаданные в экспортере

```erlang
%% router_prometheus.erl - get_metric_metadata/1
router_circuit_breaker_state ->
  {"gauge", "Current circuit breaker state (0=closed, 0.5=half_open, 1=open)"};
router_circuit_breaker_state_transitions_total ->
  {"counter", "Total circuit breaker state transitions"};
router_circuit_breaker_trigger_reason ->
  {"counter", "Count of circuit breaker openings by trigger reason"};
router_nats_publish_latency_seconds ->
  {"gauge", "Latest NATS publish latency in seconds"};
```

### 8.4 Пример Prometheus output

```prometheus
# HELP router_circuit_breaker_state Current circuit breaker state (0=closed, 0.5=half_open, 1=open)
# TYPE router_circuit_breaker_state gauge
router_circuit_breaker_state{tenant_id="acme",provider_id="openai",state="open"} 1.0
router_circuit_breaker_state{tenant_id="acme",provider_id="anthropic",state="closed"} 0.0

# HELP router_circuit_breaker_trigger_reason Count of circuit breaker openings by trigger reason
# TYPE router_circuit_breaker_trigger_reason counter
router_circuit_breaker_trigger_reason{tenant_id="acme",provider_id="openai",reason="latency_threshold_exceeded"} 3
router_circuit_breaker_trigger_reason{tenant_id="acme",provider_id="openai",reason="failure_threshold_exceeded"} 1

# HELP router_circuit_breaker_state_transitions_total Total circuit breaker state transitions
# TYPE router_circuit_breaker_state_transitions_total counter
router_circuit_breaker_state_transitions_total{tenant_id="acme",provider_id="openai",from="closed",to="open"} 4
router_circuit_breaker_state_transitions_total{tenant_id="acme",provider_id="openai",from="open",to="half_open"} 3
```

### 8.5 prometheus.yml конфигурация

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

rule_files:
  - "/etc/prometheus/rules/circuit_breaker_alerts.yml"

scrape_configs:
  - job_name: 'beamline-router'
    static_configs:
      - targets: ['router-host:8080']
    metrics_path: '/metrics'
    scheme: 'http'
```

---

## 9. Файлы проекта

### Prod-код и Alerting
| Файл | Назначение |
|------|------------|
| `src/router_circuit_breaker.erl` | Эмиттер метрик CB (emit_metric) |
| `src/router_prometheus.erl` | Prometheus экспортер (render + metadata) |
| `src/router_alert_rules.erl` | Erlang-определения alert rules |
| `src/router_alerts.erl` | Gen_server для оценки алертов |
| `priv/prometheus_circuit_breaker_alerts.yml` | Prometheus alert + recording rules |

### Test Infrastructure (добавлено)
| Файл | Назначение |
|------|------------|
| `test/router_nats_test_helper.erl` | Централизованное mocking router_nats |
| `test/router_caf_test_helper.erl` | CAF adapter test utilities |
| `test/TEST_NOTES.md` | Документация test infrastructure |
| `test/BUSINESS_PROBLEMS_MAP.md` | Этот документ |
| `scripts/ci_check_chaos_mode.sh` | CI enforcement для chaos mode |

### CI Makefile Targets (добавлено)
| Target | Назначение |
|--------|------------|
| `make test-chaos` | Chaos тесты (auto-detect Docker) |
| `make test-chaos-ci` | Chaos тесты + CI enforcement |
| `make test-chaos-ci-degraded` | Chaos тесты (mock explicitly allowed) |
| `make test-discipline` | STRICT_MOCK_DISCIPLINE enforcement |
| `make reproduce-chaos SEED=A,B,C` | Воспроизведение по seed |

### CI Pipelines (полный цикл)
| Target | Что проверяет |
|--------|---------------|
| `make ci-validate-prometheus-export` | CB метрики: TYPE/HELP + labels + trigger_reason |
| `make ci-circuit-breaker-pipeline` | Полный CB цикл: unit + integration + export + discipline |
| `make ci-full-pipeline` | Всё: quality gates + tests + CB + chaos |

### CI Цикл разработки → тесты → CI

```
┌─────────────────────────────────────────────────────────────────┐
│                        РАЗРАБОТКА                                │
├─────────────────────────────────────────────────────────────────┤
│ 1. Изменить router_circuit_breaker.erl (emit_metric)            │
│ 2. Добавить metadata в router_prometheus.erl (TYPE/HELP)        │
│ 3. Добавить alert rules в router_alert_rules.erl                │
└──────────────────────────┬──────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│                         ТЕСТЫ                                    │
├─────────────────────────────────────────────────────────────────┤
│ make test-circuit-breaker          # Unit tests                 │
│ make test-circuit-breaker-integration  # Integration            │
│ make test-prometheus-exporter      # TYPE/HELP/labels           │
│ make test-discipline               # Mock discipline            │
└──────────────────────────┬──────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│                      CI PIPELINE                                 │
├─────────────────────────────────────────────────────────────────┤
│ make ci-circuit-breaker-pipeline   # Full CB validation         │
│   ├── Step 1: unit tests                                        │
│   ├── Step 2: integration tests                                 │
│   ├── Step 3: invariants tests                                  │
│   ├── Step 4: Prometheus export validation                      │
│   └── Step 5: Mock discipline check                             │
└──────────────────────────┬──────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────────┐
│                    PROMETHEUS                                    │
├─────────────────────────────────────────────────────────────────┤
│ 1. Deploy priv/prometheus_circuit_breaker_alerts.yml            │
│ 2. Алерты работают потому что CI валидировал:                   │
│    - TYPE gauge/counter                                          │
│    - Labels tenant_id, provider_id, reason                       │
│    - Trigger reasons для alerting                                │
└─────────────────────────────────────────────────────────────────┘
```

---

---

## 10. Governance & Maturity

### Test Policy (см. TEST_GOVERNANCE.md)

| Requirement | Enforcement | Target |
|-------------|-------------|--------|
| `make test-fast` | **HARD BLOCK** | Every PR |
| No new warnings | **HARD BLOCK** | Every PR |
| CB pipeline | **HARD BLOCK** | CB changes |
| Mock discipline | **WARN** | Default (HARD in CI) |

### Test Maturity Level (см. TEST_MATURITY.md)

```
Current: Level 2 (Managed) - 70%
Target:  Level 3 (Defined) - by Q2 2025
```

| KPI | Target | Current |
|-----|--------|---------|
| Test Pass Rate | > 98% | ~95% |
| Flakiness Rate | < 2% | ~5% |
| CB Coverage | > 80% | ~70% |

### Governance Documents

| Document | Purpose |
|----------|---------|
| `TEST_GOVERNANCE.md` | Policies, standards, ownership |
| `TEST_MATURITY.md` | Maturity model, KPIs, roadmap |
| `MOCK_DISCIPLINE.md` | Mock usage guidelines |
| `BUSINESS_PROBLEMS_MAP.md` | This document |

---

## 11. Контакты и ответственность

| Область | Владелец | Файлы |
|---------|----------|-------|
| Circuit Breaker логика | Platform Team | `router_circuit_breaker.erl` |
| Chaos тесты | Platform Team | `router_chaos_engineering_SUITE.erl`, `router_intake_chaos_SUITE.erl` |
| Телеметрия | Platform Team | `router_caf_adapter.erl`, `router_tracing.erl` |
| Метрики | Platform Team | `router_r10_metrics.erl` |
| Alerting | SRE Team | `router_alert_rules.erl`, `prometheus_circuit_breaker_alerts.yml` |
| Dashboards | SRE Team | Grafana (см. раздел 6.4) |
| **Test Infrastructure** | **Platform Team** | `router_nats_test_helper.erl`, `TEST_NOTES.md`, `Makefile` |
| **CI Enforcement** | **DevOps Team** | `scripts/ci_check_chaos_mode.sh`, CI pipeline configs |
