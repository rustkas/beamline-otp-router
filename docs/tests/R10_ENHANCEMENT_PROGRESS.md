# R10 Enhancement Progress Report

**Date**: 2025-01-27  
**Status**: ✅ **IN PROGRESS** - MVP усилен, Scenario 3 добавлен

## Summary

Начата реализация рекомендаций по усилению R10 тестирования. Усилен MVP-сценарий с ассертами по retry-модели и проверками SLA/таймаутов. Добавлен Scenario 3 (latency-based trigger).

## Completed Enhancements

### 1. ✅ Усилен MVP-сценарий (scenario_mass_failure_opens_breaker)

**Добавлено**:

1. **Ассерты по retry-модели**:
   - `get_publish_attempts_delta/1` - измерение delta до/после сценария
   - `assert_retry_model_behavior/4` - проверка, что attempts ≈ requests × maxAttempts до открытия breaker
   - `assert_max_attempts_not_exceeded/1` - проверка, что ни одна попытка не превышает maxAttempts
   - Проверка снижения попыток после открытия breaker (только half-open пробы)

2. **Проверки SLA/таймаутов**:
   - `{timetrap, {seconds, 30}}` - защита от зависаний
   - Проверка `totalDeadline` (10s из R10 spec)
   - Проверка времени восстановления (recovery time)

3. **Улучшенные метрики**:
   - Delta измерение для attempts и errors
   - Проверка увеличения errors после fault injection

**Файлы**:
- `test/router_test_utils.erl` - добавлены хелперы для retry-ассертов
- `test/router_publish_failure_e2e_SUITE.erl` - усилен scenario_mass_failure_opens_breaker

### 2. ✅ Усилен Scenario 2 (scenario_recovery_after_failure)

**Добавлено**:

1. **Проверки retry-модели после recovery**:
   - Проверка, что после recovery attempts ≈ requests (нет избыточных retries)
   - Delta измерение для probe requests и normal requests

2. **Проверки SLA**:
   - Проверка времени recovery (должно быть ≤ openTimeout + buffer)
   - Timetrap защита

**Файлы**:
- `test/router_publish_failure_e2e_SUITE.erl` - усилен scenario_recovery_after_failure

### 3. ✅ Добавлен Scenario 3 (scenario_latency_based_trigger)

**Реализовано**:

1. **Latency-based trigger**:
   - Конфигурация circuit breaker с `latency_threshold_ms = 5000`
   - Fault injection: `{delay, 6000}` (превышает threshold)
   - Проверка открытия breaker по latency, а не по error count

2. **Проверки защиты**:
   - Проверка, что новые запросы блокируются быстро (< 100ms)
   - Проверка, что client latency остается в SLO
   - Проверка `should_allow` возвращает `{error, circuit_open}`

**Файлы**:
- `test/router_publish_failure_e2e_SUITE.erl` - добавлен scenario_latency_based_trigger

## New Helper Functions

### router_test_utils.erl

1. `get_publish_attempts_by_retry/0` - получение attempts по retry_count (placeholder для будущего label tracking)
2. `get_publish_attempts_delta/1` - измерение delta attempts до/после действия
3. `assert_max_attempts_not_exceeded/1` - проверка, что maxAttempts не превышен
4. `assert_retry_model_behavior/4` - проверка retry-модели (attempts ≈ requests × maxAttempts)
5. `get_publish_errors_delta/1` - измерение delta errors до/после действия

## Completed Enhancements (Continued)

### 4. ✅ Параметризация через CT-конфиг (r10-5)

**Реализовано**:

1. **CT-конфиг** (`test/ct.config`):
   - Параметры: `r10_load_clients`, `r10_requests_per_client`, `r10_failure_type`, `r10_profile`
   - Поддержка профилей: `ci` (10×20) и `heavy` (50×100)
   - Переопределение через environment variables

2. **Интеграция в тесты**:
   - `get_r10_config/0` - чтение параметров из CT config
   - Обновлен `scenario_mass_failure_opens_breaker` для использования параметров
   - Поддержка стратегий: `sync` и `concurrent`

**Файлы**:
- `test/ct.config` - конфигурация параметров
- `test/router_publish_failure_e2e_SUITE.erl` - обновлен для использования параметров

### 5. ✅ Общий helper для spawn_clients (r10-6)

**Реализовано**:

1. **router_r10_client_utils.erl**:
   - `spawn_clients/2` и `spawn_clients/3` - генерация клиентов
   - `spawn_clients_sync/2` - синхронная стратегия (CI-friendly)
   - `spawn_clients_concurrent/2` - конкурентная стратегия (load-friendly)
   - `wait_for_clients/2` - ожидание завершения клиентов
   - `get_r10_config/0` - получение конфигурации из CT config
   - `get_r10_profile/0` - получение профиля (ci/heavy)

2. **Рефакторинг E2E suite**:
   - Удалены дублирующие функции `spawn_clients` и `wait_for_clients`
   - Импорт из `router_r10_client_utils`
   - Использование параметризованной конфигурации

**Файлы**:
- `test/router_r10_client_utils.erl` - новый helper-модуль
- `test/router_publish_failure_e2e_SUITE.erl` - рефакторинг для использования helper

## Completed Enhancements (Final)

### 4. ✅ Добавлен Scenario 4: Error Rate / Partial Failure (r10-4)

**Реализовано**:

1. **Intermittent fault injection**:
   - Использование `{intermittent, Fault, 0.5}` для 50% успешных, 50% faulted запросов
   - Проверка открытия breaker по `errorRateThreshold`, а не только по consecutive failures

2. **Проверки**:
   - До порога: часть запросов проходит, часть падает, breaker всё ещё `closed`
   - После превышения `errorRateThreshold`: breaker `open`, новые запросы сразу режутся
   - Проверка увеличения errors после partial failures

**Файлы**:
- `test/router_publish_failure_e2e_SUITE.erl` - добавлен `scenario_error_rate_partial_failure`

### 5. ✅ Создан модуль констант метрик (r10-7)

**Реализовано**:

1. **router_r10_metrics.erl**:
   - Централизованные имена метрик (aligned с OBSERVABILITY_CONVENTIONS)
   - Имена label'ов (status, retry_count, error_type, state, from, to, reason, attempt)
   - Значения label'ов (status_success/error, state_closed/open/half_open, trigger_reason_*)

2. **Выравнивание с конвенциями**:
   - Все метрики используют единые имена и форматы
   - Label'ы соответствуют OBSERVABILITY_CONVENTIONS
   - Готово для использования в прод-коде и тестах

**Файлы**:
- `src/router_r10_metrics.erl` - новый модуль констант метрик

### 6. ✅ Укреплены лог-контракты (r10-8)

**Реализовано**:

1. **Circuit breaker логи**:
   - Добавлено `event="circuit_breaker_state_changed"` во все переходы состояний
   - Добавлены поля `from`, `to`, `reason` во все логи переходов
   - Все логи содержат `tenant_id`, `provider_id`

2. **Publish retry логи**:
   - Добавлено `event="publish_retry"` во все retry-логи
   - Добавлены поля `attempt`, `max_attempts`, `error_type`, `deadline_exceeded` (bool)
   - Все логи содержат необходимый контекст

**Файлы**:
- `src/router_circuit_breaker.erl` - укреплены логи переходов состояний
- `src/router_nats_publish_retry.erl` - укреплены retry-логи

### 7. ✅ Добавлены риск-сценарии (r10-9)

**Реализовано**:

1. **Scenario 5: Thundering Herd on Recovery**:
   - Эмуляция нескольких tenant/provider пар с одинаковыми таймингами
   - Проверка, что `halfOpenMaxAttempts` ограничивает суммарный трафик
   - Проверка координации recovery (jitter/разброс)

2. **Scenario 6: Deadline vs SLA**:
   - Перегрузка retries так, чтобы без `totalDeadline` запрос превышал SLA
   - Проверка, что с `totalDeadline` ответ возвращается раньше SLA
   - Проверка compliance rate (≥50% в пределах SLA)

**Файлы**:
- `test/router_publish_failure_e2e_SUITE.erl` - добавлены `scenario_thundering_herd_recovery` и `scenario_deadline_vs_sla`

## Final Status

✅ **ALL TASKS COMPLETED**

Все 9 задач выполнены:
1. ✅ Усилен MVP-сценарий с retry-ассертами
2. ✅ Добавлены проверки SLA/таймаутов
3. ✅ Добавлен Scenario 3 (latency-based trigger)
4. ✅ Добавлен Scenario 4 (error rate / partial failure)
5. ✅ Параметризация через CT-конфиг
6. ✅ Общий helper для spawn_clients
7. ✅ Модуль констант метрик
8. ✅ Укреплены лог-контракты
9. ✅ Добавлены риск-сценарии

## Files Created/Modified

### New Files
- ✅ `test/router_r10_client_utils.erl` - helper для клиентов
- ✅ `test/ct.config` - конфигурация параметров
- ✅ `src/router_r10_metrics.erl` - константы метрик
- ✅ `test/R10_ENHANCEMENT_PROGRESS.md` - отчет о прогрессе

### Modified Files
- ✅ `test/router_test_utils.erl` - добавлены retry-хелперы
- ✅ `test/router_publish_failure_e2e_SUITE.erl` - усилены сценарии, добавлены новые
- ✅ `src/router_circuit_breaker.erl` - укреплены лог-контракты
- ✅ `src/router_nats_publish_retry.erl` - укреплены лог-контракты

## Test Coverage

### E2E Scenarios (6 total)
1. ✅ `scenario_mass_failure_opens_breaker` - усилен с retry-ассертами
2. ✅ `scenario_recovery_after_failure` - усилен с проверками recovery
3. ✅ `scenario_latency_based_trigger` - новый (latency-based opening)
4. ✅ `scenario_error_rate_partial_failure` - новый (50% failures)
5. ✅ `scenario_thundering_herd_recovery` - новый (recovery coordination)
6. ✅ `scenario_deadline_vs_sla` - новый (deadline protection)

## Next Steps (Optional Future Enhancements)

3. **Error rate / partial failure сценарий** (r10-4):
   - Добавить Scenario 4 с 50% успешных, 50% faulted запросов
   - Проверка открытия breaker по `errorRateThreshold`

4. **Выравнивание метрик с OBSERVABILITY_CONVENTIONS** (r10-7):
   - Создать модуль констант метрик
   - Выровнять имена метрик с конвенциями проекта

5. **Укрепление лог-контрактов** (r10-8):
   - Добавить `event="circuit_breaker_state_changed"` с полями `from`, `to`, `reason`
   - Добавить `event="publish_retry"` с полями `attempt`, `max_attempts`, `error_type`

6. **Риск-сценарии** (r10-9):
   - Thundering herd на recovery
   - Deadline vs SLA
   - Локальные очереди / backpressure

## Technical Notes

### Retry Model Assertions

Проверки retry-модели основаны на формуле:
- **До открытия breaker**: `attempts ≈ requests × maxAttempts` (с допуском 20%)
- **После открытия breaker**: `attempts ≤ requests × 2` (только пробы)

### SLA Compliance

- `totalDeadline = 10000ms` (из R10 spec)
- `timetrap = 30s` для защиты от зависаний
- Recovery time проверяется: `≤ openTimeout + buffer`

### Latency-Based Trigger

- `latency_threshold_ms = 5000` (из R10 spec)
- Fault injection: `{delay, 6000}` превышает threshold
- Breaker должен открыться по latency, а не по error count

## Files Modified

- ✅ `test/router_test_utils.erl` - добавлены хелперы для retry-ассертов
- ✅ `test/router_publish_failure_e2e_SUITE.erl` - усилены MVP-сценарии, добавлен Scenario 3

## Test Status

- ✅ `scenario_mass_failure_opens_breaker` - усилен, компилируется
- ✅ `scenario_recovery_after_failure` - усилен, компилируется
- ✅ `scenario_latency_based_trigger` - добавлен, компилируется

**Next**: Параметризация через CT-конфиг и создание helper-модуля для клиентов.

