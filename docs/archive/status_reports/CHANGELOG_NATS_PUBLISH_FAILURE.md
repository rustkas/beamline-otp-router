# NATS Publish Failure Handling - Changes Summary

## Что изменилось

### Тесты

**Добавлено 6 новых тестов** в `router_nats_publish_failure_SUITE.erl`:
- Проверка влияния на `router` и `router_jetstream`
- Проверка fail-open поведения на верхнем уровне
- Проверка queueing поведения
- Проверка исчерпания retry попыток

**Всего тестов**: 29 (было 23, стало 29)

### Документация

**Новый документ**:
- `NATS_PUBLISH_FAILURE_MONITORING.md` - Руководство по мониторингу и алертингу для SRE

**Обновленные документы**:
- `NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Добавлен раздел о влиянии на верхний уровень
- `FULL_DOCS.md` - Добавлены ссылки на новые документы
- `NATS_CONNECTION_RESILIENCE.md` - Добавлена ссылка на мониторинг

## Что теперь "норма" при фолтах

### Fail-Open Mode (`nats_fail_open_mode = true`)

**Ожидаемое поведение**:
- ✅ `router_nats` возвращает успех (`ok` / `{ok, stub-msg-id}`) даже при ошибках
- ✅ Система продолжает принимать новые сообщения без блокировки
- ✅ Сообщения **НЕ** ставятся в очередь (могут быть потеряны)
- ✅ Ошибки логируются и метрируются, но не прокидываются наверх
- ✅ `router_caf_adapter` считает это успехом (нет retry)

**Когда использовать**: Высокая доступность важнее гарантированной доставки

### Queueing Mode (`nats_fail_open_mode = false`) - По умолчанию

**Ожидаемое поведение**:
- ✅ `router_nats` возвращает `{error, Reason}` при ошибках
- ✅ Сообщения **ставятся в очередь** для повторной отправки после переподключения
- ✅ Верхние уровни получают ошибки и реализуют retry логику
- ✅ `router_caf_adapter` делает retry до MaxRetries с exponential backoff
- ⚠️ Система может блокироваться, если очередь заполнится

**Когда использовать**: Требуется гарантированная доставка, важен порядок сообщений

### Метрики

**`router_nats_publish_failures_total`** и **`router_nats_publish_with_ack_failures_total`**:
- ✅ Увеличиваются **ровно один раз** на каждую неудачную операцию
- ✅ Увеличиваются **независимо от режима** (fail-open/queueing)
- ✅ Увеличиваются **до** возврата результата вызывающему коду
- ✅ **НЕ увеличиваются** при успешных операциях
- ✅ **НЕ сбрасываются** при переподключении (кумулятивный счетчик)

## Как запустить тесты

```bash
cd apps/otp/router

# Запустить все тесты publish failure
rebar3 ct --suite test/router_nats_publish_failure_SUITE

# Или параллельно (быстрее)
rebar3 ct -j 4 --suite test/router_nats_publish_failure_SUITE

# Или через Makefile
make test-parallel
```

**Время выполнения**: ~30-50 секунд (29 тестов)

## Мониторинг

**Ключевые метрики**:
- `rate(router_nats_publish_failures_total[5m])` - Частота ошибок publish
- `rate(router_nats_publish_with_ack_failures_total[5m])` - Частота ошибок publish_with_ack
- `router_nats_pending_operations_count` - Размер очереди
- `router_nats_connection_status` - Статус подключения

**Рекомендуемые алерты** (см. `NATS_PUBLISH_FAILURE_MONITORING.md`):
- **Критические**: Частота ошибок > 10/сек, переполнение очереди, постоянная потеря подключения
- **Предупреждения**: Частота ошибок > 5/сек, очередь > 80% заполнена

## Документация

**Для разработчиков**:
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Полная спецификация поведения

**Для SRE/операторов**:
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md` - Руководство по мониторингу и алертингу

**Быстрая справка**:
- `apps/otp/router/docs/FULL_DOCS.md` - Единая документация роутера (включает ссылки)

## Ссылки

- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Спецификация поведения
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md` - Мониторинг и алертинг
- `docs/README.md` - Общая документация (добавлены ссылки)

