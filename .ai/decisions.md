# Принятые архитектурные решения и альтернативы

## Решение 1: gRPC-сервисы на `grpcbox` для `Router.Decide` и `RouterAdmin`
**Почему**: gRPC обеспечивает строго типизированный контракт с NestJS-гейтуэеем и другими клиентами, позволяет использовать двоичную сериализацию и поддерживает высокую пропускную способность, а `grpcbox` уже используется в проекте и отлично вписывается в OTP-супервизоры.
**Альтернативы**: REST/HTTP-подход увеличил бы доп. нагрузку на парсинг JSON и усложнил бы синхронизацию контрактов; прямой NATS-вход без явного API усложнил бы дебаг и откат ошибок.

## Решение 2: Коммуникация с CAF через NATS (DecideRequest/ExecAssignment)
**Почему**: CAF уже ожидает обмен JSON-сообщениями по NATS, а текущая архитектура с `router_nats_subscriber` и `router_caf_adapter` позволяет использовать JetStream-лейеры, повторения и очереди, сохраняя отказоустойчивость.
**Альтернативы**: Прямой gRPC-поток от Router к CAF добавил бы дисцеплину нас за объёмом состояния NATS и потребовал бы дополнительного протокола согласования; HTTP/push не поддерживал бы гарантированную доставку и перезапуск потребителей.

## Решение 3: Супервизорный подход с gen_server для policy store, rate limiter, circuit breaker, RBAC и аудита
**Почему**: OTP-супервизоры позволяют изолировать состояние, быстро восстанавливать компоненты и держать отдельные ETS/DB-кэши под контролем; отдельные gen_server-функции упрощают трассировку, метрики и RBAC.
**Альтернативы**: Центральный процесс с общей базой усложнил бы масштабирование и быструю реакцию на частные сбои (например, если поломался circuit breaker, всё бы упало вместо изоляции).

## Решение 4: Direct router_nats interactions только через утверждённые helper-модули
**Почему**: `router_nats` — живая внешняя зависимость, и прямые `gen_server:call(router_nats, ...)` из тестов могут приводить к неопределённой нагрузке, если mock включает `[passthrough]`; перенаправляя все обращения через `router_nats_test_helper` (канонический) и `router_mock_helpers` (вспомогательный), мы локализуем риски и можем контролировать, когда passthrough допускается.
**Альтернативы**: Позволять произвольные прямые вызовы больше не безопасно: они размывают ответственность за mock-состояние и ломают качество тестов, поэтому добавили линтер `scripts/lint/check_router_mock_discipline.sh`, который буквально запрещает файл, содержащий одновременно `meck:new(..., [passthrough])` и `gen_server:call(router_nats, ...)` вне утверждённых helper-модулей.

## Решение 5: Стабильный API для таймаутов тестов по уровням
**Почему**: Чтобы избежать «магических чисел» в тестах, `router_test_timeouts` предлагает одну точку конфигурации и понятные ключи (`very_short`, `short`, `default`, `long`) для всех tiers (`sanity`, `fast`, `full`, `heavy`) и вычисляет значения на основании `ROUTER_TEST_LEVEL`. Сценарии пишут `router_test_timeouts:very_short_wait()` и `router_test_timeouts:long_wait()`, а не уловки с умножением чисел.
**Альтернативы**: Распылённые literals легко рассогласовываются с tier-скриптами, поэтому прописали также, что `ROUTER_TEST_LEVEL` (включён в `scripts/ct-*.sh`) — единственный источник правды, и `router_test_timeouts` по умолчанию разбирает его, выбрав `full`, если переменная отсутствует.

- Tests: timeouts must use `router_test_timeouts` (tiers: sanity/fast/full/heavy); avoid numeric literals unless explicitly justified.
- Tests: router_nats interactions must go through approved helpers; passthrough + direct call outside helpers is forbidden and enforced by lint.

- Timeout literals in tests are discouraged and tracked by check_timeout_literals.sh.
  The check currently emits warnings and will be promoted to a hard CI failure
  once existing literals are fully migrated to router_test_timeouts.

## Решение 6: Карантин отражает временное состояние теста
**Почему**: Карантин помогает дать сигнал о нестабильных тестах, но без явного lifecycle он рискует стать постоянным списком проблем; поэтому мы документируем owner/reason/периодичность обзора в `config/quarantine/quarantined_suites.txt` и завязываем вывод из карантина на подтверждённых хороших прохождениях heavy-типа.
**Альтернативы**: Оставить карантин без ограничений означало бы, что flaky suite автоматически блокирует full tier — мы уже выбрали модель, где тест может временно быть исключён, но возращается в нормальную сеть после последовательных heavy прогонов и регулярного ревью.
