# Термины
- **Router** – Erlang-компонент Beamline, отвечающий за выбор оптимального провайдера на основании политик, квот, ограничений и состояния circuit breaker.
- **CAF (Compute Action Framework)** – C++-служба, исполняющая маршрутизированные задания; получает `ExecAssignment` и инициирует downstream.
- **gRPC** – контрактный интерфейс для `Router.Decide` и `RouterAdmin`, реализованный на `grpcbox`.
- **NATS / JetStream** – шина сообщений, по которой Router подписывается на `DecideRequest` и публикует `ExecAssignment`.
- **Policy (Routing Policy)** – набор правил в ETS/Postgres, определяющий порядок провайдеров, веса, sticky sessions и fallback.
- **Circuit breaker** – механизм отслеживания ошибок/задержек по провайдерам и временного исключения проблемных узлов.
- **Rate limiter / Quota** – ограничения по частоте запросов и пространству политик на тенанта.
- **Extension pipeline** – последовательность pre/post-processor и валидаторов, расширяющих логику маршрутизации.
- **RouteDecision / ExecAssignment** – результат маршрутизации для клиентов и CAF.
- **DecideRequest** – входящее сообщение от CAF для маршрутизатора.
- **RBAC** – контроль доступа к политике на уровне ролей (admin/operator/viewer).

# Имена сущностей
- `router_core` – основной модуль, объединяющий policy store, decision logic и проверку ограничений.
- `router_policy_store` – кеширующий хранилище политик с ETS/DB и RBAC.
- `router_decider` / `router_provider_selector` – модули, запускающие расширения и выбирающие провайдеров.
- `router_nats_subscriber` / `router_caf_adapter` – NATS-клиенты для двунаправленного обмена с CAF.
- `router_grpc` / `router_admin_grpc` – gRPC-интерфейсы для клиентских запросов и управляемого API.

# Договорённости
- Входящие запросы всегда содержат `tenant_id` и `correlation_id`, используемые для RBAC и трассировки.
- Если `push_assignment=true`, Router обязан отправить `ExecAssignment` в CAF через NATS после принятия решения.
- Политики должны храниться в ETS и обновляться из PostgreSQL, чтобы избежать потери состояния при ребалансировке.
- Метрики и audit-записи доступны через `/metrics` и логируются для каждой операции с политикой.
