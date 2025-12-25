# High-level схема
```
Client (gRPC) --> Router gRPC Service --> Router Core --> Policy Engine/Extensions --> Provider Selector --> NATS Adapter --> CAF (ExecAssignment)
CAF (DecideRequest via NATS) --> Router NATS Subscriber --> Router Core (same flow)
``` 

## Основные модули
- `router_grpc`/`router_admin_grpc`: gRPC-сервисы на `grpcbox` для принятия `RouteRequest`, возвращения `RouteDecision` и управления политиками.
- `router_core`: маршрут запроса через загрузку политики, применении ограничений, проверке rate limit, circuit breaker и выбору провайдера.
- `router_policy_store`: кэш политик в ETS с постоянным хранилищем (PostgreSQL) и RBAC-ограничением операций.
- `router_decider`: запускает цепочку расширений (pre-processors, validators, post-processors) и делегирует выбор провайдера `router_provider_selector`.
- `router_nats_subscriber` и `router_caf_adapter`: принимают `DecideRequest` из JetStream/NATS, публикуют `ExecAssignment`, поддерживают устойчивость через consumer/producer.
- `router_circuit_breaker`, `router_rate_limiter`, `router_quota`, `router_audit`, `router_metrics_http`: специализированные `gen_server` в супервизорной иерархии, которые обеспечивают устойчивость и наблюдаемость.

## Потоки данных
- **gRPC Decide**: внешний клиент вызывает `router_grpc:decide/2` → `router_core:route/2` загружает политику (`router_policy_store`) → `router_policy_applier` проверяет квоты и rate limit → `router_decider` прогоняет расширения → `router_provider_selector` выбирает провайдера с учётом circuit breaker и sticky sessions → результат кодируется и отправляется обратно клиенту как `RouteDecision`.
- **NATS DecideRequest**: CAF публикует `DecideRequest` → `router_nats_subscriber` читает сообщение, валидирует → тот же `router_core` и `router_decider` создают решение → при `push_assignment=true` `router_caf_adapter` публикует `ExecAssignment` обратно в CAF через NATS.
- **Circuit breaker и наблюдаемость**: каждый запрос проходит через `router_circuit_breaker` (удачи/ошибки обновляют состояние) и `router_metrics_http`, где экспонируются метрики по переходам состояний, задержкам и счётчикам политик.
