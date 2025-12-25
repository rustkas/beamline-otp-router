# Текущее состояние

## Что сделано
- Реализован gRPC-интерфейс `Router.Decide` / `RouterAdmin` на `grpcbox`, обеспечен обработчик `RouteRequest` и `RouteDecision`.
- Развёрнут процессный супервизор с `router_policy_store`, `router_circuit_breaker`, `router_rate_limiter` и `router_nats_subscriber`, в соответствии с архитектурной документацией.
- Написана логика обработки `DecideRequest`/`ExecAssignment` через NATS, включая публикацию в CAF и сбор метрик для Prometheus.

## Что делается
- Повышается наблюдаемость: `router_metrics_http` экспортирует метрики, собирается аудит политик и отслеживаются переходы circuit breaker.
- Поддерживается интеграция с расширениями (pre/post processors) и валидацией, чтобы можно было быстро запускать новые правила маршрутизации.

## Что дальше
- Завершить автоматизацию тестов, покрывающих fallback-провайдеры и отказоустойчивость (см. `docs/NATS_RESILIENCE*`).
- Улучшить документацию по операциям и производительности (обновить RUNBOOK, PERFORMANCE_GUIDE) и согласовать с командами CAF и Gateway.
