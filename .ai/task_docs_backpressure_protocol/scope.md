# Scope — T-DOCS-01

## In Scope
- Раздел "Gateway Backpressure Protocol" в `docs/API_CONTRACTS.md`.
- Маппинг состояний `active`, `warning`, `inactive`.
- Спецификация заголовка `Retry-After`.
- Примеры JSON для gRPC и REST интерфейсов.

## Out of Scope
- Изменение кода `router_gateway_backpressure.erl` (только если найдены критические расхождения с целевым контрактом).
- Реализация троттлинга в коде шлюза.
