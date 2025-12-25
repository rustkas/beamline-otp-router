# Scope — T-INTEG-01

## In scope
- “How to integrate” гайд: subjects, payloads, error codes, retry/backpressure semantics (Option B)
- Примеры клиентов (минимум 1): publish DecideRequest + consume DecideResponse
- Mock harness: поднять локальный NATS/JS + pre-create streams/consumers (если нужно)
- Contract tests: проверки схемы сообщений на стороне интегратора

## Out of scope
- Полноценный SDK для 3+ языков
- UI/Portal для интеграторов
