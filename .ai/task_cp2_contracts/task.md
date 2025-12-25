# T-CP2-CONTRACTS-01 — CP2 Contracts: Subjects/Headers/Schemas (Router/CAF/NATS)

## RU (задание)
Сформировать и зафиксировать контракт взаимодействия Router ↔ CAF ↔ NATS/JetStream для CP2:
- taxonomy NATS subjects (v1/v2), naming rules,
- headers contract (trace, tenant, version, msg_id),
- payload schema versioning (proto/json), compatibility rules,
- correlation keys (assignment_id/request_id) и idempotency keys,
- DLQ contract (subjects, envelope, replay fields),
- contract tests (минимальный набор проверок на совместимость).

Главный артефакт: `docs/contracts/CP2_CONTRACTS.md` + минимальные contract checks в CI.

## EN (task)
Define and freeze CP2 interaction contracts for Router ↔ CAF ↔ NATS/JetStream:
- NATS subject taxonomy (v1/v2) and naming rules,
- headers contract (trace, tenant, version, msg_id),
- payload schema versioning and compatibility,
- correlation + idempotency keys,
- DLQ contract (subjects/envelope/replay),
- minimal contract tests / CI checks.

Primary artifact: `docs/contracts/CP2_CONTRACTS.md` + minimal CI checks.
