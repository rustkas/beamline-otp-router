# Scope — T-PROD-01

## In scope
- SLO/SLA: availability, p99 latency, error rate, backlog bounds
- Perf baseline: сценарии нагрузки + метрики + отчёт
- Chaos/fault validation: перечень обязательных сценариев и доказательства прогонов
- Thresholds: численные пороги (pass/fail) для CI/nightly/staging
- Rollback/upgrade runbook (минимально достаточный)

## Out of scope
- Реализация Gateway/CAF сервисов
- Реальный production rollout в клиентский кластер
- Долгие soak (>24h), если нет инфраструктуры (план + короткие прогоны допускаются)

## Constraints
- “Source of truth” = `.ai/task_prod_readiness_program/progress.md`
- Никаких “считаю готовым” без артефактов (логи, отчёты, команды)
