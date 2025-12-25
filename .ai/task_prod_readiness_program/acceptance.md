# Acceptance — T-PROD-01

## Done criteria
1. `docs/OPERATIONAL_RUNBOOK.md` (или канон) содержит секцию Production Readiness Gate: SLO/SLA + пороги.
2. Есть perf baseline отчёт с p50/p95/p99, throughput, error rate, фиксированные параметры нагрузки.
3. Определён набор обязательных validation прогонов:
   * heavy (PASS)
   * short chaos (PASS или documented blockers)
   * short soak (PASS или documented blockers)
4. Для каждого прогона есть команда запуска + артефакт логов в `_artifacts/`.
5. Определены “go/no-go” правила: что блокирует production.
