# T-PERF-01 — Performance Baseline

## Problem
Отсутствует автоматизированный способ получения и фиксации performance baseline (RPS, Latency, Error Rate). Без этого невозможно установить SLO (Service Level Objectives) и отслеживать регрессии производительности при изменениях кода.

## Goal
Создать повторяемый benchmark harness и зафиксировать baseline метрики Router:
- Sequential Throughput & Latency (P50, P95, P99)
- Concurrent Throughput & Latency (P50, P95, P99)
- Memory Usage under load
- ETS table growth baseline

## Outcome
1. Скрипт `scripts/bench_router.sh`, который:
   - Автоматически запускает NATS.
   - Прогоняет `router_performance_benchmark_SUITE`.
   - Парсит результаты и генерирует JSON-отчет в `_artifacts/`.
2. Первый зафиксированный baseline отчет `_artifacts/perf_baseline_<timestamp>.json`.
3. Обновленная документация с описанием того, как проводить замеры.
