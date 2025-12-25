# Plan — T-PERF-01

1. Исследование `router_performance_benchmark_SUITE.erl` на предмет совместимости с реальным NATS.
2. Создание `scripts/bench_router.sh`:
   - Логика start/stop NATS.
   - Запуск `rebar3 ct`.
   - Парсинг `ct_logs` регулярными выражениями (awk/grep).
   - Генерация JSON.
3. Первый запуск и фиксация baseline.
4. Обновление progress.md и финализация задачи.
