1. Поднять NATS через scripts/nats_start.sh
2. Запустить controlled load (последовательно / конкурентно)
3. Собрать метрики:
   - время ответа (p95/p99)
   - throughput (rps)
   - память (Erlang total)
   - errors count
   - backpressure counters
4. Сохранить raw metrics (JSON в _artifacts/)
5. Сформировать Markdown summary
6. Остановить NATS
7. Создать perf_gate.sh для CI integration
