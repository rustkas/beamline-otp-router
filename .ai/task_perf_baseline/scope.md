## In Scope
- Прогон Router с реальным NATS (через T-INFRA-01)
- Измерение:
  - Throughput (req/sec)
  - Latency (p50 / p95 / p99)
  - Memory usage
  - Errors count
  - Backpressure counters
- Генерация машиночитаемых артефактов (JSON)
- Человекочитаемый summary (Markdown)

## Out of Scope
- Оптимизация производительности
- Горизонтальное масштабирование
- TLS overhead (это T-SEC-01)
- ETS size (будет в CP2)
- Process count (шумно на CI)
