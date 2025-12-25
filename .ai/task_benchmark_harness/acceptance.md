# Acceptance — T-PERF-01

## Done criteria
1. Скрипт `scripts/bench_router.sh` существует и доступен для исполнения.
2. Скрипт успешно завершает прогон против реального NATS.
3. В `_artifacts/` появляется файл `perf_baseline.json` со структурой:
   ```json
   {
     "sequential": { "rps": 123, "p95_ms": 10 },
     "concurrent": { "rps": 456, "p95_ms": 25 },
     "resource": { "memory_mb": 50, "ets_size": 1000 }
   }
   ```
4. progress.md содержит ссылку на финальный лог прогона.
