## Execution Plan

1. Record baseline environment details used for `batch3_heavy_iso.json`.
2. Re-run `router_performance_regression_SUITE` in isolation with the same env:
   - CPU pinning and scheduler settings
   - same baseline JSON
3. Capture perf observations and logs from the isolated run.
4. Compare throughput metrics and compute degradation.
5. Conclude infra noise vs code regression and update progress.md.
