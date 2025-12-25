## Execution Plan

1. Identify all performance-relevant suites in Batch #3.
2. Decide baseline metrics per suite (p50/p95/p99/throughput).
3. Define a reproducible measurement protocol:
   - warm-up
   - sustained duration
   - sampling method
4. Design baseline storage format (file-based, JSON/YAML).
5. Capture initial baselines from a clean heavy run.
6. Integrate baseline comparison into test assertions.
7. Document variance and update policy.
8. Record results in progress.md.
