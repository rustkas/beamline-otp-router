## Acceptance Criteria

1. A clear definition exists for:
   - p50 latency
   - p95 latency
   - p99 latency
   - throughput (if applicable)

2. Baselines are stored in a versioned artifact, including:
   - environment description
   - test suite name
   - measurement parameters
   - baseline values

3. Performance suites in Batch #3:
   - compare observed metrics against stored baselines,
   - allow explicit tolerance ranges,
   - fail deterministically on regression.

4. A documented process exists for:
   - updating baselines intentionally,
   - reviewing baseline changes,
   - distinguishing noise from regression.

5. progress.md records at least one successful baseline capture run.
