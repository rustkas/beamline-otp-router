## Execution Plan

1. Run heavy CT tier
   - capture summary
   - list skipped tests (if any) with reason

2. Run performance benchmark
   - define benchmark command/tool
   - capture baseline: RPS, latency p50/p95/p99, error rate
   - record config + environment metadata

3. Define initial SLOs from baseline
   - choose conservative targets
   - document rationale and future tightening rule

4. Rollback
   - create rollback script or staging runbook section
   - execute dry-run or staging rollback validation

5. Enable NATS TLS
   - update NATS config (staging)
   - validate client connection over TLS
   - ensure plain-text is blocked or documented

6. Fire-test alerts
   - pick 1–2 high-signal alerts
   - trigger condition safely in staging
   - confirm delivery and recovery
