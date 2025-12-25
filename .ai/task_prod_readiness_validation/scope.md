## In Scope

- Execute heavy CT tier and capture results
- Establish perf baseline (RPS / p50/p95/p99 latency, error rate)
- Define initial SLOs based on measured baseline
- Create rollback script/procedure (staging-grade)
- Enable TLS for NATS in staging (or reference config if already present)
- Fire-test alerts in staging (prove signal path end-to-end)

## Out of Scope

- Feature development
- Refactoring tests
- Full production launch / real traffic
- Cost optimization and autoscaling strategy (later phase)

## Constraints

- Evidence-based only: every "done" must have a command/result or artifact link
- No speculative SLOs: must be derived from a measured baseline
