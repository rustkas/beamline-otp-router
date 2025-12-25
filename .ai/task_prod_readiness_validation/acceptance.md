## Acceptance Criteria

1. Heavy test tier executed:
   - Command: `ROUTER_TEST_LEVEL=heavy rebar3 ct`
   - Result recorded (pass/fail/skips, key suite notes)

2. Performance baseline established:
   - RPS and latency distribution captured (p50/p95/p99)
   - Error rate during benchmark recorded
   - Environment details recorded (host, CPU, NATS mode, config)

3. SLOs defined:
   - Availability target
   - p99 latency target
   - Error rate budget
   - Each SLO references the baseline measurement rationale

4. Rollback procedure exists:
   - Script or documented runbook steps
   - Verified on staging (dry-run acceptable if it proves commands are valid)

5. NATS TLS enabled (staging):
   - Config evidence and connection verification
   - Non-TLS connection is rejected (or explicitly scoped if cannot be enforced)

6. Alerting fire-tested:
   - At least one alert condition triggered intentionally
   - Alert delivered to expected channel (pager/Slack/etc.) and recorded
