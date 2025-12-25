# Acceptance Criteria: T-CHAOS-01

## Done When

### 1. Test Suite Exists
- [ ] `test/router_chaos_controlled_SUITE.erl` created and compiles
- [ ] Test cases cover all 4 chaos scenarios (NATS kill, Router kill, JetStream lag, Recovery SLO)
- [ ] Tests run in CI without manual intervention

### 2. NATS Kill Recovery Validated
- [ ] Test kills NATS process during active message flow
- [ ] Router logs show `NATS connection lost` followed by `NATS connection restored`
- [ ] Recovery completes within 30 seconds (SLO)
- [ ] No messages lost (verify via JetStream consumer info)
- [ ] Metrics show disconnection spike then recovery

### 3. Router Kill Recovery Validated
- [ ] Test kills Router process (via `kill -9` or supervisor:terminate)
- [ ] Router restarts and reconnects to NATS
- [ ] JetStream consumers resume from last ACK (no duplicate processing)
- [ ] Circuit breaker states are restored correctly
- [ ] Recovery completes within 30 seconds (SLO)

### 4. JetStream Lag Recovery Validated
- [ ] Test induces consumer lag (pause ACKs for 60s)
- [ ] Backpressure activates (`backpressure_active` state)
- [ ] Messages are redelivered after timeout
- [ ] Idempotency prevents duplicate side effects
- [ ] Lag clears and backpressure deactivates

### 5. Recovery SLO Measured
- [ ] SLO metric: `time_to_operational_state_seconds` recorded for each scenario
- [ ] 95th percentile < 30 seconds
- [ ] Violations logged with detailed context
- [ ] SLO dashboard created (saved as artifact)

### 6. Documentation Complete
- [ ] Chaos test results documented in `_artifacts/chaos_test_YYYYMMDD_HHMMSS.log`
- [ ] Failure modes catalog created: `docs/CHAOS_FAILURE_MODES.md`
- [ ] Recovery patterns documented in existing `docs/operations/RECOVERY_RUNBOOK.md`
