# Chaos Failure Modes Catalog

This document catalogs failure modes discovered and validated through chaos engineering tests (T-CHAOS-01).

## Overview

Chaos testing systematically validates Router resilience by inducing controlled failures and measuring recovery behavior. This catalog documents observed failure modes, their symptoms, recovery patterns, and operational recommendations.

## Failure Mode Categories

### 1. NATS Process Failures

#### FM-NATS-001: NATS Process Crash (SIGKILL)
**Description**: NATS server process is killed abruptly without graceful shutdown.

**Symptoms**:
- Router logs: `NATS connection lost`, `error_code: NATS_CONNECTION_LOST`
- Metrics: `router_nats_connection_lost_total` increments
- Metrics: `router_nats_connection_status` = 0 (disconnected)
- gRPC requests may fail with `UNAVAILABLE` if fail-open disabled

**Recovery Pattern**:
1. Router detects connection loss within 1-2 seconds
2. Router enters reconnection loop with exponential backoff
3. When NATS restarts, Router reconnects automatically
4. JetStream consumers resume from last ACK (no message loss)
5. Backpressure state is recalculated based on current queue depth

**Recovery SLO**: < 30 seconds to operational state (measured)

**Operational Recommendations**:
- Monitor `router_nats_connection_status` metric
- Alert on `router_nats_connection_lost_total` spikes
- Ensure NATS process supervisor is configured (systemd, k8s, etc.)

#### FM-NATS-002: NATS Graceful Shutdown (SIGTERM)
**Description**: NATS server is stopped gracefully (e.g., `systemctl stop nats`).

**Symptoms**:
- Similar to FM-NATS-001 but connection close is clean
- Router logs: `NATS connection closed gracefully`

**Recovery Pattern**:
- Same as FM-NATS-001
- May have slightly faster recovery due to clean close

**Recovery SLO**: < 30 seconds

---

### 2. Router Process Failures

#### FM-ROUTER-001: Router Supervisor Crash
**Description**: Router supervisor process is killed, triggering application restart.

**Symptoms**:
- All Router processes terminate
- Supervisor restarts Router application automatically
- Temporary service unavailability (gRPC port unresponsive)

**Recovery Pattern**:
1. Application supervisor detects supervisor crash
2. Router application is restarted from clean state
3. Router reconnects to NATS
4. JetStream consumers are recreated and resume from last ACK
5. Circuit breaker states are reset to `closed` (conservative restart)
6. Backpressure state is recalculated

**Recovery SLO**: < 30 seconds to operational state

**State Preservation**:
- ✅ JetStream consumer offsets (persisted in NATS)
- ❌ Circuit breaker states (ephemeral, reset on restart)
- ❌ In-memory backpressure metrics (recalculated)
- ❌ Rate limiter windows (reset)

**Operational Recommendations**:
- Circuit breakers reset to `closed` on restart (fail-safe behavior)
- Consider persisting Circuit Breaker state if frequent restarts expected
- Monitor `beamline_router_sup` process health

#### FM-ROUTER-002: Individual Worker Crash
**Description**: A single Router worker process crashes (e.g., `router_decide_consumer`).

**Symptoms**:
- Supervisor restarts the crashed worker
- Minimal user impact (other workers continue processing)
- Logs: `gen_server terminated`, `child restarted`

**Recovery Pattern**:
1. Supervisor detects worker crash
2. Worker is restarted immediately
3. Worker re-subscribes to NATS subject
4. Processing resumes within seconds

**Recovery SLO**: < 5 seconds (sub-process restart)

---

### 3. JetStream Queue Failures

#### FM-JETSTREAM-001: Consumer Lag / Slow ACKs
**Description**: Router consumer cannot keep up with message rate, causing `pending_messages` to grow.

**Symptoms**:
- Metrics: `pending_messages` increases
- Metrics: `latency_p95_ms` increases
- Backpressure activates: `backpressure_active` or `backpressure_warning`
- Gateway receives HTTP 429 / gRPC RESOURCE_EXHAUSTED

**Recovery Pattern**:
1. Backpressure mechanism throttles incoming traffic
2. Consumer processes backlog at reduced rate
3. Gateway honors `Retry-After` header
4. Queue depth decreases over time
5. Backpressure deactivates when below threshold

**Recovery SLO**: Variable (depends on backlog size and throughput)

**Operational Recommendations**:
- Scale Router horizontally to increase consumer throughput
- Monitor `pending_messages` trend
- Configure backpressure thresholds appropriately
- Ensure Gateway implements backpressure protocol correctly

#### FM-JETSTREAM-002: Message Redelivery After Timeout
**Description**: Message is not ACKed within timeout, triggering redelivery.

**Symptoms**:
- Metrics: `router_jetstream_redelivery_total` increments
- Same message processed multiple times
- Idempotency prevents duplicate side effects (if implemented)

**Recovery Pattern**:
1. Message is redelivered to consumer
2. Idempotency check detects duplicate (if enabled)
3. Message is ACKed without reprocessing

**Operational Recommendations**:
- Implement idempotency for all message handlers
- Use `idempotency_key` from Core Message Fields
- Monitor redelivery rate for anomalies

---

### 4. Network Failures

#### FM-NETWORK-001: Network Partition
**Description**: Network connectivity between Router and NATS is lost.

**Symptoms**:
- Same as FM-NATS-001 from Router perspective
- NATS server remains running but unreachable

**Recovery Pattern**:
- Same as FM-NATS-001 (reconnection loop)
- Recovery occurs when network is restored

**Recovery SLO**: < 30 seconds after network restoration

**Testing**: See `router_network_partition_*_SUITE` tests

---

## Failure Mode Summary Table

| ID | Failure Mode | Recovery SLO | State Loss | Auto-Recovery |
|----|--------------|--------------|------------|---------------|
| FM-NATS-001 | NATS crash (SIGKILL) | < 30s | None | ✅ |
| FM-NATS-002 | NATS graceful stop | < 30s | None | ✅ |
| FM-ROUTER-001 | Supervisor crash | < 30s | Circuit Breaker | ✅ |
| FM-ROUTER-002 | Worker crash | < 5s | None | ✅ |
| FM-JETSTREAM-001 | Consumer lag | Variable | None | ✅ (backpressure) |
| FM-JETSTREAM-002 | Message redelivery | N/A | None | ✅ (idempotency) |
| FM-NETWORK-001 | Network partition | < 30s | None | ✅ |

## Related Documentation

- [Recovery Runbook](operations/RECOVERY_RUNBOOK.md) - Operational recovery procedures
- [Rollback Runbook](operations/ROLLBACK_RUNBOOK.md) - Version rollback procedures
- [API Contracts](API_CONTRACTS.md) - Backpressure protocol specification

## Testing

All failure modes are validated in:
- `test/router_chaos_controlled_SUITE.erl` - Controlled chaos tests
- `scripts/chaos_test.sh` - Chaos test orchestration

Run chaos tests:
```bash
./scripts/chaos_test.sh
```
