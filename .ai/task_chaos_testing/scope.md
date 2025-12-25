# Scope: T-CHAOS-01

## In Scope

### 1. NATS Process Chaos
- Kill NATS server process during active message flow
- Verify Router reconnection within 30 seconds
- Verify no message loss (JetStream persistence)
- Verify metrics reflect disconnection/reconnection

### 2. Router Process Chaos
- Kill Router process (graceful and ungraceful)
- Verify clean restart without corrupted state
- Verify NATS consumers resume from last ACK
- Verify backpressure state is preserved/restored

### 3. JetStream Lag & Redelivery
- Induce JetStream consumer lag (pause ACKs)
- Trigger message redelivery (simulate timeout)
- Verify idempotency (duplicate detection)
- Verify backpressure activates under high lag

### 4. Recovery SLO Verification
- Measure time-to-green for each chaos scenario
- Target SLO: < 30 seconds to operational state
- "Operational" = gRPC port responsive + NATS connected + backpressure inactive
- Record SLO violations for analysis

### 5. Test Automation
- Automated test suite in Common Test
- Chaos orchestration script for CI/CD
- Failure injection using existing `router_nats_fault_injection`
- Evidence collection (logs, metrics, timestamps)

## Out of Scope

- **Distributed cluster chaos** (multi-node failures) — defer to T-CHAOS-02
- **Load testing under chaos** (combined with high throughput) — defer to T-CHAOS-03
- **Network partition simulation** (already covered in existing `router_network_partition_*_SUITE`)
- **Disk/memory exhaustion** — infrastructure-level, not application-level
- **Security chaos** (malformed messages, auth failures) — covered in T-SEC-02

## Dependencies

- Existing scripts: `scripts/nats_start.sh`, `scripts/nats_stop.sh`, `scripts/nats_status.sh`
- Existing module: `src/router_nats_fault_injection.erl`
- Existing helpers: `test/router_network_partition_helper.erl`, `test/router_triple_fault_helper.erl`
- NATS CLI for JetStream inspection
