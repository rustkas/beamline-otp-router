# Recovery Runbook (T-OPS-04)

This runbook describes recovery procedures for various failure scenarios in the Beamline Router infrastructure.

## 1. NATS Recovery

### Symptoms
- Router logs: `NATS connection lost`, `NATS_CONNECTION_LOST`
- Metrics: `router_nats_connection_status` is 0
- Command `scripts/nats_status.sh` shows `Not Running` or `Health check failed`

### Recovery Procedures

#### A. NATS Process Down
If the NATS server itself has crashed:
```bash
# Check status
./scripts/nats_status.sh

# Restart NATS
./scripts/nats_stop.sh
./scripts/nats_start.sh

# Verify
./scripts/nats_status.sh
```

#### B. Router-NATS Connection Flapping
If Router cannot maintain a stable connection:
1.  Verify network stability between Router and NATS.
2.  Check for TLS certificate expiration (if TLS enabled).
3.  Check NATS server logs for `Authentication error` or `Slow consumer` errors.
4.  Restart Router nodes one by one to refresh connections.

#### C. JetStream Queue Overflow
If a consumer (e.g., `router_decide_consumer`) is overwhelmed and `pending_messages` continues to grow:
1.  **Monitor**: `curl -s http://localhost:8222/jsz?consumers=true`
2.  **Scale Up**: Add more Router nodes to increase processing capacity.
3.  **Backpressure**: Verify that the Gateway is honoring the Backpressure Protocol (see `API_CONTRACTS.md`).
4.  **Purge (Extreme)**: If the queue is poisoned and must be cleared:
    ```bash
    # Use nats CLI to purge a stream (DANGER: Data loss)
    nats stream purge <STREAM_NAME>
    ```

## 2. Circuit Breaker Management

### Symptoms
- Router logs: `Circuit breaker is open, failing fast`
- Metrics: `router_circuit_breaker_state` gauge shows 1.0 (Open)
- Requests for a specific provider are immediately failing with `circuit_open` error.

### Management Procedures

#### A. Interpreting Alerts
A Circuit Breaker opens when a provider (e.g., OpenAI) is consistently failing or exceeding latency thresholds.
1.  Check provider status page (e.g., status.openai.com).
2.  Check Router metrics for `router_provider_latency_p95_ms` and `router_provider_errors_total`.

#### B. Manual Overrides
If you believe a provider has recovered but the Circuit Breaker is still open (or in `half_open` flapping):

**Force Recovery (Close Circuit)**:
```erlang
# From Router console (rebar3 remote_console)
router_circuit_breaker:force_recovery(<<"tenant_id">>, <<"provider_id">>).
```

**Force to Half-Open (Test Recovery)**:
```erlang
router_circuit_breaker:force_recovery_to_half_open(<<"tenant_id">>, <<"provider_id">>).
```

**View Status**:
```erlang
{ok, Status} = router_circuit_breaker:get_status(<<"tenant_id">>, <<"provider_id">>).
```

## 3. Clean State Restart

### Scenario
Use this procedure when the system is in an inconsistent state (e.g., after a major NATS cluster failure or database corruption).

### Procedure
1.  **Stop Router**: `scripts/router_stop.sh` (or `systemctl stop beamline-router`)
2.  **Stop NATS**: `scripts/nats_stop.sh`
3.  **Clear NATS Data** (Optional/DANGER): `rm -rf data/jetstream` (only if data re-population is possible)
4.  **Start NATS**: `scripts/nats_start.sh`
5.  **Initialize NATS Streams**: `scripts/setup_nats_streams.sh` (if applicable)
6.  **Start Router**: `scripts/nats_start.sh`
7.  **Verify Port 9000 & 9001**:
    ```bash
    nc -zv localhost 9000
    nc -zv localhost 9001
    ```

## 4. Troubleshooting Checklist

| Problem | Potential Cause | Verification |
|---------|-----------------|--------------|
| High Latency | Backpressure | `curl -s http://localhost:9001/metrics | grep latency` |
| 429 Errors | Rate Limiting | Check `router_rate_limiter` logs |
| 503 Errors | Circuit Breaker | Check `router_circuit_breaker_state` metric |
| Stuck Queues | Consumer Crash | Check `router_nats_subscriber` process status |

## Related Runbooks
- [ROLLBACK_RUNBOOK.md](ROLLBACK_RUNBOOK.md) - For version rollbacks.
