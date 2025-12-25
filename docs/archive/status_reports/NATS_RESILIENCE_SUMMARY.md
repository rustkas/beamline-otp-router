# NATS Connection Resilience - Summary

## Quick Reference

**Main Documentation**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md`

**Test Documentation**:
- `apps/otp/router/test/FAULT_INJECTION_TEST_CRITERIA.md` - Test criteria
- `apps/otp/router/test/RUN_TESTS.md` - How to run tests
- `apps/otp/router/test/router_nats_connection_failure_SUITE.md` - Connection failure tests
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.md` - Fault injection tests

**Metrics & Alerts**: `apps/otp/router/docs/NATS_METRICS_ALERTS.md`

## Public Contract

### Metrics (Prometheus)

All metrics follow Prometheus naming conventions:
- **Counters**: `router_nats_*_total`
- **Gauges**: `router_nats_*` (no suffix)

**Key Metrics**:
- `router_nats_connection_status` (gauge) - 0.0/0.5/1.0
- `router_nats_connection_*_total` (counters) - established/lost/restored/failures
- `router_nats_publish_total` / `router_nats_publish_failures_total` (counters)
- `router_nats_pending_operations_count` (gauge)

**See**: `NATS_CONNECTION_RESILIENCE.md` for complete metrics list.

### Logs (Structured JSON)

All logs include `error_code` and `error_tag`:
- `NATS_CONNECTION_*` - Connection events
- `NATS_PUBLISH_*` - Publish events
- `NATS_ACK_*` - ACK events
- `NATS_QUEUE_*` - Queue events
- `NATS_RETRY_*` - Retry events

**See**: `NATS_CONNECTION_RESILIENCE.md` for complete log format.

## Test Suites

1. **router_nats_connection_failure_SUITE** (22 tests) - Connection failure scenarios
2. **router_jetstream_fault_injection_SUITE** (15 tests) - Fault injection scenarios
3. **router_nats_integration_SUITE** (10 tests) - Integration with router components
4. **router_nats_performance_SUITE** (7 tests) - Queue and backoff performance

**Total**: 54 tests

**Run All**:
```bash
cd apps/otp/router
rebar3 ct --suite test/router_nats_connection_failure_SUITE test/router_jetstream_fault_injection_SUITE test/router_nats_integration_SUITE test/router_nats_performance_SUITE
```

## Configuration

### Production Recommendations

```erlang
{nats_reconnect_attempts, 10},
{nats_reconnect_delay_ms, 1000},
{nats_max_reconnect_delay_ms, 30000},
{nats_fail_open_mode, false},  %% Ensure message delivery
{nats_max_pending_operations, 5000},  %% High traffic support
```

## Monitoring

### Critical Alerts

- `RouterNATSConnectionDown` - Connection status = 0 for 1m
- `RouterNATSReconnectionExhausted` - Reconnection attempts exhausted

### Warning Alerts

- `RouterNATSPublishFailureRate` - Publish failure rate > 10%
- `RouterNATSPendingQueueFull` - Pending operations >= 1000

**See**: `NATS_METRICS_ALERTS.md` for complete alerting rules.

## How to Use

### Local Testing

```bash
# Run fault injection tests
cd apps/otp/router
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE

# Enable fault injection manually
erl -pa _build/default/lib/*/ebin
1> router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}).
2> router_nats:publish(~"test", ~"payload").
```

### Reading Metrics During Failures

```bash
# Check connection status
curl http://localhost:9001/metrics | grep router_nats_connection_status

# Check error counters
curl http://localhost:9001/metrics | grep router_nats.*failures_total

# Check queue status
curl http://localhost:9001/metrics | grep router_nats_pending_operations_count
```

### Reading Logs During Failures

```bash
# Filter by error code
cat router_2025-11-30.jsonl | jq 'select(.error_code == "NATS_CONNECTION_LOST")'

# Filter by error tag
cat router_2025-11-30.jsonl | jq 'select(.error_tag == "nats_connection_failure")'

# View recent connection events
cat router_2025-11-30.jsonl | jq 'select(.error_code | startswith("NATS_CONNECTION"))' | tail -20
```

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete documentation
- `apps/otp/router/docs/NATS_METRICS_ALERTS.md` - Metrics and alerts
- `apps/otp/router/test/RUN_TESTS.md` - Test execution guide

