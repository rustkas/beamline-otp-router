# NATS Connection Resilience

## Overview

This document describes router_nats connection resilience features, including fault injection infrastructure, monitoring, and operational procedures.

## Public Contract

### Metrics (Prometheus)

**Public metrics** (exposed via `/metrics` endpoint, port 9001):

#### Connection Metrics

- `router_nats_connection_status` (gauge) - Connection status:
  - `0.0` = disconnected
  - `0.5` = reconnecting
  - `1.0` = connected
- `router_nats_connection_established_total` (counter) - Total successful connections
- `router_nats_connection_lost_total` (counter) - Total connection losses
- `router_nats_connection_restored_total` (counter) - Total connection restorations
- `router_nats_connection_failures_total` (counter) - Total connection failures

#### Reconnection Metrics

- `router_nats_reconnect_attempts_total` (counter) - Total reconnection attempts
- `router_nats_reconnect_failures_total` (counter) - Total reconnection failures
- `router_nats_reconnection_exhausted_total` (counter) - Total times reconnection attempts exhausted

#### Operation Metrics

- `router_nats_publish_total` (counter) - Total publish operations
- `router_nats_publish_failures_total` (counter) - Total publish failures
- `router_nats_publish_with_ack_total` (counter) - Total publish_with_ack operations
- `router_nats_publish_with_ack_failures_total` (counter) - Total publish_with_ack failures
- `router_nats_ack_total` (counter) - Total ACK operations
- `router_nats_ack_failures_total` (counter) - Total ACK failures
- `router_nats_nak_total` (counter) - Total NAK operations
- `router_nats_nak_failures_total` (counter) - Total NAK failures
- `router_nats_subscribe_total` (counter) - Total subscribe operations
- `router_nats_subscribe_failures_total` (counter) - Total subscribe failures

#### Queue Metrics

- `router_nats_pending_operations_count` (gauge) - Current number of pending operations in queue
- `router_nats_pending_operations_retry` (counter) - Total pending operations retry attempts
- `router_nats_pending_operations_retry_success` (counter) - Total successful retries
- `router_nats_pending_operations_retry_failed` (counter) - Total failed retries
- `router_nats_pending_operations_dropped_total` (counter) - Total operations dropped due to queue full

**Naming Convention**: All metrics follow Prometheus naming:
- Counters: `router_nats_*_total`
- Gauges: `router_nats_*` (no suffix)

### Logs (Structured JSON)

**Public log entries** (structured JSON with error codes):

#### Connection Events

- **NATS_CONNECTION_ESTABLISHED** (INFO):
  ```json
  {
    "level": "INFO",
    "message": "NATS connection established",
    "error_code": "NATS_CONNECTION_ESTABLISHED"
  }
  ```

- **NATS_CONNECTION_LOST** (ERROR):
  ```json
  {
    "level": "ERROR",
    "message": "NATS connection lost",
    "error_code": "NATS_CONNECTION_LOST",
    "error_tag": "nats_connection_failure",
    "reason": "connection_closed",
    "reconnect_attempts": 0
  }
  ```

- **NATS_CONNECTION_RESTORED** (INFO):
  ```json
  {
    "level": "INFO",
    "message": "NATS connection restored",
    "error_code": "NATS_CONNECTION_RESTORED",
    "error_tag": "nats_connection_recovered",
    "reconnect_attempts": 2
  }
  ```

#### Reconnection Events

- **NATS_RECONNECT_ATTEMPT** (INFO):
  ```json
  {
    "level": "INFO",
    "message": "Attempting NATS reconnection",
    "error_code": "NATS_RECONNECT_ATTEMPT",
    "error_tag": "nats_reconnecting",
    "attempt": 1,
    "max_attempts": 5
  }
  ```

#### Operation Events

- **NATS_PUBLISH_ERROR** (WARN):
  ```json
  {
    "level": "WARN",
    "message": "NATS publish failed",
    "error_code": "NATS_PUBLISH_ERROR",
    "subject": "beamline.router.v1.decide",
    "reason": "nats_unavailable"
  }
  ```

- **NATS_PUBLISH_QUEUED** (WARN):
  ```json
  {
    "level": "WARN",
    "message": "NATS publish queued: not connected",
    "error_code": "NATS_PUBLISH_QUEUED",
    "subject": "beamline.router.v1.decide",
    "connection_state": "disconnected"
  }
  ```

- **NATS_QUEUE_FULL** (WARN):
  ```json
  {
    "level": "WARN",
    "message": "NATS pending operations queue full, dropping oldest",
    "error_code": "NATS_QUEUE_FULL",
    "queue_size": 1000,
    "max_size": 1000
  }
  ```

- **NATS_RETRY_PENDING** (INFO):
  ```json
  {
    "level": "INFO",
    "message": "Retrying pending NATS operations",
    "error_code": "NATS_RETRY_PENDING",
    "pending_count": 5
  }
  ```

- **NATS_RETRY_COMPLETE** (INFO):
  ```json
  {
    "level": "INFO",
    "message": "Pending NATS operations retry complete",
    "error_code": "NATS_RETRY_COMPLETE",
    "success_count": 4,
    "failed_count": 1
  }
  ```

**Naming Convention**: All error codes follow pattern `NATS_<OPERATION>_<STATUS>`:
- `NATS_CONNECTION_*` - Connection events
- `NATS_PUBLISH_*` - Publish events
- `NATS_ACK_*` - ACK events
- `NATS_QUEUE_*` - Queue events
- `NATS_RETRY_*` - Retry events

### Internal Implementation Details

**Not part of public contract** (may change without notice):
- Internal state machine transitions
- Reconnect delay calculation formulas
- Queue implementation details
- Fault injection module internals

## Configuration

### Application Environment Variables

```erlang
%% Reconnection settings
{nats_reconnect_attempts, 5},              %% Max reconnection attempts
{nats_reconnect_delay_ms, 500},           %% Base reconnect delay (exponential backoff)
{nats_max_reconnect_delay_ms, 2000},       %% Max reconnect delay

%% Fail-open mode
{nats_fail_open_mode, false},              %% Enable fail-open mode (default: false)

%% Queue settings
{nats_max_pending_operations, 1000},       %% Max pending operations queue size
```

### Recommended Production Settings

```erlang
%% Production configuration
{nats_reconnect_attempts, 10},             %% More attempts for production
{nats_reconnect_delay_ms, 1000},           %% 1 second base delay
{nats_max_reconnect_delay_ms, 30000},      %% 30 seconds max delay
{nats_fail_open_mode, false},              %% Disable fail-open (ensure message delivery)
{nats_max_pending_operations, 5000},       %% Larger queue for high traffic
```

## Monitoring

### Prometheus Alerts

#### Critical Alerts

```yaml
# NATS connection down
- alert: RouterNATSConnectionDown
  expr: router_nats_connection_status == 0
  for: 1m
  labels:
    severity: critical
    component: router
  annotations:
    summary: "Router NATS connection is down"
    description: "Router has been disconnected from NATS for {{ $for }}"

# Reconnection exhausted
- alert: RouterNATSReconnectionExhausted
  expr: rate(router_nats_reconnection_exhausted_total[5m]) > 0
  for: 1m
  labels:
    severity: critical
    component: router
  annotations:
    summary: "Router NATS reconnection attempts exhausted"
    description: "Router has exhausted NATS reconnection attempts"
```

#### Warning Alerts

```yaml
# High publish failure rate
- alert: RouterNATSPublishFailureRate
  expr: rate(router_nats_publish_failures_total[5m]) / rate(router_nats_publish_total[5m]) > 0.1
  for: 5m
  labels:
    severity: warning
    component: router
  annotations:
    summary: "Router NATS publish failure rate is high"
    description: "Router NATS publish failure rate is {{ $value | humanizePercentage }}"

# Pending operations queue full
- alert: RouterNATSPendingQueueFull
  expr: router_nats_pending_operations_count >= 1000
  for: 2m
  labels:
    severity: warning
    component: router
  annotations:
    summary: "Router NATS pending operations queue is full"
    description: "Router NATS pending operations queue is at {{ $value }} (max: 1000)"
```

### Grafana Dashboard Queries

#### Connection Status

```promql
# Connection status gauge
router_nats_connection_status

# Connection events over time
rate(router_nats_connection_established_total[5m])
rate(router_nats_connection_lost_total[5m])
rate(router_nats_connection_restored_total[5m])
```

#### Operation Success Rate

```promql
# Publish success rate
sum(rate(router_nats_publish_total[5m])) - sum(rate(router_nats_publish_failures_total[5m]))
/
sum(rate(router_nats_publish_total[5m]))

# ACK success rate
sum(rate(router_nats_ack_total[5m])) - sum(rate(router_nats_ack_failures_total[5m]))
/
sum(rate(router_nats_ack_total[5m]))
```

#### Queue Metrics

```promql
# Pending operations count
router_nats_pending_operations_count

# Retry success rate
sum(rate(router_nats_pending_operations_retry_success[5m]))
/
sum(rate(router_nats_pending_operations_retry[5m]))
```

## Fault Injection

### Enabling Fault Injection (Testing Only)

Fault injection is available for testing via `router_nats_fault_injection` module:

```erlang
%% Enable fault injection
router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),

%% Disable fault injection
router_nats_fault_injection:disable_fault(publish),

%% Clear all faults
router_nats_fault_injection:clear_all_faults(),
```

**Warning**: Fault injection should **only** be used in test environments, never in production.

### Supported Fault Types

- `{error, Reason}` - Return error (e.g., `{error, connection_refused}`)
- `timeout` - Simulate operation timeout
- `close_connection` - Close connection during operation
- `{delay, Milliseconds}` - Add delay to operation

### Supported Operations

- `connect` - Connection attempts
- `publish` - Publish operations
- `publish_with_ack` - Publish with acknowledgment
- `ack` - Message acknowledgment
- `subscribe` - JetStream subscriptions

## How to Read Metrics/Logs During NATS Failures

### Step 1: Check Connection Status

```bash
# Query connection status gauge
curl http://localhost:9001/metrics | grep router_nats_connection_status

# Expected output:
# router_nats_connection_status{state="disconnected"} 0.0
# router_nats_connection_status{state="reconnecting"} 0.5
# router_nats_connection_status{state="connected"} 1.0
```

### Step 2: Check Error Counters

```bash
# Query error counters
curl http://localhost:9001/metrics | grep router_nats.*failures_total

# Expected output:
# router_nats_connection_lost_total 5
# router_nats_publish_failures_total 10
# router_nats_reconnect_failures_total 3
```

### Step 3: Check Logs

```bash
# Filter logs by error code
cat router_2025-11-30.jsonl | jq 'select(.error_code == "NATS_CONNECTION_LOST")'

# Filter logs by error tag
cat router_2025-11-30.jsonl | jq 'select(.error_tag == "nats_connection_failure")'

# View recent connection events
cat router_2025-11-30.jsonl | jq 'select(.error_code | startswith("NATS_CONNECTION"))' | tail -20
```

### Step 4: Check Queue Status

```bash
# Query pending operations count
curl http://localhost:9001/metrics | grep router_nats_pending_operations_count

# Expected output:
# router_nats_pending_operations_count 42
```

### Step 5: Monitor Recovery

```bash
# Watch connection status change
watch -n 1 'curl -s http://localhost:9001/metrics | grep router_nats_connection_status'

# Watch logs for recovery events
tail -f router_2025-11-30.jsonl | jq 'select(.error_code == "NATS_CONNECTION_RESTORED")'
```

## Local Testing

### Running Fault Injection Tests

```bash
cd apps/otp/router

# Run all fault injection tests
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE

# Run connection failure tests
rebar3 ct --suite test/router_nats_connection_failure_SUITE

# Run specific test
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE --case test_fault_publish_error
```

### Manual Fault Injection

```erlang
%% In Erlang shell
1> router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}).
ok

2> router_nats:publish(~"test.subject", ~"payload").
{error, nats_unavailable}

3> router_nats_fault_injection:disable_fault(publish).
ok

4> router_nats:publish(~"test.subject", ~"payload").
ok
```

## Test Status and CI Integration

### Test Suites Status

| Suite | Status | CI Pipeline | Duration |
|-------|--------|-------------|----------|
| `router_nats_connection_failure_SUITE` | ✅ Stable | PR (standard) | ~30-60s |
| `router_jetstream_fault_injection_SUITE` | ✅ Stable | PR (standard) | ~20-40s |
| `router_nats_integration_SUITE` | ✅ Stable | PR (standard) | ~15-30s |
| `router_nats_performance_SUITE` | ⚠️ Slow | Nightly/Extended | ~10-20s |

**Total**: 54 tests (47 in standard PR pipeline, 7 in extended pipeline)

### CI/CD Integration

**Standard PR Pipeline** (recommended):
- `router_nats_connection_failure_SUITE` (22 tests)
- `router_jetstream_fault_injection_SUITE` (15 tests)
- `router_nats_integration_SUITE` (10 tests)

**Nightly/Extended Pipeline**:
- All standard suites
- `router_nats_performance_SUITE` (7 tests)

**See**: `apps/otp/router/test/RUN_TESTS.md` for detailed CI/CD integration instructions.

## Status

**Implementation Status**: ✅ **PRODUCTION READY**

All technical implementation, testing, and documentation completed. Ready for production deployment.

**See**: `apps/otp/router/docs/NATS_RESILIENCE_STATUS.md` for implementation status summary.

**Next Steps**: Operational tasks (monitoring, feedback, maintenance) - see `apps/otp/router/docs/NATS_RESILIENCE_NEXT_STEPS.md`.

## References

- `apps/otp/router/docs/NATS_RESILIENCE_STATUS.md` - Implementation status
- `apps/otp/router/docs/NATS_RESILIENCE_NEXT_STEPS.md` - Operational next steps
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - **Publish/Publish_with_ack failure behavior specification**
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_MONITORING.md` - **Operational monitoring and alerting guide (SRE recommendations)**
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` - Fault injection test suite
- `apps/otp/router/test/router_nats_connection_failure_SUITE.erl` - Connection failure test suite
- `apps/otp/router/test/router_nats_publish_failure_SUITE.erl` - **Publish/Publish_with_ack failure test suite**
- `apps/otp/router/test/router_nats_integration_SUITE.erl` - Integration test suite
- `apps/otp/router/test/router_nats_performance_SUITE.erl` - Performance test suite
- `apps/otp/router/test/FAULT_INJECTION_TEST_CRITERIA.md` - Test criteria documentation
- `apps/otp/router/test/RUN_TESTS.md` - Test execution guide and CI/CD integration
- `apps/otp/router/src/router_nats_fault_injection.erl` - Fault injection module
- `apps/otp/router/src/router_nats.erl` - NATS client implementation

