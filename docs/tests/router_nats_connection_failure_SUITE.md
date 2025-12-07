# router_nats Connection Failure Test Suite

## Overview

Comprehensive test suite for `router_nats` connection failure handling. Tests verify that router behaves correctly during NATS/JetStream failures.

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete documentation including metrics, logs, monitoring, and operational procedures.

## Test Criteria

Each test verifies three categories of criteria (see `FAULT_INJECTION_TEST_CRITERIA.md` for details):

1. **Resilience**: All key processes remain alive, no unexpected supervisor restarts
2. **Message Semantics**: Messages either redelivered or handled via fail-open
3. **Observability**: Logs and metrics reflect problems and recovery

## Test Coverage

### 1. Basic Failure Handling (6 tests)

- `test_router_nats_no_crash_on_connection_loss` - Verifies router_nats and supervisor remain alive
- `test_router_nats_reconnect_after_failure` - Tests reconnection mechanism
- `test_router_nats_fail_open_mode` - Tests fail-open mode operation
- `test_router_nats_metrics_on_failure` - Verifies metrics are emitted
- `test_router_nats_logs_on_failure` - Verifies logs are written
- `test_router_nats_message_redelivery_after_reconnect` - Tests message redelivery

### 2. Short-term Connection Drops (2 tests)

- `test_router_nats_tcp_connection_break` - Simulates TCP connection break
- `test_router_nats_timeout_on_operations` - Tests timeout on NATS operations

### 3. Long-term Connection Issues (2 tests)

- `test_router_nats_connection_flapping` - Tests periodic connection drops/recoveries
- `test_router_nats_multiple_reconnect_attempts` - Tests multiple reconnect attempts with exponential backoff

### 4. JetStream-Specific Errors (5 tests)

- `test_router_nats_jetstream_no_responders` - Tests "no responders" error handling
- `test_router_nats_jetstream_not_enabled` - Tests "JetStream not enabled" error
- `test_router_nats_jetstream_consumer_deleted` - Tests "consumer deleted" error
- `test_router_nats_jetstream_publish_errors` - Tests various publish errors (timeout, connection_failed, nats_unavailable)
- `test_router_nats_jetstream_ack_errors` - Tests various ACK errors (timeout, invalid_msg_id, not_connected)

### 5. Router Workflows During Failure (4 tests)

- `test_router_nats_incoming_requests_during_failure` - Tests incoming requests requiring publish/read from JetStream
- `test_router_nats_ongoing_sessions_during_failure` - Tests continuation of ongoing sessions/messages
- `test_router_nats_decide_consumer_during_failure` - Tests decide consumer behavior during failure
- `test_router_nats_result_consumer_during_failure` - Tests result consumer behavior during failure

### 6. Recovery Behavior (3 tests)

- `test_router_nats_recovery_after_short_drop` - Tests recovery after short connection drop
- `test_router_nats_recovery_after_long_drop` - Tests recovery after long connection drop
- `test_router_nats_message_retry_after_recovery` - Tests message retry after recovery

**Total: 22 tests**

## Running Tests

```bash
cd apps/otp/router

# Run all connection failure tests
rebar3 ct --suite test/router_nats_connection_failure_SUITE

# Run specific test
rebar3 ct --suite test/router_nats_connection_failure_SUITE --case test_router_nats_no_crash_on_connection_loss
```

## Test Configuration

Tests use the following configuration:

- `nats_reconnect_attempts`: 5
- `nats_reconnect_delay_ms`: 500
- `nats_max_reconnect_delay_ms`: 2000
- `nats_fail_open_mode`: false (default)

## Expected Behavior

### During Connection Failure

1. **Router doesn't crash**: `router_nats` gen_server and supervisor remain alive
2. **Metrics emitted**: Connection failures, reconnect attempts, operation failures
3. **Logs written**: Error/warn logs for connection loss, reconnection attempts
4. **Operations fail gracefully**: Publish/ACK/NAK operations return errors (or ok in fail-open mode)

### During Recovery

1. **Reconnection scheduled**: Automatic reconnection with exponential backoff
2. **Connection restored**: State transitions to `connected`
3. **Operations resume**: Pending operations can be retried
4. **Metrics updated**: Connection restored metrics emitted

### Fail-Open Mode

When `nats_fail_open_mode = true`:
- Publish operations return `ok` even if disconnected
- Router continues processing without blocking
- Messages may be lost but router remains operational

## Test Implementation Notes

- Tests use `gen_server:cast` to simulate connection loss
- Tests use `meck` for mocking `router_logger` and `router_metrics`
- Tests verify process aliveness using `is_process_alive/1`
- Tests verify state transitions using `router_nats:get_connection_status/0`

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete NATS resilience documentation
- `apps/otp/router/test/FAULT_INJECTION_TEST_CRITERIA.md` - Test criteria documentation
- `apps/otp/router/src/router_nats.erl` - Implementation

