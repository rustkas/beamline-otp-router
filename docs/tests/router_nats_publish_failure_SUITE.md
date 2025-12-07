# router_nats_publish_failure Test Suite

## Overview

Comprehensive test suite for `router_nats` publish and publish_with_ack failure scenarios. Tests explicitly verify the behavior described in `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md`.

## Test Coverage

### Publish Failure Scenarios (8 tests)

1. **test_publish_error_connected_fail_open** - `{error, Reason}` in fail-open mode
2. **test_publish_error_connected_queueing** - `{error, Reason}` in queueing mode
3. **test_publish_timeout_connected_fail_open** - `timeout` in fail-open mode
4. **test_publish_timeout_connected_queueing** - `timeout` in queueing mode
5. **test_publish_close_connection_fail_open** - `close_connection` in fail-open mode
6. **test_publish_close_connection_queueing** - `close_connection` in queueing mode
7. **test_publish_not_connected_fail_open** - Not connected state in fail-open mode
8. **test_publish_not_connected_queueing** - Not connected state in queueing mode

### Publish_with_ack Failure Scenarios (8 tests)

1. **test_publish_with_ack_error_connected_fail_open** - `{error, Reason}` in fail-open mode
2. **test_publish_with_ack_error_connected_queueing** - `{error, Reason}` in queueing mode
3. **test_publish_with_ack_timeout_connected_fail_open** - `timeout` in fail-open mode
4. **test_publish_with_ack_timeout_connected_queueing** - `timeout` in queueing mode
5. **test_publish_with_ack_close_connection_fail_open** - `close_connection` in fail-open mode
6. **test_publish_with_ack_close_connection_queueing** - `close_connection` in queueing mode
7. **test_publish_with_ack_not_connected_fail_open** - Not connected state in fail-open mode
8. **test_publish_with_ack_not_connected_queueing** - Not connected state in queueing mode

### msg_id Behavior Tests (3 tests)

1. **test_msg_id_stub_in_fail_open_mode** - Verifies stub-msg-id is returned in fail-open mode
2. **test_msg_id_no_duplicates_on_retry** - Verifies no duplicate msg_id on retry
3. **test_msg_id_unique_per_operation** - Verifies unique msg_id per operation

### Metrics Behavior Tests (4 tests)

1. **test_metrics_publish_failures_incremented** - Verifies `router_nats_publish_failures_total` incremented
2. **test_metrics_publish_with_ack_failures_incremented** - Verifies `router_nats_publish_with_ack_failures_total` incremented
3. **test_metrics_queue_operations_count** - Verifies queue operations count metric updated
4. **test_metrics_retry_after_reconnection** - Verifies retry metrics after reconnection

**Total**: 23 tests

## Running Tests

### Run All Tests

```bash
cd apps/otp/router
rebar3 ct --suite test/router_nats_publish_failure_SUITE
```

### Run Specific Test

```bash
rebar3 ct --suite test/router_nats_publish_failure_SUITE --case test_publish_error_connected_fail_open
```

### Run Test Group

```bash
# Run all publish failure tests
rebar3 ct --suite test/router_nats_publish_failure_SUITE --case test_publish_error_connected_fail_open test_publish_error_connected_queueing

# Run all msg_id behavior tests
rebar3 ct --suite test/router_nats_publish_failure_SUITE --case test_msg_id_stub_in_fail_open_mode test_msg_id_no_duplicates_on_retry test_msg_id_unique_per_operation
```

## Test Assertions

Each test verifies:

1. **Return Value**: Correct return value for fail-open vs queueing mode
2. **Metrics**: Correct metrics incremented (failures_total, etc.)
3. **State Changes**: Connection state changes correctly
4. **msg_id Behavior**: Correct msg_id handling (stub ID vs real ID)
5. **Queue Behavior**: Operations queued correctly (queueing mode)

## Expected Behavior

### Fail-Open Mode

- `publish` returns `ok` even on errors
- `publish_with_ack` returns `{ok, <<"stub-msg-id">>}` even on errors
- No queueing (operations not preserved)
- Metrics still incremented (failures tracked)

### Queueing Mode

- `publish` returns `{error, Reason}` on errors
- `publish_with_ack` returns `{error, Reason}` on errors
- Operations queued for retry after reconnection
- Metrics incremented (failures tracked)

## Dependencies

- `router_nats_fault_injection` - Fault injection module
- `router_nats` - NATS client implementation
- `router_metrics` - Metrics module (mocked in tests)
- `router_logger` - Logger module (mocked in tests)

## See Also

- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Complete behavior specification
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Connection resilience documentation
- `apps/otp/router/src/router_nats.erl` - Implementation
- `apps/otp/router/src/router_nats_fault_injection.erl` - Fault injection module

