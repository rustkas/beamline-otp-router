# NATS Publish/Publish_with_ack Failure Behavior Specification

## Purpose

This document explicitly describes the expected behavior of `router_nats` and `router` when `publish` and `publish_with_ack` operations fail. It covers:

- **Failure handling strategy**: fail-open vs queueing
- **msg_id handling**: stub IDs, retries, duplicate prevention
- **Metrics**: when and how metrics are updated
- **Error scenarios**: all failure types and their handling

## Scope

This specification covers:

1. **publish** operation failure scenarios
2. **publish_with_ack** operation failure scenarios
3. **msg_id** behavior in fail-open mode
4. **Metrics** behavior for all failure types
5. **Queueing** behavior when connection is lost

## Failure Handling Strategy

### Fail-Open Mode

**Configuration**: `nats_fail_open_mode = true`

**Behavior**:
- When NATS is unavailable or operations fail, `router_nats` returns success (`ok` for `publish`, `{ok, stub_msg_id}` for `publish_with_ack`)
- Messages are **NOT queued** for retry
- System continues accepting new messages without blocking
- Failures are logged and metered, but do not propagate to upper layers

**Use Cases**:
- High availability requirements (system must continue operating)
- Best-effort delivery (some message loss is acceptable)
- Degraded mode operation

**Trade-offs**:
- ✅ System remains available
- ✅ No blocking on NATS failures
- ❌ Messages may be lost (not queued)
- ❌ No retry mechanism

### Queueing Mode (Default)

**Configuration**: `nats_fail_open_mode = false`

**Behavior**:
- When NATS is unavailable, operations return `{error, not_connected}`
- Messages are **queued** for retry after reconnection
- Upper layers receive errors and can handle retries
- Queue has maximum size limit (default: 1000 operations)

**Use Cases**:
- Guaranteed delivery requirements
- Message ordering must be preserved
- System can tolerate temporary blocking

**Trade-offs**:
- ✅ Messages are preserved (queued)
- ✅ Automatic retry after reconnection
- ❌ System may block if queue fills up
- ❌ Memory usage increases with queue size

## Publish Operation Failure Scenarios

### Scenario 1: `{error, Reason}` During Connected State

**Condition**: Connection is `connected`, but `do_publish_internal` returns `{error, Reason}`

**Fault Injection**:
```erlang
router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}).
```

**Expected Behavior**:

1. **Error Handling**:
   - `do_publish` receives `{error, Reason}` from `do_publish_internal`
   - Error is logged with `error_code: "NATS_PUBLISH_ERROR"`
   - Metric `router_nats_publish_failures_total` is incremented

2. **Return Value**:
   - **Fail-open mode**: Returns `ok` (even though operation failed)
   - **Queueing mode**: Returns `{error, Reason}` (caller handles retry)

3. **State Changes**:
   - Connection state remains `connected` (no state change)
   - No queueing (error occurred during connected state)
   - No retry attempt (single attempt only)

4. **Metrics**:
   - `router_nats_publish_failures_total` incremented
   - `router_nats_publish_total` **NOT** incremented (operation failed)

**Example**:
```erlang
%% Fail-open mode
application:set_env(beamline_router, nats_fail_open_mode, true),
router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
%% Result = ok (fail-open returns success)

%% Queueing mode
application:set_env(beamline_router, nats_fail_open_mode, false),
Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
%% Result = {error, nats_unavailable} (caller handles retry)
```

### Scenario 2: `timeout` During Connected State

**Condition**: Connection is `connected`, but operation times out

**Fault Injection**:
```erlang
router_nats_fault_injection:enable_fault(publish, timeout).
```

**Expected Behavior**:

1. **Timeout Handling**:
   - `do_publish_internal` sleeps for 10 seconds (simulating timeout)
   - After timeout, returns `{error, timeout}`
   - Error is logged with `error_code: "NATS_PUBLISH_ERROR"`
   - Metric `router_nats_publish_failures_total` is incremented

2. **Return Value**:
   - **Fail-open mode**: Returns `ok` (timeout treated as non-critical)
   - **Queueing mode**: Returns `{error, timeout}` (caller handles retry)

3. **State Changes**:
   - Connection state remains `connected` (timeout doesn't change state)
   - No queueing (timeout occurred during connected state)
   - No automatic retry (single attempt only)

4. **Metrics**:
   - `router_nats_publish_failures_total` incremented
   - `router_nats_publish_total` **NOT** incremented

**Note**: Timeout is considered a **critical error** (operation failed), but connection state is not changed (TCP connection may still be alive).

### Scenario 3: `close_connection` During Operation

**Condition**: Connection is `connected`, but connection process is killed during operation

**Fault Injection**:
```erlang
router_nats_fault_injection:enable_fault(publish, close_connection).
```

**Expected Behavior**:

1. **Connection Closure**:
   - `do_publish_internal` calls `exit(ConnectionPid, normal)` to kill connection
   - Returns `{error, connection_closed}`
   - Connection process death triggers `{'DOWN', ...}` message to `router_nats`
   - `handle_connection_lost` is called automatically

2. **State Changes**:
   - Connection state changes to `disconnected` or `reconnecting`
   - Connection PID is cleared
   - Reconnection is scheduled (if attempts remaining)

3. **Return Value**:
   - **Fail-open mode**: Returns `ok` (connection loss treated as non-critical)
   - **Queueing mode**: Returns `{error, connection_closed}` (caller handles retry)

4. **Metrics**:
   - `router_nats_publish_failures_total` incremented
   - `router_nats_connection_lost_total` incremented (via `handle_connection_lost`)
   - `router_nats_connection_status` gauge updated to `0` (disconnected)

5. **Subsequent Operations**:
   - Next `publish` call will see `disconnected` state
   - Will be queued (if queueing mode) or return `ok` (if fail-open mode)

### Scenario 4: Not Connected State

**Condition**: Connection state is `disconnected`, `connecting`, or `reconnecting`

**Expected Behavior**:

1. **Operation Handling**:
   - `do_publish` checks connection state (not `connected`)
   - Logs warning with `error_code: "NATS_PUBLISH_QUEUED"`
   - Metric `router_nats_publish_failures_total` is incremented

2. **Return Value**:
   - **Fail-open mode**: Returns `ok` (operation accepted, but not queued)
   - **Queueing mode**: Returns `{error, not_connected}` (caller updates state to queue)

3. **Queueing** (Queueing mode only):
   - Caller receives `{error, not_connected}`
   - Caller calls `queue_operation({publish, Subject, Payload}, State)`
   - Operation is added to `pending_operations` list
   - Queue size is checked (max: 1000 by default)
   - If queue full, oldest operation is dropped

4. **Metrics**:
   - `router_nats_publish_failures_total` incremented
   - `router_nats_pending_operations_count` gauge updated (if queued)
   - `router_nats_pending_operations_dropped_total` incremented (if queue full)

5. **Retry After Reconnection**:
   - When connection is restored, `retry_pending_operations` is called
   - Queued operations are retried in FIFO order (oldest first)
   - Successful retries increment `router_nats_pending_operations_retry_success`
   - Failed retries increment `router_nats_pending_operations_retry_failed`

## Publish_with_ack Operation Failure Scenarios

### Scenario 1: `{error, Reason}` During Connected State

**Condition**: Connection is `connected`, but `do_publish_with_ack_internal` returns `{error, Reason}`

**Fault Injection**:
```erlang
router_nats_fault_injection:enable_fault(publish_with_ack, {error, nats_unavailable}).
```

**Expected Behavior**:

1. **Error Handling**:
   - `do_publish_with_ack` receives `{error, Reason}` from `do_publish_with_ack_internal`
   - Error is logged with `error_code: "NATS_PUBLISH_WITH_ACK_ERROR"`
   - Metric `router_nats_publish_with_ack_failures_total` is incremented

2. **Return Value**:
   - **Fail-open mode**: Returns `{ok, <<"stub-msg-id">>}` (stub ID for fail-open)
   - **Queueing mode**: Returns `{error, Reason}` (caller handles retry)

3. **msg_id Behavior**:
   - **Fail-open mode**: Always returns `<<"stub-msg-id">>` (no real msg_id)
   - **Queueing mode**: No msg_id returned (error prevents msg_id generation)

4. **State Changes**:
   - Connection state remains `connected`
   - No queueing (error occurred during connected state)
   - No retry attempt

5. **Metrics**:
   - `router_nats_publish_with_ack_failures_total` incremented
   - `router_nats_publish_with_ack_total` **NOT** incremented

**Critical**: In fail-open mode, `msg_id` is **always** `<<"stub-msg-id">>`. This means:
- Upper layers cannot track message delivery
- No duplicate detection possible (all messages get same stub ID)
- ACK operations on stub ID will fail (stub ID is not a real NATS message ID)

### Scenario 2: `timeout` During Connected State

**Condition**: Connection is `connected`, but operation times out

**Fault Injection**:
```erlang
router_nats_fault_injection:enable_fault(publish_with_ack, timeout).
```

**Expected Behavior**:

1. **Timeout Handling**:
   - `do_publish_with_ack_internal` sleeps for 10 seconds
   - Returns `{error, timeout}`
   - Error is logged with `error_code: "NATS_PUBLISH_WITH_ACK_ERROR"`
   - Metric `router_nats_publish_with_ack_failures_total` is incremented

2. **Return Value**:
   - **Fail-open mode**: Returns `{ok, <<"stub-msg-id">>}` (timeout treated as non-critical)
   - **Queueing mode**: Returns `{error, timeout}` (caller handles retry)

3. **msg_id Behavior**:
   - **Fail-open mode**: Returns `<<"stub-msg-id">>` (no real msg_id)
   - **Queueing mode**: No msg_id returned

4. **State Changes**:
   - Connection state remains `connected`
   - No queueing
   - No retry attempt

5. **Metrics**:
   - `router_nats_publish_with_ack_failures_total` incremented
   - `router_nats_publish_with_ack_total` **NOT** incremented

**Note**: Timeout is considered a **critical error** for ack operations (ack not received), but connection state is not changed.

### Scenario 3: `close_connection` During Operation

**Condition**: Connection is `connected`, but connection process is killed during operation

**Fault Injection**:
```erlang
router_nats_fault_injection:enable_fault(publish_with_ack, close_connection).
```

**Expected Behavior**:

1. **Connection Closure**:
   - `do_publish_with_ack_internal` calls `exit(ConnectionPid, normal)`
   - Returns `{error, connection_closed}`
   - Connection process death triggers `handle_connection_lost`

2. **State Changes**:
   - Connection state changes to `disconnected` or `reconnecting`
   - Connection PID is cleared
   - Reconnection is scheduled

3. **Return Value**:
   - **Fail-open mode**: Returns `{ok, <<"stub-msg-id">>}` (connection loss treated as non-critical)
   - **Queueing mode**: Returns `{error, connection_closed}` (caller handles retry)

4. **msg_id Behavior**:
   - **Fail-open mode**: Returns `<<"stub-msg-id">>` (no real msg_id)
   - **Queueing mode**: No msg_id returned

5. **Metrics**:
   - `router_nats_publish_with_ack_failures_total` incremented
   - `router_nats_connection_lost_total` incremented
   - `router_nats_connection_status` gauge updated to `0`

6. **Message Fate**:
   - If message was sent before connection closed: **message fate unknown** (may or may not be delivered)
   - If message was not sent: **message not delivered**
   - No ACK will be received (connection closed)

### Scenario 4: Not Connected State

**Condition**: Connection state is `disconnected`, `connecting`, or `reconnecting`

**Expected Behavior**:

1. **Operation Handling**:
   - `do_publish_with_ack` checks connection state (not `connected`)
   - Logs warning with `error_code: "NATS_PUBLISH_WITH_ACK_QUEUED"`
   - Metric `router_nats_publish_with_ack_failures_total` is incremented

2. **Return Value**:
   - **Fail-open mode**: Returns `{ok, <<"stub-msg-id">>}` (stub ID for fail-open)
   - **Queueing mode**: Returns `{error, not_connected}` (caller updates state to queue)

3. **msg_id Behavior**:
   - **Fail-open mode**: Always returns `<<"stub-msg-id">>` (no real msg_id)
   - **Queueing mode**: No msg_id returned (operation queued)

4. **Queueing** (Queueing mode only):
   - Caller receives `{error, not_connected}`
   - Caller calls `queue_operation({publish_with_ack, Subject, Payload, Headers}, State)`
   - Operation is added to `pending_operations` list
   - Queue size is checked (max: 1000 by default)

5. **Metrics**:
   - `router_nats_publish_with_ack_failures_total` incremented
   - `router_nats_pending_operations_count` gauge updated (if queued)
   - `router_nats_pending_operations_dropped_total` incremented (if queue full)

6. **Retry After Reconnection**:
   - When connection is restored, queued operations are retried
   - Each retry generates a **new msg_id** (no duplicate msg_id)
   - Successful retries increment `router_nats_pending_operations_retry_success`

## msg_id Handling

### Fail-Open Mode

**Behavior**:
- **Always** returns `<<"stub-msg-id">>` for `publish_with_ack` failures
- Stub ID is **not** a real NATS message ID
- Stub ID is **not** unique (all failed operations return same stub ID)
- ACK operations on stub ID will **fail** (stub ID is not valid)

**Implications**:
- Upper layers cannot track message delivery
- No duplicate detection possible
- No ACK tracking possible
- Messages are considered "best-effort" (may be lost)

**Example**:
```erlang
%% Fail-open mode, connection lost
application:set_env(beamline_router, nats_fail_open_mode, true),
gen_server:cast(router_nats, {connection_lost, test}),
timer:sleep(100),

%% All operations return stub ID
{ok, MsgId1} = router_nats:publish_with_ack(<<"test.1">>, <<"payload1">>, #{}),
{ok, MsgId2} = router_nats:publish_with_ack(<<"test.2">>, <<"payload2">>, #{}),
%% MsgId1 = <<"stub-msg-id">>
%% MsgId2 = <<"stub-msg-id">>
%% MsgId1 =:= MsgId2  %% true (same stub ID)
```

### Queueing Mode

**Behavior**:
- Returns `{error, Reason}` (no msg_id) when operation fails
- When operation is retried after reconnection, **new msg_id** is generated
- Each retry generates a **unique msg_id** (no duplicate msg_id)
- msg_id is stable for successful operations (doesn't change on retry)

**Implications**:
- Upper layers can track message delivery (real msg_id)
- Duplicate detection possible (unique msg_id per message)
- ACK tracking possible (real msg_id can be ACKed)
- Messages are preserved (queued for retry)

**Example**:
```erlang
%% Queueing mode, connection lost
application:set_env(beamline_router, nats_fail_open_mode, false),
gen_server:cast(router_nats, {connection_lost, test}),
timer:sleep(100),

%% Operations return error (no msg_id)
Error1 = router_nats:publish_with_ack(<<"test.1">>, <<"payload1">>, #{}),
Error2 = router_nats:publish_with_ack(<<"test.2">>, <<"payload2">>, #{}),
%% Error1 = {error, not_connected}
%% Error2 = {error, not_connected}

%% After reconnection, operations are retried with new msg_id
StubPid = spawn_link(fun() -> receive _ -> ok end end),
gen_server:cast(router_nats, {connection_restored, StubPid}),
timer:sleep(500),

%% Retried operations generate new msg_id
%% (retry happens automatically in retry_pending_operations)
```

## Metrics Behavior

### router_nats_publish_with_ack_failures_total

**Incremented When**:
- ✅ `{error, Reason}` returned from `do_publish_with_ack_internal`
- ✅ `timeout` returned from `do_publish_with_ack_internal`
- ✅ `close_connection` triggers connection loss
- ✅ Connection state is not `connected` (operation queued)

**NOT Incremented When**:
- ❌ Operation succeeds (`{ok, MsgId}`)
- ❌ Operation is retried successfully (retry success is separate metric)

**Labels** (if supported):
- `reason`: Error reason (e.g., `nats_unavailable`, `timeout`, `connection_closed`, `not_connected`)
- `error_type`: Error type (e.g., `operation_error`, `timeout`, `connection_lost`)

**Current Implementation**: No labels (counter only)

**Expected Behavior**:
- Metric increments **exactly once** per failed operation
- Metric increments **regardless of fail-open/queueing mode**
- Metric increments **before** returning result to caller
- Metric is **not reset** on reconnection (cumulative counter)

**Testing**:
- All failure scenarios explicitly test metric incrementation
- Tests verify metric is incremented using meck tracking
- Tests verify successful operations do NOT increment metric

### router_nats_publish_failures_total

**Incremented When**:
- ✅ `{error, Reason}` returned from `do_publish_internal`
- ✅ `timeout` returned from `do_publish_internal`
- ✅ `close_connection` triggers connection loss
- ✅ Connection state is not `connected` (operation queued)

**NOT Incremented When**:
- ❌ Operation succeeds (`ok`)
- ❌ Operation is retried successfully

**Expected Behavior**:
- Metric increments **exactly once** per failed operation
- Metric increments **regardless of fail-open/queueing mode**
- Metric increments **before** returning result to caller
- Metric is **not reset** on reconnection (cumulative counter)

**Testing**:
- All failure scenarios explicitly test metric incrementation
- Tests verify metric is incremented using meck tracking
- Tests verify successful operations do NOT increment metric

### Additional Metrics

**router_nats_pending_operations_count** (gauge):
- Updated when operations are queued or retried
- Value = length of `pending_operations` list

**router_nats_pending_operations_retry** (counter):
- Incremented when `retry_pending_operations` is called
- Value = number of operations retried

**router_nats_pending_operations_retry_success** (counter):
- Incremented for each successful retry
- Value = number of successful retries

**router_nats_pending_operations_retry_failed** (counter):
- Incremented for each failed retry
- Value = number of failed retries

**router_nats_pending_operations_dropped_total** (counter):
- Incremented when queue is full and oldest operation is dropped
- Value = number of operations dropped

## Summary Table

| Scenario | Connection State | Fail-Open Mode | Queueing Mode | Metrics |
|----------|-----------------|----------------|---------------|---------|
| `{error, Reason}` | `connected` | `ok` | `{error, Reason}` | `publish_failures_total` incremented |
| `timeout` | `connected` | `ok` | `{error, timeout}` | `publish_failures_total` incremented |
| `close_connection` | `connected` → `disconnected` | `ok` | `{error, connection_closed}` | `publish_failures_total` + `connection_lost_total` |
| Not connected | `disconnected` | `ok` | `{error, not_connected}` (queued) | `publish_failures_total` + `pending_operations_count` |

| Scenario | Connection State | Fail-Open Mode | Queueing Mode | msg_id | Metrics |
|----------|-----------------|----------------|---------------|--------|---------|
| `{error, Reason}` | `connected` | `{ok, <<"stub-msg-id">>}` | `{error, Reason}` | Stub ID (fail-open) / None (queueing) | `publish_with_ack_failures_total` |
| `timeout` | `connected` | `{ok, <<"stub-msg-id">>}` | `{error, timeout}` | Stub ID (fail-open) / None (queueing) | `publish_with_ack_failures_total` |
| `close_connection` | `connected` → `disconnected` | `{ok, <<"stub-msg-id">>}` | `{error, connection_closed}` | Stub ID (fail-open) / None (queueing) | `publish_with_ack_failures_total` + `connection_lost_total` |
| Not connected | `disconnected` | `{ok, <<"stub-msg-id">>}` | `{error, not_connected}` (queued) | Stub ID (fail-open) / None (queueing) | `publish_with_ack_failures_total` + `pending_operations_count` |

## Upper-Level Behavior (router/router_jetstream)

### Impact on router (router_caf_adapter)

**router_caf_adapter** uses `router_nats:publish_with_ack` with retry logic:

1. **Fail-Open Mode**:
   - `router_nats` returns `{ok, stub-msg-id}` even on failures
   - `router_caf_adapter` treats this as success
   - **No retries** are attempted (operation appears successful)
   - **Message may be lost** (not queued, not retried)
   - System continues processing new requests without blocking

2. **Queueing Mode**:
   - `router_nats` returns `{error, Reason}` on failures
   - `router_caf_adapter` implements retry logic with exponential backoff
   - **Retries up to MaxRetries** (default: 3)
   - If all retries fail: returns `{error, ErrorKind, Retries, Error}`
   - **Metrics**: `router_retry_exhausted_total` incremented on exhaustion
   - System may block if many operations are queued

**Example** (from `router_caf_adapter:publish_with_retries`):
```erlang
case router_nats:publish_with_ack(Subject, Json, Headers) of
    {ok, PubAckId} ->
        %% Success - emit metrics and return
        {ok, Retries};
    Error ->
        %% Error - classify and retry if attempts remaining
        ErrorKind = classify_nats_error(Error),
        NextRetry = Retries + 1,
        case NextRetry < MaxRetries of
            true ->
                %% Retry with exponential backoff
                BackoffMs = calculate_backoff(NextRetry),
                timer:sleep(BackoffMs),
                publish_with_retries(...);
            false ->
                %% Exhausted - emit metric and return error
                {error, ErrorKind, NextRetry, Error}
        end
end
```

### Impact on router_jetstream

**router_jetstream** uses `router_nats` for message delivery and ACK operations:

1. **Publish Failures**:
   - If `publish_with_ack` fails, message is not delivered to JetStream
   - **No ACK tracking** possible (no msg_id returned)
   - **No redelivery** (message never reached JetStream)
   - **No DLQ** (message was never in stream)

2. **ACK Failures**:
   - If `ack_message` fails, message may be redelivered by JetStream
   - **Redelivery count** increases
   - May trigger **MaxDeliver exhaustion** → DLQ

3. **Fail-Open vs Queueing**:
   - **Fail-open**: Messages may be lost, but system remains available
   - **Queueing**: Messages are preserved, but system may block

### Expected Behavior Summary

| Component | Fail-Open Mode | Queueing Mode |
|-----------|----------------|---------------|
| **router_nats** | Returns success (`ok` / `{ok, stub-msg-id}`) | Returns error (`{error, Reason}`) |
| **router_caf_adapter** | Treats as success, no retries | Implements retry logic, may exhaust |
| **router_jetstream** | Messages may be lost | Messages preserved, may block |
| **System Availability** | ✅ Always available | ⚠️ May block if queue fills |
| **Message Guarantees** | ❌ Best-effort (may lose) | ✅ Guaranteed (queued) |

## Test Coverage

All scenarios described in this document are covered by tests in:
- `apps/otp/router/test/router_nats_publish_failure_SUITE.erl`

**Test Categories**:
1. **Publish failure scenarios** (8 tests)
2. **Publish_with_ack failure scenarios** (8 tests)
3. **msg_id behavior tests** (3 tests)
4. **Metrics behavior tests** (4 tests)
5. **Upper-level behavior tests** (6 tests)

**See**: Test suite documentation for detailed test scenarios and assertions.

## References

- `apps/otp/router/src/router_nats.erl` - Implementation
- `apps/otp/router/src/router_nats_fault_injection.erl` - Fault injection module
- `apps/otp/router/src/router_caf_adapter.erl` - Upper-level retry logic
- `apps/otp/router/src/router_jetstream.erl` - JetStream integration
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Connection resilience documentation
- `apps/otp/router/docs/NATS_METRICS_COMPLIANCE.md` - Metrics documentation

