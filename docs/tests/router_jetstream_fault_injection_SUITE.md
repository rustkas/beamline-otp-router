# router_jetstream_fault_injection Test Suite

## Overview

Comprehensive fault injection test suite for `router_nats` and `router_jetstream`. Tests router behavior under controlled failure scenarios using the fault injection infrastructure.

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete documentation including metrics, logs, monitoring, and operational procedures.

## Fault Injection Infrastructure

### Module: `router_nats_fault_injection`

Provides controlled fault injection for NATS/JetStream operations:

- **Enable fault**: `router_nats_fault_injection:enable_fault(Operation, Fault)`
- **Disable fault**: `router_nats_fault_injection:disable_fault(Operation)`
- **Clear all faults**: `router_nats_fault_injection:clear_all_faults()`

### Supported Operations

- `connect` - Connection attempts
- `publish` - Publish operations
- `publish_with_ack` - Publish with acknowledgment
- `ack` - Message acknowledgment
- `nak` - Negative acknowledgment
- `subscribe` - JetStream subscriptions

### Supported Fault Types

- `{error, Reason}` - Return error (e.g., `{error, connection_refused}`)
- `timeout` - Simulate operation timeout (hangs for 10 seconds)
- `close_connection` - Close connection process during operation
- `{delay, Milliseconds}` - Add delay to operation

## Test Coverage

### Connection Fault Injection (3 tests)

- `test_fault_connect_error` - Connect returns error
- `test_fault_connect_timeout` - Connect times out
- `test_fault_connect_close` - Connect closes connection

### Publish Fault Injection (4 tests)

- `test_fault_publish_error` - Publish returns error
- `test_fault_publish_timeout` - Publish times out
- `test_fault_publish_close_connection` - Publish closes connection
- `test_fault_publish_with_ack_error` - Publish with ack returns error

### ACK/NAK Fault Injection (3 tests)

- `test_fault_ack_error` - ACK returns error
- `test_fault_ack_timeout` - ACK times out
- `test_fault_ack_close_connection` - ACK closes connection

### Subscribe Fault Injection (2 tests)

- `test_fault_subscribe_error` - Subscribe returns error
- `test_fault_subscribe_timeout` - Subscribe times out

### Combined Fault Scenarios (3 tests)

- `test_fault_intermittent_publish_errors` - Intermittent publish failures
- `test_fault_cascading_failures` - Multiple operations fail
- `test_fault_recovery_after_faults` - Recovery after faults disabled

**Total: 15 tests**

## Test Criteria

Each test verifies **three categories of criteria** (see `FAULT_INJECTION_TEST_CRITERIA.md` for complete details):

### 1. Resilience (Устойчивость)

**Required Checks:**
- ✅ All key router processes remain alive:
  - `router_nats` process
  - `beamline_router_sup` supervisor
  - `router_decide_consumer` process
  - `router_result_consumer` process
- ✅ No unexpected supervisor restarts:
  - Supervisor restart count ≤ expected restarts
  - Expected restarts documented per scenario

**Verification**:
```erlang
%% 1.1: All key processes remain alive
true = is_process_alive(RouterNatsPid),
true = is_process_alive(RouterSupPid),
true = is_process_alive(DecideConsumerPid),
true = is_process_alive(ResultConsumerPid),

%% 1.2: No unexpected supervisor restarts
InitialRestartCount = get_supervisor_restart_count(beamline_router_sup),
%% ... perform fault injection ...
FinalRestartCount = get_supervisor_restart_count(beamline_router_sup),
ExpectedRestarts = 0,  %% Document expected restarts per scenario
true = FinalRestartCount =< InitialRestartCount + ExpectedRestarts,
```

### 2. Message Semantics (Семантика сообщений)

**Required Checks:**
- ✅ **Either** guaranteed redelivery (after recovery):
  - Messages queued when disconnected
  - Messages retried after reconnect
  - No silent message loss
- ✅ **Or** documented fail-open (messages considered processed):
  - Fail-open mode enabled
  - Messages return `ok` immediately
  - Behavior documented in test

**Verification**:
```erlang
%% 2.1: Messages queued for redelivery (redelivery mode)
PublishResult = router_nats:publish(Subject, Payload),
true = (is_tuple(PublishResult) andalso element(1, PublishResult) =:= error),
%% Message queued, will be retried after reconnect

%% OR

%% 2.1: Messages processed via fail-open (fail-open mode)
FailOpenEnabled = application:get_env(beamline_router, nats_fail_open_mode, false),
case FailOpenEnabled of
    true ->
        PublishResult = router_nats:publish(Subject, Payload),
        true = (PublishResult =:= ok);  %% Fail-open: ok returned
    false ->
        %% Redelivery mode: error returned, message queued
        ok
end,

%% 2.2: No silent message loss
true = (PublishResult =/= undefined),  %% Result returned, not lost
```

### 3. Observability (Наблюдаемость)

**Required Checks:**
- ✅ **Logs** contain clear messages:
  - Error logs with `error_code` and `error_tag`
  - Recovery logs with `error_code` "NATS_CONNECTION_RESTORED"
  - Operation failure logs with operation-specific codes
- ✅ **Metrics** reflect problems:
  - **Error counters** incremented (fact of error):
    - `router_nats_connection_lost_total`
    - `router_nats_publish_failures_total`
    - `router_nats_ack_failures_total`
    - `router_nats_reconnect_failures_total`
  - **Status gauge** reflects duration (gauge value):
    - `router_nats_connection_status = 0` (disconnected)
    - `router_nats_connection_status = 0.5` (reconnecting)
    - `router_nats_connection_status = 1` (connected)
- ✅ **Metrics return to normal** after recovery:
  - Status gauge = 1 (connected)
  - Error counters stop increasing
  - Success counters increase

**Verification**:
```erlang
%% 3.1: Error log contains clear message about failure
AllLogs = ets:tab2list(LogCalls),
ErrorLogs = [L || {log, _, Message, Context} = L <- AllLogs,
                  binary:match(Message, <<"NATS publish failed">>) =/= nomatch,
                  maps:get(<<"error_code">>, Context, undefined) =:= <<"NATS_PUBLISH_ERROR">>],
true = length(ErrorLogs) > 0,

%% 3.2: Metrics - error counters incremented (fact of error)
InitialFailures = get_metric_count(router_nats_publish_failures_total),
%% ... perform operation ...
FinalFailures = get_metric_count(router_nats_publish_failures_total),
true = FinalFailures > InitialFailures,

%% 3.3: Metrics - status gauge reflects duration of violation
AllStatuses = ets:tab2list(StatusGauges),
DisconnectedStatuses = [S || {status, Value, State} = S <- AllStatuses,
                            (Value =:= 0.0 orelse Value =:= 0.5),
                            (State =:= disconnected orelse State =:= reconnecting)],
true = length(DisconnectedStatuses) > 0,

%% 3.4: Metrics return to normal after recovery
%% After recovery:
ConnectedStatuses = [S || {status, Value, State} = S <- AllStatuses,
                         Value =:= 1.0,
                         State =:= connected],
true = length(ConnectedStatuses) > 0,
```

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete NATS resilience documentation
- `apps/otp/router/test/FAULT_INJECTION_TEST_CRITERIA.md` - Detailed test criteria
- `apps/otp/router/src/router_nats_fault_injection.erl` - Fault injection module
- `apps/otp/router/src/router_nats.erl` - NATS client implementation

## Running Tests

```bash
cd apps/otp/router

# Run all fault injection tests
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE

# Run specific test
rebar3 ct --suite test/router_jetstream_fault_injection_SUITE --case test_fault_publish_error
```

## Example Usage

### Example 1: Inject Publish Error

```erlang
%% Enable fault
router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),

%% Try to publish (will fail with injected error)
Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
%% Result = {error, nats_unavailable}

%% Disable fault
router_nats_fault_injection:disable_fault(publish),

%% Publish should now succeed
Result2 = router_nats:publish(<<"test.subject">>, <<"payload">>),
%% Result2 = ok
```

### Example 2: Inject Connection Timeout

```erlang
%% Enable timeout fault
router_nats_fault_injection:enable_fault(connect, timeout),

%% Trigger reconnect (will timeout)
{ok, _} = router_nats:reconnect(),
timer:sleep(1000),

%% Verify router_nats is still alive
true = is_process_alive(whereis(router_nats)),
```

### Example 3: Inject Intermittent Errors

```erlang
%% Enable fault
router_nats_fault_injection:enable_fault(publish, {error, intermittent}),
Result1 = router_nats:publish(<<"test.1">>, <<"payload1">>),
%% Result1 = {error, intermittent}

%% Disable fault
router_nats_fault_injection:disable_fault(publish),
Result2 = router_nats:publish(<<"test.2">>, <<"payload2">>),
%% Result2 = ok

%% Re-enable fault
router_nats_fault_injection:enable_fault(publish, {error, intermittent}),
Result3 = router_nats:publish(<<"test.3">>, <<"payload3">>),
%% Result3 = {error, intermittent}
```

## Integration with CI/CD

Tests are automatically run in CI/CD pipelines:

- **GitHub Actions**: `.github/workflows/validate.yml`
- **GitLab CI**: `.gitlab-ci.yml`
- **Drone CI**: `.drone.yml`

## References

- `apps/otp/router/src/router_nats_fault_injection.erl` - Fault injection module
- `apps/otp/router/src/router_nats.erl` - NATS client with fault injection support
- `apps/otp/router/test/router_nats_connection_failure_SUITE.erl` - Connection failure tests
- `apps/otp/router/docs/OBSERVABILITY.md` - Observability documentation

