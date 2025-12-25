# Fault Injection Test Criteria

## Overview

This document defines **explicit pass criteria** for each fault injection test scenario. Every test must verify all three categories of criteria.

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete documentation including metrics, logs, monitoring, and operational procedures.

## Test Pass Criteria Template

For each test scenario, the following criteria must be verified:

### 1. Resilience Criteria

**Required Checks:**
- ✅ All key router processes remain alive:
  - `router_nats` process
  - `beamline_router_sup` supervisor
  - `router_decide_consumer` process
  - `router_result_consumer` process
- ✅ No unexpected supervisor restarts:
  - Supervisor restart count ≤ expected restarts
  - Expected restarts documented per scenario

**Verification Code:**
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

### 2. Message Semantics Criteria

**Required Checks:**
- ✅ **Either** guaranteed redelivery (after recovery):
  - Messages queued when disconnected
  - Messages retried after reconnect
  - No silent message loss
- ✅ **Or** documented fail-open (messages considered processed):
  - Fail-open mode enabled
  - Messages return `ok` immediately
  - Behavior documented in test

**Verification Code:**
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

### 3. Observability Criteria

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

**Verification Code:**
```erlang
%% 3.1: Error log contains clear message about failure
AllLogs = ets:tab2list(LogCalls),
ErrorLogs = [L || {log, _, Message, Context} = L <- AllLogs,
                  binary:match(Message, ~"NATS publish failed") =/= nomatch,
                  maps:get(~"error_code", Context, undefined) =:= ~"NATS_PUBLISH_ERROR"],
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

## Scenario-Specific Criteria

### Scenario: Connect Error Fault

**Resilience:**
- Expected restarts: 0
- All processes alive

**Message Semantics:**
- Mode: Redelivery (messages queued)
- Messages queued when disconnected

**Observability:**
- Log: "reconnection attempt failed" with error_code
- Metric: `router_nats_reconnect_failures_total` incremented
- Gauge: `router_nats_connection_status = 0` or `0.5`

### Scenario: Publish Error Fault

**Resilience:**
- Expected restarts: 0
- All processes alive

**Message Semantics:**
- Mode: Error returned (not queued, not fail-open)
- Error returned immediately, not silently lost

**Observability:**
- Log: "NATS publish failed" with error_code "NATS_PUBLISH_ERROR"
- Metric: `router_nats_publish_failures_total` incremented

### Scenario: Publish Close Connection Fault

**Resilience:**
- Expected restarts: 0
- All processes alive

**Message Semantics:**
- Mode: Redelivery (connection lost, messages queued)
- Connection state transitions to disconnected/reconnecting

**Observability:**
- Log: "NATS connection lost" with error_code "NATS_CONNECTION_LOST"
- Metric: `router_nats_connection_lost_total` incremented
- Gauge: `router_nats_connection_status = 0`

### Scenario: Recovery After Faults

**Resilience:**
- Expected restarts: 0
- All processes alive

**Message Semantics:**
- Mode: Redelivery → Success
- Messages succeed after recovery

**Observability:**
- Log: Error logs during fault, recovery logs after
- Metrics: Error counters incremented during fault, success counters after recovery
- Gauge: Status returns to 1 (connected) after recovery

## Test Implementation Checklist

For each test, verify:

- [ ] **Resilience:**
  - [ ] All key processes checked with `is_process_alive/1`
  - [ ] Supervisor restart count checked
  - [ ] Expected restarts documented

- [ ] **Message Semantics:**
  - [ ] Redelivery mode OR fail-open mode documented
  - [ ] Message handling verified (queued/processed/error)
  - [ ] No silent message loss verified

- [ ] **Observability:**
  - [ ] Error logs verified with error_code
  - [ ] Recovery logs verified (if applicable)
  - [ ] Error counters verified (delta > 0)
  - [ ] Status gauge verified (correct value)
  - [ ] Metrics return to normal verified (if recovery scenario)

## Helper Functions

Use `router_fault_injection_helpers` module for standardized verification:

```erlang
%% Verify all criteria
{ok, Details} = router_fault_injection_helpers:verify_all_criteria(
    redelivery,  %% Mode
    [0],         %% Expected restarts
    InitialMetrics,
    FinalMetrics
),
```

## Quick Reference

### Public Metrics Contract

**Connection Metrics**:
- `router_nats_connection_status` (gauge) - 0.0/0.5/1.0
- `router_nats_connection_*_total` (counters) - established/lost/restored/failures

**Operation Metrics**:
- `router_nats_publish_total` / `router_nats_publish_failures_total` (counters)
- `router_nats_ack_total` / `router_nats_ack_failures_total` (counters)
- `router_nats_subscribe_total` / `router_nats_subscribe_failures_total` (counters)

**Queue Metrics**:
- `router_nats_pending_operations_count` (gauge)
- `router_nats_pending_operations_retry*` (counters)

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete metrics list.

### Public Log Contract

**Error Codes** (public contract):
- `NATS_CONNECTION_*` - Connection events
- `NATS_PUBLISH_*` - Publish events
- `NATS_ACK_*` - ACK events
- `NATS_QUEUE_*` - Queue events
- `NATS_RETRY_*` - Retry events

**See**: `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` for complete log format.

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete NATS resilience documentation
- `apps/otp/router/test/router_fault_injection_helpers.erl` - Helper functions
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` - Test suite
- `apps/otp/router/src/router_nats_fault_injection.erl` - Fault injection module

