# Concurrent Faults Test Documentation

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Comprehensive documentation of concurrent fault scenarios and their test coverage.

## Overview

This document provides detailed documentation of concurrent fault scenarios tested in `router_result_consumer_SUITE.erl` and `router_decide_consumer_SUITE.erl`. It complements the general fault injection scenarios documented in `FAULT_INJECTION_TEST_SCENARIOS.md` by focusing specifically on concurrent fault handling.

## Test Suites

### `router_result_consumer_SUITE.erl`

Tests for the result consumer that processes execution results from CAF and emits usage events.

**Concurrent Fault Tests**:
- `test_ack_error_with_tenant_validation_fail_concurrent` - Basic concurrent faults (S1)
- `test_ack_error_with_tenant_validation_fail_same_message` - Sequential retry with validation fail (S2)
- `test_nak_with_publish_failure_recovery` - NAK + publish failure with recovery (S3)
- `test_batch_nak_publish_failure_mixed` - Batch messages with mixed failures (S4)
- `test_prolonged_faults_with_consumer_restart` - Extended: Consumer restart during faults
- `test_tenant_isolation_during_concurrent_faults` - Extended: Tenant isolation
- `test_final_state_and_idempotency_multi_retry` - Extended: Final states and idempotency
- `test_comprehensive_metrics_and_logging_validation` - Extended: Comprehensive metrics/logging

### `router_decide_consumer_SUITE.erl`

Tests for the decide consumer that processes routing decisions and publishes replies.

**Concurrent Fault Tests**:
- `test_decide_ack_error_with_tenant_validation_fail_concurrent` - Basic concurrent faults (S1)
- `test_decide_ack_error_with_tenant_validation_fail_same_message` - Sequential retry with validation fail (S2)
- `test_decide_nak_with_publish_failure_recovery` - NAK + publish failure with recovery (S3)
- `test_decide_batch_nak_publish_failure_mixed` - Batch messages with mixed failures (S4)
- `test_decide_prolonged_faults_with_consumer_restart` - Extended: Consumer restart during faults
- `test_decide_tenant_isolation_during_concurrent_faults` - Extended: Tenant isolation
- `test_decide_final_state_and_idempotency_multi_retry` - Extended: Final states and idempotency
- `test_decide_comprehensive_metrics_and_logging_validation` - Extended: Comprehensive metrics/logging

## Basic Concurrent Fault Scenarios

### Scenario S1: ACK Error + Tenant Validation Fail (Concurrent Messages)

**Test**: `test_ack_error_with_tenant_validation_fail_concurrent` / `test_decide_ack_error_with_tenant_validation_fail_concurrent`

**Description**: 
- Message A: Successful tenant validation, but ACK fails
- Message B: Tenant validation fails, processed in parallel with Message A

**Fault Injection**:
- Message A: `router_nats:ack_message/1` returns `{error, timeout}`
- Message B: `router_tenant_validator:validate_tenant/2` returns `{error, tenant_not_allowed, ...}`

**Verification Criteria**:
- ✅ **Resilience**: Consumer process remains alive
- ✅ **Message Semantics**: 
  - Message A: ACK is retried (count > 0)
  - Message B: NAK is called (count > 0)
  - No message loss
- ✅ **Observability**:
  - Tenant rejection metrics emitted with correct metadata
  - Error logs contain tenant validation failures
  - Metrics have correct tenant_id and reason labels

**Parallelism**: Uses `spawn` for concurrent message processing with ETS synchronization

### Scenario S2: ACK Error + Tenant Validation Fail (Same Message, Sequential)

**Test**: `test_ack_error_with_tenant_validation_fail_same_message` / `test_decide_ack_error_with_tenant_validation_fail_same_message`

**Description**: 
- Message passes validation initially, but ACK fails
- On retry, tenant validation fails (tenant may have been disabled)

**Fault Injection**:
- First attempt: `router_nats:ack_message/1` returns `{error, timeout}`
- Retry attempt: `router_tenant_validator:validate_tenant/2` returns `{error, tenant_not_allowed, ...}`

**Verification Criteria**:
- ✅ **Resilience**: Consumer process remains alive
- ✅ **Message Semantics**: 
  - Delivery count increments correctly
  - Message transitions to rejection state after validation fail
- ✅ **Observability**:
  - Tenant rejection metrics emitted
  - Delivery count tracked correctly

### Scenario S3: NAK + Publish Failure with Recovery

**Test**: `test_nak_with_publish_failure_recovery` / `test_decide_nak_with_publish_failure_recovery`

**Description**: 
- Publish result/reply fails (connection error, quota exceeded)
- Consumer does NAK for original message
- After connection recovery, publish succeeds

**Fault Injection**:
- `router_nats:publish/2` returns `{error, connection_refused}` initially
- After recovery: `router_nats:publish/2` returns `ok`

**Verification Criteria**:
- ✅ **Resilience**: Consumer process remains alive
- ✅ **Message Semantics**: 
  - Message is NAK'd when publish fails
  - Message is reprocessed after recovery
  - Publish succeeds after recovery (count >= 2)
  - No unexpected duplicates
- ✅ **Observability**:
  - NAK metrics emitted with correct reason
  - Publish failure metrics emitted
  - Recovery events logged

### Scenario S4: Batch Messages with Mixed NAK + Publish Failure

**Test**: `test_batch_nak_publish_failure_mixed` / `test_decide_batch_nak_publish_failure_mixed`

**Description**: 
- Multiple messages processed in parallel
- Some messages: publish failures → NAK
- Other messages: succeed normally
- Connection recovers, failed messages retry successfully

**Fault Injection**:
- First N messages: `router_nats:publish/2` returns `{error, connection_refused}`
- Remaining messages: `router_nats:publish/2` returns `ok`
- After recovery: All publishes succeed

**Verification Criteria**:
- ✅ **Resilience**: Consumer process remains alive
- ✅ **Message Semantics**: 
  - Failed messages: NAK count > 0, retry count >= 2
  - Successful messages: Processed once
  - All messages eventually succeed
  - No global blocking
- ✅ **Observability**:
  - Metrics for each message type (failed/successful) are correct
  - No contradictory metrics
  - Parallel processing verified via ETS synchronization

**Parallelism**: Uses `spawn` for concurrent processing of all messages with ETS synchronization

## Extended Concurrent Fault Scenarios

### Scenario: Prolonged Faults with Consumer Restart

**Test**: `test_prolonged_faults_with_consumer_restart` / `test_decide_prolonged_faults_with_consumer_restart`

**Description**: 
- During a fault period (ACK/NAK/publish errors), consumer restarts
- After restart: All subscriptions restored, messages not lost, no double processing

**Fault Injection**:
- Extended fault period: Multiple ACK/publish failures
- Consumer restart: Simulated via supervisor restart
- Recovery: Faults cleared, normal operation resumes

**Verification Criteria**:
- ✅ **Resilience**: 
  - Consumer restarts successfully
  - All subscriptions restored after restart
  - Consumer continues processing after restart
- ✅ **Message Semantics**: 
  - Messages not lost during restart
  - No double processing beyond expected retries
  - Final states correct for all messages
- ✅ **Observability**:
  - Restart events logged
  - Metrics reflect correct state after restart

### Scenario: Tenant Isolation During Concurrent Faults

**Test**: `test_tenant_isolation_during_concurrent_faults` / `test_decide_tenant_isolation_during_concurrent_faults`

**Description**: 
- Tenant A: Generates combinations of failures (ACK error + validation fail, NAK + publish failure)
- Tenant B: Processes without failures in parallel
- Verify: Tenant B not slowed/blocked, metrics/logs not mixed

**Fault Injection**:
- Tenant A: Multiple fault types (ACK errors, validation failures, publish failures)
- Tenant B: Normal processing (no faults)

**Verification Criteria**:
- ✅ **Resilience**: 
  - Both tenants processed in parallel
  - Tenant B not blocked by Tenant A faults
- ✅ **Message Semantics**: 
  - Tenant A: Messages handled according to fault type
  - Tenant B: Messages processed normally
  - No cross-tenant interference
- ✅ **Observability**:
  - Metrics correctly tagged with tenant_id
  - Logs correctly tagged with tenant_id
  - No mixing of Tenant A and Tenant B metrics/logs

**Parallelism**: Uses `spawn` for concurrent processing of Tenant A and Tenant B messages

### Scenario: Final State and Idempotency with Multiple Retries

**Test**: `test_final_state_and_idempotency_multi_retry` / `test_decide_final_state_and_idempotency_multi_retry`

**Description**: 
- Complex scenario with multiple retries
- Verify: Final message states, idempotency at business result level

**Fault Injection**:
- Multiple retry cycles with different fault types
- Mix of ACK errors, publish failures, validation failures

**Verification Criteria**:
- ✅ **Resilience**: Consumer process remains alive
- ✅ **Message Semantics**: 
  - Each message reaches final status (success, DLQ, finally rejected)
  - Number of processing/publications does not exceed contract
  - Idempotency preserved at business result level
- ✅ **Observability**:
  - Final state metrics correct
  - Idempotency metrics correct
  - No duplicate processing beyond designed retries

### Scenario: Comprehensive Metrics and Logging Validation

**Test**: `test_comprehensive_metrics_and_logging_validation` / `test_decide_comprehensive_metrics_and_logging_validation`

**Description**: 
- Simultaneous ACK errors, tenant validation fail, and publish failures
- Verify: All metrics correct, no contradictions, no duplicates

**Fault Injection**:
- Multiple fault types simultaneously:
  - ACK errors
  - Tenant validation failures
  - Publish failures

**Verification Criteria**:
- ✅ **Resilience**: Consumer process remains alive
- ✅ **Message Semantics**: 
  - All messages handled correctly
  - No message loss
- ✅ **Observability**:
  - All necessary metrics incremented with correct metadata (tenant, reason)
  - No contradictory metrics (e.g., success + tenant_fail simultaneously)
  - No duplicate metrics for the same event
  - Logs correctly structured with expected information

## Test Implementation Details

### Parallelism

**Approach**: Real parallelism using `spawn` for concurrent message processing

**Synchronization**: ETS tables for tracking parallel operations

**Example**:
```erlang
%% Spawn concurrent message processing
PidA = spawn(fun() -> 
    router_result_consumer:handle_info({nats_message, Subject, ResultAJson, #{}, MsgIdA}, #{})
end),
PidB = spawn(fun() -> 
    router_result_consumer:handle_info({nats_message, Subject, ResultBJson, #{}, MsgIdB}, #{})
end),

%% Wait for completion using ETS synchronization
test_helpers:wait_for_condition(fun() ->
    ets:lookup(SyncTable, msg_a_processed) =/= [] andalso
    ets:lookup(SyncTable, msg_b_processed) =/= []
end, 5000),
```

### Verification Strictness

**Assertions**: Replaced `>= 0` with `> 0` for critical checks

**Examples**:
- `true = AckACount > 0` (ACK must be called)
- `true = NakBCount > 0` (NAK must be called)
- `true = length(TenantErrorLogs) > 0` (Error logs must exist)

### Metric Specificity

**Checks**: Specific metric values and metadata

**Examples**:
- Verify `tenant_id` in metric metadata
- Verify `reason` in metric metadata
- Verify metric counts match expected values

## Test Coverage Summary

| Scenario | Result Consumer | Decide Consumer | Parallelism | Metric Validation |
|----------|----------------|----------------|-------------|-------------------|
| S1: ACK error + validation fail (concurrent) | ✅ | ✅ | ✅ | ✅ |
| S2: ACK error + validation fail (sequential) | ✅ | ✅ | ❌ | ✅ |
| S3: NAK + publish failure recovery | ✅ | ✅ | ❌ | ✅ |
| S4: Batch mixed failures | ✅ | ✅ | ✅ | ✅ |
| Extended: Consumer restart | ✅ | ✅ | ❌ | ✅ |
| Extended: Tenant isolation | ✅ | ✅ | ✅ | ✅ |
| Extended: Final states/idempotency | ✅ | ✅ | ❌ | ✅ |
| Extended: Comprehensive metrics | ✅ | ✅ | ❌ | ✅ |

## Running Tests

### Run all concurrent fault tests
```bash
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
  --case test_ack_error_with_tenant_validation_fail_concurrent
rebar3 ct --suite apps/otp/router/test/router_decide_consumer_SUITE \
  --case test_decide_ack_error_with_tenant_validation_fail_concurrent
```

### Run extended scenarios
```bash
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
  --case test_prolonged_faults_with_consumer_restart
rebar3 ct --suite apps/otp/router/test/router_result_consumer_SUITE \
  --case test_tenant_isolation_during_concurrent_faults
```

## References

- **General Fault Scenarios**: `FAULT_INJECTION_TEST_SCENARIOS.md`
- **Test Suites**: 
  - `apps/otp/router/test/router_result_consumer_SUITE.erl`
  - `apps/otp/router/test/router_decide_consumer_SUITE.erl`
- **Metrics Contract**: `apps/otp/router/docs/dev/METRICS_CONTRACT_SPECIFICATION.md`
- **Test Helpers**: `apps/otp/router/test/test_helpers.erl`

## Change History

**v1.0 (2025-11-30)**:
- Initial documentation
- All basic scenarios (S1-S4) documented
- All extended scenarios documented
- Test coverage summary added

