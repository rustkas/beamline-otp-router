# Triple-Fault Patterns Catalog

## Purpose

This document provides a formal catalog of triple-fault and extended mixed-pattern scenarios with explicit contract rules. Each pattern defines:
- Expected Router behavior (fail-open, guarantees)
- Metrics and labels requirements
- Redelivery limits
- MaxDeliver semantics
- Cross-tenant isolation rules

## Glossary

### MaxDeliver

**Definition**: JetStream configuration limit on delivery attempts.

**Details**:
- **Default**: 3 attempts (configurable via JetStream consumer configuration)
- **Purpose**: Prevents infinite retries at JetStream level
- **Metric**: `router_jetstream_maxdeliver_exhausted_total` (incremented when limit is reached)
- **When exhausted**: Message transitions to DLQ (Dead Letter Queue) or is dropped
- **Level**: JetStream infrastructure level

**Example**: If MaxDeliver = 3, a message will be attempted at most 3 times before being sent to DLQ.

### MaxRedelivery

**Definition**: Router configuration limit on redelivery attempts.

**Details**:
- **Default**: 50 redeliveries (configurable)
- **Purpose**: Prevents excessive redelivery loops at Router level (safety limit)
- **Metric**: `router_jetstream_redelivery_total` (tracks redelivery count)
- **Relationship**: MaxRedelivery is a safety limit, MaxDeliver is the hard limit
- **Level**: Router application level

**Example**: If MaxRedelivery = 50, Router will attempt redelivery at most 50 times before applying other limits.

**Relationship to MaxDeliver**:
- MaxDeliver is the **hard limit** enforced by JetStream
- MaxRedelivery is a **safety limit** enforced by Router
- If MaxDeliver = 3 and MaxRedelivery = 50, MaxDeliver will be reached first (hard limit)
- MaxRedelivery prevents scenarios where MaxDeliver is high but Router creates excessive redelivery loops

### delivery_count

**Definition**: Number of delivery attempts for a message.

**Details**:
- **Storage**: ETS table `router_delivery_count` (key: `msg_id`, value: count)
- **Increment**: On each redelivery (when message is redelivered by JetStream)
- **Usage**: Determines when MaxDeliver is exhausted
- **Tracking**: Router tracks `delivery_count` for each message to enforce MaxDeliver semantics

**Example**: If a message is redelivered 3 times, `delivery_count` = 3. When `delivery_count` >= MaxDeliver, the message is considered exhausted.

**Relationship to MaxDeliver**:
- `delivery_count` is compared against MaxDeliver to determine exhaustion
- When `delivery_count` >= MaxDeliver, message transitions to final state (DLQ/drop)

### fail-open

**Definition**: Strategy where Router continues operating even if operations fail.

**Details**:
- **Behavior**: Router doesn't crash, processes remain alive
- **Verification**: Process liveness checks in all tests (`is_process_alive/1`)
- **Context**: JetStream operations (ACK, NAK, publish, connect)
- **Alternative**: Fail-closed (not implemented) - Router would stop processing if operations fail

**Note**: Different from "fail-open mode" in NATS publish context (see `router_nats_publish_failure_SUITE.erl`).

**Example**: If ACK fails, Router continues processing other messages instead of crashing.

### Redelivery

**Definition**: Process of re-delivering a message that was not successfully processed.

**Details**:
- **Trigger**: NAK (Negative Acknowledgment) or ACK failure
- **Metric**: `router_jetstream_redelivery_total` (incremented on each redelivery)
- **Limit**: Controlled by MaxRedelivery (Router) and MaxDeliver (JetStream)
- **Purpose**: Ensures at-least-once delivery semantics

**Example**: If tenant validation fails, Router sends NAK, JetStream redelivers the message.

### Cross-Tenant Isolation

**Definition**: Property that faults for one tenant do not affect other tenants.

**Details**:
- **Requirement**: Faults (connection failures, validation errors, etc.) for tenant A must not block or affect tenant B
- **Verification**: Multi-tenant test scenarios with fault injection
- **Metrics**: Tenant-specific metrics must be correctly labeled (`tenant_id` label)

**Example**: If tenant "acme" experiences connection failures, tenant "corp" should continue processing normally.

## Pattern Categories

### Category 1: Triple-Fault Combinations (3 simultaneous faults)

#### Pattern 1.1: Connect + Publish + ACK Failures

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`

**Expected Behavior**:
- ✅ **Fail-open**: Router must not crash, processes remain alive
- ✅ **MaxDeliver semantics**: Messages either deliver successfully or exhaust MaxDeliver (no infinite retries)
- ✅ **Redelivery limits**: Redelivery count ≤ 50 per message (configurable)
- ✅ **Delivery count tracking**: `delivery_count` correctly incremented for each redelivery
- ✅ **Metrics**: Error metrics (`router_nats_connection_lost_total`, `router_nats_publish_failures_total`, `router_nats_ack_failures_total`) reflect actual failures
- ✅ **Cross-tenant isolation**: Faults for one tenant don't affect others

**Contract Assertions**:
```erlang
%% Process liveness
true = is_process_alive(whereis(router_nats)),
true = is_process_alive(whereis(beamline_router_sup)),

%% MaxDeliver exhaustion
MaxDeliverExhaustedDelta =< ExpectedMaxDeliverExhaustion + Tolerance,

%% Redelivery limits
RedeliveryDelta =< MaxExpectedRedelivery,

%% Metrics correctness
FinalConnectionLost > InitialConnectionLost orelse
FinalPublishFailures > InitialPublishFailures orelse
FinalAckFailures > InitialAckFailures
```

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_connect_publish_ack_contract/1`
- `router_stress_soak_SUITE:test_multi_fault_triple_soak/1` (long-running variant)

#### Pattern 1.2: Connect + Validation + NAK/Publish Issues

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `validation`: Tenant validation failures (via mock)
- `nak`: `{error, timeout}` or `publish`: `{error, timeout}`

**Expected Behavior**:
- ✅ **Fail-open**: Router handles validation failures gracefully
- ✅ **NAK/publish issues**: Don't cause infinite loops
- ✅ **Validation failures**: Messages with validation failures are handled correctly (rejected, logged)
- ✅ **Cross-tenant isolation**: Validation failures for one tenant don't affect others

**Contract Assertions**:
```erlang
%% No infinite retry loops
RedeliveryDelta =< MaxExpectedRedelivery,

%% Validation failures are logged
ValidationFailuresLogged = true,

%% Cross-tenant isolation
OtherTenantMessagesProcessed = true
```

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_connect_validation_nak_contract/1`

#### Pattern 1.3: Publish + MaxDeliver Near-Exhaustion + Intermittent ACK

**Fault Configuration**:
- `publish`: `{error, timeout}`
- `maxdeliver`: Near-exhaustion (configured low MaxDeliver or pre-redelivered messages)
- `ack`: `{intermittent, {error, timeout}, 0.5}` (50% probability)

**Expected Behavior**:
- ✅ **MaxDeliver exhaustion**: Handled correctly, messages transition to final state (DLQ/drop)
- ✅ **Intermittent ACK**: Don't cause infinite retries (MaxDeliver limits respected)
- ✅ **MaxDeliver exhaustion metric**: `router_jetstream_maxdeliver_exhausted_total` incremented
- ✅ **Final state transition**: Messages don't remain in retry loop after exhaustion

**Contract Assertions**:
```erlang
%% MaxDeliver exhaustion occurs
MaxDeliverExhaustedDelta >= ExpectedMinExhaustion,

%% No messages stuck in retry loop
NoStuckMessages = true,

%% MaxDeliver exhaustion metric emitted
MaxDeliverExhaustedMetricIncremented = true
```

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_publish_maxdeliver_ack_contract/1`

#### Pattern 1.4: Connect + Publish + MaxDeliver Exhaustion

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `maxdeliver`: Exhaustion (messages reach MaxDeliver limit)

**Expected Behavior**:
- ✅ **MaxDeliver exhaustion**: Occurs correctly under connect/publish failures
- ✅ **Final state transition**: Messages transition to DLQ/drop after exhaustion
- ✅ **No infinite retries**: MaxDeliver limit prevents infinite retries
- ✅ **Recovery**: New messages process correctly after recovery

**Contract Assertions**:
```erlang
%% MaxDeliver exhaustion
MaxDeliverExhaustedDelta >= ExpectedMinExhaustion,

%% Final state reached
MessagesInFinalState = true,

%% Recovery works
NewMessagesProcessedAfterRecovery = true
```

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_connect_publish_maxdeliver_contract/1`

#### Pattern 1.5: ACK + NAK + Publish Failures

**Fault Configuration**:
- `ack`: `{error, timeout}`
- `nak`: `{error, timeout}`
- `publish`: `{error, timeout}`

**Expected Behavior**:
- ✅ **ACK/NAK failures**: Don't cause message loss
- ✅ **Publish failures**: Handled correctly
- ✅ **Redelivery**: Occurs correctly when ACK/NAK fail
- ✅ **No infinite retry loops**: Redelivery limits respected

**Contract Assertions**:
```erlang
%% No message loss
MessagesProcessed = MessagesSent - ExpectedLosses,

%% Redelivery occurs
RedeliveryDelta >= ExpectedMinRedelivery,

%% No infinite retries
RedeliveryDelta =< MaxExpectedRedelivery
```

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_ack_nak_publish_contract/1`

### Category 2: Extended Mixed Patterns (>2 faults, different durations)

#### Pattern 2.1: Intermittent Connect + Persistent Publish Errors

**Fault Configuration**:
- `connect`: `{intermittent, close_connection, 0.5}` (50% probability)
- `publish`: `{error, timeout}` (always fails)

**Expected Behavior**:
- ✅ **Intermittent connect**: Router handles connection flapping correctly
- ✅ **Persistent publish**: Router doesn't waste resources on meaningless retries
- ✅ **Degraded mode**: Router enters degraded mode gracefully
- ✅ **Metrics**: Publish failure metrics reflect persistent failures
- ✅ **Recovery**: Router recovers when publish failures clear

**Contract Rules**:
- Router must not enter retry storm (exponential backoff respected)
- Publish failure rate matches fault probability
- Connection metrics reflect intermittent failures
- No resource leaks from persistent failures

**Test Coverage**:
- `router_stress_soak_SUITE:test_multi_fault_mixed_pattern_soak/1`
- `router_advanced_concurrent_faults_SUITE:test_mixed_intermittent_connect_persistent_publish/1`

#### Pattern 2.2: Intermittent ACK Failures + Persistent Tenant Validation Failures

**Fault Configuration**:
- `ack`: `{intermittent, {error, timeout}, 0.3}` (30% probability)
- `validation`: Persistent tenant validation failures (via mock)

**Expected Behavior**:
- ✅ **Intermittent ACK**: Redelivery occurs correctly, no infinite duplication
- ✅ **Persistent validation**: Validation failures are logged and handled
- ✅ **Cross-tenant isolation**: Validation failures for one tenant don't affect others
- ✅ **Redelivery limits**: Redelivery count within reasonable bounds

**Contract Rules**:
- ACK failure rate matches intermittent probability (30%)
- Validation failures don't cause infinite retries
- Redelivery count ≤ MaxRedelivery per message
- Cross-tenant messages process correctly

**Test Coverage**:
- `router_advanced_concurrent_faults_SUITE:test_mixed_persistent_connect_intermittent_ack/1`

#### Pattern 2.3: Cascading Fault Chains

**Fault Configuration**:
- Sequential chain of different faults (connect → publish → ACK)
- Each fault triggers the next in sequence
- Faults cascade over time (not simultaneous)

**Expected Behavior**:
- ✅ **Cascading handling**: Router handles cascading faults correctly
- ✅ **Recovery after chain**: Router recovers correctly after fault chain completes
- ✅ **No state corruption**: Cascading faults don't cause state corruption
- ✅ **Message semantics**: Message semantics preserved through cascade

**Contract Rules**:
- Cascading faults don't cause infinite retry loops
- Recovery works correctly after fault chain
- No state corruption from cascading faults
- Metrics reflect cascading fault progression

**Test Coverage**:
- `router_stress_soak_SUITE:test_multi_fault_cascading_soak/1`
- `router_advanced_concurrent_faults_SUITE:test_cascading_connect_publish_ack_chain/1`

#### Pattern 2.4: Persistent NATS Lag/Latency + Intermittent Policy Changes

**Fault Configuration**:
- `nats_latency`: Persistent high latency (via delay injection)
- `policy_change`: Intermittent policy changes (via mock)

**Expected Behavior**:
- ✅ **NATS latency**: Router handles high latency gracefully (timeouts, retries)
- ✅ **Policy changes**: Router handles policy changes correctly (cache invalidation, reload)
- ✅ **No state corruption**: Policy changes don't cause state corruption
- ✅ **Performance**: Latency remains within acceptable bounds

**Contract Rules**:
- NATS latency doesn't cause message loss
- Policy changes are applied correctly
- No state corruption from concurrent policy changes
- Performance degradation within acceptable limits

**Test Coverage**:
- Future enhancement (not yet implemented)

**Implementation Task**: See `PATTERN_2_4_IMPLEMENTATION_TASK.md` for detailed implementation requirements and acceptance criteria.

## Contract Invariants Summary

### I1: Fail-Open Behavior

**Rule**: Router must not crash under any fault combination.

**Verification**:
- Process liveness checks: `is_process_alive/1` for critical processes
- No unhandled exceptions
- System remains responsive

**Coverage**: All triple-fault and mixed-pattern tests

### I2: MaxDeliver Semantics

**Rule**: Messages must either deliver successfully or exhaust MaxDeliver (no infinite retries).

**Verification**:
- MaxDeliver exhaustion metric: `router_jetstream_maxdeliver_exhausted_total`
- No messages stuck in retry loop
- Final state transition (DLQ/drop) after exhaustion

**Coverage**: Patterns 1.3, 1.4, and MaxDeliver-related tests

### I3: Redelivery Limits

**Rule**: Redelivery count must be within reasonable bounds (configurable, default ≤50).

**Verification**:
- Redelivery metric: `router_jetstream_redelivery_total`
- Redelivery count per message ≤ MaxRedelivery
- No infinite redelivery loops

**Coverage**: All triple-fault and mixed-pattern tests

### I4: Delivery Count Tracking

**Rule**: `delivery_count` must be correctly tracked and incremented for each redelivery.

**Verification**:
- ETS table consistency checks
- Delivery count increases correctly
- No "eternal" entries in ETS

**Coverage**: ETS consistency tests and triple-fault contract tests

### I5: Metrics and Labels Correctness

**Rule**: Metrics must accurately reflect actual system behavior.

**Verification**:
- Error metrics increase during faults
- Recovery metrics reflect recovery events
- Labels are correct (tenant_id, operation, etc.)

**Coverage**: All triple-fault and mixed-pattern tests

### I6: Cross-Tenant Isolation

**Rule**: Faults for one tenant must not affect other tenants.

**Verification**:
- Multi-tenant test scenarios
- Isolation checks (one tenant's faults don't block others)
- Tenant-specific metrics correctness

**Coverage**: Multi-tenant tests and triple-fault contract tests

## Additional Scenarios

### Category 3: Multi-Tenant and Multi-Stream Scenarios

#### Pattern 3.1: Multi-Tenant Isolation with Triple Faults

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- Multiple tenants: `acme`, `corp`, `startup`

**Expected Behavior**:
- ✅ **Cross-tenant isolation**: Faults for one tenant don't affect others
- ✅ **Tenant-specific metrics**: Metrics are correctly labeled per tenant
- ✅ **No cross-tenant state corruption**: State remains isolated per tenant

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_multi_tenant_isolation/1`

#### Pattern 3.2: Multi-Stream/Subject Isolation with Triple Faults

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- Multiple streams/subjects: Different NATS subjects

**Expected Behavior**:
- ✅ **Cross-stream isolation**: Faults on one stream don't affect others
- ✅ **Stream-specific metrics**: Metrics are correctly labeled per stream
- ✅ **No cross-stream state corruption**: State remains isolated per stream

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_multi_stream_subject/1`

### Category 4: Metrics Degradation and Delayed Operations

#### Pattern 4.1: Metrics Degradation During Triple Faults

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`

**Expected Behavior**:
- ✅ **Metrics accuracy**: Metrics remain accurate even during degradation
- ✅ **No metric corruption**: No incorrect values or corruption
- ✅ **Reflect actual behavior**: Metrics reflect actual system behavior

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_metrics_degradation/1`

#### Pattern 4.2: Delayed ACK/NAK with Triple Faults

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{delay, 5000}` (5 second delay)
- `nak`: `{delay, 3000}` (3 second delay)

**Expected Behavior**:
- ✅ **No message loss**: Delayed ACK/NAK don't cause message loss
- ✅ **Correct redelivery**: Redelivery occurs correctly with delays
- ✅ **No infinite loops**: No infinite retry loops despite delays

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_delayed_ack_nak/1`

### Category 5: Boundary Value Tests

#### Pattern 5.1: MaxDeliver Boundary Values

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- MaxDeliver: Low value (e.g., 3) to test boundary

**Expected Behavior**:
- ✅ **Exact boundary**: MaxDeliver exhaustion occurs at exact boundary
- ✅ **Final state transition**: Messages transition to final state at MaxDeliver limit
- ✅ **No exceed**: No messages exceed MaxDeliver
- ✅ **Metric emitted**: MaxDeliver exhaustion metric emitted correctly

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_maxdeliver_boundary/1`

#### Pattern 5.2: MaxRedelivery Boundary Values

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- MaxRedelivery: Default limit (50)

**Expected Behavior**:
- ✅ **Boundary respected**: Redelivery stops at MaxRedelivery boundary
- ✅ **No infinite loops**: No infinite redelivery loops
- ✅ **Limit respected**: Redelivery count respects MaxRedelivery limit

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_maxredelivery_boundary/1`

### Category 6: Edge-Case Scenarios

#### Pattern 6.1: Triple-Fault with Partial Recovery

**Fault Configuration**:
- `connect`: `{error, connection_refused}` (cleared during partial recovery)
- `publish`: `{error, timeout}` (persists during partial recovery)
- `ack`: `{error, timeout}` (persists during partial recovery)

**Expected Behavior**:
- ✅ **Partial recovery handling**: System handles partial recovery correctly
- ✅ **System stability**: System remains stable during partial recovery
- ✅ **Metrics accuracy**: Metrics reflect partial recovery state
- ✅ **No message loss**: No messages lost during partial recovery

**Contract Rules**:
- Partial recovery doesn't cause system instability
- Metrics accurately reflect partial recovery state
- No message loss during partial recovery transitions
- System continues processing despite partial recovery

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_partial_recovery/1`

#### Pattern 6.2: Triple-Fault with AckPolicy Variations

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- AckPolicy: `explicit`, `none`, `all` (variations)

**Expected Behavior**:
- ✅ **AckPolicy handling**: Different AckPolicy values handled correctly
- ✅ **Triple fault compatibility**: Triple faults work correctly with different AckPolicy
- ✅ **Metrics accuracy**: Metrics reflect AckPolicy-specific behavior

**Contract Rules**:
- Explicit AckPolicy: Requires explicit ACK/NAK, triple faults handled correctly
- None AckPolicy: No acknowledgment required, triple faults don't break behavior
- All AckPolicy: All messages in batch acknowledged, triple faults handled correctly

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_ackpolicy_variations/1`

#### Pattern 6.3: Triple-Fault with DeliverPolicy Variations

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- DeliverPolicy: `all`, `new`, `last` (variations)

**Expected Behavior**:
- ✅ **DeliverPolicy handling**: Different DeliverPolicy values handled correctly
- ✅ **Message delivery**: Message delivery respects DeliverPolicy
- ✅ **Triple fault compatibility**: Triple faults work correctly with different DeliverPolicy

**Contract Rules**:
- All DeliverPolicy: All messages delivered, triple faults handled correctly
- New DeliverPolicy: Only new messages delivered, triple faults don't affect old messages
- Last DeliverPolicy: Last message per subject delivered, triple faults handled correctly

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_deliverpolicy_variations/1`

#### Pattern 6.4: Triple-Fault with Consumer Group Isolation

**Fault Configuration**:
- `connect`: `{error, connection_refused}`
- `publish`: `{error, timeout}`
- `ack`: `{error, timeout}`
- Consumer Groups: `router-results-group`, `router-acks-group`, `router-decide-group`

**Expected Behavior**:
- ✅ **Consumer group isolation**: Faults in one consumer group don't affect others
- ✅ **Group-specific metrics**: Metrics are correctly labeled per consumer group
- ✅ **No cross-group influence**: No cross-group state corruption or blocking

**Contract Rules**:
- Faults in `router-results-group` don't affect `router-acks-group` or `router-decide-group`
- Metrics correctly labeled per consumer group
- No cross-group state corruption
- Each consumer group processes independently

**Test Coverage**:
- `router_triple_fault_contract_SUITE:test_triple_fault_consumer_group_isolation/1`

## Test Coverage Matrix

| Pattern | Contract Test | Stress/Soak Test | Status |
|---------|---------------|------------------|--------|
| 1.1: Connect + Publish + ACK | `test_triple_connect_publish_ack_contract` | `test_multi_fault_triple_soak` | ✅ Covered |
| 1.2: Connect + Validation + NAK | `test_triple_connect_validation_nak_contract` | - | ✅ Covered |
| 1.3: Publish + MaxDeliver + ACK | `test_triple_publish_maxdeliver_ack_contract` | - | ✅ Covered |
| 1.4: Connect + Publish + MaxDeliver | `test_triple_connect_publish_maxdeliver_contract` | - | ✅ Covered |
| 1.5: ACK + NAK + Publish | `test_triple_ack_nak_publish_contract` | - | ✅ Covered |
| 2.1: Intermittent Connect + Persistent Publish | - | `test_multi_fault_mixed_pattern_soak` | ✅ Covered |
| 2.2: Intermittent ACK + Persistent Validation | - | `test_multi_fault_mixed_pattern_soak` | ✅ Covered |
| 2.3: Cascading Fault Chains | - | `test_multi_fault_cascading_soak` | ✅ Covered |
| 2.4: Persistent NATS Latency + Intermittent Policy | - | - | ⏳ Future |
| 3.1: Multi-Tenant Isolation | `test_triple_fault_multi_tenant_isolation` | - | ✅ Covered |
| 3.2: Multi-Stream/Subject Isolation | `test_triple_fault_multi_stream_subject` | - | ✅ Covered |
| 4.1: Metrics Degradation | `test_triple_fault_metrics_degradation` | - | ✅ Covered |
| 4.2: Delayed ACK/NAK | `test_triple_fault_delayed_ack_nak` | - | ✅ Covered |
| 5.1: MaxDeliver Boundary | `test_triple_fault_maxdeliver_boundary` | - | ✅ Covered |
| 5.2: MaxRedelivery Boundary | `test_triple_fault_maxredelivery_boundary` | - | ✅ Covered |
| 6.1: Triple-Fault with Partial Recovery | `test_triple_fault_partial_recovery` | - | ✅ Covered |
| 6.2: Triple-Fault with AckPolicy Variations | `test_triple_fault_ackpolicy_variations` | - | ✅ Covered |
| 6.3: Triple-Fault with DeliverPolicy Variations | `test_triple_fault_deliverpolicy_variations` | - | ✅ Covered |
| 6.4: Triple-Fault with Consumer Group Isolation | `test_triple_fault_consumer_group_isolation` | - | ✅ Covered |
| 3.1: Multi-Tenant Isolation | `test_triple_fault_multi_tenant_isolation` | - | ✅ Covered |
| 3.2: Multi-Stream/Subject Isolation | `test_triple_fault_multi_stream_subject` | - | ✅ Covered |
| 4.1: Metrics Degradation | `test_triple_fault_metrics_degradation` | - | ✅ Covered |
| 4.2: Delayed ACK/NAK | `test_triple_fault_delayed_ack_nak` | - | ✅ Covered |
| 5.1: MaxDeliver Boundary | `test_triple_fault_maxdeliver_boundary` | - | ✅ Covered |
| 5.2: MaxRedelivery Boundary | `test_triple_fault_maxredelivery_boundary` | - | ✅ Covered |
| 6.1: Triple-Fault with Partial Recovery | `test_triple_fault_partial_recovery` | - | ✅ Covered |
| 6.2: Triple-Fault with AckPolicy Variations | `test_triple_fault_ackpolicy_variations` | - | ✅ Covered |
| 6.3: Triple-Fault with DeliverPolicy Variations | `test_triple_fault_deliverpolicy_variations` | - | ✅ Covered |
| 6.4: Triple-Fault with Consumer Group Isolation | `test_triple_fault_consumer_group_isolation` | - | ✅ Covered |

## References

- `router_triple_fault_contract_SUITE.erl`: Contract tests for triple-fault combinations (15 tests)
- `router_stress_soak_SUITE.erl`: Stress/soak tests for long-running scenarios (3 tests)
- `router_advanced_concurrent_faults_SUITE.erl`: Advanced concurrent fault scenarios (3 tests)
- `R8_COVERAGE_MATRIX.md`: Comprehensive coverage matrix (pattern × test-case)
- `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`: Requirements traceability matrix
- `PATTERN_2_4_IMPLEMENTATION_TASK.md`: Implementation task for Pattern 2.4 (Persistent NATS Latency + Intermittent Policy) - Detailed requirements, acceptance criteria, and implementation plan

