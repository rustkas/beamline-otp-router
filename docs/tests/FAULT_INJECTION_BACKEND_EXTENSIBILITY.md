# Fault Injection Test Backend Extensibility Guide

## Purpose

This document describes how fault injection tests are structured to support multiple message broker backends (NATS/JetStream, Kafka, Redis Streams, etc.). It identifies backend-specific dependencies and reusable abstractions.

## Current Backend: NATS/JetStream

### Backend-Specific Components

#### 1. Connection Management

**Module**: `router_nats`  
**Functions**:
- `connect/0` - Establish NATS connection
- `disconnect/0` - Close NATS connection
- `is_connected/0` - Check connection status

**Test usage**:
- Mocked in tests via `meck:new(router_nats, [passthrough])`
- Fault injection: Simulate connection loss via mock errors

**Extension**: For new backend, create equivalent module (e.g., `router_kafka`, `router_redis_streams`)

#### 2. Message Publishing

**Module**: `router_nats`  
**Function**: `publish/2` (Subject, Payload)

**Test usage**:
- Mocked to simulate publish failures
- Tracked in ETS tables for verification

**Extension**: For new backend, adapt publish API:
- Kafka: `router_kafka:publish/2` (Topic, Payload)
- Redis Streams: `router_redis_streams:publish/2` (Stream, Payload)

#### 3. Message Acknowledgment

**Module**: `router_jetstream`  
**Functions**:
- `ack/1` - Acknowledge message (Message map)
- `nak/2` - Negative acknowledge (Message map, Reason)

**Test usage**:
- Mocked to simulate ACK/NAK failures
- Delivery count tracking via ETS

**Extension**: For new backend, adapt acknowledgment:
- Kafka: Consumer group commit offsets
- Redis Streams: XACK command

#### 4. Consumer Configuration

**JetStream specific**:
- `MaxDeliver` - Maximum delivery attempts
- `AckPolicy` - Acknowledgment policy (explicit, none, all)
- `DeliverGroup` - Consumer group name
- `Durable` - Durable consumer name

**Test usage**:
- MaxDeliver exhaustion scenarios
- Consumer reconnection scenarios

**Extension**: For new backend, map to equivalent concepts:
- Kafka: `max.poll.records`, consumer group, offset management
- Redis Streams: Consumer group, pending entries, delivery count

#### 5. Subject/Topic Patterns

**NATS/JetStream subjects**:
- `beamline.router.v1.decide` - Decide requests
- `caf.exec.result.v1` - Execution results
- `beamline.router.v1.reply` - Decide replies

**Test usage**:
- Message routing verification
- Subject-based fault injection

**Extension**: For new backend, map to topics/streams:
- Kafka: Topics (e.g., `beamline.router.v1.decide`)
- Redis Streams: Streams (e.g., `beamline:router:v1:decide`)

## Reusable Abstractions

### 1. Test Helpers

**Module**: `test_helpers`  
**Functions**:
- `wait_for_condition/2` - Bounded waits for conditions
- `wait_for_meck_call/4` - Wait for mocked function calls
- `wait_for_metric/4` - Wait for metric events
- `wait_for_log/4` - Wait for log entries
- `wait_for_ets_entry/3` - Wait for ETS entries

**Reusability**: ✅ **Fully reusable** - No backend dependencies

### 2. Fault Injection Patterns

**Pattern**: Mock-based error injection

**Current implementation**:
```erlang
meck:expect(router_nats, publish, fun(Subject, Payload) ->
    case fault_active() of
        true -> {error, connection_lost};
        false -> ok
    end
end)
```

**Reusability**: ✅ **Fully reusable** - Works with any mocked module

**Extension**: For new backend, mock equivalent publish function:
```erlang
meck:expect(router_kafka, publish, fun(Topic, Payload) ->
    case fault_active() of
        true -> {error, broker_unavailable};
        false -> ok
    end
end)
```

### 3. ETS Tracking Patterns

**Pattern**: Track delivery counts, processing state in ETS

**Current implementation**:
```erlang
DeliveryTable = router_decide_delivery_count,
ets:insert(DeliveryTable, {MsgId, Count})
```

**Reusability**: ✅ **Fully reusable** - ETS is backend-agnostic

**Extension**: For new backend, use same ETS pattern:
```erlang
DeliveryTable = router_kafka_delivery_count,
ets:insert(DeliveryTable, {MsgId, Count})
```

### 4. Test Scenario Structure

**Pattern**: Fault period → Recovery → Verification

**Current structure**:
1. Setup mocks and ETS tracking
2. Inject faults (connection loss, ACK errors, etc.)
3. Process messages during fault period
4. Disable faults (recovery)
5. Verify recovery behavior
6. Verify state consistency

**Reusability**: ✅ **Fully reusable** - Scenario structure is backend-agnostic

**Extension**: For new backend, keep same structure, adapt mocks:
1. Setup mocks for new backend modules
2. Inject faults via new backend mocks
3. Process messages during fault period
4. Disable faults (recovery)
5. Verify recovery behavior
6. Verify state consistency

## Extension Guide for New Backend

### Step 1: Identify Backend-Specific Components

**Checklist**:
- [ ] Connection management module
- [ ] Message publishing function
- [ ] Message acknowledgment mechanism
- [ ] Consumer configuration parameters
- [ ] Topic/stream naming patterns

### Step 2: Create Backend Abstraction Module

**Example**: For Kafka

```erlang
-module(router_kafka).

-export([connect/0, disconnect/0, is_connected/0]).
-export([publish/2, subscribe/2, ack/1, nak/2]).

%% Connection management
connect() -> ...
disconnect() -> ...
is_connected() -> ...

%% Message operations
publish(Topic, Payload) -> ...
subscribe(Topic, ConsumerGroup) -> ...
ack(Message) -> ...
nak(Message, Reason) -> ...
```

### Step 3: Adapt Test Mocks

**Example**: Update mocks for Kafka

```erlang
%% Old (NATS):
meck:expect(router_nats, publish, fun(Subject, Payload) -> ... end)

%% New (Kafka):
meck:expect(router_kafka, publish, fun(Topic, Payload) -> ... end)
```

### Step 4: Adapt Test Scenarios

**Example**: Update connection loss scenario

```erlang
%% Old (NATS):
meck:expect(router_nats, connect, fun() -> {error, connection_refused} end)

%% New (Kafka):
meck:expect(router_kafka, connect, fun() -> {error, broker_unavailable} end)
```

### Step 5: Update Documentation

**Files to update**:
- `FAULT_INJECTION_TEST_SCENARIOS.md` - Add backend-specific scenarios
- `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md` - Add backend-specific requirements
- `FAULT_INJECTION_SMOKE_TESTS.md` - Add backend-specific smoke tests
- This document - Add new backend to "Supported Backends" section

## Backend-Specific Test Patterns

### NATS/JetStream Patterns

**Pattern 1: JetStream Consumer Reconnection**
- Test: `test_jetstream_consumer_reconnection`
- Backend-specific: JetStream consumer lifecycle
- Extension: For Kafka, test consumer group rebalancing

**Pattern 2: MaxDeliver Exhaustion**
- Test: `test_max_delivery_count_exhaustion`
- Backend-specific: JetStream MaxDeliver configuration
- Extension: For Kafka, test max retries or DLQ routing

**Pattern 3: NATS Subject Routing**
- Test: Various routing tests
- Backend-specific: NATS subject patterns
- Extension: For Kafka, test topic routing

### Kafka Patterns (Future)

**Pattern 1: Consumer Group Rebalancing**
- Equivalent to: JetStream consumer reconnection
- Test structure: Same (fault period → recovery → verification)
- Adaptations: Mock Kafka consumer group APIs

**Pattern 2: Offset Management**
- Equivalent to: Delivery count tracking
- Test structure: Same (ETS tracking, verification)
- Adaptations: Track offsets instead of delivery counts

**Pattern 3: Topic Partitioning**
- Equivalent to: NATS subject routing
- Test structure: Same (message routing verification)
- Adaptations: Test partition assignment and routing

### Redis Streams Patterns (Future)

**Pattern 1: Consumer Group Management**
- Equivalent to: JetStream consumer reconnection
- Test structure: Same (fault period → recovery → verification)
- Adaptations: Mock Redis Streams consumer group APIs

**Pattern 2: Pending Entries**
- Equivalent to: Delivery count tracking
- Test structure: Same (ETS tracking, verification)
- Adaptations: Track pending entries instead of delivery counts

**Pattern 3: Stream Routing**
- Equivalent to: NATS subject routing
- Test structure: Same (message routing verification)
- Adaptations: Test stream routing instead of subject routing

## Migration Checklist

When adding support for a new backend:

- [ ] Identify backend-specific components
- [ ] Create backend abstraction module
- [ ] Adapt test mocks for new backend
- [ ] Adapt test scenarios for new backend APIs
- [ ] **Update metrics and contract** (see [Metrics Extension](#metrics-extension) below)
- [ ] Update documentation (scenarios, traceability, smoke tests)
- [ ] Verify all tests pass with new backend
- [ ] Add backend-specific smoke tests if needed
- [ ] Update this document with new backend patterns

## Metrics Extension

### Overview

When extending fault injection tests to a new backend, you must also:
1. **Define backend-specific metrics** (if needed)
2. **Update metric contract constants** in test files
3. **Update observability documentation** (alerts, dashboards)
4. **Verify metric contract compliance** in tests

### Step-by-Step: Adding Metrics for New Backend

**Example**: Adding Kafka support with metrics

#### 1. Define Metrics Contract

**Location**: `docs/PROMETHEUS_ALERTS.md`

Add new metrics (if backend-specific):
```yaml
- router_kafka_redelivery_total{assignment_id,request_id,reason,source}: Kafka message redeliveries
- router_kafka_maxdeliver_exhausted_total{assignment_id,request_id,msg_id,delivery_count,max_deliver,reason}: Kafka messages that exceeded MaxDeliver
```

**Or reuse existing metrics** (if behavior is identical):
- Reuse `router_jetstream_redelivery_total` → rename to `router_message_redelivery_total` (generic)
- Reuse `router_jetstream_maxdeliver_exhausted_total` → rename to `router_message_maxdeliver_exhausted_total` (generic)

#### 2. Update Test Contract Constants

**Location**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` (or new suite)

Add/update contract constants:
```erlang
-define(KAFKA_REDELIVERY_REQUIRED_LABELS, [assignment_id, request_id, reason, source]).
-define(KAFKA_REDELIVERY_OPTIONAL_LABELS, [msg_id, delivery_count]).
```

#### 3. Update Metric Validation Tests

**Location**: Test suite file

Add metric validation tests:
- `test_kafka_redelivery_metric_labels/1`
- `test_kafka_maxdeliver_exhausted_metric_labels/1`

Use `assert_metric_labels_by_contract/3` for contract validation.

#### 4. Update Observability Documentation

**Locations**:
- `docs/PROMETHEUS_ALERTS.md` - Add alert definitions
- `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` - Add dashboard queries
- `apps/otp/router/test/FAULT_INJECTION_TEST_SCENARIOS.md` - Add observability links

#### 5. Update Maintenance Process

**Location**: `apps/otp/router/test/FAULT_INJECTION_MAINTENANCE_PROCESS.md`

Ensure Process 5 (Metrics Contract Synchronization) covers new backend metrics.

### Quick Reference

**Metrics extension workflow**:
1. Define contract → `docs/PROMETHEUS_ALERTS.md`
2. Add constants → Test suite file
3. Write validation tests → Test suite file
4. Update observability docs → `PROMETHEUS_ALERTS.md`, `OBSERVABILITY_ROUTER_DASHBOARD.md`
5. Link to fault injection → `FAULT_INJECTION_TEST_SCENARIOS.md`
6. Verify contract → Run metric validation tests

**See also**:
- `FAULT_INJECTION_MAINTENANCE_PROCESS.md` (Process 5: Metrics Contract Synchronization)
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` (example: metric contract constants and validation)

## Supported Backends

### NATS/JetStream

**Status**: ✅ **Fully supported**  
**Test suites**: All fault injection test suites  
**Coverage**: 100% of fault scenarios

### Kafka (Future)

**Status**: ⏳ **Not yet implemented**  
**Test suites**: TBD  
**Coverage**: TBD

### Redis Streams (Future)

**Status**: ⏳ **Not yet implemented**  
**Test suites**: TBD  
**Coverage**: TBD

## References

- `FAULT_INJECTION_TEST_SCENARIOS.md`: Test scenarios documentation
- `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`: Requirements traceability
- `FAULT_INJECTION_MAINTENANCE_PROCESS.md`: Maintenance process
- `apps/otp/router/src/router_nats.erl`: NATS connection module
- `apps/otp/router/src/router_jetstream.erl`: JetStream interaction module
- `apps/otp/router/test/test_helpers.erl`: Test helper functions

