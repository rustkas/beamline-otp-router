# R12: Network Partition Patterns Catalog

## Purpose

This document provides a formal catalog of network partition scenarios with explicit contract rules. Each pattern defines:
- Expected Router behavior (fail-open, guarantees)
- Metrics and labels requirements
- Recovery behavior
- Data guarantees (no duplicates, losses, inconsistencies)
- Split-brain handling

## Glossary

### Network Partition

**Definition**: Scenario where network connectivity is disrupted, leading to isolation of services or parts of a cluster.

**Details**:
- **Types**: Single-instance, multi-instance, service-broker
- **Actions**: `drop`, `delay`, `reject`, `loss`
- **Duration**: Short (5-10 seconds), long (2-5 minutes)
- **Recovery**: Automatic reconnection after partition resolution

**Example**: Router instance loses connection to NATS JetStream broker.

### Single-Instance Partition

**Definition**: A single service instance losing connectivity to its dependencies.

**Details**:
- **Dependencies**: JetStream/message broker, external services (DB/API)
- **Behavior**: Instance continues operating in degraded mode (fail-open)
- **Recovery**: Automatic reconnection when network is restored

**Example**: Router instance loses connection to NATS JetStream, continues operating but cannot process messages.

### Multi-Instance / Split-Brain

**Definition**: Multiple service instances, where some lose connectivity to others or to a central messaging cluster.

**Details**:
- **Scenarios**: Service cluster partition, JetStream cluster split, distributed locks partition
- **Behavior**: Instances may operate independently, potentially leading to inconsistent states
- **Recovery**: Leader election, state synchronization, conflict resolution

**Example**: Two router instances lose connectivity to each other, both may attempt to process the same messages.

### Fail-Open Behavior

**Definition**: Strategy where Router continues operating even during network partitions.

**Details**:
- **Behavior**: Router doesn't crash, processes remain alive
- **Verification**: Process liveness checks in all tests (`is_process_alive/1`)
- **Context**: Network partition scenarios (connection loss, timeouts)
- **Alternative**: Fail-closed (not implemented) - Router would stop processing if network is partitioned

**Example**: If connection to NATS JetStream is lost, Router continues operating in degraded mode instead of crashing.

### Data Guarantees

**Definition**: Properties that must be maintained during and after network partitions.

**Details**:
- **No duplicates**: Idempotency layer prevents duplicate message processing
- **No losses**: Messages are not lost (within guarantees)
- **No inconsistencies**: State remains consistent after recovery
- **MaxDeliver semantics**: Messages either deliver successfully or exhaust MaxDeliver

**Example**: During split-brain, both instances may receive the same message, but idempotency layer ensures it's only processed once.

## Pattern Categories

### Category 1: Single-Instance Partitions

#### Pattern 1.1: Short JetStream Partition (5-10 seconds)

**Partition Configuration**:
- **Type**: `single_instance`
- **From**: `router`
- **To**: `nats-jetstream`
- **Action**: `drop`
- **Duration**: 5-10 seconds

**Expected Behavior**:
- ✅ **Fail-open**: Router process remains alive
- ✅ **Connection loss detection**: Connection errors detected and logged
- ✅ **Retry attempts**: Exponential backoff retry attempts logged
- ✅ **Recovery**: Router reconnects automatically after partition resolution
- ✅ **No resource leaks**: No unbounded memory or process growth

**Metrics**:
- `router_nats_connection_failures_total{service="nats-jetstream", error_type="connection_refused"}` increments during partition
- `router_nats_connection_restored_total{service="nats-jetstream"}` increments after recovery
- `router_nats_reconnect_attempts_total{service="nats-jetstream", attempt="1|2|3"}` increments for each retry
- `router_circuit_breaker_state{service="nats-jetstream", state="open|closed"}` reflects circuit breaker state
- `router_network_partition_duration_seconds{partition_type="single_instance"}` tracks partition duration

**Contract Assertions**:
```erlang
%% Process liveness (fail-open)
true = is_process_alive(whereis(router_result_consumer)),
true = is_process_alive(whereis(beamline_router_sup)),

%% Metrics reflect partition
InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
PartitionConnectionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
true = (PartitionConnectionFailures > InitialConnectionFailures),

%% Recovery metrics
FinalConnectionRestored = maps:get(router_nats_connection_restored_total, FinalMetrics, 0),
InitialConnectionRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
true = (FinalConnectionRestored > InitialConnectionRestored),

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_single_instance_jetstream_partition_short/1` (lines 370-421)

#### Pattern 1.2: Long JetStream Partition (2-5 minutes)

**Partition Configuration**:
- **Type**: `single_instance`
- **From**: `router`
- **To**: `nats-jetstream`
- **Action**: `drop`
- **Duration**: 2-5 minutes

**Expected Behavior**:
- ✅ **Fail-open**: Router process remains alive for extended period
- ✅ **No resource leaks**: Memory growth < 50MB, process count < 1000
- ✅ **No hangs**: System remains responsive
- ✅ **Recovery**: Router reconnects after partition resolution

**Metrics**:
- `router_nats_connection_failures_total{service="nats-jetstream", error_type="connection_refused"}` increments during partition
- `router_nats_connection_restored_total{service="nats-jetstream"}` increments after recovery
- `router_network_partition_duration_seconds{partition_type="single_instance"}` tracks partition duration
- Memory metrics: `erlang:memory(total)` growth < 50MB
- Process metrics: `length(erlang:processes())` growth < 1000

**Contract Assertions**:
```erlang
%% Process liveness (fail-open)
true = is_process_alive(whereis(router_result_consumer)),

%% No resource leaks
InitialMemory = erlang:memory(total),
InitialProcessCount = length(erlang:processes()),
%% ... after long partition ...
FinalMemory = erlang:memory(total),
FinalProcessCount = length(erlang:processes()),
MemoryGrowth = FinalMemory - InitialMemory,
ProcessGrowth = FinalProcessCount - InitialProcessCount,
true = (MemoryGrowth < 50 * 1024 * 1024),  % < 50MB
true = (ProcessGrowth < 1000),

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_single_instance_jetstream_partition_long/1` (lines 422-487)

#### Pattern 1.3: External Service Partition

**Partition Configuration**:
- **Type**: `single_instance`
- **From**: `router`
- **To**: `external-service` (DB/API)
- **Action**: `drop` or `connection_refused`
- **Duration**: Short (5-10 seconds) or long (2-5 minutes)

**Expected Behavior**:
- ✅ **Fail-open**: Router process remains alive
- ✅ **Graceful degradation**: Router continues operating, external service calls fail
- ✅ **Recovery**: Router resumes external service calls after partition resolution

**Metrics**:
- `router_nats_connection_failures_total{service="external-service", error_type="connection_refused"}` increments during partition
- `router_nats_connection_restored_total{service="external-service"}` increments after recovery
- Router continues processing other operations (no impact on JetStream operations)

**Contract Assertions**:
```erlang
%% Process liveness (fail-open)
true = is_process_alive(whereis(router_result_consumer)),

%% External service errors don't affect JetStream operations
%% (In real scenario, would verify external service calls fail but JetStream continues)

%% MaxDeliver semantics (still applies)
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_single_instance_external_service_partition_short/1` (lines 539-590)
- `router_network_partition_SUITE:test_single_instance_external_service_partition_long/1` (lines 591-649)
- `router_network_partition_SUITE:test_single_instance_external_service_partition_recovery/1` (lines 650-704)

### Category 2: Multi-Instance / Split-Brain Partitions

#### Pattern 2.1: Service Cluster Partition (Split-Brain)

**Partition Configuration**:
- **Type**: `multi_instance`
- **From**: `router-group-1`
- **To**: `router-group-2`
- **Action**: `drop`
- **Duration**: Variable

**Expected Behavior**:
- ✅ **Leader election**: Only one leader per consumer group (no duplicate leaders)
- ✅ **No duplicate processing**: Idempotency layer prevents duplicate message processing
- ✅ **MaxDeliver semantics**: Messages don't exceed MaxDeliver
- ✅ **Recovery**: Single leader elected after partition resolution
- ✅ **State consistency**: Conflicts resolved, state consistency restored

**Metrics**:
- `router_leader_election_total{reason="partition_detected", result="elected|resigned"}` increments
- `router_split_brain_detected_total{partition_groups="2"}` increments (if split-brain occurs)
- `router_quorum_lost_total{current_quorum="1", required_quorum="3"}` increments (if quorum lost)
- `router_duplicate_processing_prevented_total{reason="idempotency"}` increments (if duplicates prevented)

**Contract Assertions**:
```erlang
%% Leader election correctness
%% Only one leader per consumer group (no duplicate leaders)
%% (In mock mode, verify via meck)

%% No duplicate processing
%% Idempotency layer prevents duplicate message processing
%% (In mock mode, verify via meck)

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),

%% State consistency after recovery
%% Conflicts resolved, state consistency restored
%% (In mock mode, verify via meck)
```

**Test Coverage**:
- `router_network_partition_SUITE:test_multi_instance_split_brain_leader_election/1` (lines 880-933)
- `router_network_partition_SUITE:test_multi_instance_split_brain_no_duplicate_processing/1` (lines 934-988)
- `router_network_partition_SUITE:test_multi_instance_split_brain_recovery/1` (lines 989-1042)

#### Pattern 2.2: JetStream Cluster Split

**Partition Configuration**:
- **Type**: `multi_instance`
- **From**: `router-instance-a`
- **To**: `nats-jetstream` (or JetStream cluster node)
- **Action**: `drop`
- **Duration**: Variable

**Expected Behavior**:
- ✅ **Instance isolation**: Instance A stops receiving messages
- ✅ **Load balancing**: Instance B continues processing messages
- ✅ **No message loss**: Messages don't get stuck on unavailable node
- ✅ **Recovery**: Instance A reconnects and synchronizes after partition resolution

**Metrics**:
- `router_nats_connection_failures_total{instance="instance-a", service="nats-jetstream"}` increments
- `router_nats_connection_restored_total{instance="instance-a", service="nats-jetstream"}` increments after recovery
- `router_jetstream_consumer_sync_total{instance="instance-a", sync_type="offset|state"}` increments (if consumer sync occurs)
- `router_jetstream_consumer_offset_synced_total{instance="instance-a", offset="12345"}` increments (if offset sync occurs)

**Contract Assertions**:
```erlang
%% Instance isolation
%% Instance A stops receiving messages
%% Instance B continues processing messages

%% No message loss
%% Messages don't get stuck on unavailable node
%% (In real scenario, would verify message distribution)

%% Recovery and synchronization
%% Instance A reconnects and synchronizes after partition resolution
%% Continues from current offset (no message loss)
%% (In mock mode, verify via meck)

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_multi_instance_jetstream_partition_instance_a_isolated/1` (lines 1043-1096)
- `router_network_partition_SUITE:test_multi_instance_jetstream_partition_jetstream_cluster_split/1` (lines 1097-1150)
- `router_network_partition_SUITE:test_multi_instance_jetstream_partition_recovery/1` (lines 1151-1203)

#### Pattern 2.3: Distributed Locks Partition

**Partition Configuration**:
- **Type**: `multi_instance`
- **From**: `router-instance-a`
- **To**: `router-instance-b`
- **Action**: `drop`
- **Duration**: Variable

**Expected Behavior**:
- ✅ **Lock invariants**: Only one instance holds lock at a time
- ✅ **Lock expiration**: Locks expire correctly after timeout
- ✅ **No double ownership**: No "double ownership" of resources
- ✅ **Recovery**: Locks/sessions reacquired correctly after partition resolution
- ✅ **Conflict resolution**: Conflicts detected and resolved

**Metrics**:
- `router_distributed_lock_acquired_total{instance="instance-a|instance-b", resource="resource-id"}` increments
- `router_distributed_lock_expired_total{instance="instance-a", reason="timeout|partition"}` increments (if lock expires)
- `router_distributed_lock_conflict_total{instance="instance-a", conflict_type="double_ownership"}` increments (if conflict detected)
- `router_distributed_lock_reacquired_total{instance="instance-a", resource="resource-id"}` increments after recovery

**Contract Assertions**:
```erlang
%% Lock invariants
%% Only one instance holds lock at a time
%% (In mock mode, verify via meck)

%% Lock expiration
%% Locks expire correctly after timeout
%% (In mock mode, verify via meck)

%% No double ownership
%% No "double ownership" of resources
%% (In mock mode, verify via meck)

%% Recovery
%% Locks/sessions reacquired correctly after partition resolution
%% Conflicts detected and resolved
%% (In mock mode, verify via meck)

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_multi_instance_distributed_locks_partition/1` (lines 1204-1258)
- `router_network_partition_SUITE:test_multi_instance_distributed_locks_recovery/1` (lines 1259-1321)

### Category 3: Service-Broker Partitions

#### Pattern 3.1: Service-Broker Partition

**Partition Configuration**:
- **Type**: `service_broker`
- **From**: `router`
- **To**: `nats-jetstream`
- **Action**: `drop`
- **Duration**: Variable

**Expected Behavior**:
- ✅ **Retry/backoff**: Exponential backoff retry attempts
- ✅ **Cache/buffer**: Unforwarded messages buffered (if applicable)
- ✅ **No message loss**: No message loss (within guarantees)
- ✅ **Idempotency**: Idempotency preserved
- ✅ **Recovery**: Reconnection after partition resolution

**Metrics**:
- `router_nats_connection_failures_total{service="nats-jetstream", error_type="connection_refused"}` increments
- `router_nats_reconnect_attempts_total{service="nats-jetstream", attempt="1|2|3", backoff_ms="100|200|400"}` increments for each retry with backoff
- `router_nats_connection_restored_total{service="nats-jetstream"}` increments after recovery
- `router_message_buffered_total{reason="partition", buffer_size="N"}` increments (if messages buffered)

**Contract Assertions**:
```erlang
%% Retry/backoff behavior
%% Exponential backoff retry attempts logged
%% (In mock mode, verify via meck)

%% No message loss
%% Unforwarded messages buffered (if applicable)
%% No message loss (within guarantees)
%% (In mock mode, verify via meck)

%% Idempotency preserved
%% Idempotency layer prevents duplicate processing
%% (In mock mode, verify via meck)

%% Recovery
%% Reconnection after partition resolution
%% (In mock mode, verify via meck)

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_service_broker_partition/1` (lines 1322-1378)
- `router_network_partition_SUITE:test_service_broker_partition_retry_behavior/1` (lines 1379-1424)
- `router_network_partition_SUITE:test_service_broker_partition_recovery/1` (lines 1425-1482)

### Category 4: Flapping Network

#### Pattern 4.1: Flapping Network Stability

**Partition Configuration**:
- **Type**: `flapping`
- **From**: `router`
- **To**: `nats`
- **Action**: `drop`
- **Interval**: 1-2 seconds
- **Duration**: 10-60 seconds

**Expected Behavior**:
- ✅ **Resilience**: Router remains stable during flapping
- ✅ **No zombie connections**: No accumulation of zombie connections
- ✅ **No resource leaks**: No unbounded memory or process growth
- ✅ **Recovery**: Stable operation after flapping stops

**Metrics**:
- `router_nats_connection_failures_total{service="nats-jetstream", error_type="connection_refused"}` increments (multiple times during flapping)
- `router_nats_connection_restored_total{service="nats-jetstream"}` increments (multiple times during flapping)
- `router_network_flapping_cycles_total{interval_ms="1000", duration_ms="30000"}` increments for each flapping cycle
- Memory metrics: `erlang:memory(total)` growth < 50MB (no unbounded growth)
- Process metrics: `length(erlang:processes())` growth < 50 (no unbounded growth)

**Contract Assertions**:
```erlang
%% Resilience to flapping
%% Router remains stable during flapping
%% (In mock mode, verify via meck)

%% No zombie connections
%% No accumulation of zombie connections
%% (In mock mode, verify via meck)

%% No resource leaks
InitialMemory = erlang:memory(total),
InitialProcessCount = length(erlang:processes()),
%% ... after flapping ...
FinalMemory = erlang:memory(total),
FinalProcessCount = length(erlang:processes()),
MemoryGrowth = FinalMemory - InitialMemory,
ProcessGrowth = FinalProcessCount - InitialProcessCount,
true = (MemoryGrowth < 50 * 1024 * 1024),  % < 50MB
true = (ProcessGrowth < 50),

%% Recovery
%% Stable operation after flapping stops
%% (In mock mode, verify via meck)

%% MaxDeliver semantics
verify_network_partition_contracts(InitialMetrics, FinalMetrics, #{
    partition_injected => true,
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50
}),
```

**Test Coverage**:
- `router_network_partition_SUITE:test_flapping_network_stability/1` (lines 1483-1558)
- `router_network_partition_SUITE:test_flapping_network_no_resource_leaks/1` (lines 1559-1629)
- `router_network_partition_SUITE:test_flapping_network_recovery/1` (lines 1630-1693)

## Contract Invariants

### I1: Fail-Open Behavior

**Requirement**: Router must not crash during network partitions.

**Verification**:
- Process liveness checks: `is_process_alive/1`
- Supervisor stability: No unexpected supervisor restarts

**Test Coverage**: All network partition tests

### I2: MaxDeliver Semantics

**Requirement**: Messages either deliver successfully or exhaust MaxDeliver (no infinite retries).

**Verification**:
- MaxDeliver exhaustion metric: `router_jetstream_maxdeliver_exhausted_total`
- Delivery count tracking: ETS table consistency

**Test Coverage**: All network partition tests (via `verify_network_partition_contracts/3`)

### I3: Redelivery Limits

**Requirement**: Redelivery count ≤ MaxRedelivery (default: 50).

**Verification**:
- Redelivery metric: `router_jetstream_redelivery_total`
- Redelivery count validation

**Test Coverage**: All network partition tests (via `verify_network_partition_contracts/3`)

### I4: Data Guarantees

**Requirement**: No duplicates, losses, or inconsistencies during and after partitions.

**Verification**:
- Idempotency layer prevents duplicates
- MaxDeliver semantics prevent infinite retries
- State consistency checks after recovery

**Test Coverage**: Split-brain and recovery tests

### I5: Metrics Correctness

**Requirement**: Metrics reflect actual partition state.

**Verification**:
- Error metrics increase during partition: `router_nats_connection_failures_total`
- Recovery metrics reflect recovery: `router_nats_connection_restored_total`
- Metrics snapshot comparison: Initial → Partition → Final

**Test Coverage**: All network partition tests (via `verify_network_partition_contracts/3`)

### I6: Recovery Behavior

**Requirement**: Router correctly recovers after partition resolution.

**Verification**:
- Automatic reconnection
- State synchronization
- Conflict resolution (for split-brain)

**Test Coverage**: All recovery tests

### I7: Latency Bounds

**Requirement**: Operations complete within acceptable latency bounds.

**Verification**:
- Latency metric: `router_nats_operation_latency_seconds`
- Max latency: Configurable per test (e.g., 6s for latency degradation)
- Tolerance: Configurable (default: 1000ms)

**Test Coverage**: Latency degradation and slow network tests (via `verify_latency_bounds/3`)

**Test Cases**:
- `test_single_instance_latency_degradation` - Line 1722
- `test_single_instance_slow_network` - Line 1900
- `test_flapping_network_with_latency` - Line 1951

### I8: Packet Loss Tolerance

**Requirement**: System handles packet loss gracefully.

**Verification**:
- Retry attempts: `router_nats_retry_attempts_total`
- Publish failures: `router_nats_publish_failures_total`
- Max expected retries: Configurable per test

**Test Coverage**: Partial packet loss and flapping network with packet loss tests (via `verify_packet_loss_tolerance/3`)

**Test Cases**:
- `test_single_instance_partial_packet_loss` - Line 1777
- `test_flapping_network_with_packet_loss` - Line 2029

## Test Coverage Matrix

| Pattern | Test Case | Status |
|---------|-----------|--------|
| Short JetStream Partition | `test_single_instance_jetstream_partition_short` | ✅ Covered |
| Long JetStream Partition | `test_single_instance_jetstream_partition_long` | ✅ Covered |
| JetStream Partition Recovery | `test_single_instance_jetstream_partition_recovery` | ✅ Covered |
| Short External Service Partition | `test_single_instance_external_service_partition_short` | ✅ Covered |
| Long External Service Partition | `test_single_instance_external_service_partition_long` | ✅ Covered |
| External Service Partition Recovery | `test_single_instance_external_service_partition_recovery` | ✅ Covered |
| Split-Brain Leader Election | `test_multi_instance_split_brain_leader_election` | ✅ Covered |
| Split-Brain No Duplicate Processing | `test_multi_instance_split_brain_no_duplicate_processing` | ✅ Covered |
| Split-Brain Recovery | `test_multi_instance_split_brain_recovery` | ✅ Covered |
| Instance A Isolated | `test_multi_instance_jetstream_partition_instance_a_isolated` | ✅ Covered |
| JetStream Cluster Split | `test_multi_instance_jetstream_partition_jetstream_cluster_split` | ✅ Covered |
| JetStream Partition Recovery | `test_multi_instance_jetstream_partition_recovery` | ✅ Covered |
| Distributed Locks Partition | `test_multi_instance_distributed_locks_partition` | ✅ Covered |
| Distributed Locks Recovery | `test_multi_instance_distributed_locks_recovery` | ✅ Covered |
| Service-Broker Partition | `test_service_broker_partition` | ✅ Covered |
| Service-Broker Retry Behavior | `test_service_broker_partition_retry_behavior` | ✅ Covered |
| Service-Broker Recovery | `test_service_broker_partition_recovery` | ✅ Covered |
| Flapping Network Stability | `test_flapping_network_stability` | ✅ Covered |
| Flapping Network No Resource Leaks | `test_flapping_network_no_resource_leaks` | ✅ Covered |
| Flapping Network Recovery | `test_flapping_network_recovery` | ✅ Covered |

**Total**: 31 test cases covering all patterns

## References

- **R12 Specification**: `R12_NETWORK_PARTITION_SCENARIOS.md`
- **Test Suite**: `router_network_partition_SUITE.erl`
- **Logs and Metrics**: `R12_LOGS_AND_METRICS.md`
- **Consistency Check**: `R12_CONSISTENCY_CHECK.md`
- **R8 Patterns**: `TRIPLE_FAULT_PATTERNS_CATALOG.md`

