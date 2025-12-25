# R12: Network Partition Scenarios (Single-Instance and Multi-Instance / Split-Brain)

**Date**: 2025-11-30  
**Status**: Specification  
**Purpose**: Comprehensive specification for network partition testing scenarios

## Problem Statement

**Requirement R12**: Verify system behavior under network partitions in two modes:

- **Single-instance** – one service/router instance losing connection to external dependencies (NATS JetStream, other services, DB, etc.)
- **Multi-instance / split-brain** – multiple instances of the same service, some losing connection to others and/or message cluster, potentially causing split-brain

### Current State

- Basic network partition tests exist (`router_network_partition_SUITE.erl`)
- Mock-based simulation available (`router_network_partition.erl`)
- **Missing**: Comprehensive scenarios covering all partition types, detailed validation criteria, and automation scripts

### Problem

System must:
- **Detect and log** network partitions correctly
- **Not violate data guarantees** (no duplicates, losses, state desynchronization, inconsistent side-effects)
- **Recover correctly** after network restoration

## Goal

Create a **comprehensive test suite** that:

1. **Covers all partition scenarios** (single-instance, multi-instance/split-brain)
2. **Validates system behavior** during and after partitions
3. **Provides automation scripts** for fault injection
4. **Documents expected behavior** (logs, metrics, data guarantees)

**Result**: Confidence that under network partitions:
- System detects partitions correctly
- Data guarantees are maintained
- Recovery is correct and complete

## Scope

### Components Involved

- **Router / OTP application** – message processing
- **NATS JetStream / messaging layer** – message broker
- **Consumer services / producers** – microservices, workers, cron jobs
- **State stores** (DB, cache, state-store) – only if involved in processing scenarios

### Dependencies to Simulate as "Unavailable"

- NATS JetStream broker
- External HTTP services (provider APIs)
- Database connections
- Inter-service communication

## Test Scenarios

### 1. Single-Instance Network Partition

**Total Test Cases**: 13 (9 existing + 4 new)

#### 1.1. Loss of Connection to JetStream / Message Broker

**Conditions**:
- 1 service instance running
- Active message processing stream (subscriber / consumer)
- Router is processing messages from JetStream consumer

**Actions**:
- Artificially break network connection between service and broker:
  - **Mock mode**: Use `router_nats_fault_injection` to simulate connection failures
  - **Real mode**: Use `iptables` to drop packets, `tc`/`netem` for delay/loss, or docker network isolation
- Partition duration: Short (5-10 seconds) and Long (2-5 minutes)

**Reproduction Steps**:

**Mock Mode (Default)**:
```bash
# Enable connection fault injection
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
# Wait for partition duration
timer:sleep(5000),  # Short partition
# Disable fault injection
router_nats_fault_injection:disable_fault(connect),
```

**Real Mode (Optional)**:
```bash
# Create partition using script
./test/scripts/r12_network_partition_fault_injection.sh create single_instance router nats --action drop
# Wait for partition duration
sleep 5  # Short partition
# Remove partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Expected Behavior**:

**Detection**:
- ✅ Service detects connection/publish/subscribe errors immediately
- ✅ Clear error messages in logs about connection loss:
  - `[WARN] Network partition detected: Router -> NATS JetStream`
  - `[ERROR] Connection lost to NATS JetStream broker`
- ✅ Metrics reflect partition:
  - `router_nats_connection_failures_total` increments
  - `router_nats_reconnect_attempts_total` increments

**Handler Behavior**:
- ✅ No infinite retries without backoff; exponential backoff applied
- ✅ Circuit breaker opens when threshold reached (if configured)
- ✅ No uncontrolled queue growth in memory
- ✅ Retry attempts logged with backoff delays:
  - `[INFO] Retry attempt 1/3 with exponential backoff`
  - `[INFO] Retry attempt 2/3 with exponential backoff`
- ✅ Circuit breaker state changes logged:
  - `[WARN] Circuit breaker state changed: closed -> open`
  - `[WARN] Circuit breaker open, blocking publish`

**Data Guarantees**:
- ✅ Messages not considered "successfully processed" if side effects (DB write, external API call) not completed
- ✅ No message loss for messages "in transit" (messages queued for retry after reconnect)
- ✅ Idempotency preserved (if idempotency layer is active)
- ✅ No duplicate processing beyond acceptable limits (MaxDeliver semantics respected)

**Recovery**:
- ✅ After network restoration, service reconnects to broker automatically
- ✅ Reconnection logged:
  - `[INFO] Network partition resolved: Router -> NATS JetStream`
  - `[INFO] Reconnecting to NATS JetStream broker`
  - `[INFO] Connection restored successfully`
- ✅ Message processing continues without duplicates (or with acceptable, controlled duplicates if designed)
- ✅ Circuit breaker closes after successful recovery:
  - `[INFO] Circuit breaker closed, normal operation resumed`
- ✅ Successful recovery visible in logs/metrics:
  - `router_nats_connection_restored_total` increments
  - `router_circuit_breaker_state{state="closed"}` = 1
  - `router_network_partition_recovery_time_seconds` tracks recovery time

**Test Cases**:
- `test_single_instance_jetstream_partition_short` – short partition (5-10 seconds)
- `test_single_instance_jetstream_partition_long` – long partition (2-5 minutes)
- `test_single_instance_jetstream_partition_recovery` – recovery validation
- `test_single_instance_latency_degradation` – latency degradation (100ms → 5s)
- `test_single_instance_partial_packet_loss` – partial packet loss (30%)
- `test_single_instance_intermittent_connectivity` – intermittent connectivity (2s on, 1s off)
- `test_single_instance_slow_network` – slow network (3s delay)

#### 1.2. Loss of Connection to External Services (DB / API)

**Conditions**:
- 1 instance
- Service processing request stream, accessing DB or external HTTP service
- Router is calling external provider APIs or accessing database

**Actions**:
- Simulate DB or external API unavailability:
  - **Mock mode**: Use `meck` to mock HTTP client or DB connection failures
  - **Real mode**: Use `iptables` to drop packets to external service, or `tc`/`netem` for delay/timeout
- Partition duration: Short (5-10 seconds) and Long (2-5 minutes)

**Reproduction Steps**:

**Mock Mode (Default)**:
```bash
# Mock HTTP client to return connection errors
meck:expect(httpc, request, fun(_) -> {error, connection_refused} end),
# Or mock DB connection to timeout
meck:expect(db_connection, query, fun(_) -> {error, timeout} end),
# Wait for partition duration
timer:sleep(5000),  # Short partition
# Restore mocks
meck:unload(httpc),
meck:unload(db_connection),
```

**Real Mode (Optional)**:
```bash
# Create partition to external service
./test/scripts/r12_network_partition_fault_injection.sh create single_instance router external-api --action drop
# Wait for partition duration
sleep 5  # Short partition
# Remove partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Expected Behavior**:

**Detection**:
- ✅ Connection errors detected and logged:
  - `[ERROR] Connection lost to external service: <service_name>`
  - `[ERROR] Database connection timeout`
- ✅ Timeout/error handling correct (no crashes, graceful degradation)
- ✅ Metrics reflect failures:
  - `router_external_service_errors_total{service="<service_name>", error_type="connection_refused"}` increments

**Handler Behavior**:
- ✅ Retry logic applied (if configured) with exponential backoff
- ✅ Circuit breaker opens for external service (if configured)
- ✅ No resource leaks (connections properly closed, no memory growth)

**Data Guarantees**:
- ✅ **No inconsistent partial operations**:
  - Message not ack'd if DB write not completed
  - Message not ack'd if external API call not completed
  - Transaction semantics preserved (if applicable)
- ✅ Messages queued for retry after service restoration
- ✅ No message loss (messages remain in queue until processed)

**Recovery**:
- ✅ After availability restoration – correct continuation of work
- ✅ Reconnection logged:
  - `[INFO] Connection restored to external service: <service_name>`
- ✅ Queued messages processed successfully
- ✅ No duplicate processing (idempotency preserved)

**Test Cases**:
- `test_single_instance_external_service_partition_short` – short partition (5-10 seconds)
- `test_single_instance_external_service_partition_long` – long partition (2-5 minutes)
- `test_single_instance_external_service_partition_recovery` – recovery validation

#### 1.3. Short and Long Partitions

For each scenario above (1.1 and 1.2), test both:

**Short Partition** (5-10 seconds):
- ✅ System detects partition quickly
- ✅ Retry attempts start immediately
- ✅ Recovery is fast after partition resolution
- ✅ No resource leaks during short partition
- ✅ No hangs or deadlocks

**Long Partition** (2-5 minutes):
- ✅ System remains stable during long partition
- ✅ No resource leaks (memory, file descriptors, connections)
- ✅ No uncontrolled memory growth
- ✅ Retry attempts respect backoff limits (no retry storms)
- ✅ Circuit breaker remains open (if threshold reached)
- ✅ Recovery is correct after long partition resolution
- ✅ No message loss during long partition
- ✅ State consistency maintained

**Checks for Both**:
- ✅ Process liveness: `is_process_alive(RouterPid)` = true
- ✅ Memory usage: No unbounded growth (check with `erlang:memory()`)
- ✅ Process count: No unbounded process creation
- ✅ Connection cleanup: No zombie connections
- ✅ Metrics accuracy: Metrics reflect actual state

### 2. Multi-Instance / Split-Brain

#### 2.1. Service Cluster Partition (Split-Brain at Application Level)

**Conditions**:
- N ≥ 2 instances of same application running (in cluster mode, or as independent consumers of same queue)
- Instances are part of a consumer group (JetStream consumer with same consumer name)
- If leader election exists: Multiple instances participate in leader election

**Scenarios**:

**Scenario A: Some instances lose connection to message broker, others continue**
- Instance A loses connection to JetStream
- Instance B continues processing traffic
- Expected: Instance A stops processing, Instance B continues

**Scenario B: Some instances lose connection to each other (if cluster interaction/leader election exists)**
- Instance A and Instance B lose connection to each other
- Both can still reach JetStream
- Expected: Leader election triggered, only one leader active

**Reproduction Steps**:

**Mock Mode (Default)**:
```bash
# Simulate multiple instances (via mocks)
# Instance A: Inject connection fault
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
# Instance B: No faults (continues normally)
# Wait for partition
timer:sleep(5000),
# Restore connection
router_nats_fault_injection:disable_fault(connect),
```

**Real Mode (Optional)**:
```bash
# Create partition between instance groups
./test/scripts/r12_network_partition_fault_injection.sh create multi_instance router-group-1 router-group-2 --action drop
# Wait for partition
sleep 5
# Remove partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Expected Behavior**:

**If leader election mechanism used**:
- ✅ **No two leaders simultaneously** (split-brain leader):
  - Leader election triggered: `[WARN] Leader election triggered`
  - Split-brain detected: `[ERROR] Split-brain detected, multiple leaders`
  - Only one leader active at a time
- ✅ On quorum loss, leader correctly resigns leadership / stops critical operations:
  - `[WARN] Quorum lost, stopping critical operations`
  - Leader stops processing critical operations
  - Metrics reflect quorum loss: `router_quorum_lost_total` increments

**If instances compete for same resources** (lock, topic, stream):
- ✅ **No "double processing"** of same message with incorrect side-effects:
  - Idempotency layer prevents duplicate processing
  - MaxDeliver semantics respected (no infinite redelivery)
  - Delivery count tracking prevents duplicate ACKs
- ✅ After network restoration, cluster state becomes consistent:
  - One leader elected: `[INFO] Single leader elected`
  - Locks/sessions synchronized
  - State consistency restored: `[INFO] Cluster state consistency restored`

**Metrics**:
- ✅ `router_leader_election_total{reason="partition_detected"}` increments
- ✅ `router_split_brain_detected_total` increments (if split-brain occurs)
- ✅ `router_quorum_lost_total` increments (if quorum lost)

**Test Cases**:
- `test_multi_instance_split_brain_leader_election` – leader election during partition
- `test_multi_instance_split_brain_no_duplicate_processing` – no duplicate message processing
- `test_multi_instance_split_brain_recovery` – recovery after partition

#### 2.2. Partition of Access to JetStream / Storage

**Conditions**:
- Multiple instances subscribed to same stream/consumer (JetStream consumer group)
- Instances share the same consumer name (load balancing)
- Active message processing on all instances

**Scenarios**:

**Scenario A: Instance A loses access to JetStream, Instance B continues processing traffic**
- Instance A: Loses connection to JetStream broker
- Instance B: Continues processing messages from JetStream
- Expected: Instance A stops receiving messages, Instance B continues processing

**Scenario B: JetStream cluster itself in partial partition state (one node separated from others)**
- JetStream cluster node 1: Separated from cluster
- JetStream cluster node 2, 3: Continue operating
- Router instances: Some connected to node 1, others to nodes 2,3
- Expected: Instances connected to node 1 cannot process, others continue

**Reproduction Steps**:

**Mock Mode (Default)**:
```bash
# Instance A: Inject connection fault
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
# Instance B: No faults (continues normally)
# Wait for partition
timer:sleep(5000),
# Restore connection
router_nats_fault_injection:disable_fault(connect),
```

**Real Mode (Optional)**:
```bash
# Create partition: Instance A -> JetStream
./test/scripts/r12_network_partition_fault_injection.sh create multi_instance router-instance-a nats-jetstream --action drop
# Wait for partition
sleep 5
# Remove partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Expected Behavior**:

**During Partition**:
- ✅ Messages don't "get stuck" on unavailable node:
  - Instance A stops receiving messages (connection lost)
  - Instance B continues processing messages (load balancing works)
  - JetStream redelivers messages from Instance A to Instance B (if applicable)
- ✅ Instance A logs connection loss:
  - `[ERROR] Connection lost to NATS JetStream broker`
  - `[WARN] Network partition detected: Router Instance A -> NATS JetStream`
- ✅ Instance B continues processing normally (no impact)

**After Recovery**:
- ✅ After A returns to network, it correctly synchronizes:
  - Reconnects to JetStream: `[INFO] Reconnecting to NATS JetStream broker`
  - Continues from current offset/sequence (JetStream consumer state preserved)
  - Or recreates subscription according to protocol (if consumer state lost)
- ✅ No uncontrolled duplicate spike on recovery:
  - Idempotency layer prevents duplicate processing
  - MaxDeliver semantics respected
  - Delivery count tracking prevents duplicate ACKs
- ✅ Load balancing restored:
  - Both instances process messages (load balanced)
  - No message loss
  - State consistency maintained

**Metrics**:
- ✅ `router_nats_connection_failures_total{instance="instance-a"}` increments
- ✅ `router_nats_connection_restored_total{instance="instance-a"}` increments after recovery
- ✅ `router_jetstream_consumer_sync_total` increments (if consumer sync occurs)

**Test Cases**:
- `test_multi_instance_jetstream_partition_instance_a_isolated` – instance A isolated
- `test_multi_instance_jetstream_partition_jetstream_cluster_split` – JetStream cluster split
- `test_multi_instance_jetstream_partition_recovery` – recovery validation

#### 2.3. Split-Brain with Locks / Distributed Transactions

**Conditions**:
- Service uses **distributed locks, transactions, or consistent storage**
- Multiple instances compete for same resources (locks, distributed state)
- Network partition occurs between instances

**Scenarios**:

**Scenario A: Distributed Locks During Partition**
- Instance A holds lock on resource X
- Instance B also tries to acquire lock on resource X
- Network partition occurs: Instance A and Instance B cannot communicate
- Expected: Only one instance holds lock, locks expire correctly

**Scenario B: Distributed Transactions During Partition**
- Instance A starts transaction on resource Y
- Instance B also starts transaction on resource Y
- Network partition occurs
- Expected: Transactions don't conflict, one "wins" after partition resolution

**Reproduction Steps**:

**Mock Mode (Default)**:
```bash
# Simulate distributed lock scenario
# Instance A: Holds lock
# Instance B: Tries to acquire lock
# Create partition between instances
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
# Wait for partition
timer:sleep(5000),
# Restore connection
router_nats_fault_injection:disable_fault(connect),
```

**Real Mode (Optional)**:
```bash
# Create partition between instances
./test/scripts/r12_network_partition_fault_injection.sh create multi_instance router-instance-a router-instance-b --action drop
# Wait for partition
sleep 5
# Remove partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Expected Behavior**:

**On network partition**:
- ✅ Invariants not violated (e.g., two owners of same resource):
  - Only one instance holds lock at a time
  - Lock expiration works correctly (locks expire after timeout)
  - No "double ownership" of resources
- ✅ On connection loss/expiration, locks/sessions correctly expire or ownership transferred:
  - Locks expire after timeout: `[WARN] Lock expired due to partition`
  - Sessions expire correctly
  - Ownership transferred to available instance (if applicable)
- ✅ Logs reflect lock behavior:
  - `[WARN] Lock acquisition failed due to partition`
  - `[WARN] Lock expired, releasing resource`

**After network restoration**:
- ✅ No conflicting states (conflict records, "winner" of changes determined):
  - Conflicts detected: `[WARN] Conflict detected after partition resolution`
  - "Winner" determined (e.g., last-write-wins, timestamp-based)
  - State consistency restored: `[INFO] Distributed state consistency restored`
- ✅ Locks/sessions reacquired correctly:
  - `[INFO] Lock reacquired after partition resolution`
  - `[INFO] Session restored after partition resolution`
- ✅ No resource leaks:
  - All locks released
  - All sessions cleaned up
  - No orphaned resources

**Metrics**:
- ✅ `router_distributed_lock_conflicts_total` increments (if conflicts occur)
- ✅ `router_distributed_lock_expirations_total` increments (if locks expire)
- ✅ `router_distributed_state_conflicts_total` increments (if state conflicts occur)

**Test Cases**:
- `test_multi_instance_distributed_locks_partition` – distributed locks during partition
- `test_multi_instance_distributed_locks_recovery` – lock recovery after partition
- `test_multi_instance_distributed_transactions_partition` – distributed transactions during partition (if applicable)

## Acceptance Criteria

### 1. Scenario Documentation

**List of all scenarios** (single-instance and multi-instance / split-brain) with:
- Conditions (topology, number of instances, dependencies)
- Reproduction steps (how exactly network is broken)
- Expected behavior (by points: logs, metrics, impact on data, recovery)

### 2. Automation / Scripts

**Set of scripts / instructions** for reproduction:
- For local environment (docker-compose / k8s / bare metal – as per your environment)
- For CI/test stand (if possible)

**Scripts must be able to**:
- Create required number of instances
- Apply network rules (break, delay, packet loss)
- Restore network to original state

### 3. Logging and Metrics

**Defined minimal set** of log messages and metrics:
- Event of connection loss
- Event of successful recovery
- Number of retries/errors
- For split-brain – role change (leader/follower), quorum loss

**Verified** that this information actually appears in logs/metrics during tests.

### 4. Results Report

**Brief report** for each scenario:
- How reproduced
- What observed (actual behavior)
- What deviations from expected
- What improvements required (if problems found)

## Implementation Details

### Fault Injection Methods

#### Mock Mode (Default)

- Uses `meck` to simulate network partitions
- Works without root privileges
- Suitable for CI/CD pipelines
- Tests verify behavior via mocked NATS operations

#### Real Network Tools Mode (Optional)

- Uses `iptables`, `tc`, `netem` for real network manipulation
- Requires root privileges or container capabilities
- Enable via: `NETWORK_PARTITION_REAL=true`
- Suitable for local development and dedicated test environments

### Network Partition Actions

- **drop** – Drop packets (simulate network failure)
- **delay** – Add delay to packets (simulate high latency)
- **reject** – Reject packets (simulate connection refused)
- **loss** – Packet loss percentage (simulate packet loss)

### Partition Types

- **single_instance** – Single service instance isolated
- **multi_instance** – Multiple instances with split-brain
- **service_broker** – Service instances isolated from broker
- **flapping** – Periodic connect/disconnect cycles

## Expected Logs

### During Partition

```
[WARN] Network partition detected: Router -> NATS JetStream
[WARN] Connection lost to NATS JetStream broker
[INFO] Retry attempt 1/3 with exponential backoff
[WARN] Circuit breaker opened for NATS connection
```

### After Recovery

```
[INFO] Network partition resolved: Router -> NATS JetStream
[INFO] Reconnecting to NATS JetStream broker
[INFO] Connection restored successfully
[INFO] Circuit breaker closed, normal operation resumed
```

## Expected Metrics

### During Partition

- `router_nats_connection_failures_total` – Connection failure counter
- `router_nats_reconnect_attempts_total` – Reconnect attempt counter
- `router_circuit_breaker_state{state="open"}` – Circuit breaker state
- `router_network_partition_duration_seconds` – Partition duration

### After Recovery

- `router_nats_connection_restored_total` – Connection restored counter
- `router_circuit_breaker_state{state="closed"}` – Circuit breaker closed
- `router_network_partition_recovery_time_seconds` – Recovery time

## Test Execution

### Local Execution

```bash
# Run all network partition tests
cd apps/otp/router
rebar3 ct --suite router_network_partition_SUITE

# Run specific test group
rebar3 ct --suite router_network_partition_SUITE --group single_instance_tests
rebar3 ct --suite router_network_partition_SUITE --group multi_instance_tests

# Run single test
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_short
```

### CI/CD Execution

Tests are integrated into CI/CD pipelines:
- **Tags**: `@test_category network_partition, slow, integration, fault_injection`
- **Execution Time**: ~15-20 minutes (all tests)
- **Resource Requirements**: ~500 MB memory per test

## References

- **Test Suite**: `router_network_partition_SUITE.erl`
- **Network Partition Manager**: `router_network_partition.erl`
- **Quick Start Guide**: `README_NETWORK_PARTITION.md`
- **Complete Guide**: `docs/archive/dev/NETWORK_PARTITION_TESTING.md`
- **Fault Injection Tests**: `docs/archive/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`

