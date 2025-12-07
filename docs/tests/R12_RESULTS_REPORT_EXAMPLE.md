# R12: Network Partition Scenarios - Results Report Example

**Date**: 2025-11-30  
**Test Run ID**: r12-test-run-20250127-001  
**Environment**: local  
**Test Mode**: mock

## Executive Summary

Brief summary of test execution:
- Total scenarios tested: 18
- Passed: 16
- Failed: 0
- Warnings: 2

**Overall Status**: ✅ **PASS** (with minor warnings)

All critical network partition scenarios were tested successfully. System demonstrates correct behavior during partitions and proper recovery after partition resolution. Minor warnings relate to log timing delays, which do not affect functionality.

## Test Environment

- **OS**: Linux (WSL2)
- **Network Tools**: mock (meck-based simulation)
- **Router Version**: 1.0.0-dev
- **NATS Version**: mock (simulated)
- **Test Duration**: ~25 minutes (all tests)

## Scenario Results

### 1. Single-Instance Network Partition

#### 1.1. Loss of Connection to JetStream / Message Broker

**Test Case**: `test_single_instance_jetstream_partition_short`

**How Reproduced**:
```bash
# Mock mode (default)
cd apps/otp/router
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_short

# Or using fault injection directly:
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
timer:sleep(5000),
router_nats_fault_injection:disable_fault(connect),
```

**Observed Behavior**:
- ✅ Connection loss detected: 2025-11-30T12:00:01Z
- ✅ Retry attempts logged: 3 attempts with exponential backoff
- ✅ Circuit breaker opened: 2025-11-30T12:00:05Z (after 5 consecutive failures)
- ✅ Process remained alive: yes (fail-open strategy working)
- ✅ No message loss: yes (messages queued for retry)
- ✅ Recovery successful: 2025-11-30T12:00:08Z (3 seconds after partition resolution)

**Deviations from Expected**:
- None

**Improvements Required**:
- None

**Logs**:
```
[2025-11-30T12:00:01Z] [WARN] Network partition detected: Router -> NATS JetStream
[2025-11-30T12:00:01Z] [ERROR] Connection lost to NATS JetStream broker
[2025-11-30T12:00:02Z] [INFO] Retry attempt 1/3 with exponential backoff (100ms)
[2025-11-30T12:00:03Z] [INFO] Retry attempt 2/3 with exponential backoff (200ms)
[2025-11-30T12:00:05Z] [WARN] Circuit breaker state changed: closed -> open
[2025-11-30T12:00:05Z] [WARN] Circuit breaker open, blocking publish
[2025-11-30T12:00:08Z] [INFO] Network partition resolved: Router -> NATS JetStream
[2025-11-30T12:00:08Z] [INFO] Reconnecting to NATS JetStream broker
[2025-11-30T12:00:09Z] [INFO] Connection restored successfully
[2025-11-30T12:00:10Z] [INFO] Circuit breaker closed, normal operation resumed
```

**Metrics**:
- `router_nats_connection_failures_total{service="nats-jetstream", error_type="connection_refused"}`: 5
- `router_nats_reconnect_attempts_total{service="nats-jetstream", attempt="1"}`: 1
- `router_circuit_breaker_state{service="nats-jetstream", state="open"}`: 1 (during partition)
- `router_circuit_breaker_state{service="nats-jetstream", state="closed"}`: 1 (after recovery)
- `router_network_partition_duration_seconds{partition_type="single_instance"}`: 7.0
- `router_network_partition_recovery_time_seconds{partition_type="single_instance"}`: 2.0

---

**Test Case**: `test_single_instance_jetstream_partition_long`

**How Reproduced**:
```bash
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_long
```

**Observed Behavior**:
- ✅ Connection loss detected: 2025-11-30T12:05:00Z
- ✅ Retry attempts logged: 3 attempts, then circuit breaker opened
- ✅ Circuit breaker opened: 2025-11-30T12:05:05Z
- ✅ Process remained alive: yes
- ✅ No message loss: yes
- ✅ No resource leaks: yes (memory growth < 10MB, process count stable)
- ✅ Recovery successful: 2025-11-30T12:05:35Z

**Deviations from Expected**:
- None

**Improvements Required**:
- None

**Metrics**:
- `router_network_partition_duration_seconds{partition_type="single_instance"}`: 30.0
- Memory growth: 8.5 MB (within acceptable limits)
- Process count: 245 → 248 (stable, no leaks)

---

**Test Case**: `test_single_instance_jetstream_partition_recovery`

**How Reproduced**:
```bash
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_recovery
```

**Observed Behavior**:
- ✅ Recovery successful: yes
- ✅ New messages processed: yes (verified via mocks)
- ✅ State consistency: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

#### 1.2. Loss of Connection to External Services (DB / API)

**Test Case**: `test_single_instance_external_service_partition_short`

**How Reproduced**:
```bash
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_external_service_partition_short
```

**Observed Behavior**:
- ✅ Connection errors detected: yes (via mocked httpc)
- ✅ Timeout/error handling: correct (no crashes)
- ✅ No inconsistent partial operations: yes (messages not ack'd if external call fails)
- ✅ Process remained alive: yes
- ✅ Recovery successful: yes

**Deviations from Expected**:
- ⚠️ Minor: Log message delay of ~100ms (not critical)

**Improvements Required**:
- Consider reducing log message delay (non-critical)

**Logs**:
```
[2025-11-30T12:10:00Z] [ERROR] Connection lost to external service: provider-api
[2025-11-30T12:10:00Z] [WARN] External service unavailable, queuing message for retry
[2025-11-30T12:10:05Z] [INFO] Connection restored to external service: provider-api
[2025-11-30T12:10:05Z] [INFO] Queued messages processed successfully
```

---

**Test Case**: `test_single_instance_external_service_partition_long`

**Observed Behavior**:
- ✅ No resource leaks: yes
- ✅ Recovery successful: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

**Test Case**: `test_single_instance_external_service_partition_recovery`

**Observed Behavior**:
- ✅ Recovery successful: yes
- ✅ Queued messages processed: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

### 2. Multi-Instance / Split-Brain

#### 2.1. Service Cluster Partition (Split-Brain)

**Test Case**: `test_multi_instance_split_brain_leader_election`

**How Reproduced**:
```bash
rebar3 ct --suite router_network_partition_SUITE --case test_multi_instance_split_brain_leader_election

# Or using script:
./test/scripts/r12_network_partition_fault_injection.sh create multi_instance router-group-1 router-group-2 --action drop
# Wait for partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Observed Behavior**:
- ✅ Split-brain detected: 2025-11-30T12:15:00Z
- ✅ Leader election triggered: 2025-11-30T12:15:01Z
- ✅ No duplicate processing: yes (idempotency layer working)
- ✅ Single leader after recovery: yes (2025-11-30T12:15:10Z)
- ✅ State consistency restored: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

**Logs**:
```
[2025-11-30T12:15:00Z] [WARN] Network partition detected: router-group-1 <-> router-group-2
[2025-11-30T12:15:01Z] [WARN] Leader election triggered: reason=partition_detected
[2025-11-30T12:15:02Z] [ERROR] Split-brain detected, multiple leaders
[2025-11-30T12:15:02Z] [WARN] Quorum lost, stopping critical operations
[2025-11-30T12:15:10Z] [INFO] Network partition resolved
[2025-11-30T12:15:11Z] [INFO] Single leader elected: router-group-1
[2025-11-30T12:15:12Z] [INFO] Cluster state consistency restored
```

**Metrics**:
- `router_leader_election_total{reason="partition_detected", result="elected"}`: 1
- `router_split_brain_detected_total{partition_groups="2"}`: 1
- `router_quorum_lost_total{current_quorum="1", required_quorum="3"}`: 1

---

**Test Case**: `test_multi_instance_split_brain_no_duplicate_processing`

**Observed Behavior**:
- ✅ No duplicate message processing: yes
- ✅ Idempotency preserved: yes
- ✅ MaxDeliver semantics respected: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

**Test Case**: `test_multi_instance_split_brain_recovery`

**Observed Behavior**:
- ✅ Conflicts resolved: yes
- ✅ State consistency restored: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

#### 2.2. Partition of Access to JetStream / Storage

**Test Case**: `test_multi_instance_jetstream_partition_instance_a_isolated`

**How Reproduced**:
```bash
rebar3 ct --suite router_network_partition_SUITE --case test_multi_instance_jetstream_partition_instance_a_isolated
```

**Observed Behavior**:
- ✅ Instance A isolated: yes (connection lost)
- ✅ Instance B continues: yes (in real scenario)
- ✅ Messages don't get stuck: yes
- ✅ Instance A reconnects: yes
- ✅ Synchronization correct: yes (continues from current offset)

**Deviations from Expected**:
- None

**Improvements Required**:
- None

**Logs**:
```
[2025-11-30T12:20:00Z] [ERROR] Connection lost to NATS JetStream broker (instance A)
[2025-11-30T12:20:00Z] [INFO] Instance B continues processing messages
[2025-11-30T12:20:05Z] [INFO] Reconnecting to NATS JetStream broker (instance A)
[2025-11-30T12:20:06Z] [INFO] Connection restored successfully (instance A)
[2025-11-30T12:20:07Z] [INFO] Consumer synchronized, continuing from offset: 12345
```

---

**Test Case**: `test_multi_instance_jetstream_partition_jetstream_cluster_split`

**Observed Behavior**:
- ✅ Cluster split handled: yes
- ✅ Recovery successful: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

**Test Case**: `test_multi_instance_jetstream_partition_recovery`

**Observed Behavior**:
- ✅ Synchronization correct: yes
- ✅ No duplicate spike: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

#### 2.3. Split-Brain with Locks / Distributed Transactions

**Test Case**: `test_multi_instance_distributed_locks_partition`

**Observed Behavior**:
- ✅ Invariants not violated: yes (only one lock holder)
- ✅ Locks expire correctly: yes (after timeout)
- ✅ No double ownership: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

**Logs**:
```
[2025-11-30T12:25:00Z] [WARN] Network partition detected: router-instance-a <-> router-instance-b
[2025-11-30T12:25:05Z] [WARN] Lock expired due to partition: resource-x
[2025-11-30T12:25:10Z] [INFO] Network partition resolved
[2025-11-30T12:25:11Z] [INFO] Lock reacquired after partition resolution: resource-x
```

---

**Test Case**: `test_multi_instance_distributed_locks_recovery`

**Observed Behavior**:
- ✅ No conflicting states: yes
- ✅ Conflicts resolved: yes (last-write-wins)
- ✅ No resource leaks: yes

**Deviations from Expected**:
- None

**Improvements Required**:
- None

---

## Summary Table

| Scenario | Test Case | Status | Duration | Notes |
|----------|-----------|--------|----------|-------|
| Single-instance JetStream partition (short) | `test_single_instance_jetstream_partition_short` | ✅ Pass | 8s | - |
| Single-instance JetStream partition (long) | `test_single_instance_jetstream_partition_long` | ✅ Pass | 35s | No resource leaks |
| Single-instance JetStream partition recovery | `test_single_instance_jetstream_partition_recovery` | ✅ Pass | 10s | - |
| Single-instance external service partition (short) | `test_single_instance_external_service_partition_short` | ⚠️ Warning | 8s | Minor log delay |
| Single-instance external service partition (long) | `test_single_instance_external_service_partition_long` | ✅ Pass | 35s | - |
| Single-instance external service partition recovery | `test_single_instance_external_service_partition_recovery` | ✅ Pass | 10s | - |
| Multi-instance split-brain | `test_multi_instance_split_brain` | ✅ Pass | 10s | - |
| Multi-instance split-brain leader election | `test_multi_instance_split_brain_leader_election` | ✅ Pass | 10s | - |
| Multi-instance split-brain no duplicate | `test_multi_instance_split_brain_no_duplicate_processing` | ✅ Pass | 10s | - |
| Multi-instance split-brain recovery | `test_multi_instance_split_brain_recovery` | ✅ Pass | 10s | - |
| Multi-instance JetStream partition (instance A) | `test_multi_instance_jetstream_partition_instance_a_isolated` | ✅ Pass | 10s | - |
| Multi-instance JetStream partition (cluster split) | `test_multi_instance_jetstream_partition_jetstream_cluster_split` | ✅ Pass | 10s | - |
| Multi-instance JetStream partition recovery | `test_multi_instance_jetstream_partition_recovery` | ✅ Pass | 10s | - |
| Multi-instance distributed locks partition | `test_multi_instance_distributed_locks_partition` | ✅ Pass | 10s | - |
| Multi-instance distributed locks recovery | `test_multi_instance_distributed_locks_recovery` | ✅ Pass | 10s | - |
| Service-broker partition | `test_service_broker_partition` | ✅ Pass | 10s | - |
| Service-broker partition retry | `test_service_broker_partition_retry_behavior` | ✅ Pass | 13s | - |
| Service-broker partition recovery | `test_service_broker_partition_recovery` | ✅ Pass | 10s | - |
| Flapping network stability | `test_flapping_network_stability` | ✅ Pass | 35s | - |
| Flapping network no leaks | `test_flapping_network_no_resource_leaks` | ✅ Pass | 65s | - |
| Flapping network recovery | `test_flapping_network_recovery` | ✅ Pass | 25s | - |

**Total**: 21 test cases, 19 passed, 0 failed, 2 warnings

## Overall Assessment

### Strengths

1. **Fail-open strategy**: System remains stable during all partition scenarios
2. **Recovery correctness**: All partitions recover correctly without data loss
3. **Resource management**: No memory leaks or unbounded process growth
4. **Idempotency**: Duplicate processing prevented correctly
5. **Circuit breaker**: Opens/closes correctly during partitions
6. **Logging**: Comprehensive logs for all partition events
7. **Metrics**: Metrics accurately reflect partition state

### Weaknesses

1. **Log timing**: Minor delays in log messages (~100ms) for external service partitions (non-critical)
2. **Real network tools**: Limited testing with real network tools (iptables, tc) - mostly mock mode

### Recommendations

1. **Reduce log delay**: Optimize log message timing for external service partitions (low priority)
2. **Real network tools testing**: Add more tests with real network tools (iptables, tc) in dedicated test environment
3. **Long partition duration**: Consider adding tests with longer partition durations (10+ minutes) for stress testing
4. **Multi-instance real testing**: Add tests with real multiple router instances (not just mocks)

## Next Steps

- [x] Fix identified issues (none critical)
- [x] Re-run failed tests (none failed)
- [ ] Optimize log timing (low priority)
- [x] Update documentation (completed)
- [x] Integrate into CI/CD pipeline (completed)

## References

- **Specification**: `R12_NETWORK_PARTITION_SCENARIOS.md`
- **Logs and Metrics**: `R12_LOGS_AND_METRICS.md`
- **Quick Start**: `R12_README.md`
- **Test Suite**: `router_network_partition_SUITE.erl`
- **Fault Injection Scripts**: `scripts/r12_network_partition_fault_injection.sh`, `scripts/r12_network_partition_fault_injection.ps1`

