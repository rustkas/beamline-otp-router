# R12: Network Partition Scenarios - Results Report Template

**Date**: [YYYY-MM-DD]  
**Test Run ID**: [unique-id]  
**Environment**: [local|ci|test-stand]  
**Test Mode**: [mock|real]

## Executive Summary

Brief summary of test execution:
- Total scenarios tested: [N]
- Passed: [N]
- Failed: [N]
- Warnings: [N]

## Test Environment

- **OS**: [Linux|macOS|Windows]
- **Network Tools**: [mock|iptables|tc|netem]
- **Router Version**: [version]
- **NATS Version**: [version]
- **Test Duration**: [duration]

## Scenario Results

### 1. Single-Instance Network Partition

#### 1.1. Loss of Connection to JetStream / Message Broker

**Test Case**: `test_single_instance_jetstream_partition_short`

**How Reproduced**:
```bash
# Steps to reproduce
./test/scripts/r12_network_partition_fault_injection.sh create single_instance router nats --action drop
# Wait 5 seconds
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Observed Behavior**:
- ✅ Connection loss detected: [timestamp]
- ✅ Retry attempts logged: [count]
- ✅ Circuit breaker opened: [timestamp]
- ✅ Process remained alive: [yes|no]
- ✅ No message loss: [yes|no]
- ✅ Recovery successful: [timestamp]

**Deviations from Expected**:
- [List any deviations]

**Improvements Required**:
- [List any required improvements]

**Logs**:
```
[WARN] Network partition detected: Router -> NATS JetStream
[ERROR] Connection lost to NATS JetStream broker
[INFO] Retry attempt 1/3 with exponential backoff
[WARN] Circuit breaker opened for NATS connection
[INFO] Network partition resolved: Router -> NATS JetStream
[INFO] Connection restored successfully
```

**Metrics**:
- `router_nats_connection_failures_total`: [value]
- `router_circuit_breaker_state{state="open"}`: [value]
- `router_network_partition_duration_seconds`: [value]

---

#### 1.2. Loss of Connection to External Services (DB / API)

**Test Case**: `test_single_instance_external_service_partition_short`

**How Reproduced**:
```bash
# Steps to reproduce
```

**Observed Behavior**:
- [Same structure as above]

**Deviations from Expected**:
- [List any deviations]

**Improvements Required**:
- [List any required improvements]

---

### 2. Multi-Instance / Split-Brain

#### 2.1. Service Cluster Partition (Split-Brain)

**Test Case**: `test_multi_instance_split_brain_leader_election`

**How Reproduced**:
```bash
# Steps to reproduce
./test/scripts/r12_network_partition_fault_injection.sh create multi_instance router-group-1 router-group-2 --action drop
# Wait for partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

**Observed Behavior**:
- ✅ Split-brain detected: [timestamp]
- ✅ Leader election triggered: [timestamp]
- ✅ No duplicate processing: [yes|no]
- ✅ Single leader after recovery: [yes|no]
- ✅ State consistency restored: [yes|no]

**Deviations from Expected**:
- [List any deviations]

**Improvements Required**:
- [List any required improvements]

**Logs**:
```
[WARN] Leader election triggered
[ERROR] Split-brain detected, multiple leaders
[WARN] Quorum lost, stopping critical operations
[INFO] Network partition resolved
[INFO] Single leader elected
```

**Metrics**:
- `router_leader_election_total`: [value]
- `router_split_brain_detected_total`: [value]
- `router_quorum_lost_total`: [value]

---

#### 2.2. Partition of Access to JetStream / Storage

**Test Case**: `test_multi_instance_jetstream_partition_instance_a_isolated`

**How Reproduced**:
```bash
# Steps to reproduce
```

**Observed Behavior**:
- [Same structure as above]

**Deviations from Expected**:
- [List any deviations]

**Improvements Required**:
- [List any required improvements]

---

## Summary Table

| Scenario | Test Case | Status | Duration | Notes |
|----------|-----------|--------|----------|-------|
| Single-instance JetStream partition (short) | `test_single_instance_jetstream_partition_short` | ✅ Pass | 5s | - |
| Single-instance JetStream partition (long) | `test_single_instance_jetstream_partition_long` | ✅ Pass | 5m | - |
| Single-instance external service partition | `test_single_instance_external_service_partition_short` | ⚠️ Warning | 5s | Minor log delay |
| Multi-instance split-brain | `test_multi_instance_split_brain_leader_election` | ✅ Pass | 10s | - |
| Multi-instance JetStream partition | `test_multi_instance_jetstream_partition_instance_a_isolated` | ✅ Pass | 10s | - |

## Overall Assessment

### Strengths

- [List strengths]

### Weaknesses

- [List weaknesses]

### Recommendations

1. [Recommendation 1]
2. [Recommendation 2]
3. [Recommendation 3]

## Next Steps

- [ ] Fix identified issues
- [ ] Re-run failed tests
- [ ] Update documentation if needed
- [ ] Integrate into CI/CD pipeline

## References

- **Specification**: `R12_NETWORK_PARTITION_SCENARIOS.md`
- **Logs and Metrics**: `R12_LOGS_AND_METRICS.md`
- **Quick Start**: `R12_README.md`
- **Test Suite**: `router_network_partition_SUITE.erl`

