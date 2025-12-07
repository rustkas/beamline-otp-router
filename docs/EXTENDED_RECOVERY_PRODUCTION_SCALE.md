# Extended Recovery Scenarios - Production Scale

## Overview

This document describes **production-scale scenarios** for Extended Recovery Scenarios tests. These scenarios simulate real-world production environments with:

- Multi-node JetStream clusters
- Cross-region deployments
- Rolling restarts with zero downtime
- High-scale message processing

## Production Scale Scenarios

### Scenario 5.1: Multi-Node JetStream Cluster Failures

**Description**: Simulates failures in a multi-node JetStream cluster (3+ nodes).

**Key Features**:
- Quorum-based availability
- Node failure and recovery
- Cluster state consistency
- Message processing during node failures

**Phases**:
1. **Baseline Phase** (5 minutes): Normal cluster operation
2. **Failure Cycles** (60 minutes):
   - Every 10 minutes: Fail one node
   - Verify quorum maintained (2+ nodes available)
   - Process messages during failures
   - Recover failed node
3. **Recovery Phase** (10 minutes): All nodes healthy

**Success Criteria**:
- ✅ Quorum maintained during failures
- ✅ No message loss
- ✅ Throughput degrades gracefully (not to zero)
- ✅ Full throughput restored after recovery
- ✅ No resource leaks

**Metrics Collected**:
- Cluster node availability
- Quorum status
- Throughput during failures
- Recovery time
- Resource usage

### Scenario 5.2: Cross-Region Network Partitions

**Description**: Simulates network partitions between regions in a multi-region deployment.

**Key Features**:
- Multi-region connectivity
- Partition detection and handling
- Message routing during partitions
- Automatic reconnection

**Phases**:
1. **Baseline Phase** (5 minutes): Normal multi-region operation
2. **Partition Cycles** (60 minutes):
   - Every 8 minutes: Partition one region
   - Verify system handles partition gracefully
   - Process messages in available regions
   - Reconnect partitioned region
3. **Recovery Phase** (10 minutes): All regions connected

**Success Criteria**:
- ✅ System handles partitions gracefully
- ✅ Messages process in available regions
- ✅ No message loss (messages queued during partition)
- ✅ Automatic reconnection after partition ends
- ✅ Full throughput restored after recovery

**Metrics Collected**:
- Region connectivity status
- Throughput per region
- Partition duration
- Reconnection time
- Message queue depth during partitions

### Scenario 5.3: Rolling Restart Zero Downtime

**Description**: Simulates rolling restart of router processes without downtime.

**Key Features**:
- Multiple router instances
- Rolling restart sequence
- Zero downtime requirement
- State preservation

**Phases**:
1. **Baseline Phase** (5 minutes): Normal multi-instance operation
2. **Rolling Restart** (40 minutes):
   - Restart one router instance at a time
   - Verify at least one router always active
   - Process messages during restarts
   - Verify state preserved across restarts
3. **Recovery Phase** (10 minutes): All instances restarted and healthy

**Success Criteria**:
- ✅ Zero downtime (at least one router active at all times)
- ✅ No message loss
- ✅ Throughput maintained during restarts
- ✅ State preserved across restarts
- ✅ All instances healthy after rolling restart

**Metrics Collected**:
- Active router count
- Throughput during restarts
- Restart duration per instance
- State preservation verification
- Resource usage per instance

## Implementation Details

### Multi-Node Cluster Simulation

**Current Implementation**:
- Uses ETS table to track cluster state
- Simulates node availability via mock functions
- Verifies quorum logic (2+ nodes for 3-node cluster)

**Future Enhancements**:
- Real multi-node JetStream cluster setup
- Actual node failure injection
- Cluster state monitoring via NATS API

### Cross-Region Partition Simulation

**Current Implementation**:
- Uses ETS table to track region connectivity
- Simulates partition via mock functions
- Verifies message routing logic

**Future Enhancements**:
- Real multi-region NATS setup
- Network partition injection (iptables, network namespaces)
- Region-specific message routing

### Rolling Restart Simulation

**Current Implementation**:
- Uses ETS table to track router instances
- Simulates restart via mock functions
- Verifies zero downtime requirement

**Future Enhancements**:
- Real multi-instance router deployment
- Actual process restart injection
- Load balancer integration

## Production Deployment Considerations

### Cluster Configuration

**Recommended Setup**:
- 3+ JetStream nodes for quorum
- 2+ router instances for redundancy
- Load balancer for router instances
- Health checks for all components

### Monitoring Requirements

**Required Metrics**:
- Cluster node health
- Region connectivity
- Router instance health
- Throughput per region/instance
- Resource usage per node/instance

### Failure Handling

**Automatic Recovery**:
- Node failure detection and recovery
- Partition detection and reconnection
- Router instance health monitoring
- Automatic failover

**Manual Intervention**:
- Cluster quorum loss
- Extended partition (> 5 minutes)
- Router instance persistent failures

## Testing in Production-Like Environment

### Prerequisites

1. **Multi-node JetStream cluster**:
   - 3+ nodes configured
   - JetStream enabled
   - Cluster quorum configured

2. **Multi-region setup** (optional):
   - Multiple NATS clusters
   - Cross-region connectivity
   - Region-specific routing

3. **Multiple router instances**:
   - 2+ router processes
   - Load balancer configured
   - Health checks enabled

### Test Execution

**Local Development**:
```bash
# Run production-scale scenarios
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --group production_scale_scenarios
```

**CI/CD**:
- Production-scale scenarios run in nightly jobs
- Require production-like environment setup
- May need dedicated test infrastructure

### Environment Variables

```bash
# Cluster configuration
JETSTREAM_CLUSTER_NODES=3
JETSTREAM_QUORUM_SIZE=2

# Region configuration
REGION_COUNT=2
REGION_A_ENABLED=true
REGION_B_ENABLED=true

# Router instances
ROUTER_INSTANCE_COUNT=3
ROUTER_LOAD_BALANCER_ENABLED=true
```

## Success Criteria Summary

### Multi-Node Cluster

- ✅ Quorum maintained during failures
- ✅ No message loss
- ✅ Graceful throughput degradation
- ✅ Full recovery after all nodes healthy

### Cross-Region Partitions

- ✅ Graceful partition handling
- ✅ Messages process in available regions
- ✅ Automatic reconnection
- ✅ Full throughput after recovery

### Rolling Restart

- ✅ Zero downtime
- ✅ No message loss
- ✅ State preserved
- ✅ All instances healthy

## References

- `EXTENDED_RECOVERY_SCENARIOS_SPEC.md`: Scenario specifications
- `router_jetstream_extended_recovery_SUITE.erl`: Test implementation
- `EXTENDED_RECOVERY_RESOURCE_LIMITS.md`: Resource limits
- NATS JetStream clustering documentation
- Multi-region deployment guides

