# R12: Network Partition Logs and Metrics

**Date**: 2025-11-30  
**Status**: Documentation  
**Purpose**: Define required logs and metrics for network partition scenarios

## Overview

This document defines the minimal set of log messages and metrics that must be present during network partition tests to verify system behavior.

## Log Messages

### During Partition

#### Connection Loss Detection

**Level**: `WARN`  
**Message**: Network partition detected  
**Fields**:
- `component`: Router identifier
- `from`: Source component (e.g., "router")
- `to`: Target component (e.g., "nats-jetstream")
- `partition_type`: `single_instance` | `multi_instance` | `service_broker`
- `action`: `drop` | `delay` | `reject` | `loss`

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:00Z",
  "level": "WARN",
  "component": "router",
  "message": "Network partition detected",
  "context": {
    "from": "router",
    "to": "nats-jetstream",
    "partition_type": "single_instance",
    "action": "drop"
  }
}
```

#### Connection Error

**Level**: `ERROR`  
**Message**: Connection lost to external service  
**Fields**:
- `component`: Router identifier
- `service`: Service name (e.g., "nats-jetstream", "database", "external-api")
- `error_type`: `connection_refused` | `timeout` | `network_unreachable`
- `retry_count`: Current retry attempt number

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:01Z",
  "level": "ERROR",
  "component": "router",
  "message": "Connection lost to NATS JetStream broker",
  "context": {
    "service": "nats-jetstream",
    "error_type": "connection_refused",
    "retry_count": 0
  }
}
```

#### Retry Attempt

**Level**: `INFO`  
**Message**: Retry attempt with backoff  
**Fields**:
- `component`: Router identifier
- `service`: Service name
- `attempt`: Current attempt number
- `max_attempts`: Maximum retry attempts
- `backoff_delay_ms`: Backoff delay in milliseconds
- `error`: Last error message

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:02Z",
  "level": "INFO",
  "component": "router",
  "message": "Retry attempt 1/3 with exponential backoff",
  "context": {
    "service": "nats-jetstream",
    "attempt": 1,
    "max_attempts": 3,
    "backoff_delay_ms": 100,
    "error": "connection_refused"
  }
}
```

#### Circuit Breaker State Change

**Level**: `WARN`  
**Message**: Circuit breaker state changed  
**Fields**:
- `component`: Router identifier
- `service`: Service name
- `from`: Previous state (`closed` | `open` | `half_open`)
- `to`: New state (`closed` | `open` | `half_open`)
- `reason`: `consecutive_failures` | `error_rate` | `latency` | `manual`

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:05Z",
  "level": "WARN",
  "component": "router",
  "message": "Circuit breaker state changed",
  "context": {
    "service": "nats-jetstream",
    "from": "closed",
    "to": "open",
    "reason": "consecutive_failures"
  }
}
```

#### Circuit Breaker Open

**Level**: `WARN`  
**Message**: Circuit breaker open, blocking operations  
**Fields**:
- `component`: Router identifier
- `service`: Service name
- `blocked_operations`: List of blocked operations (e.g., ["publish", "subscribe"])

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:05Z",
  "level": "WARN",
  "component": "router",
  "message": "Circuit breaker open, blocking publish",
  "context": {
    "service": "nats-jetstream",
    "blocked_operations": ["publish"]
  }
}
```

### After Recovery

#### Network Partition Resolved

**Level**: `INFO`  
**Message**: Network partition resolved  
**Fields**:
- `component`: Router identifier
- `from`: Source component
- `to`: Target component
- `partition_duration_seconds`: Duration of partition in seconds

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:30Z",
  "level": "INFO",
  "component": "router",
  "message": "Network partition resolved: Router -> NATS JetStream",
  "context": {
    "from": "router",
    "to": "nats-jetstream",
    "partition_duration_seconds": 30
  }
}
```

#### Reconnection Attempt

**Level**: `INFO`  
**Message**: Reconnecting to service  
**Fields**:
- `component`: Router identifier
- `service`: Service name
- `attempt`: Current reconnection attempt number

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:31Z",
  "level": "INFO",
  "component": "router",
  "message": "Reconnecting to NATS JetStream broker",
  "context": {
    "service": "nats-jetstream",
    "attempt": 1
  }
}
```

#### Connection Restored

**Level**: `INFO`  
**Message**: Connection restored successfully  
**Fields**:
- `component`: Router identifier
- `service`: Service name
- `recovery_time_seconds`: Time to restore connection in seconds

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:32Z",
  "level": "INFO",
  "component": "router",
  "message": "Connection restored successfully",
  "context": {
    "service": "nats-jetstream",
    "recovery_time_seconds": 2
  }
}
```

#### Circuit Breaker Closed

**Level**: `INFO`  
**Message**: Circuit breaker closed, normal operation resumed  
**Fields**:
- `component`: Router identifier
- `service`: Service name

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:33Z",
  "level": "INFO",
  "component": "router",
  "message": "Circuit breaker closed, normal operation resumed",
  "context": {
    "service": "nats-jetstream"
  }
}
```

### Multi-Instance / Split-Brain

#### Leader Election

**Level**: `WARN`  
**Message**: Leader election triggered  
**Fields**:
- `component`: Router identifier
- `reason`: `partition_detected` | `quorum_lost` | `leader_timeout`
- `current_leader`: Current leader identifier (if known)
- `quorum_size`: Current quorum size
- `required_quorum`: Required quorum size

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:10Z",
  "level": "WARN",
  "component": "router",
  "message": "Leader election triggered",
  "context": {
    "reason": "quorum_lost",
    "current_leader": "router-instance-1",
    "quorum_size": 1,
    "required_quorum": 3
  }
}
```

#### Split-Brain Detected

**Level**: `ERROR`  
**Message**: Split-brain detected, multiple leaders  
**Fields**:
- `component`: Router identifier
- `leaders`: List of leader identifiers
- `partition_groups`: List of partition groups

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:11Z",
  "level": "ERROR",
  "component": "router",
  "message": "Split-brain detected, multiple leaders",
  "context": {
    "leaders": ["router-instance-1", "router-instance-3"],
    "partition_groups": [["router-instance-1", "router-instance-2"], ["router-instance-3", "router-instance-4"]]
  }
}
```

#### Quorum Lost

**Level**: `WARN`  
**Message**: Quorum lost, stopping critical operations  
**Fields**:
- `component`: Router identifier
- `current_quorum`: Current quorum size
- `required_quorum`: Required quorum size
- `stopped_operations`: List of stopped operations

**Example**:
```json
{
  "timestamp": "2025-11-30T12:00:12Z",
  "level": "WARN",
  "component": "router",
  "message": "Quorum lost, stopping critical operations",
  "context": {
    "current_quorum": 1,
    "required_quorum": 3,
    "stopped_operations": ["leader_election", "distributed_locks"]
  }
}
```

## Metrics

### Connection Metrics

#### `router_nats_connection_failures_total`

**Type**: Counter  
**Labels**:
- `service`: Service name (e.g., "nats-jetstream", "database")
- `error_type`: Error type (`connection_refused` | `timeout` | `network_unreachable`)

**Description**: Total number of connection failures

**During Partition**: Should increment when partition is active

#### `router_nats_reconnect_attempts_total`

**Type**: Counter  
**Labels**:
- `service`: Service name
- `attempt`: Attempt number (1, 2, 3, ...)

**Description**: Total number of reconnection attempts

**During Partition**: Should increment with each retry attempt

#### `router_nats_connection_restored_total`

**Type**: Counter  
**Labels**:
- `service`: Service name

**Description**: Total number of successful connection restorations

**After Recovery**: Should increment when connection is restored

### Circuit Breaker Metrics

#### `router_circuit_breaker_state`

**Type**: Gauge  
**Labels**:
- `service`: Service name
- `state`: `closed` | `open` | `half_open`

**Description**: Current circuit breaker state

**During Partition**: Should be `open` when partition is active  
**After Recovery**: Should be `closed` after recovery

#### `router_circuit_breaker_transitions_total`

**Type**: Counter  
**Labels**:
- `service`: Service name
- `from`: Previous state
- `to`: New state
- `reason`: Transition reason

**Description**: Total number of circuit breaker state transitions

**During Partition**: Should increment when circuit breaker opens  
**After Recovery**: Should increment when circuit breaker closes

### Network Partition Metrics

#### `router_network_partition_duration_seconds`

**Type**: Histogram  
**Labels**:
- `partition_type`: `single_instance` | `multi_instance` | `service_broker`
- `from`: Source component
- `to`: Target component

**Description**: Duration of network partitions in seconds

**During Partition**: Should track partition duration

#### `router_network_partition_recovery_time_seconds`

**Type**: Histogram  
**Labels**:
- `partition_type`: Partition type
- `service`: Service name

**Description**: Time to recover from network partition in seconds

**After Recovery**: Should track recovery time

### Multi-Instance / Split-Brain Metrics

#### `router_leader_election_total`

**Type**: Counter  
**Labels**:
- `reason`: Election reason (`partition_detected` | `quorum_lost` | `leader_timeout`)
- `result`: Election result (`elected` | `failed` | `split_brain`)

**Description**: Total number of leader elections

**During Partition**: Should increment when leader election is triggered

#### `router_split_brain_detected_total`

**Type**: Counter  
**Labels**:
- `partition_groups`: Number of partition groups

**Description**: Total number of split-brain detections

**During Partition**: Should increment when split-brain is detected

#### `router_quorum_lost_total`

**Type**: Counter  
**Labels**:
- `current_quorum`: Current quorum size
- `required_quorum`: Required quorum size

**Description**: Total number of quorum loss events

**During Partition**: Should increment when quorum is lost

## Verification Checklist

### During Partition

- [ ] Connection loss detected and logged
- [ ] Retry attempts logged with backoff delays
- [ ] Circuit breaker state changes logged
- [ ] Metrics reflect partition state:
  - `router_nats_connection_failures_total` increments
  - `router_nats_reconnect_attempts_total` increments
  - `router_circuit_breaker_state{state="open"}` = 1
  - `router_network_partition_duration_seconds` tracks duration

### After Recovery

- [ ] Network partition resolution logged
- [ ] Reconnection attempts logged
- [ ] Connection restoration logged
- [ ] Circuit breaker closure logged
- [ ] Metrics reflect recovery:
  - `router_nats_connection_restored_total` increments
  - `router_circuit_breaker_state{state="closed"}` = 1
  - `router_network_partition_recovery_time_seconds` tracks recovery time

### Multi-Instance / Split-Brain

- [ ] Leader election events logged
- [ ] Split-brain detection logged (if applicable)
- [ ] Quorum loss logged (if applicable)
- [ ] Metrics reflect split-brain state:
  - `router_leader_election_total` increments
  - `router_split_brain_detected_total` increments (if applicable)
  - `router_quorum_lost_total` increments (if applicable)

## References

- **Test Suite**: `router_network_partition_SUITE.erl`
- **Network Partition Scenarios**: `R12_NETWORK_PARTITION_SCENARIOS.md`
- **Fault Injection Scripts**: `scripts/r12_network_partition_fault_injection.sh`

