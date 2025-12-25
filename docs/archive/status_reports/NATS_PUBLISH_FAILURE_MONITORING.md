# NATS Publish Failure Monitoring and Alerting Guide

## Purpose

This document provides operational guidance for monitoring and alerting on `router_nats` publish and `publish_with_ack` failures. It complements the behavior specification in `NATS_PUBLISH_FAILURE_BEHAVIOR.md` with SRE/operator recommendations.

## Key Metrics

### Primary Failure Metrics

**`router_nats_publish_failures_total`** (counter)
- **Incremented when**: All types of `publish` failures
  - `{error, Reason}` during connected state
  - `timeout` during connected state
  - `close_connection` during operation
  - Not connected state (operation queued)
- **Current labels**: None (counter only)
- **Recommended monitoring**: Rate of failures over time windows

**`router_nats_publish_with_ack_failures_total`** (counter)
- **Incremented when**: All types of `publish_with_ack` failures
  - Same failure scenarios as `publish`
- **Current labels**: None
- **Recommended monitoring**: Rate of failures over time windows

### Queue Metrics

**`router_nats_pending_operations_count`** (gauge)
- **Current value**: Number of operations in retry queue
- **Max value**: Configurable (default: 1000)
- **Alert threshold**: 
  - **Warning**: 80% capacity (800 operations)
  - **Critical**: 95% capacity (950 operations)

**`router_nats_pending_operations_dropped_total`** (counter)
- **Incremented when**: Queue is full and oldest operation is dropped
- **Alert threshold**: Any increment should trigger warning (queue overflow)

### Connection Metrics

**`router_nats_connection_status`** (gauge)
- **Values**: `0.0` (disconnected), `0.5` (reconnecting), `1.0` (connected)
- **Alert threshold**: `0.0` for > 1 minute (critical)

## Recommended Alerts

### Critical Alerts

#### 1. High Publish_with_ack Failure Rate

**Alert**: `RouterNATSPublishWithAckHighFailureRate`

**Condition**:
```promql
rate(router_nats_publish_with_ack_failures_total[5m]) > 10
```

**Severity**: Critical

**Description**: More than 10 `publish_with_ack` failures per second over 5 minutes indicates severe NATS connectivity or configuration issues.

**Actions**:
- Check NATS server availability
- Verify connection configuration
- Check for network issues
- Review fail-open vs queueing mode settings

#### 2. High Publish Failure Rate

**Alert**: `RouterNATSPublishHighFailureRate`

**Condition**:
```promql
rate(router_nats_publish_failures_total[5m]) > 50
```

**Severity**: Critical

**Description**: More than 50 `publish` failures per second over 5 minutes indicates severe NATS connectivity issues.

**Actions**:
- Check NATS server availability
- Verify connection configuration
- Check for network issues

#### 3. Queue Overflow

**Alert**: `RouterNATSQueueOverflow`

**Condition**:
```promql
increase(router_nats_pending_operations_dropped_total[5m]) > 0
```

**Severity**: Critical

**Description**: Operations are being dropped due to queue overflow. Messages may be lost.

**Actions**:
- Check NATS connection status
- Verify reconnection is working
- Consider increasing queue size
- Review fail-open vs queueing mode settings

#### 4. Persistent Connection Loss

**Alert**: `RouterNATSPersistentConnectionLoss`

**Condition**:
```promql
router_nats_connection_status == 0.0
```

**Duration**: > 1 minute

**Severity**: Critical

**Description**: NATS connection has been lost for more than 1 minute. System may be operating in degraded mode.

**Actions**:
- Check NATS server availability
- Verify network connectivity
- Check firewall rules
- Review connection configuration

### Warning Alerts

#### 1. Elevated Publish Failure Rate

**Alert**: `RouterNATSPublishElevatedFailureRate`

**Condition**:
```promql
rate(router_nats_publish_failures_total[5m]) > 5
```

**Severity**: Warning

**Description**: Elevated rate of publish failures. Monitor for escalation.

**Actions**:
- Monitor failure rate trends
- Check NATS server health
- Review recent configuration changes

#### 2. Queue Approaching Capacity

**Alert**: `RouterNATSQueueHighUtilization`

**Condition**:
```promql
router_nats_pending_operations_count > 800
```

**Severity**: Warning

**Description**: Queue is at 80% capacity. Risk of overflow if failures continue.

**Actions**:
- Monitor queue size trends
- Check NATS connection status
- Consider increasing queue size
- Review reconnection behavior

#### 3. Intermittent Connection Issues

**Alert**: `RouterNATSIntermittentConnectionIssues`

**Condition**:
```promql
increase(router_nats_connection_lost_total[15m]) > 3
```

**Severity**: Warning

**Description**: Multiple connection losses in 15 minutes. May indicate network instability.

**Actions**:
- Check network stability
- Review NATS server logs
- Monitor connection metrics

## Monitoring Dashboards

### Recommended Panels

1. **Failure Rate Over Time**
   - Query: `rate(router_nats_publish_failures_total[5m])`
   - Query: `rate(router_nats_publish_with_ack_failures_total[5m])`
   - Visualization: Line graph
   - Use cases: Trend analysis, capacity planning

2. **Queue Size Over Time**
   - Query: `router_nats_pending_operations_count`
   - Visualization: Line graph
   - Use cases: Queue capacity monitoring, overflow prevention

3. **Connection Status**
   - Query: `router_nats_connection_status`
   - Visualization: Gauge
   - Use cases: Real-time connection health

4. **Failure Rate by Operation Type**
   - Query: `rate(router_nats_publish_failures_total[5m])`
   - Query: `rate(router_nats_publish_with_ack_failures_total[5m])`
   - Visualization: Stacked area graph
   - Use cases: Comparing failure rates between operation types

5. **Queue Metrics**
   - Query: `router_nats_pending_operations_count`
   - Query: `increase(router_nats_pending_operations_dropped_total[5m])`
   - Query: `increase(router_nats_pending_operations_retry_total[5m])`
   - Visualization: Multi-line graph
   - Use cases: Queue health monitoring

## Operational Procedures

### Investigating High Failure Rates

1. **Check Connection Status**:
   ```promql
   router_nats_connection_status
   ```
   - If `0.0`: Connection is lost, check NATS server
   - If `0.5`: Reconnecting, monitor reconnection attempts
   - If `1.0`: Connected, check for other issues

2. **Check Failure Types**:
   - Review logs for error codes:
     - `NATS_PUBLISH_ERROR`: Operation error during connected state
     - `NATS_PUBLISH_QUEUED`: Operation queued (not connected)
     - `NATS_PUBLISH_WITH_ACK_ERROR`: Operation error during connected state
     - `NATS_PUBLISH_WITH_ACK_QUEUED`: Operation queued (not connected)

3. **Check Queue Status**:
   ```promql
   router_nats_pending_operations_count
   ```
   - High values indicate operations are queued
   - Check if reconnection is working

4. **Check NATS Server**:
   - Verify NATS server is running
   - Check NATS server logs
   - Verify network connectivity

### Handling Queue Overflow

1. **Immediate Actions**:
   - Check NATS connection status
   - Verify reconnection is working
   - Review queue size configuration

2. **Configuration Changes**:
   - Consider increasing `nats_max_pending_operations` (default: 1000)
   - Review fail-open vs queueing mode settings
   - Consider enabling fail-open mode temporarily

3. **Long-term Solutions**:
   - Improve NATS connection stability
   - Implement circuit breaker pattern
   - Review message processing rate

### Fail-Open vs Queueing Mode Decision

**Fail-Open Mode** (`nats_fail_open_mode = true`):
- ✅ System remains available
- ✅ No blocking on NATS failures
- ❌ Messages may be lost
- ❌ No retry mechanism

**Queueing Mode** (`nats_fail_open_mode = false`):
- ✅ Messages are preserved
- ✅ Automatic retry after reconnection
- ❌ System may block if queue fills
- ❌ Memory usage increases with queue size

**Recommendation**: Use fail-open mode for high-availability requirements, queueing mode for guaranteed delivery requirements.

## Expected Behavior Summary

### Normal Operation

- **Failure Rate**: < 1 failure per second
- **Queue Size**: < 100 operations
- **Connection Status**: `1.0` (connected)
- **Queue Drops**: 0

### Degraded Operation

- **Failure Rate**: 1-10 failures per second
- **Queue Size**: 100-800 operations
- **Connection Status**: `0.5` (reconnecting) or intermittent `0.0`
- **Queue Drops**: 0

### Critical Operation

- **Failure Rate**: > 10 failures per second
- **Queue Size**: > 800 operations
- **Connection Status**: `0.0` (disconnected) for > 1 minute
- **Queue Drops**: > 0

## References

- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Behavior specification
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Connection resilience documentation
- `apps/otp/router/docs/NATS_METRICS_COMPLIANCE.md` - Metrics documentation

