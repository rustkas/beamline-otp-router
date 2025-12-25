# NATS Publish Failure Metrics Enhancement Plan

## Purpose

This document outlines the plan for enhancing metrics with labels to improve observability for `router_nats` publish and publish_with_ack failures.

## Current State

### Metrics Without Labels

**Current metrics**:
- `router_nats_publish_with_ack_failures_total` (counter, no labels)
- `router_nats_publish_failures_total` (counter, no labels)

**Limitations**:
- Cannot filter by error reason
- Cannot distinguish between error types
- Cannot analyze fail-open vs queueing mode separately
- Difficult to create detailed dashboards

## Proposed Enhancement

### Enhanced Metrics with Labels

**Proposed metric structure**:

```erlang
router_metrics:inc(router_nats_publish_with_ack_failures_total, #{
    reason => Reason,           %% Error reason (nats_unavailable, timeout, connection_closed, not_connected)
    error_type => ErrorType,    %% Error category (operation_error, timeout, connection_lost)
    mode => Mode                %% Operation mode (fail_open, queueing)
}).
```

**Example labels**:
- `reason`: `nats_unavailable`, `timeout`, `connection_closed`, `not_connected`
- `error_type`: `operation_error`, `timeout`, `connection_lost`
- `mode`: `fail_open`, `queueing`

### Implementation Plan

#### Phase 1: Use Existing `emit_metric/3` API

**File**: `apps/otp/router/src/router_metrics.erl`

**Status**: ✅ **ALREADY SUPPORTS LABELS**

**Finding**: `router_metrics:emit_metric/3` already supports labels via Metadata parameter.

**No changes needed** - Use existing API:
```erlang
router_metrics:emit_metric(MetricName, #{count => 1}, Labels).
```

**Example**:
```erlang
router_metrics:emit_metric(router_nats_publish_with_ack_failures_total, 
    #{count => 1}, 
    #{reason => Reason, error_type => ErrorType, mode => Mode}).
```

#### Phase 2: Update router_nats.erl

**File**: `apps/otp/router/src/router_nats.erl`

**Changes**:
1. Update `do_publish` to include labels:
   ```erlang
   router_metrics:inc(router_nats_publish_failures_total, #{
       reason => sanitize_reason(Reason),
       error_type => classify_error_type(Reason),
       mode => case State#state.fail_open_mode of true -> fail_open; false -> queueing end
   }),
   ```

2. Update `do_publish_with_ack` similarly

3. Add helper functions:
   ```erlang
   classify_error_type(timeout) -> timeout;
   classify_error_type(connection_closed) -> connection_lost;
   classify_error_type(not_connected) -> connection_lost;
   classify_error_type(_) -> operation_error.
   ```

#### Phase 3: Update Tests

**File**: `apps/otp/router/test/router_nats_publish_failure_SUITE.erl`

**Changes**:
1. Update metric tracking to capture labels
2. Verify labels are correct in assertions
3. Test backward compatibility (metrics without labels still work)

#### Phase 4: Update Documentation

**Files**:
- `NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Update metrics section
- `NATS_PUBLISH_FAILURE_METRICS_ALERTS.md` - Update with label examples
- `NATS_METRICS_COMPLIANCE.md` - Document label structure

### Benefits

1. **Better Observability**:
   - Filter failures by reason
   - Analyze error patterns
   - Compare fail-open vs queueing behavior

2. **Improved Dashboards**:
   - Breakdown by error type
   - Trend analysis by reason
   - Mode-specific metrics

3. **Enhanced Alerts**:
   - Alert on specific error types
   - Different thresholds for different reasons
   - Mode-specific alerting

### Migration Strategy

1. **Backward Compatibility**:
   - Keep `inc/1` for metrics without labels
   - Labels are optional
   - Existing code continues to work

2. **Gradual Migration**:
   - Start with new metrics (publish failures)
   - Migrate existing metrics gradually
   - Document label structure

3. **Testing**:
   - Verify backward compatibility
   - Test label validation
   - Verify Prometheus export format

### Example Prometheus Export

**Before** (no labels):
```
router_nats_publish_with_ack_failures_total 42
```

**After** (with labels):
```
router_nats_publish_with_ack_failures_total{reason="nats_unavailable",error_type="operation_error",mode="queueing"} 15
router_nats_publish_with_ack_failures_total{reason="timeout",error_type="timeout",mode="queueing"} 10
router_nats_publish_with_ack_failures_total{reason="connection_closed",error_type="connection_lost",mode="fail_open"} 17
```

### Implementation Checklist

- [x] Phase 1: Verify `router_metrics:emit_metric/3` supports labels ✅ **COMPLETE**
- [ ] Phase 2: Update `router_nats.erl` to use `emit_metric/3` with labels
- [ ] Phase 3: Update tests to verify labels
- [ ] Phase 4: Update documentation
- [ ] Phase 5: Verify Prometheus export format
- [ ] Phase 6: Test backward compatibility (keep `inc/1` for non-labeled metrics)
- [ ] Phase 7: Update dashboards with new labels
- [ ] Phase 8: Update alerts to use labels

### Estimated Effort

- **Phase 1**: ✅ **COMPLETE** (API already supports labels)
- **Phase 2**: 4-6 hours (router_nats updates to use `emit_metric/3`)
- **Phase 3**: 2-3 hours (test updates)
- **Phase 4**: 1-2 hours (documentation)
- **Phase 5-6**: 2-3 hours (testing)
- **Phase 7-8**: 2-4 hours (dashboards/alerts)

**Total**: ~11-18 hours (reduced from 13-22 hours)

### Dependencies

- `router_metrics` module must support labels
- Prometheus exporter must handle labeled metrics
- Dashboard templates need updates
- Alert rules need updates

### Risks

1. **Breaking Changes**: 
   - Mitigation: Maintain backward compatibility
   - Labels are optional

2. **Performance Impact**:
   - Mitigation: Labels are lightweight (maps)
   - Minimal overhead

3. **Label Cardinality**:
   - Mitigation: Limit label values
   - Use predefined label sets

## References

- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Behavior specification
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_METRICS_ALERTS.md` - Current metrics/alerts
- `apps/otp/router/src/router_metrics.erl` - Metrics implementation
- `apps/otp/router/src/router_nats.erl` - NATS client implementation

