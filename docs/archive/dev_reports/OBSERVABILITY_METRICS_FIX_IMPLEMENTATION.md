# Implementation: Fix router_jetstream_redelivery_total Metric

**Version**: 1.0  
**Date**: 2025-11-30  
**Status**: ✅ **COMPLETED**

## Summary

Fixed critical issues with `router_jetstream_redelivery_total` metric:
1. ✅ Changed metric name from `router_redelivery_total` to `router_jetstream_redelivery_total`
2. ✅ Added support for labels: `assignment_id`, `request_id`, `reason`, `source`
3. ✅ Updated all call sites to pass context with labels
4. ✅ Updated Prometheus metadata
5. ✅ Maintained backward compatibility

## Changes Made

### 1. `apps/otp/router/src/router_jetstream.erl`

**Changes**:
- Added `nak/3` function with context parameter for labels
- Updated `nak/2` to call `nak/3` with empty context (backward compatible)
- Changed metric name: `router_redelivery_total` → `router_jetstream_redelivery_total`
- Added `reason_to_binary/1` helper function for label conversion
- Updated `metrics/0` export list

**Key Implementation**:
```erlang
%% Backward compatible nak/2
nak(#{id := Id} = Msg, Reason) ->
  nak(Msg, Reason, #{}).

%% New nak/3 with context labels
nak(#{id := Id} = Msg, Reason, Context) ->
  %% Extract labels from context or use defaults
  AssignmentId = maps:get(assignment_id, Context, ~"unknown"),
  RequestId = maps:get(request_id, Context, ~"unknown"),
  Source = maps:get(source, Context, ~"router_jetstream"),
  ReasonBin = reason_to_binary(Reason),
  
  %% Emit metric with labels
  router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
    assignment_id => AssignmentId,
    request_id => RequestId,
    reason => ReasonBin,
    source => Source
  }),
  %% ... rest of implementation
```

### 2. `apps/otp/router/src/router_prometheus.erl`

**Changes**:
- Added metadata for `router_jetstream_redelivery_total`
- Kept `router_redelivery_total` as deprecated (for backward compatibility)

### 3. `apps/otp/router/src/router_result_consumer.erl`

**Changes**:
- Updated 2 call sites to use `nak/3` with context:
  - Line 594: Tenant validation failure (first occurrence)
  - Line 624: Tenant validation failure (second occurrence, after idempotency error)
- Added labels: `assignment_id`, `request_id`, `source => ~"router_result_consumer"`

### 4. `apps/otp/router/src/router_ack_consumer.erl`

**Changes**:
- Updated 2 call sites to use `nak/3` with context:
  - Line 262: Tenant validation failure (first occurrence)
  - Line 287: Tenant validation failure (second occurrence, after idempotency error)
- Added labels: `assignment_id`, `source => ~"router_ack_consumer"`
- Note: `request_id` not available in ACK consumer context

### 5. `apps/otp/router/src/router_decide_consumer.erl`

**Changes**:
- Updated 1 call site to use `nak/3` with context:
  - Line 496: Backpressure scenario
- Added labels: `request_id`, `source => ~"router_decide_consumer"`
- Note: `assignment_id` not available in decide consumer context

## Label Values

### Reason Values
- `tenant_validation_failed` → `~"tenant_validation_failed"`
- `backoff` → `~"backoff"`
- `backpressure` → `~"backpressure"`

### Source Values
- `~"router_result_consumer"` - from result consumer
- `~"router_ack_consumer"` - from ack consumer
- `~"router_decide_consumer"` - from decide consumer
- `~"router_jetstream"` - from internal router_jetstream calls (backoff, default)

## Backward Compatibility

✅ **Maintained**: `nak/2` still works (calls `nak/3` with empty context)
- Default values used: `assignment_id => ~"unknown"`, `request_id => ~"unknown"`, `source => ~"router_jetstream"`
- Existing code continues to work without changes

## Testing Requirements

**Next Steps** (not implemented in this change):
1. Unit tests: Verify metric is emitted with correct labels
2. Integration tests: Verify metric appears in Prometheus export
3. Fault injection tests: Verify metric is emitted during fault scenarios
4. Alert validation: Verify alerts fire when metric values exceed thresholds

## Validation Checklist

- [x] Metric name changed to `router_jetstream_redelivery_total`
- [x] All labels (`assignment_id`, `request_id`, `reason`, `source`) are present
- [x] All call sites updated to pass context
- [x] Prometheus metadata updated
- [x] Metrics export list updated
- [ ] Tests pass (requires test execution)
- [ ] Alerts work correctly (requires alert validation)
- [ ] Dashboard queries work correctly (requires dashboard validation)

## Files Modified

1. `apps/otp/router/src/router_jetstream.erl` - Core implementation
2. `apps/otp/router/src/router_prometheus.erl` - Metadata update
3. `apps/otp/router/src/router_result_consumer.erl` - Call site updates
4. `apps/otp/router/src/router_ack_consumer.erl` - Call site updates
5. `apps/otp/router/src/router_decide_consumer.erl` - Call site updates

## Impact

**Before**:
- Metric name: `router_redelivery_total` (no labels)
- Alerts: Will never fire (wrong metric name)
- Dashboards: Show no data (wrong metric name)
- Cannot filter by source/reason

**After**:
- Metric name: `router_jetstream_redelivery_total` (with labels)
- Alerts: Will fire correctly (correct metric name and labels)
- Dashboards: Will show data (correct metric name)
- Can filter by source/reason for detailed analysis

## Next Steps

1. **Run tests** to verify changes work correctly
2. **Update alert rules** if needed (should work with current rules after this fix)
3. **Validate dashboards** show data correctly
4. **Monitor in production** to ensure metrics are emitted correctly

## References

- **Validation Report**: `apps/otp/router/docs/dev/OBSERVABILITY_VALIDATION_REPORT.md`
- **Fix Plan**: `apps/otp/router/docs/dev/OBSERVABILITY_METRICS_FIX_PLAN.md`
- **Alert Rules**: `docs/observability/router-alert-rules.yaml`
- **Dashboard**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`

