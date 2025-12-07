# Plan: Fix router_jetstream_redelivery_total Metric

**Version**: 1.0  
**Date**: 2025-11-30  
**Status**: 🚧 **Implementation Plan**

## Problem Summary

1. **Metric name mismatch**: Code uses `router_redelivery_total`, docs/alerts use `router_jetstream_redelivery_total`
2. **Missing labels**: Metric is created without required labels (`assignment_id`, `request_id`, `reason`, `source`)
3. **Broken alerts**: Alerts will never fire due to wrong metric name and missing labels

## Solution Design

### 1. Change Metric Name

**Decision**: Use `router_jetstream_redelivery_total` (consistent with other JetStream metrics)

**Changes**:
- `router_jetstream.erl`: Change `router_redelivery_total` → `router_jetstream_redelivery_total`
- `router_prometheus.erl`: Update metadata
- `router_jetstream.erl`: Update `metrics/0` export list

### 2. Add Labels Support

**Approach**: Make `nak/2` accept optional context map with labels

**New Signature**:
```erlang
%% Option 1: Add optional context parameter (backward compatible)
nak(#{id := Id} = Msg, Reason) ->
    nak(Msg, Reason, #{}).

nak(#{id := Id} = Msg, Reason, Context) ->
    %% Extract labels from Context or use defaults
    AssignmentId = maps:get(assignment_id, Context, <<"unknown">>),
    RequestId = maps:get(request_id, Context, <<"unknown">>),
    Source = maps:get(source, Context, <<"router_jetstream">>),
    ReasonBin = reason_to_binary(Reason),
    
    %% Emit metric with labels
    emit_counter(router_jetstream_redelivery_total, #{
        assignment_id => AssignmentId,
        request_id => RequestId,
        reason => ReasonBin,
        source => Source
    }),
    %% ... rest of implementation
```

**Alternative**: Keep `nak/2` for backward compatibility, add `nak/3` for labeled version

### 3. Update All Call Sites

**Call sites to update**:
- `router_result_consumer.erl`: Lines 594, 624 (tenant validation failures)
- `router_ack_consumer.erl`: Lines 262, 287 (tenant validation failures)
- `router_decide_consumer.erl`: Line 496 (backpressure)
- `router_jetstream.erl`: Line 129 (backoff in handle/2)

**Context extraction**:
- `router_result_consumer`: Has `AssignmentId`, `RequestId` available
- `router_ack_consumer`: Has `AssignmentId` available (may not have `RequestId`)
- `router_decide_consumer`: Has `RequestId` available (may not have `AssignmentId`)
- `router_jetstream`: Internal call, no context available

### 4. Label Value Mapping

**Reason values** (from atom to binary):
- `tenant_validation_failed` → `<<"tenant_validation_failed">>`
- `backoff` → `<<"backoff">>`
- `backpressure` → `<<"backpressure">>`
- `ack_error` → `<<"ack_error">>` (if added)
- `nak_error` → `<<"nak_error">>` (if added)

**Source values**:
- `<<"router_result_consumer">>` - from result consumer
- `<<"router_ack_consumer">>` - from ack consumer
- `<<"router_decide_consumer">>` - from decide consumer
- `<<"router_jetstream">>` - from internal router_jetstream calls (backoff)

### 5. Implementation Steps

1. **Add helper function for emit_counter in router_jetstream.erl**
   - Or use existing telemetry pattern
   - Need to check if `emit_counter` is available or need to add it

2. **Update nak/2 signature** (backward compatible)
   - Add `nak/3` variant with context
   - Keep `nak/2` calling `nak/3` with empty context

3. **Update all call sites** to pass context
   - Extract available IDs from surrounding code
   - Pass appropriate source value

4. **Update Prometheus metadata**
   - Add `router_jetstream_redelivery_total` with label descriptions

5. **Update metrics/0 export**
   - Change `router_redelivery_total` → `router_jetstream_redelivery_total`

## Implementation Details

### Helper Function for emit_counter

Since `router_jetstream.erl` doesn't have `emit_counter` helper, we have two options:

**Option A**: Add emit_counter helper to router_jetstream.erl
```erlang
%% Internal: Emit telemetry counter
-spec emit_counter(atom(), map()) -> ok.
emit_counter(CounterName, Metadata) ->
    telemetry:execute(
        [router, jetstream, CounterName],
        #{count => 1},
        Metadata
    ),
    %% Also update ETS for Prometheus export
    router_metrics:emit_metric(CounterName, #{count => 1}, Metadata).
```

**Option B**: Use router_metrics:emit_metric directly
```erlang
router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
    assignment_id => AssignmentId,
    request_id => RequestId,
    reason => ReasonBin,
    source => Source
}).
```

**Decision**: Use Option B (router_metrics:emit_metric) - it's already available and handles both telemetry and ETS.

### Backward Compatibility

To maintain backward compatibility, we'll:
1. Keep `nak/2` signature
2. Add `nak/3` with context
3. Make `nak/2` call `nak/3` with default empty context

This ensures existing code continues to work, but new code can pass context.

## Testing Requirements

1. **Unit tests**: Verify metric is emitted with correct labels
2. **Integration tests**: Verify metric appears in Prometheus export
3. **Fault injection tests**: Verify metric is emitted during fault scenarios
4. **Alert tests**: Verify alerts fire when metric values exceed thresholds

## Files to Modify

1. `apps/otp/router/src/router_jetstream.erl`
   - Update `nak/2` and add `nak/3`
   - Add helper for reason conversion
   - Update `metrics/0` export

2. `apps/otp/router/src/router_prometheus.erl`
   - Update metadata for `router_jetstream_redelivery_total`

3. `apps/otp/router/src/router_result_consumer.erl`
   - Update calls to `router_jetstream:nak/2` → `router_jetstream:nak/3`

4. `apps/otp/router/src/router_ack_consumer.erl`
   - Update calls to `router_jetstream:nak/2` → `router_jetstream:nak/3`

5. `apps/otp/router/src/router_decide_consumer.erl`
   - Update calls to `router_jetstream:nak/2` → `router_jetstream:nak/3`

## Validation Checklist

- [ ] Metric name changed to `router_jetstream_redelivery_total`
- [ ] All labels (`assignment_id`, `request_id`, `reason`, `source`) are present
- [ ] All call sites updated to pass context
- [ ] Prometheus metadata updated
- [ ] Metrics export list updated
- [ ] Tests pass
- [ ] Alerts work correctly
- [ ] Dashboard queries work correctly

