# Release Notes: router_jetstream_redelivery_total Metric Enhancement

**Version**: CP2  
**Date**: 2025-11-30  
**Type**: Observability Enhancement

## Summary

Enhanced `router_jetstream_redelivery_total` metric with full label support, enabling detailed observability and fault injection coverage for redelivery scenarios.

## Changes

### Metric Enhancement
- **Metric**: `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}`
- **Labels Added**: `assignment_id`, `request_id`, `reason`, `source`
- **Storage**: ETS with labeled metrics support
- **Export**: Prometheus text format with labels

### Logging
- **Added**: Structured log entry on redelivery
- **Message**: `"Message redelivery requested"` (INFO level)
- **Context**: Full redelivery context (assignment_id, request_id, reason, source, delivery_count, msg_id)

### Infrastructure
- **ETS**: Support for labeled metrics (`{{Name, LabelsKey}, Value}`)
- **Prometheus**: Label formatting in text export
- **Backward Compatibility**: Unlabeled metrics continue to work

## Impact

### Observability
- ✅ **Filtering**: Metrics can be filtered by source/reason
- ✅ **Alerting**: Alerts can target specific failure modes
- ✅ **Dashboards**: Grafana can visualize by source/reason
- ✅ **Correlation**: Logs enable correlation with metrics

### Fault Injection Coverage
- ✅ **S1 (ACK/NAK Errors)**: Covered via `reason="ack_error"/"nak_error"`
- ✅ **S2 (Processing Delays)**: Covered via `reason="backpressure"`
- ✅ **S3 (MaxDeliver Exhaustion)**: Already covered (separate metric)

### Backward Compatibility
- ✅ Unlabeled metrics continue to work
- ✅ `nak/2` function maintained
- ✅ No breaking changes

## Files Changed

**Source (6 files)**:
- `router_jetstream.erl` - Core implementation + logging
- `router_metrics.erl` - Label support
- `router_prometheus.erl` - Label formatting
- `router_result_consumer.erl` - Context passing
- `router_ack_consumer.erl` - Context passing
- `router_decide_consumer.erl` - Context passing

**Tests (4 files)**:
- `router_jetstream_redelivery_metrics_SUITE.erl` (new)
- `router_jetstream_redelivery_runtime_SUITE.erl` (new)
- `router_metrics_dump_SUITE.erl` (updated)
- `router_jetstream_e2e_SUITE.erl` (updated)

**Documentation (10+ files)**:
- `OBSERVABILITY_ROUTER_DASHBOARD.md` (updated)
- Comprehensive dev reports and guides

## Validation

- ✅ Static validation: PASSED
- ✅ Runtime validation: PASSED
- ✅ Staging validation: Ready (guide provided)
- ✅ Alert rules: Validated (4 alerts)

## Migration

**No migration required** - backward compatible.

**For new code**: Use `router_jetstream:nak/3` with context for labeled metrics:
```erlang
router_jetstream:nak(Msg, Reason, #{
    assignment_id => AssignmentId,
    request_id => RequestId,
    source => Source
})
```

## References

- **Implementation**: `docs/dev/OBSERVABILITY_METRICS_FIX_IMPLEMENTATION.md`
- **Labels Support**: `docs/dev/OBSERVABILITY_METRICS_LABELS_IMPLEMENTATION.md`
- **Staging Guide**: `docs/dev/STAGING_VALIDATION_GUIDE.md`
- **Task Completion**: `docs/dev/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md`

