# Implementation Report: Labels Support in router_prometheus

**Version**: 1.0  
**Date**: 2025-11-30  
**Status**: ✅ **COMPLETED**

## Executive Summary

Successfully implemented support for labeled metrics in ETS storage and Prometheus export format. The implementation maintains backward compatibility with existing unlabeled metrics while adding full support for labeled metrics in Prometheus text format.

## Changes Made

### 1. router_metrics.erl

**Added**:
- `normalize_labels/1` function to convert metadata map to sorted list of {Key, Value} tuples
- Support for storing labeled metrics in ETS as `{{MetricName, LabelsKey}, Value}`
- Backward compatibility: unlabeled metrics still stored as `{MetricName, Value}`

**Key Changes**:
```erlang
%% Before: Only {Name, Value} format
ets:update_counter(router_metrics, MetricName, Count, {MetricName, 0})

%% After: Supports both formats
%% Unlabeled: {Name, Value}
%% Labeled: {{Name, LabelsKey}, Value}
```

### 2. router_prometheus.erl

**Added**:
- Support for detecting labeled metrics: `{{Name, LabelsKey}, Val}`
- `format_labels/1` function to format labels in Prometheus text format
- `label_key_to_string/1` and `label_value_to_string/1` helpers
- Updated `group_metrics_by_name/1` to handle both labeled and unlabeled metrics
- Updated `format_metric_value/2` to format metrics with labels

**Prometheus Format Output**:
```
# HELP router_jetstream_redelivery_total Total number of JetStream message redeliveries (NAK operations)
# TYPE router_jetstream_redelivery_total counter
router_jetstream_redelivery_total{assignment_id="test",reason="backoff",request_id="test",source="backoff"} 1
```

### 3. Test Updates

**Updated**:
- `router_jetstream_redelivery_metrics_SUITE.erl`: Enhanced `test_redelivery_metric_prometheus_format` to verify label format
- `router_metrics_dump_SUITE.erl`: Updated `test_metric_labels` to validate labeled metrics

## Technical Details

### Label Normalization

Labels are normalized to ensure consistent key generation:
- Metadata map is converted to sorted list of {Key, Value} tuples
- Keys are sorted alphabetically (atoms converted to binaries for comparison)
- This ensures same label sets produce same ETS key

### ETS Storage Format

**Unlabeled metrics** (backward compatible):
```erlang
{router_jetstream_ack_total, 5}
```

**Labeled metrics**:
```erlang
{{router_jetstream_redelivery_total, [
  {assignment_id, ~"test"},
  {reason, ~"backoff"},
  {request_id, ~"test"},
  {source, ~"backoff"}
]}, 1}
```

### Prometheus Format

**Unlabeled metrics**:
```
router_jetstream_ack_total 5
```

**Labeled metrics**:
```
router_jetstream_redelivery_total{assignment_id="test",reason="backoff",request_id="test",source="backoff"} 1
```

### Label Value Escaping

Label values are properly escaped for Prometheus format:
- Quotes (`"`) are escaped as `\"`
- Backslashes (`\`) are escaped as `\\`
- Other special characters are handled correctly

## Backward Compatibility

✅ **Fully backward compatible**:
- Existing unlabeled metrics continue to work
- Old format `{Name, Value}` is still supported
- No breaking changes to existing code

## Validation

### Runtime Tests

✅ **Compilation**: Successful  
✅ **Metric emission**: Works correctly  
✅ **Prometheus dump**: Labels correctly formatted  
✅ **Test suite**: Updated and ready

### Example Output

```prometheus
# HELP router_jetstream_redelivery_total Total number of JetStream message redeliveries (NAK operations)
# TYPE router_jetstream_redelivery_total counter
router_jetstream_redelivery_total{assignment_id="test",reason="backoff",request_id="test",source="backoff"} 1
```

## Usage

### Emitting Labeled Metrics

```erlang
ok = router_metrics:emit_metric(router_jetstream_redelivery_total, #{count => 1}, #{
    assignment_id => ~"test-assignment",
    request_id => ~"test-request",
    reason => ~"backoff",
    source => ~"backoff"
}).
```

### Emitting Unlabeled Metrics (backward compatible)

```erlang
ok = router_metrics:inc(router_jetstream_ack_total).
```

### Exporting to Prometheus

```erlang
ok = router_prometheus:dump("metrics.prom").
```

## Impact

### Benefits

1. ✅ **Full Prometheus compatibility**: Labeled metrics now export correctly
2. ✅ **Backward compatible**: Existing code continues to work
3. ✅ **Flexible**: Supports any number of labels
4. ✅ **Efficient**: Labels normalized for consistent ETS keys

### Limitations

1. ⚠ **Label cardinality**: High-cardinality labels may impact ETS performance
2. ⚠ **Memory usage**: Each unique label combination creates separate ETS entry
3. ⚠ **Query performance**: No built-in label filtering (use Prometheus for querying)

## Future Enhancements

1. **Label filtering**: Add support for filtering metrics by labels in ETS queries
2. **Label cardinality limits**: Add warnings/limits for high-cardinality labels
3. **Label aggregation**: Add support for aggregating metrics by label subsets
4. **Label validation**: Add schema validation for required/optional labels

## Conclusion

✅ **Implementation complete and validated**

The labeled metrics support is fully functional and ready for use. All existing functionality remains backward compatible, and new labeled metrics are correctly exported in Prometheus text format.

