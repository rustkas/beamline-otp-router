# PR Description Template

**Copy this content to PR description when opening the PR.**

---

# router_jetstream_redelivery_total Metric Enhancement

## Summary

Complete implementation of `router_jetstream_redelivery_total` metric with full label support, ETS/Prometheus export, logging, tests, and comprehensive validation.

## Changes Overview

### Core Implementation
- **Metric**: `router_jetstream_redelivery_total{assignment_id,request_id,reason,source}`
- **Storage**: ETS with labeled metrics support
- **Export**: Prometheus text format with labels
- **Logging**: Structured log entry on redelivery (`"Message redelivery requested"`)

### Files Changed
- **Source**: 6 files (router_jetstream, router_metrics, router_prometheus, 3 consumers)
- **Tests**: 4 suites (2 new, 2 updated)
- **Scripts**: 7 validation scripts
- **Documentation**: 10+ files

## Impact

### ✅ Observability
- **Metrics**: Full label support enables filtering by source/reason
- **Alerts**: 4 alerts correctly configured and validated
- **Dashboards**: Ready for Grafana visualization by source/reason
- **Logs**: Structured logging enables correlation with metrics

### ✅ Backward Compatibility
- Unlabeled metrics continue to work (`{Name, Value}` format)
- `nak/2` function maintained for backward compatibility
- No breaking changes

### ✅ Fault Injection Coverage
- **S1 (ACK/NAK Errors)**: Covered via `reason="ack_error"/"nak_error"`
- **S2 (Processing Delays)**: Covered via `reason="backpressure"`
- **S3 (MaxDeliver Exhaustion)**: Already covered (separate metric)

## Validation

- ✅ Static validation: PASSED
- ✅ Runtime validation: PASSED
- ✅ Staging validation: Guide provided (`docs/archive/dev/STAGING_VALIDATION_GUIDE.md`)

## Testing

- `router_jetstream_redelivery_metrics_SUITE.erl` (new)
- `router_jetstream_redelivery_runtime_SUITE.erl` (new)
- `router_metrics_dump_SUITE.erl` (updated)
- `router_jetstream_e2e_SUITE.erl` (updated)

## Documentation

- User-facing: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` (updated)
- Dev reports: 10+ comprehensive reports
- ADR updates: ADR-014, ADR-011

## References

- **Task Completion**: `apps/otp/router/docs/dev/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md`
- **Staging Validation**: `apps/otp/router/docs/dev/STAGING_VALIDATION_COMPLETE.md`
- **Final Report**: `apps/otp/router/docs/dev/OBSERVABILITY_REDELIVERY_FINAL_REPORT.md`
- **ADR-014**: `docs/ADR/ADR-014-metrics-tracing.md#updates`
- **ADR-011**: `docs/ADR/ADR-011-jetstream-e2e.md#updates`

## Checklist

- [x] Code implemented and tested
- [x] Tests pass
- [x] Documentation updated
- [x] Alert rules validated
- [x] Validation scripts created
- [x] Staging validation guide provided
- [x] Backward compatibility maintained
- [x] Ready for staging deployment

---

**Status**: ✅ **READY FOR REVIEW**

