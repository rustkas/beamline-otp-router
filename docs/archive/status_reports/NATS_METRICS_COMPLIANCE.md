# NATS Metrics Compliance Check

## Overview

This document verifies that `router_nats` metrics comply with:
- Project naming conventions
- Prometheus standards
- Architectural documentation requirements

## Naming Convention Compliance

### Prometheus Standards

✅ **All metrics follow Prometheus naming conventions**:

- **Counters**: `router_nats_*_total` ✅
  - `router_nats_connection_established_total`
  - `router_nats_connection_lost_total`
  - `router_nats_publish_total`
  - `router_nats_publish_failures_total`
  - etc.

- **Gauges**: `router_nats_*` (no suffix) ✅
  - `router_nats_connection_status`
  - `router_nats_pending_operations_count`

### Project Conventions

✅ **All metrics use `router_` prefix** (consistent with other router metrics):
- `router_nats_*` (not `nats_*` or `beamline_router_nats_*`)

✅ **All metrics use consistent naming**:
- Operation metrics: `router_nats_<operation>_total` / `router_nats_<operation>_failures_total`
- Connection metrics: `router_nats_connection_*_total`
- Queue metrics: `router_nats_pending_operations_*`

## Metrics Catalog

### Connection Metrics

| Metric Name | Type | Description | Public Contract |
|------------|------|-------------|-----------------|
| `router_nats_connection_status` | gauge | Connection status (0.0/0.5/1.0) | ✅ Public |
| `router_nats_connection_established_total` | counter | Total successful connections | ✅ Public |
| `router_nats_connection_lost_total` | counter | Total connection losses | ✅ Public |
| `router_nats_connection_restored_total` | counter | Total connection restorations | ✅ Public |
| `router_nats_connection_failures_total` | counter | Total connection failures | ✅ Public |

### Reconnection Metrics

| Metric Name | Type | Description | Public Contract |
|------------|------|-------------|-----------------|
| `router_nats_reconnect_attempts_total` | counter | Total reconnection attempts | ✅ Public |
| `router_nats_reconnect_failures_total` | counter | Total reconnection failures | ✅ Public |
| `router_nats_reconnection_exhausted_total` | counter | Total times reconnection exhausted | ✅ Public |

### Operation Metrics

| Metric Name | Type | Description | Public Contract |
|------------|------|-------------|-----------------|
| `router_nats_publish_total` | counter | Total publish operations | ✅ Public |
| `router_nats_publish_failures_total` | counter | Total publish failures | ✅ Public |
| `router_nats_publish_with_ack_total` | counter | Total publish_with_ack operations | ✅ Public |
| `router_nats_publish_with_ack_failures_total` | counter | Total publish_with_ack failures | ✅ Public |
| `router_nats_ack_total` | counter | Total ACK operations | ✅ Public |
| `router_nats_ack_failures_total` | counter | Total ACK failures | ✅ Public |
| `router_nats_nak_total` | counter | Total NAK operations | ✅ Public |
| `router_nats_nak_failures_total` | counter | Total NAK failures | ✅ Public |
| `router_nats_subscribe_total` | counter | Total subscribe operations | ✅ Public |
| `router_nats_subscribe_failures_total` | counter | Total subscribe failures | ✅ Public |

### Queue Metrics

| Metric Name | Type | Description | Public Contract |
|------------|------|-------------|-----------------|
| `router_nats_pending_operations_count` | gauge | Current pending operations count | ✅ Public |
| `router_nats_pending_operations_retry` | counter | Total retry attempts | ✅ Public |
| `router_nats_pending_operations_retry_success` | counter | Total successful retries | ✅ Public |
| `router_nats_pending_operations_retry_failed` | counter | Total failed retries | ✅ Public |
| `router_nats_pending_operations_dropped_total` | counter | Total operations dropped (queue full) | ✅ Public |

## Log Compliance

### Error Code Naming

✅ **All error codes follow pattern `NATS_<OPERATION>_<STATUS>`**:
- `NATS_CONNECTION_ESTABLISHED`
- `NATS_CONNECTION_LOST`
- `NATS_CONNECTION_RESTORED`
- `NATS_RECONNECT_ATTEMPT`
- `NATS_PUBLISH_ERROR`
- `NATS_PUBLISH_QUEUED`
- `NATS_ACK_*`
- `NATS_QUEUE_*`
- `NATS_RETRY_*`

### Error Tag Naming

✅ **All error tags use lowercase with underscores**:
- `nats_connection_failure`
- `nats_connection_recovered`
- `nats_reconnecting`

### Log Format Compliance

✅ **All logs include required fields**:
- `error_code` (machine-readable)
- `error_tag` (for filtering)
- `message` (human-readable)
- `timestamp` (ISO 8601)

## Compliance Summary

### Metrics

- ✅ **Naming**: All metrics follow Prometheus conventions
- ✅ **Prefix**: All metrics use `router_` prefix
- ✅ **Suffix**: Counters use `_total`, gauges have no suffix
- ✅ **Public Contract**: All metrics documented in `NATS_CONNECTION_RESILIENCE.md`

### Logs

- ✅ **Error Codes**: All error codes follow `NATS_<OPERATION>_<STATUS>` pattern
- ✅ **Error Tags**: All error tags use lowercase with underscores
- ✅ **Format**: All logs include `error_code`, `error_tag`, `message`, `timestamp`
- ✅ **Public Contract**: All error codes documented in `NATS_CONNECTION_RESILIENCE.md`

## Recommendations

### No Changes Required

All metrics and logs comply with:
- ✅ Prometheus naming conventions
- ✅ Project naming conventions
- ✅ Architectural documentation requirements

### Future Considerations

If adding new metrics:
1. Follow existing naming patterns
2. Document in `NATS_CONNECTION_RESILIENCE.md`
3. Add to Prometheus alerts if critical
4. Update Grafana dashboards

## References

- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Complete metrics/logs documentation
- `apps/otp/router/docs/NATS_METRICS_ALERTS.md` - Prometheus alerts
- `apps/otp/router/docs/OBSERVABILITY.md` - General observability documentation

