# Release Notes: gRPC API Documentation

**Date**: 2025-11-09  
**Document Version**: Update for production readiness

## Overview

The gRPC API documentation (`docs/GRPC_API.md`) has been expanded with practical recommendations for operations, diagnostics, and integration. Added sections on retries, error handling, event deduplication, and correlation with system events.

## Added Sections

### Error Handling

- **Canonical reasons (summary)**: Unified list of canonical `reason` values for all error statuses
  - `unavailable`: `upstream_unreachable`, `transport_error`, `resolver_failure`
  - `resource_exhausted`: `queue_overflow`, `concurrency_limit`, `rate_limited`
  - `deadline_exceeded`: `queue_wait_timeout`, `processing_timeout`
  - `cancelled`: `client_cancelled`, `deadline_exceeded` (client deadline)

- **Retryability (UNAVAILABLE vs RESOURCE_EXHAUSTED)**: Retry recommendations for different error types
  - `UNAVAILABLE (14)`: temporary error, retries with exponential backoff (full jitter)
  - `RESOURCE_EXHAUSTED (8)`: retries only when recovery signs are present (decreasing `queue_len`)

- **RPC Response Examples**: Added examples for `CANCELLED (1)`, `UNAVAILABLE (14)`, `RESOURCE_EXHAUSTED (8)`, `DEADLINE_EXCEEDED (4)`

### Request Metadata

- **Retries and Idempotency**: Preserving `x-correlation-id` and `operation_id`/`transfer_id` during retries
- **Idempotent RPCs**: Checklist for clients with implementation examples
- **Retry Policy (Recommendations)**: Backoff parameters (min/max/multiplier + full jitter)
- **Client Interceptors and Retries**: Recommendations for working with middleware and service config
- **Deadlines and RPC Cancellation**: Preserving `x-correlation-id` on cancellation, publishing `rpc_cancelled`
- **Event Publishing Policy: timeout vs cancel**: Distinction between `rpc_timeout` and `rpc_cancelled`

### Telemetry and Observability

- **Event Delivery and Deduplication**: "at-least-once" semantics, deduplication by `table + correlation_id + operation_id/transfer_id`
- **Idempotency and Deduplication Window**: TTL window (10–30 minutes), accounting for `clock skew` for cross-DC deployments
- **Resilience to Consumer Restarts**: Persistent storage for deduplication

### Performance

- **Playbook: Latency Diagnostics**: Checklist for SRE/integrators
  - Checking `queue_len` and `wait_duration_us` by `service`
  - Event deduplication
  - Checking client retries (backoff, full jitter)
  - Accounting for `clock skew`

- **Playbook: Correlation with System Events**: Tips for correlating errors
  - `unavailable`: correlation with network/balancer/resolver logs
  - `resource_exhausted`: checking worker limits, `queue_len`, traffic changes
  - `deadline_exceeded`: correlation with `latency_crit_ms` thresholds and retry policy

- **Metric Cardinality**: Recommendations for using labels (avoid `correlation_id` in metrics)
- **Mini Examples of Requests/Alerts**: Examples for timeout alerts and cancellation spikes

### Security

- **Metadata Security**: Maximum metadata size (≤8KB), truncation policy
- **Property-based Testing**: Checklist for testing `extract_correlation_id/1`

## Improvements to Existing Sections

- **Correlation ID**: Added gRPC metadata rules, value hygiene, ULID monotonicity, streaming retries
- **Streaming RPCs**: Bidi-streaming semantics, placing `x-correlation-id` in initial metadata
- **Units of Measurement**: Explicit specification of units for all measurements
- **Stability of `.proto` References**: Recommendations for using `GITHUB_SHA` in CI

## Recommendations for Integrators

1. **Retries**: Use exponential backoff with full jitter (not linear backoff)
   - Minimum: 100ms, maximum: 30s, multiplier: 2.0
   - Preserve original `x-correlation-id` and `operation_id`/`transfer_id` for all attempts

2. **Deduplication**: Use TTL window of 10–30 minutes for key `table + correlation_id + operation_id/transfer_id`
   - Account for `clock skew` in cross-DC deployments (+5–10 minutes)
   - Use persistent storage for resilience to consumer restarts

3. **Monitoring**: Avoid using `correlation_id` as a metric label
   - Allowed labels: `service`, `otp_version`, operation type
   - Keep `correlation_id` in logs/traces

4. **Error Handling**: Use canonical `reason` from the "Canonical reasons (summary)" section
   - For RPC: human-readable `message` in gRPC Status
   - For telemetry: compact `reason` atom in metadata

## Quality Checks

- ✅ All TOC anchors verified and correct (37 links)
- ✅ `reason` uniformity confirmed (all examples match canonical reasons)
- ✅ Links from README added to new sections

## CI Recommendations

For automatic link/anchor checking in CI, you can use:

```yaml
- name: Check markdown links
  run: |
    # Use markdown-link-check or similar tool
    npx markdown-link-check docs/GRPC_API.md --config .markdown-link-check.json
```

Or for anchor checking:

```bash
# Simple TOC anchor check
python3 scripts/check_markdown_anchors.py docs/GRPC_API.md
```

## See Also

- [docs/GRPC_API.md](GRPC_API.md) — complete API documentation
- [docs/PERFORMANCE.md](PERFORMANCE.md) — performance details and threshold values
- [docs/ETS_SEMANTICS.md](ETS_SEMANTICS.md) — ETS transfer protocol and fault tolerance
