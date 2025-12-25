# R10 Metrics Review: Specification vs Implementation

## Purpose

This document maps R10 specification metrics to actual implementation metrics in the codebase. It identifies gaps and provides recommendations for metric implementation.

## Metric Mapping Table

| R10 Specification | Implementation Status | Actual Metric Name | Location | Notes |
|-------------------|----------------------|-------------------|----------|-------|
| `publish_attempts_total{status="success\|error", retry_count="0\|1\|2\|..."}` | ❌ **MISSING** | N/A | N/A | Need to implement with retry count tracking |
| `publish_errors_total{error_type="timeout\|connection\|nack\|..."}` | ⚠️ **PARTIAL** | `router_nats_publish_failures_total` | `router_nats.erl:420` | Has failures counter, but no `error_type` label |
| `publish_latency_seconds{quantile="0.5\|0.9\|0.99"}` | ❌ **MISSING** | N/A | N/A | Need to implement histogram |
| `circuit_breaker_state{state="closed\|open\|half_open"}` | ❌ **MISSING** | N/A | N/A | Need to implement gauge |
| `circuit_breaker_transitions_total{from="...", to="..."}` | ✅ **EXISTS** | `router_circuit_breaker_state_transitions_total` | `router_circuit_breaker.erl:466,689` | Has state transitions counter |
| `circuit_breaker_trigger_reason{reason="consecutive_failures\|error_rate\|latency"}` | ❌ **MISSING** | N/A | N/A | Need to implement info/gauge metric |
| `publish_retry_delay_seconds{attempt="1\|2\|3\|..."}` | ❌ **MISSING** | N/A | N/A | Need to implement histogram |
| `system_cpu_usage_percent` | ⚠️ **EXTERNAL** | Standard Prometheus | node_exporter | Provided by monitoring stack |
| `system_memory_usage_bytes` | ⚠️ **EXTERNAL** | Standard Prometheus | node_exporter | Provided by monitoring stack |
| `input_request_latency_seconds{quantile="0.5\|0.9\|0.99"}` | ⚠️ **UNKNOWN** | Gateway metrics | NestJS Gateway | Need to verify Gateway implementation |

## Detailed Metric Analysis

### 1. Publish Attempts with Retry Count

**R10 Specification**:
```
publish_attempts_total{status="success|error", retry_count="0|1|2|..."}
```

**Status**: ❌ **MISSING**

**Current Implementation**:
- `router_nats_publish_total` (counter) - Total publish operations
- No retry count tracking
- No status label (success/error)

**Gap**:
- Need to track retry count per publish attempt
- Need to add `status` label (success/error)
- Need to add `retry_count` label

**Recommendation**:
```erlang
%% In router_nats.erl or new router_nats_publish_retry.erl
router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
    status => ~"success",  % or ~"error"
    retry_count => RetryCount  % 0, 1, 2, ...
}),
```

**Implementation Priority**: **HIGH** (required for R10)

### 2. Publish Errors with Error Type

**R10 Specification**:
```
publish_errors_total{error_type="timeout|connection|nack|..."}
```

**Status**: ⚠️ **PARTIAL**

**Current Implementation**:
- `router_nats_publish_failures_total` (counter) - Total publish failures
- Location: `router_nats.erl:420,443`
- No `error_type` label

**Gap**:
- Have failures counter, but no error type classification
- Need to add `error_type` label with values: `timeout`, `connection`, `nack`, etc.

**Recommendation**:
```erlang
%% In router_nats.erl
ErrorType = classify_error(Error),  % timeout, connection, nack, etc.
router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
    error_type => ErrorType
}),
```

**Implementation Priority**: **MEDIUM** (useful for debugging, not critical for R10)

### 3. Publish Latency

**R10 Specification**:
```
publish_latency_seconds{quantile="0.5|0.9|0.99"}
```

**Status**: ❌ **MISSING**

**Current Implementation**:
- No publish latency tracking
- No histogram metric

**Gap**:
- Need to measure time from publish attempt to broker response
- Need to implement histogram metric with quantiles

**Recommendation**:
```erlang
%% In router_nats.erl
StartTime = erlang:system_time(microsecond),
Result = publish_impl(...),
LatencyMs = (erlang:system_time(microsecond) - StartTime) / 1000.0,
router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => LatencyMs / 1000.0}, #{}),
%% Or use histogram:
router_metrics:observe(router_nats_publish_latency_seconds, LatencyMs / 1000.0, #{}),
```

**Implementation Priority**: **HIGH** (required for latency-based circuit breaker trigger)

### 4. Circuit Breaker State Gauge

**R10 Specification**:
```
circuit_breaker_state{state="closed|open|half_open"}
```

**Status**: ❌ **MISSING**

**Current Implementation**:
- `router_circuit_breaker_state_transitions_total` (counter) - State transitions
- No current state gauge
- State stored in ETS, not exposed as metric

**Gap**:
- Need to expose current circuit breaker state as gauge
- Need to update gauge on state changes

**Recommendation**:
```erlang
%% In router_circuit_breaker.erl
update_state(ExtensionId, NewState, Timestamp) ->
    %% ... existing code ...
    %% Emit state gauge
    StateValue = case NewState of
        closed -> 0;
        open -> 1;
        half_open -> 2
    end,
    router_metrics:emit_metric(router_circuit_breaker_state, #{value => StateValue}, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => atom_to_binary(NewState, utf8)  % "closed", "open", "half_open"
    }),
    ok.
```

**Implementation Priority**: **HIGH** (required for R10 test assertions)

### 5. Circuit Breaker Transitions

**R10 Specification**:
```
circuit_breaker_transitions_total{from="...", to="..."}
```

**Status**: ✅ **EXISTS**

**Current Implementation**:
- `router_circuit_breaker_state_transitions_total` (counter)
- Location: `router_circuit_breaker.erl:466,689`
- Has `state` label (target state: "closed", "open", "half_open")
- Missing `from` label (source state)

**Gap**:
- Have transitions counter, but missing `from` label
- Current: `router_circuit_breaker_state_transitions_total{state="open"}`
- Needed: `router_circuit_breaker_state_transitions_total{from="closed", to="open"}`

**Recommendation**:
```erlang
%% In router_circuit_breaker.erl
CurrentState = CBState#circuit_breaker_state.state,
NewState = open,  % or closed, half_open
router_metrics:emit_metric(router_circuit_breaker_state_transitions_total, #{count => 1}, #{
    tenant_id => TenantId,
    provider_id => ProviderId,
    from => atom_to_binary(CurrentState, utf8),
    to => atom_to_binary(NewState, utf8)
}),
```

**Implementation Priority**: **LOW** (nice to have, but current implementation is usable)

### 6. Circuit Breaker Trigger Reason

**R10 Specification**:
```
circuit_breaker_trigger_reason{reason="consecutive_failures|error_rate|latency"}
```

**Status**: ❌ **MISSING**

**Current Implementation**:
- No trigger reason tracking
- Reason calculated in code but not exposed as metric

**Gap**:
- Need to track why circuit breaker opened
- Need to expose as info/gauge metric

**Recommendation**:
```erlang
%% In router_circuit_breaker.erl
Reason = if
    FailureCount >= FailureThreshold -> ~"consecutive_failures";
    SlidingErrorRate >= ErrorRateThreshold -> ~"error_rate";
    LatencyExceeded -> ~"latency";  % When latency trigger is implemented
    true -> ~"unknown"
end,
router_metrics:emit_metric(router_circuit_breaker_trigger_reason, #{value => 1}, #{
    tenant_id => TenantId,
    provider_id => ProviderId,
    reason => Reason
}),
```

**Implementation Priority**: **MEDIUM** (useful for debugging, not critical for R10)

### 7. Publish Retry Delay

**R10 Specification**:
```
publish_retry_delay_seconds{attempt="1|2|3|..."}
```

**Status**: ❌ **MISSING**

**Current Implementation**:
- No retry delay tracking
- Retry logic not fully implemented

**Gap**:
- Need to track delay between retry attempts
- Need to implement histogram metric

**Recommendation**:
```erlang
%% In router_nats_publish_retry.erl (to be created)
DelayMs = calculate_backoff(Attempt, BackoffStrategy, BackoffBase, BackoffMax, Jitter),
timer:sleep(DelayMs),
router_metrics:observe(router_nats_publish_retry_delay_seconds, DelayMs / 1000.0, #{
    attempt => integer_to_binary(Attempt)
}),
```

**Implementation Priority**: **MEDIUM** (useful for validation, not critical for R10)

### 8. System Metrics

**R10 Specification**:
```
system_cpu_usage_percent
system_memory_usage_bytes
```

**Status**: ⚠️ **EXTERNAL**

**Current Implementation**:
- Not implemented in Router code
- Provided by monitoring stack (node_exporter, Prometheus)

**Gap**:
- Need to ensure system metrics are available in test environment
- May need to add node_exporter or similar

**Recommendation**:
- Use standard Prometheus node_exporter metrics:
  - `process_cpu_seconds_total` (rate to get CPU usage)
  - `process_resident_memory_bytes` (memory usage)
- Or implement custom system metrics in Router if needed

**Implementation Priority**: **LOW** (provided by monitoring stack)

### 9. Input Request Latency

**R10 Specification**:
```
input_request_latency_seconds{quantile="0.5|0.9|0.99"}
```

**Status**: ⚠️ **UNKNOWN**

**Current Implementation**:
- Not in Router code (measured at Gateway)
- Need to verify Gateway (NestJS) implementation

**Gap**:
- Need to check Gateway metrics implementation
- May be `http_request_duration_seconds` or `grpc_request_duration_seconds`

**Recommendation**:
- Verify Gateway metrics implementation
- Use standard HTTP/gRPC metrics if available
- Or implement custom metric in Gateway

**Implementation Priority**: **MEDIUM** (required for E2E latency validation)

## Metric Name Mapping for Tests

When implementing R10 tests, use this mapping:

| R10 Specification | Test Code (Actual Metric) | Notes |
|-------------------|---------------------------|-------|
| `publish_attempts_total` | `router_nats_publish_total` (temporary) | Use until `publish_attempts_total` is implemented |
| `publish_errors_total` | `router_nats_publish_failures_total` | Use until `error_type` label is added |
| `publish_latency_seconds` | `router_nats_publish_latency_seconds` | To be implemented |
| `circuit_breaker_state` | `router_circuit_breaker_state` | To be implemented |
| `circuit_breaker_transitions_total` | `router_circuit_breaker_state_transitions_total` | Use current implementation |
| `circuit_breaker_trigger_reason` | `router_circuit_breaker_trigger_reason` | To be implemented |
| `publish_retry_delay_seconds` | `router_nats_publish_retry_delay_seconds` | To be implemented |
| `system_cpu_usage_percent` | `process_cpu_seconds_total` (rate) | From node_exporter |
| `system_memory_usage_bytes` | `process_resident_memory_bytes` | From node_exporter |
| `input_request_latency_seconds` | Gateway metrics (TBD) | Verify Gateway implementation |

## Implementation Recommendations

### Phase 1: Critical Metrics (Before R10 Tests)

1. **Circuit breaker state gauge** (`router_circuit_breaker_state`)
   - Priority: HIGH
   - Required for: Test assertions
   - Effort: Low (add gauge emission on state change)

2. **Publish latency histogram** (`router_nats_publish_latency_seconds`)
   - Priority: HIGH
   - Required for: Latency-based circuit breaker trigger
   - Effort: Medium (add timing measurement)

3. **Publish attempts with retry count** (`router_nats_publish_attempts_total`)
   - Priority: HIGH
   - Required for: Retry validation
   - Effort: High (requires retry logic implementation)

### Phase 2: Important Metrics (During R10 Tests)

4. **Circuit breaker trigger reason** (`router_circuit_breaker_trigger_reason`)
   - Priority: MEDIUM
   - Useful for: Debugging and validation
   - Effort: Low (add reason tracking)

5. **Publish errors with error type** (`router_nats_publish_errors_total` with `error_type` label)
   - Priority: MEDIUM
   - Useful for: Error classification
   - Effort: Medium (add error classification)

6. **Input request latency** (Gateway metrics)
   - Priority: MEDIUM
   - Required for: E2E latency validation
   - Effort: Low (verify Gateway implementation)

### Phase 3: Nice-to-Have Metrics (After R10 Tests)

7. **Publish retry delay** (`router_nats_publish_retry_delay_seconds`)
   - Priority: LOW
   - Useful for: Retry validation
   - Effort: Medium (add delay tracking)

8. **Circuit breaker transitions with from/to** (enhance existing)
   - Priority: LOW
   - Useful for: Better observability
   - Effort: Low (add `from` label)

## Test Code Examples

### Example: Query Circuit Breaker State

```erlang
%% Query circuit breaker state gauge
get_circuit_breaker_state(TenantId, ProviderId) ->
    %% Query Prometheus metrics endpoint
    Metrics = get_metrics(),
    %% Find gauge with labels
    State = find_metric(Metrics, "router_circuit_breaker_state", #{
        tenant_id => TenantId,
        provider_id => ProviderId
    }),
    %% Convert numeric value to state
    case State of
        0 -> closed;
        1 -> open;
        2 -> half_open
    end.
```

### Example: Query Publish Latency

```erlang
%% Query publish latency p99
get_publish_latency_p99() ->
    Metrics = get_metrics(),
    %% Query histogram quantile
    P99 = query_histogram_quantile(Metrics, "router_nats_publish_latency_seconds", 0.99),
    P99.
```

### Example: Query Publish Attempts with Retry Count

```erlang
%% Query publish attempts by retry count
get_publish_attempts_by_retry_count(RetryCount) ->
    Metrics = get_metrics(),
    %% Query counter with label
    Count = query_counter(Metrics, "router_nats_publish_attempts_total", #{
        retry_count => integer_to_binary(RetryCount)
    }),
    Count.
```

## References

- [R10 Specification](./R10_PUBLISH_FAILURE_E2E_SPEC.md) - Section 7.1 (Metrics)
- [R10 Consistency Check](./R10_CONSISTENCY_CHECK.md) - Section 4 (Metrics Consistency)
- [NATS Connection Resilience](../docs/NATS_CONNECTION_RESILIENCE.md) - Metrics section
- `apps/otp/router/src/router_circuit_breaker.erl` - Circuit breaker metrics
- `apps/otp/router/src/router_nats.erl` - NATS publish metrics

