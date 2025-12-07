# R10 Consistency Check: R10 ↔ R8 ↔ Implementation

## Purpose

This document verifies consistency between:
- **R10 specification** (publish failure E2E tests)
- **R8 implementation** (triple-fault patterns, fault injection)
- **Actual code** (Erlang/OTP Router + NestJS Gateway)

## 1. Fault Injection Mechanism Consistency

### R10 Specification

**Requirement**: Faults MUST be injected using `router_nats_fault_injection` mechanism to keep tests consistent with R8.

**R10 Reference**: Section 3.1.2, 8.1

### R8 Implementation

**Mechanism**: `router_nats_fault_injection` module
- Operations: `publish`, `publish_with_ack`, `connect`, `ack`, `nak`, `subscribe`
- Fault types: `{error, Reason}`, `timeout`, `close_connection`, `{delay, Milliseconds}`
- API: `enable_fault(Operation, Fault)`, `disable_fault(Operation)`, `clear_all_faults()`

**R8 Reference**: `router_jetstream_fault_injection_SUITE.erl`, `NATS_CONNECTION_RESILIENCE.md`

### Code Implementation

**File**: `apps/otp/router/src/router_nats_fault_injection.erl`

**Status**: ✅ **CONSISTENT**

- Module exists and matches R8/R10 specification
- Supports all required operations
- Supports all required fault types
- API matches specification

**Verification**:
```erlang
%% R10 test code should use:
router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
router_nats_fault_injection:enable_fault(publish, timeout),
router_nats_fault_injection:enable_fault(publish, {delay, 6000}),
router_nats_fault_injection:disable_fault(publish),
```

## 2. Retry Model Consistency

### R10 Specification

**Retry Parameters**:
- `maxAttempts`: 3 (default)
- `backoffStrategy`: Exponential
- `backoffBase`: 100ms
- `backoffMax`: 5000ms
- `jitter`: ±20%
- `timeoutPerAttempt`: 2s
- `totalDeadline`: 10s

**R10 Reference**: Section 2.1.2

### Code Implementation

**Current State**: ⚠️ **PARTIAL IMPLEMENTATION**

**Files**:
- `router_extension_invoker.erl`: Has `invoke_with_retry/4` but retry logic is basic
- `router_nats.erl`: Has pending operations queue with retry after reconnect, but no explicit retry with backoff for publish failures

**Gap Analysis**:

1. **Publish retry logic**: Not explicitly implemented in `router_nats:publish/2`
   - Current: Publish fails → queued for retry after reconnect
   - R10 requires: Publish fails → retry with backoff → circuit breaker

2. **Retry parameters**: Not configurable in current implementation
   - R10 requires: Configurable `maxAttempts`, `backoffStrategy`, `backoffBase`, etc.
   - Current: Hard-coded or not present

3. **Retry tracking**: Not tracked in metrics
   - R10 requires: `publish_attempts_total{retry_count="0|1|2|..."}`
   - Current: No retry count tracking

**Recommendation**: 
- Implement explicit retry logic in `router_nats:publish/2` or create `router_nats_publish_retry` module
- Add retry configuration to application config
- Add retry count tracking to metrics

## 3. Circuit Breaker Consistency

### R10 Specification

**Circuit Breaker Parameters**:
- `failureThreshold`: 5 consecutive failures
- `errorRateThreshold`: 50%
- `errorRateWindow`: 30s
- `latencyThreshold`: 5s
- `openTimeout`: 30s
- `halfOpenMaxAttempts`: 3
- `successThreshold`: 2

**States**: `closed`, `open`, `half_open`

**R10 Reference**: Section 2.2.3

### Code Implementation

**File**: `apps/otp/router/src/router_circuit_breaker.erl`

**Status**: ✅ **CONSISTENT** (with minor differences)

**Implementation Details**:

1. **States**: ✅ Match R10
   - `closed`, `open`, `half_open` (lines 7-9)

2. **Default Parameters**: ⚠️ **DIFFERENT** (but configurable)
   - `failureThreshold`: 5 ✅ (matches R10)
   - `errorRateThreshold`: 0.5 (50%) ✅ (matches R10)
   - `errorRateWindow`: 60s ⚠️ (R10: 30s)
   - `openTimeout`: 60000ms (60s) ⚠️ (R10: 30s)
   - `halfOpenMaxAttempts`: 3 ✅ (matches R10)
   - `successThreshold`: 2 ✅ (matches R10)
   - `latencyThreshold`: ❌ **NOT IMPLEMENTED** (R10 requires)

3. **State Transitions**: ✅ Match R10
   - `closed → open`: On failure threshold or error rate threshold (lines 12-13)
   - `open → half_open`: After timeout (line 13)
   - `half_open → closed`: On success threshold (line 14)
   - `half_open → open`: On any failure (line 15)

4. **Trigger Criteria**: ⚠️ **PARTIAL**
   - ✅ Consecutive failures: Implemented
   - ✅ Error rate threshold: Implemented (sliding window)
   - ❌ Latency threshold: **NOT IMPLEMENTED** (R10 requires)

**Gap Analysis**:

1. **Latency-based trigger**: Missing
   - R10 requires: Circuit breaker opens when publish latency exceeds threshold
   - Current: Only failure count and error rate triggers

2. **Default timeout**: Different
   - R10 default: 30s
   - Current default: 60s
   - **Note**: Configurable, so can be set to 30s in tests

3. **Error rate window**: Different
   - R10 default: 30s
   - Current default: 60s
   - **Note**: Configurable, so can be set to 30s in tests

**Recommendation**:
- Add latency-based trigger to circuit breaker
- Use R10 defaults in test configuration
- Document parameter differences in R10 test setup

## 4. Metrics Consistency

### R10 Specification

**Required Metrics**:
- `publish_attempts_total{status="success|error", retry_count="0|1|2|..."}`
- `publish_errors_total{error_type="timeout|connection|nack|..."}`
- `publish_latency_seconds{quantile="0.5|0.9|0.99"}`
- `circuit_breaker_state{state="closed|open|half_open"}`
- `circuit_breaker_transitions_total{from="...", to="..."}`
- `circuit_breaker_trigger_reason{reason="consecutive_failures|error_rate|latency"}`
- `publish_retry_delay_seconds{attempt="1|2|3|..."}`
- `system_cpu_usage_percent`
- `system_memory_usage_bytes`
- `input_request_latency_seconds{quantile="0.5|0.9|0.99"}`

**R10 Reference**: Section 7.1

### Code Implementation

**Status**: ⚠️ **PARTIAL IMPLEMENTATION**

#### Existing Metrics (from code analysis)

**Publish Metrics**:
- ✅ `router_nats_publish_total` (counter) - Total publish operations
- ✅ `router_nats_publish_failures_total` (counter) - Total publish failures
- ⚠️ **Missing**: `publish_attempts_total` with `retry_count` label
- ⚠️ **Missing**: `publish_errors_total` with `error_type` label (have failures_total but no error_type)
- ⚠️ **Missing**: `publish_latency_seconds` histogram
- ⚠️ **Missing**: `publish_retry_delay_seconds` histogram

**Circuit Breaker Metrics** (from `router_circuit_breaker.erl`):
- ✅ `router_circuit_breaker_events_total` (counter) - Events (success/failure)
- ✅ `router_circuit_breaker_state_transitions_total` (counter) - State transitions
- ✅ `router_circuit_breaker_error_rate` (gauge) - Error rate
- ✅ `router_circuit_breaker_window_requests_total` (counter) - Window requests
- ✅ `router_circuit_breaker_window_failures_total` (counter) - Window failures
- ⚠️ **Missing**: `circuit_breaker_state` gauge with `state` label
- ⚠️ **Missing**: `circuit_breaker_trigger_reason` gauge/info metric

**System Metrics**:
- ⚠️ **Missing**: Standard Prometheus system metrics (CPU, memory)
- **Note**: May be provided by node_exporter or similar

**Gateway Metrics** (NestJS):
- ⚠️ **Unknown**: Need to check Gateway implementation for `input_request_latency_seconds`

**Gap Analysis**:

1. **Publish retry tracking**: Not implemented
   - Need: `publish_attempts_total{retry_count="0|1|2|..."}`
   - Need: `publish_retry_delay_seconds{attempt="1|2|3|..."}`

2. **Publish error classification**: Partial
   - Have: `router_nats_publish_failures_total`
   - Need: `publish_errors_total{error_type="timeout|connection|nack|..."}`

3. **Publish latency**: Not implemented
   - Need: `publish_latency_seconds` histogram

4. **Circuit breaker state gauge**: Not implemented
   - Have: State transitions counter
   - Need: Current state gauge `circuit_breaker_state{state="closed|open|half_open"}`

5. **Circuit breaker trigger reason**: Not implemented
   - Need: `circuit_breaker_trigger_reason{reason="consecutive_failures|error_rate|latency"}`

**Recommendation**:
- Implement missing metrics before R10 test implementation
- Map R10 metric names to actual implementation names in test code
- Document metric name mapping in R10 test suite

## 5. Logging Consistency

### R10 Specification

**Required Log Events**:
- Retry attempt: `level=INFO msg="Publish retry attempt" attempt=N max_attempts=M error=...`
- Circuit breaker state change: `level=WARN msg="Circuit breaker state changed" from=... to=... reason=...`
- Breaker open: `level=WARN msg="Circuit breaker open, blocking publish" ...`
- Breaker half-open: `level=INFO msg="Circuit breaker half-open, allowing trial publishes" ...`
- Breaker closed: `level=INFO msg="Circuit breaker closed, normal operation resumed" ...`
- Deadline exceeded: `level=WARN msg="Publish deadline exceeded" ...`

**R10 Reference**: Section 7.2

### Code Implementation

**Status**: ⚠️ **PARTIAL IMPLEMENTATION**

**Existing Logs** (from `router_circuit_breaker.erl`):
- ✅ Circuit breaker opened: `router_logger:warn(<<"Circuit breaker opened for provider">>, ...)`
- ✅ Circuit breaker closed: `router_logger:info(<<"Circuit breaker closed for provider">>, ...)`
- ⚠️ **Missing**: Explicit "half-open" log message
- ⚠️ **Missing**: Retry attempt logs
- ⚠️ **Missing**: Deadline exceeded logs

**Existing Logs** (from `router_nats.erl`):
- ✅ Publish error: `NATS_PUBLISH_ERROR` error code
- ✅ Publish queued: `NATS_PUBLISH_QUEUED` error code
- ⚠️ **Missing**: Retry attempt logs with attempt count

**Recommendation**:
- Add retry attempt logging in publish retry logic
- Add explicit half-open state logging
- Add deadline exceeded logging
- Ensure log format matches R10 specification

## 6. R8 vs R10 Scope Consistency

### R8 Scope

**Focus**: Triple-fault combinations, MaxDeliver semantics, redelivery limits, delivery count tracking

**Test Level**: Integration tests with fault injection

**R8 Reference**: `R8_SUMMARY.md`

### R10 Scope

**Focus**: Publish failure under high load, explicit retry model, E2E circuit breaker

**Test Level**: E2E tests with high load and fault injection

**R10 Reference**: `R10_PUBLISH_FAILURE_E2E_SPEC.md`

### Consistency Check

**Status**: ✅ **COMPLEMENTARY** (not conflicting)

**Relationship**:
- **R8**: Tests fault handling at consumer level (ACK/NAK/publish failures)
- **R10**: Tests fault handling at publish level (publish failures → retry → circuit breaker)

**Overlap**:
- Both use `router_nats_fault_injection` mechanism ✅
- Both test publish failures ✅
- R8 focuses on consumer-side faults, R10 focuses on publish-side faults

**Gaps**:
- R8 doesn't test high load scenarios (R10 does)
- R8 doesn't test explicit retry model (R10 does)
- R8 doesn't test E2E circuit breaker (R10 does)
- R10 doesn't test MaxDeliver semantics (R8 does)

**Recommendation**: 
- R10 complements R8, no conflicts
- Both should be run in CI/CD pipeline
- R10 tests should reuse R8 fault injection patterns

## 7. Summary and Recommendations

### ✅ Consistent

1. **Fault injection mechanism**: R10, R8, and code all use `router_nats_fault_injection`
2. **Circuit breaker states**: R10 and code match (`closed`, `open`, `half_open`)
3. **Circuit breaker transitions**: R10 and code match
4. **Scope relationship**: R10 and R8 are complementary, not conflicting

### ⚠️ Partial Implementation

1. **Retry model**: Specification exists in R10, but implementation is incomplete
   - **Action**: Implement explicit retry logic with backoff in `router_nats:publish/2`
   - **Action**: Add retry configuration to application config
   - **Action**: Add retry count tracking to metrics

2. **Circuit breaker parameters**: Defaults differ, but configurable
   - **Action**: Use R10 defaults in test configuration
   - **Action**: Document parameter differences

3. **Circuit breaker latency trigger**: Not implemented
   - **Action**: Add latency-based trigger to circuit breaker

4. **Metrics**: Some metrics missing
   - **Action**: Implement missing metrics before R10 test implementation
   - **Action**: Map R10 metric names to actual implementation names

5. **Logging**: Some log events missing
   - **Action**: Add retry attempt logging
   - **Action**: Add explicit half-open state logging
   - **Action**: Add deadline exceeded logging

### ❌ Missing

1. **Publish retry logic**: Not explicitly implemented
2. **Latency-based circuit breaker trigger**: Not implemented
3. **Some metrics**: `publish_attempts_total` with retry_count, `publish_latency_seconds`, `circuit_breaker_state` gauge, `circuit_breaker_trigger_reason`
4. **Some log events**: Retry attempt logs, deadline exceeded logs

### Implementation Priority

**Before R10 Test Implementation**:
1. ✅ Fault injection mechanism (already exists)
2. ⚠️ Implement publish retry logic with backoff
3. ⚠️ Add missing metrics
4. ⚠️ Add missing log events
5. ⚠️ Add latency-based circuit breaker trigger (optional, can be added later)

**During R10 Test Implementation**:
1. Map R10 metric names to actual implementation names
2. Configure circuit breaker with R10 defaults
3. Document parameter differences

## 8. References

- [R10 Specification](./R10_PUBLISH_FAILURE_E2E_SPEC.md)
- [R8 Summary](./R8_SUMMARY.md)
- [Fault Injection Requirements Traceability](./FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md)
- [NATS Connection Resilience](../docs/NATS_CONNECTION_RESILIENCE.md)
- `apps/otp/router/src/router_nats_fault_injection.erl`
- `apps/otp/router/src/router_circuit_breaker.erl`
- `apps/otp/router/src/router_nats.erl`

