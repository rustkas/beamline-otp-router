# R10: Publish Failure Under High Load with Explicit Retry Model and E2E Circuit Breaker

## 1. Overview

**Requirement ID**: R10  
**Title**: Publish Failure Under High Load with Explicit Retry Model and E2E Circuit Breaker  
**Type**: E2E Fault Injection Test  
**Priority**: High  
**Status**: Specification

### 1.1. Requirement Statement

R10 validates system behavior when `publish` operations (message/event/command publishing to JetStream/broker) fail massively under high load conditions, with explicit retry model and circuit breaker protection at E2E level.

**Traceability**: See FI-REQ-R10 in [Fault Injection Requirements Traceability](./FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md).

### 1.2. Scope

This requirement covers:

- **Publish failure scenarios**: Mass failures, latency degradation, intermittent errors
- **High load conditions**: High QPS/OPS, multiple concurrent producers, production-like scenarios
- **Explicit retry model**: Clearly defined retry policy (attempts, intervals, backoff, jitter)
- **E2E circuit breaker**: Circuit breaker behavior visible across entire request flow

#### Out of Scope

The following are explicitly **not** covered by R10:

- **Long-term stability (soak tests)**: Multi-hour stability testing under continuous load (covered by separate soak test suites)
- **Multi-region/quorum scenarios**: Complex JetStream quorum and multi-region replication scenarios
- **Business semantics**: Exactly-once delivery guarantees, message ordering semantics (only retry/CB behavior is tested)
- **Cross-component integration**: Deep integration with Gateway, Ingress, or other services beyond publish path
- **Performance benchmarking**: Detailed performance profiling and optimization (only degradation behavior is verified)

### 1.3. Goals

- **Controlled degradation**: System must not overwhelm cluster with retries, hang, or lose latency/SLI control
- **Broker and resource protection**: Circuit breaker + limited retries prevent broker self-DDoS and resource exhaustion
- **Predictable behavior**: Clients receive clear errors or degraded mode notifications
- **Observability**: Logs/metrics show publish errors, circuit breaker state transitions, and system health

### 1.4. System Under Test

R10 tests the following components in E2E environment:

#### Components

- **Router (Erlang/OTP)**: Main router service handling publish operations
  - Publish client library (JetStream client)
  - Retry logic implementation
  - Circuit breaker implementation
  - Metrics and logging

- **Gateway (NestJS)**: Optional frontend API gateway (if part of E2E flow)
  - HTTP/gRPC request handling
  - Request routing to Router
  - Response formatting

- **JetStream/Broker**: Message broker (real or controlled fake)
  - Publish endpoint
  - Error injection point
  - Latency injection point

#### SLO Measurement Points

- **Input request latency** (`input_request_latency_seconds`): Measured at API Gateway entry point (if Gateway present) or Router entry point
- **Publish latency** (`publish_latency_seconds`): Measured from Router publish attempt to broker response
- **E2E latency**: Full request flow from client → Gateway → Router → JetStream

#### Client Types

- **External clients**: HTTP/gRPC clients making requests to Gateway/Router
- **Internal clients**: Services/components making direct publish calls to Router (if applicable)

**Note**: Test scenarios focus on external client flow, but internal publish paths may also be validated if they use the same retry/CB mechanisms.

## 2. Architecture and Behavior Model

### 2.1. Retry Model

#### 2.1.1. Retry Types

- **Synchronous retries**: Within single request context, N publish attempts, then error
- **Asynchronous retries**: First attempt during processing, then N background retries via workers/queue

#### 2.1.2. Retry Parameters

| Parameter | Description | Default/Example |
|-----------|-------------|-----------------|
| `maxAttempts` | Total number of attempts (1 = no retry, 3-5 = reasonable minimum) | 3 |
| `backoffStrategy` | Linear or exponential backoff | Exponential |
| `backoffBase` | Base interval for backoff | 100ms |
| `backoffMax` | Maximum backoff interval | 5000ms |
| `jitter` | Random deviation to smooth peaks | ±20% |
| `timeoutPerAttempt` | Maximum wait time per publish attempt | 2s |
| `totalDeadline` | Total time budget for publish within one user request/task | 10s |

#### 2.1.3. Retryable vs Non-Retryable Errors

**Retryable (transient errors)**:
- Network timeout
- Broker 5xx errors
- Temporary unavailability
- Connection refused (transient)

**Non-retryable (hard errors)**:
- Validation errors
- Malformed messages
- Authentication/authorization errors
- Invalid configuration

#### 2.1.4. Retry Validation

Test must demonstrate:
- Retries occur **before** reaching `maxAttempts`
- Retries stop **after** reaching `maxAttempts`
- Inter-attempt intervals match backoff model
- Total retry time does not exceed `totalDeadline`

### 2.2. Circuit Breaker Model

#### 2.2.1. States

**Closed (normal operation)**:
- Publications proceed normally
- Errors/successes tracked via sliding window or rolling counters

**Open (traffic blocked)**:
- All new publish requests immediately rejected without broker attempts
- Alternative behaviors: local logging, DLQ, drop per policy
- Duration: `openTimeout` (e.g., 10-60 seconds)

**Half-open (probing)**:
- Limited number of trial publishes allowed (e.g., 1-10)
- If successful → transition to `Closed`
- If failures → transition back to `Open`, reset timer

#### 2.2.2. Trigger Criteria

Circuit breaker opens when one of:
- N consecutive errors
- Error rate > X% over sliding window (e.g., 50%+ over 30 seconds)
- Latency threshold exceeded (publish becomes too slow)

#### 2.2.3. Circuit Breaker Parameters

| Parameter | Description | Default/Example |
|-----------|-------------|-----------------|
| `failureThreshold` | Number of consecutive failures to open | 5 |
| `errorRateThreshold` | Error rate percentage to open | 50% |
| `errorRateWindow` | Time window for error rate calculation | 30s |
| `latencyThreshold` | Publish latency threshold | 5s |
| `openTimeout` | Duration to stay in Open state | 30s |
| `halfOpenMaxAttempts` | Maximum trial attempts in half-open | 3 |
| `successThreshold` | Successful attempts to close from half-open | 2 |

#### 2.2.4. Circuit Breaker Validation

Test must verify:
- Transition `Closed → Open` under failure stream
- No real publish requests in `Open` state
- Correct transition to `Half-open` after timeout
- Recovery to `Closed` on successful probes

## 3. E2E Test Scenarios

### 3.1. Scenario 1: Mass Publish Failure → Circuit Breaker Activation

#### 3.1.1. Prerequisites

- E2E environment running (services, JetStream, test clients)
- Retry configuration set
- Circuit breaker configuration set
- Metrics/logging enabled for publish operations
- Baseline metrics captured

#### 3.1.2. Test Steps

1. **Induce high load**:
   - Start N parallel clients making requests that trigger `publish` to JetStream
   - Target RPS: 100-500 requests/second
   - Duration: 60 seconds

2. **Simulate publish failure** (after 10 seconds of normal operation):
   - **CRITICAL**: Faults MUST be injected using `router_nats_fault_injection` mechanism to keep tests consistent with R8 and other fault injection tests.
   - Use `router_nats_fault_injection:enable_fault(publish, Fault)` with one of:
     - `{error, Reason}` - Return error (e.g., `{error, connection_refused}`, `{error, nack}`)
     - `timeout` - Simulate publish timeout (hangs for configured duration)
     - `close_connection` - Close connection during publish
     - `{delay, Milliseconds}` - Add latency to publish (for latency-based trigger scenario)
   - Alternative (if network-level injection needed): Use `iptables`/`tc` for network disconnection/throttling, but prefer `router_nats_fault_injection` for consistency.

3. **Observe retry behavior**:
   - Verify each request makes ≤ `maxAttempts` publish attempts
   - Verify inter-attempt intervals match backoff model (with jitter)
   - Verify no "too many attempts" errors beyond configured limits
   - Verify deadlines are respected

4. **Observe circuit breaker**:
   - After failure threshold, breaker transitions to `Open`
   - New requests either:
     - Immediately receive "503 / circuit open" (or equivalent)
     - Have messages blocked before reaching publish client
   - Verify no real network attempts to broker in `Open` state (via logs/metrics)

5. **Verify degradation**:
   - System remains alive: no OOM, no latency explosion
   - Input request latency stays within bounds
   - Degraded path works: events logged, local buffer, or "accepted with degraded mode" response

#### 3.1.3. Expected Artifacts

**Logs**:
- Explicit retry attempts with timestamps
- Circuit breaker state transition events
- Messages indicating breaker `open` and requests being blocked
- Error messages with retry context

**Metrics**:
- `publish_errors_total` increases
- `circuit_breaker_state` changes (0=closed, 1=open, 2=half-open)
- `publish_attempts_total` stabilizes/limits after breaker opens
- `publish_latency_seconds` stays within bounds
- `circuit_breaker_transitions_total` increments

**Assertions**:
- Circuit breaker opens within expected time window
- Publish attempts drop to near-zero after breaker opens
- System resources (CPU, memory, connections) remain stable
- Input request latency does not exceed SLO

### 3.2. Scenario 2: Recovery After Failure (Half-open → Closed)

#### 3.2.1. Prerequisites

- Scenario 1 completed, circuit breaker in `Open` state
- JetStream/broker ready to be restored

#### 3.2.2. Test Steps

1. **Restore JetStream/broker**:
   - Disable fault injection: `router_nats_fault_injection:disable_fault(publish)`
   - Or reconnect network if network-level injection was used
   - Verify broker is healthy

2. **Wait for half-open transition**:
   - Wait for `openTimeout` to elapse
   - Verify breaker transitions to `Half-open` state

3. **Send probe requests**:
   - Send limited number of requests (within `halfOpenMaxAttempts`)
   - Verify these requests actually reach broker (trial publishes)

4. **Verify recovery**:
   - On successful publishes, breaker transitions to `Closed`
   - Subsequent publishes proceed normally
   - System returns to normal metrics

#### 3.2.3. Expected Artifacts

**Logs**:
- Half-open state entry
- Trial publish attempts
- Successful publishes and transition to `Closed`
- Normal operation resumption

**Metrics**:
- `circuit_breaker_state` transitions: Open → Half-open → Closed
- `publish_success_total` increases
- `publish_errors_total` stabilizes
- System metrics return to baseline

**Assertions**:
- Breaker transitions to half-open after `openTimeout`
- Limited trial publishes occur in half-open
- Breaker closes after successful probes
- Normal operation resumes

### 3.3. Scenario 3: Latency-Based Circuit Breaker Trigger

#### 3.3.1. Prerequisites

- E2E environment running
- Circuit breaker configured with latency threshold
- Metrics enabled

#### 3.3.2. Test Steps

1. **Induce latency degradation**:
   - Use `router_nats_fault_injection:enable_fault(publish, {delay, 6000})` to inject 6s latency per publish
   - Start normal load

2. **Observe latency threshold trigger**:
   - Verify circuit breaker opens when publish latency exceeds threshold
   - Verify no latency-based retries after breaker opens

3. **Verify protection**:
   - System does not accumulate pending publishes
   - Input requests receive timely responses (degraded mode)

#### 3.3.3. Expected Artifacts

**Metrics**:
- `publish_latency_seconds` exceeds threshold
- `circuit_breaker_state` transitions to `Open`
- `circuit_breaker_trigger_reason` = "latency"

**Assertions**:
- Breaker opens based on latency, not just errors
- System protected from latency cascade

## 4. Test Parameters and Configuration

### 4.1. Load Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Target RPS | 100-500 req/s | Production-like load |
| Load duration | 60-120s | Sufficient for circuit breaker activation |
| Concurrent clients | 10-50 | Multiple producers |
| Request types | Mixed (batches, streams, user events) | Realistic mix |

### 4.2. Failure Model Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Failure type | Hard fail, timeout, connection refused, NACK | Configurable |
| Failure start | After 10s of normal operation | Allow baseline |
| Failure rate | 100% (all publishes fail) | Worst case |
| Recovery time | After 30s of failure | For recovery scenario |

### 4.3. Retry Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| `maxAttempts` | 3 | Reasonable minimum |
| `backoffStrategy` | Exponential | With jitter |
| `backoffBase` | 100ms | Initial delay |
| `backoffMax` | 5000ms | Maximum delay |
| `jitter` | ±20% | Smooth peaks |
| `timeoutPerAttempt` | 2s | Per-attempt timeout |
| `totalDeadline` | 10s | Total time budget |

### 4.4. Circuit Breaker Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| `failureThreshold` | 5 consecutive failures | Trigger condition |
| `errorRateThreshold` | 50% | Alternative trigger |
| `errorRateWindow` | 30s | Sliding window |
| `latencyThreshold` | 5s | Latency-based trigger |
| `openTimeout` | 30s | Open state duration |
| `halfOpenMaxAttempts` | 3 | Trial attempts |
| `successThreshold` | 2 | Close from half-open |

### 4.5. Expected Behavior Boundaries

| Metric | Threshold | Notes |
|--------|-----------|-------|
| Publish attempts/sec (Open state) | ≤ 1 | Near-zero, only probes |
| Request latency (p99) | ≤ 2s | SLO compliance |
| System CPU usage | ≤ 80% | No resource exhaustion |
| System memory usage | ≤ 90% | No OOM |
| Circuit breaker open time | ≤ 35s | Within timeout + margin |

## 5. Test Assertions

### 5.1. Retry Assertions

- ✅ Number of publish attempts per request ≤ `maxAttempts`
- ✅ Inter-attempt intervals match backoff model (within jitter tolerance)
- ✅ Retries stop after `maxAttempts` reached
- ✅ Total retry time ≤ `totalDeadline`
- ✅ Only retryable errors trigger retries

### 5.2. Circuit Breaker Assertions

- ✅ Circuit breaker state transitions visible in metrics/logs
- ✅ Breaker opens within expected time window after threshold
- ✅ No real publish attempts to broker in `Open` state
- ✅ Breaker transitions to `Half-open` after `openTimeout`
- ✅ Limited trial publishes in `Half-open` state
- ✅ Breaker closes after successful probes

### 5.3. E2E Degradation Assertions

- ✅ Clients receive expected responses (error codes/messages)
- ✅ No unexpected error types
- ✅ System remains responsive (no hangs, no OOM)
- ✅ Input request latency within SLO
- ✅ Degraded path works (logging, buffering, or degraded mode response)
  - **Note**: "Degraded mode" means event not persisted to JetStream yet, but queued locally or logged with guarantee of eventual delivery/retry

### 5.4. Recovery Assertions

- ✅ After broker recovery and `openTimeout`, breaker enters `Half-open`
- ✅ Trial publishes reach broker
- ✅ Successful publishes return breaker to `Closed`
- ✅ System returns to normal operation metrics

## 6. Risks and Edge Cases

### 6.1. Thundering Herd on Recovery

**Risk**: Multiple instances transition to `Half-open` simultaneously, causing broker overload.

**Mitigation**:
- Coordinate breaker state across instances (if applicable)
- Limit trial traffic in `Half-open` state
- Stagger recovery attempts with jitter

**Test validation**: Verify trial traffic is limited and coordinated.

### 6.2. Long Retries vs SLA

**Risk**: Total retry time (sum of backoffs + timeouts) exceeds E2E latency SLA.

**Mitigation**:
- Cap total publish time with `totalDeadline`
- Move to background/async retries if deadline exceeded
- Return "accepted with degraded mode" to client (event queued locally, not yet persisted to JetStream)

**Test validation**: Verify deadlines are respected and async path works.

### 6.3. Local Queue Accumulation

**Risk**: When breaker is `Open`, messages accumulate in local buffers/queues, causing memory/disk exhaustion.

**Mitigation**:
- Implement queue size limits
- Drop policy when queue full
- Monitor queue metrics

**Test validation**: Verify queue limits and drop policies work correctly.

### 6.4. Partial Failures

**Risk**: Some publishes succeed while others fail, causing inconsistent breaker state.

**Mitigation**:
- Use error rate threshold (not just consecutive failures)
- Sliding window for error rate calculation
- Handle partial success gracefully

**Test validation**: Verify breaker behavior with mixed success/failure patterns.

## 7. Observability Requirements

### 7.1. Metrics

**Note**: Metric names below are examples. Actual implementation MUST be mapped to real observability stack (Prometheus/OpenTelemetry). Label names and formats may differ based on implementation.

Required metrics for R10 validation:

- `publish_attempts_total{status="success|error", retry_count="0|1|2|..."}`
  - **Implementation note**: `status` label may be `code` or `result` in actual implementation. `retry_count` may be numeric or string.
- `publish_errors_total{error_type="timeout|connection|nack|..."}`
  - **Implementation note**: `error_type` may be `reason`, `error_code`, or similar. Values depend on error classification.
- `publish_latency_seconds{quantile="0.5|0.9|0.99"}`
  - **Implementation note**: May be histogram or summary metric. Quantiles may be pre-calculated or computed via PromQL.
- `circuit_breaker_state{state="closed|open|half_open"}`
  - **Implementation note**: Likely implemented as gauge with numeric value (0=closed, 1=open, 2=half-open) + label/annotation for human-readable state. Format: `circuit_breaker_state{state="closed|open|half_open"}` or `circuit_breaker_state_gauge{value="0|1|2"}` with annotation.
- `circuit_breaker_transitions_total{from="...", to="..."}`
  - **Implementation note**: Counter metric tracking state transitions. Labels `from` and `to` contain state names.
- `circuit_breaker_trigger_reason{reason="consecutive_failures|error_rate|latency"}`
  - **Implementation note**: Gauge or info metric indicating why breaker opened. May be combined with `circuit_breaker_state` as additional label.
- `publish_retry_delay_seconds{attempt="1|2|3|..."}`
  - **Implementation note**: Histogram or summary metric tracking retry delays. `attempt` label may be numeric.
- `system_cpu_usage_percent`
  - **Implementation note**: Standard system metric, may be `process_cpu_seconds_total` or similar.
- `system_memory_usage_bytes`
  - **Implementation note**: Standard system metric, may be `process_resident_memory_bytes` or similar.
- `input_request_latency_seconds{quantile="0.5|0.9|0.99"}`
  - **Implementation note**: Measured at Gateway or Router entry point. May be `http_request_duration_seconds` or `grpc_request_duration_seconds` depending on protocol.

### 7.2. Logs

Required log events:

- Retry attempt: `level=INFO msg="Publish retry attempt" attempt=N max_attempts=M error=...`
- Circuit breaker state change: `level=WARN msg="Circuit breaker state changed" from=... to=... reason=...`
- Breaker open: `level=WARN msg="Circuit breaker open, blocking publish" ...`
- Breaker half-open: `level=INFO msg="Circuit breaker half-open, allowing trial publishes" ...`
- Breaker closed: `level=INFO msg="Circuit breaker closed, normal operation resumed" ...`
- Deadline exceeded: `level=WARN msg="Publish deadline exceeded" ...`

### 7.3. Traces

Optional but recommended:

- Distributed trace showing request flow through system
- Circuit breaker state in trace context
- Retry attempts visible in trace spans

## 8. Test Implementation Notes

### 8.1. Test Environment Setup

- Use E2E test environment with real JetStream or controlled fake broker
- **Fault injection mechanism**: Use `router_nats_fault_injection` module (same as R8 tests) for consistency:
  - Module: `router_nats_fault_injection`
  - Operations: `publish`, `publish_with_ack`
  - Fault types: `{error, Reason}`, `timeout`, `close_connection`, `{delay, Milliseconds}`
  - API: `router_nats_fault_injection:enable_fault(Operation, Fault)`, `router_nats_fault_injection:disable_fault(Operation)`
  - See: `router_jetstream_fault_injection_SUITE.erl` for examples
- **Alternative network-level injection** (if needed): Use `iptables`/`tc` for network disconnection/throttling, but prefer `router_nats_fault_injection` for consistency
- Enable all required metrics and logging

### 8.2. Test Data

- Use realistic message payloads
- Vary message sizes (small, medium, large)
- Include different message types (events, commands, queries)

### 8.3. Test Execution

- Run scenarios sequentially or in parallel (if isolated)
- Capture metrics/logs throughout execution
- Verify assertions post-execution
- Generate test report with artifacts

### 8.4. Test Cleanup

- Restore normal broker operation
- Reset circuit breaker state
- Clear test data
- Verify system returns to baseline

## 9. Acceptance Criteria

R10 is considered **complete** when:

1. ✅ All three test scenarios pass with assertions verified
2. ✅ Retry model behaves according to configuration
3. ✅ Circuit breaker activates and protects system
4. ✅ E2E degradation is predictable and observable
5. ✅ Recovery works correctly after broker restoration
6. ✅ All required metrics and logs are present
7. ✅ System resources remain stable under failure conditions
8. ✅ Test documentation is complete and accurate

### 9.1. Automation Status

**Target**: R10 scenarios MUST be automated and integrated into E2E test suite.

**Status**: Specification (to be implemented)

**Integration points**:
- Test suite: `router_publish_failure_e2e_SUITE.erl` (to be created)
- CI/CD: Add to `.github/workflows/test.yml` or equivalent E2E test pipeline
- Execution: Automated in CI, optionally run as nightly/long-running tests

**Acceptance criterion**: R10 scenarios are automated and integrated into E2E suite CI pipeline, with clear pass/fail criteria and artifact collection.

## 10. References

- [R8: JetStream Fault Injection Tests](./R8_SUMMARY.md)
- [Triple Fault Patterns Catalog](./TRIPLE_FAULT_PATTERNS_CATALOG.md)
- [Fault Injection Requirements Traceability](./FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md)
- [JetStream Fault Injection Tests Documentation](../docs/archive/dev/JETSTREAM_FAULT_INJECTION_TESTS.md)

## 11. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.1 | 2025-11-30 | Specification | Added System Under Test, fault injection mechanism details, metrics implementation notes, out of scope, automation status |
| 1.0 | 2025-11-30 | Specification | Initial R10 specification |

