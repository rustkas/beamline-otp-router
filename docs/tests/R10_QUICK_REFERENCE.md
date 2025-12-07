# R10 Quick Reference for Developers and QA

## What is R10?

**R10** validates system behavior when `publish` operations fail massively under high load, with explicit retry model and circuit breaker protection at E2E level.

**Key Points**:
- Tests publish failures (not consumer failures)
- Tests under high load (100-500 RPS)
- Tests explicit retry model (maxAttempts, backoff, jitter)
- Tests E2E circuit breaker (visible across entire request flow)

## Quick Facts

| Aspect | Value |
|--------|-------|
| **Type** | E2E Fault Injection Test |
| **Priority** | High |
| **Status** | Specification (to be implemented) |
| **Test Suite** | `router_publish_failure_e2e_SUITE.erl` (to be created) |
| **Fault Injection** | `router_nats_fault_injection` (same as R8) |
| **Estimated Duration** | 60-120 seconds per scenario |

## Test Scenarios

### Scenario 1: Mass Publish Failure → Circuit Breaker Activation

**What it tests**: System behavior when all publishes fail under high load.

**Steps**:
1. Start high load (100-500 RPS)
2. After 10s, inject publish failure
3. Observe retries (≤ 3 attempts per request)
4. Observe circuit breaker (opens after threshold)
5. Verify system health (no OOM, latency within bounds)

**Key Assertions**:
- Retries ≤ `maxAttempts` (default: 3)
- Circuit breaker opens within expected time
- No real publish attempts when breaker is `open`
- System resources stable (CPU ≤ 80%, memory ≤ 90%)

### Scenario 2: Recovery After Failure (Half-open → Closed)

**What it tests**: System recovery after broker is restored.

**Steps**:
1. Start from Scenario 1 (breaker in `open` state)
2. Restore broker (disable fault injection)
3. Wait for `openTimeout` (30s)
4. Send probe requests (limited to 3)
5. Verify breaker closes after successful probes

**Key Assertions**:
- Breaker transitions `open → half_open → closed`
- Limited trial publishes in `half_open` state
- Normal operation resumes after breaker closes

### Scenario 3: Latency-Based Circuit Breaker Trigger

**What it tests**: Circuit breaker activation based on latency (not just errors).

**Steps**:
1. Inject latency (6s per publish)
2. Start normal load
3. Verify breaker opens when latency exceeds threshold (5s)
4. Verify system protected from latency cascade

**Key Assertions**:
- Breaker opens based on latency threshold
- System doesn't accumulate pending publishes

## Configuration Parameters

### Retry Configuration

| Parameter | Default | Description |
|-----------|---------|-------------|
| `maxAttempts` | 3 | Total number of retry attempts |
| `backoffStrategy` | exponential | Backoff strategy (exponential/linear) |
| `backoffBase` | 100ms | Initial backoff delay |
| `backoffMax` | 5000ms | Maximum backoff delay |
| `jitter` | ±20% | Random deviation to smooth peaks |
| `timeoutPerAttempt` | 2s | Maximum wait per attempt |
| `totalDeadline` | 10s | Total time budget for publish |

### Circuit Breaker Configuration

| Parameter | Default | Description |
|-----------|---------|-------------|
| `failureThreshold` | 5 | Consecutive failures to open |
| `errorRateThreshold` | 50% | Error rate to open |
| `errorRateWindow` | 30s | Sliding window for error rate |
| `latencyThreshold` | 5s | Latency threshold to open |
| `openTimeout` | 30s | Duration in `open` state |
| `halfOpenMaxAttempts` | 3 | Trial attempts in `half_open` |
| `successThreshold` | 2 | Successful attempts to close |

## Fault Injection

**Mechanism**: `router_nats_fault_injection` (same as R8)

**Usage**:
```erlang
%% Enable fault
router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
router_nats_fault_injection:enable_fault(publish, timeout),
router_nats_fault_injection:enable_fault(publish, {delay, 6000}),

%% Disable fault
router_nats_fault_injection:disable_fault(publish),
```

**Supported Fault Types**:
- `{error, Reason}` - Return error
- `timeout` - Simulate timeout
- `close_connection` - Close connection
- `{delay, Milliseconds}` - Add latency

## Key Metrics

### Publish Metrics

| Metric | Description | Expected Change |
|--------|-------------|-----------------|
| `router_nats_publish_total` | Total publish operations | Increases during load |
| `router_nats_publish_failures_total` | Total publish failures | Increases after fault injection |
| `router_nats_pending_operations_count` | Pending operations queue | May increase, then stabilize |

### Circuit Breaker Metrics

| Metric | Description | Expected Change |
|--------|-------------|-----------------|
| `router_circuit_breaker_state_transitions_total{state="open"}` | Breaker opens | Increments when threshold exceeded |
| `router_circuit_breaker_error_rate` | Error rate | Exceeds threshold (50%) |
| `router_circuit_breaker_events_total` | Events (success/failure) | Tracks all events |

**Note**: See [R10 Metrics Review](./R10_METRICS_REVIEW.md) for complete metric mapping.

## Key Logs

### Publish Logs

| Log Event | Level | When |
|-----------|-------|------|
| `NATS_PUBLISH_ERROR` | WARN | Publish fails |
| `NATS_PUBLISH_QUEUED` | WARN | Publish queued (not connected) |

### Circuit Breaker Logs

| Log Event | Level | When |
|-----------|-------|------|
| `Circuit breaker opened` | WARN | Breaker opens |
| `Circuit breaker closed` | INFO | Breaker closes |
| `Circuit breaker half-open` | INFO | Breaker enters half-open |

## Expected Behavior

### Retry Behavior

- **Before maxAttempts**: Retries occur with backoff
- **After maxAttempts**: Retries stop, error returned
- **Inter-attempt intervals**: Match backoff model (with jitter)
- **Total retry time**: ≤ `totalDeadline` (10s)

### Circuit Breaker Behavior

- **Closed → Open**: When failure threshold or error rate exceeded
- **Open state**: No real publish attempts, requests blocked immediately
- **Open → Half-open**: After `openTimeout` (30s)
- **Half-open → Closed**: After `successThreshold` (2) successful probes
- **Half-open → Open**: On any failure

### System Health

- **CPU usage**: ≤ 80%
- **Memory usage**: ≤ 90%
- **Input latency (p99)**: ≤ 2s
- **No OOM**: System remains alive

## Common Issues and Solutions

### Issue: Circuit breaker doesn't open

**Possible causes**:
- Fault injection not enabled
- Threshold not reached
- Configuration incorrect

**Solution**:
- Verify fault injection: `router_nats_fault_injection:enable_fault(publish, ...)`
- Check failure count: Should reach `failureThreshold` (5)
- Check error rate: Should exceed `errorRateThreshold` (50%)

### Issue: Retries exceed maxAttempts

**Possible causes**:
- Retry logic not implemented correctly
- Configuration not applied
- Multiple retry mechanisms conflicting

**Solution**:
- Verify retry configuration is applied
- Check retry logic implementation
- Ensure only one retry mechanism is active

### Issue: System resources exceed thresholds

**Possible causes**:
- Too many pending operations
- Memory leak in retry queue
- Circuit breaker not blocking requests

**Solution**:
- Check pending operations queue size
- Verify circuit breaker is blocking requests in `open` state
- Check for memory leaks in retry logic

## Implementation Status

### ✅ Ready

- Fault injection mechanism (`router_nats_fault_injection`)
- Circuit breaker implementation (`router_circuit_breaker.erl`)
- Basic publish metrics (`router_nats_publish_total`, `router_nats_publish_failures_total`)

### ⚠️ Partial

- Retry logic (needs explicit implementation with backoff)
- Circuit breaker metrics (some missing: `circuit_breaker_state` gauge, `circuit_breaker_trigger_reason`)
- Publish latency metrics (not implemented)

### ❌ Missing

- Explicit publish retry with backoff
- Latency-based circuit breaker trigger
- Some metrics and log events

**See**: [R10 Consistency Check](./R10_CONSISTENCY_CHECK.md) for details.

## Quick Test Checklist

Before running R10 tests:

- [ ] E2E environment running (Router, Gateway, JetStream)
- [ ] Retry configuration set (maxAttempts, backoff, etc.)
- [ ] Circuit breaker configuration set (thresholds, timeouts)
- [ ] Metrics/logging enabled
- [ ] Baseline metrics captured
- [ ] Fault injection mechanism verified

During test:

- [ ] Load generator running (100-500 RPS)
- [ ] Fault injection enabled at correct time
- [ ] Metrics/logs being collected
- [ ] System health monitored

After test:

- [ ] Fault injection disabled
- [ ] System returned to baseline
- [ ] Test data cleaned up
- [ ] Assertions verified
- [ ] Metrics/logs analyzed

## References

- **Full Specification**: [R10_PUBLISH_FAILURE_E2E_SPEC.md](./R10_PUBLISH_FAILURE_E2E_SPEC.md)
- **Consistency Check**: [R10_CONSISTENCY_CHECK.md](./R10_CONSISTENCY_CHECK.md)
- **Metrics Review**: [R10_METRICS_REVIEW.md](./R10_METRICS_REVIEW.md)
- **Test Cases Template**: [R10_TEST_CASES_TEMPLATE.md](./R10_TEST_CASES_TEMPLATE.md)
- **R8 Summary**: [R8_SUMMARY.md](./R8_SUMMARY.md)

