# R10 Test Cases Template

## Purpose

This document provides a structured template for R10 test cases. Use this template to create detailed test case specifications for each R10 scenario.

## Test Case Structure

Each test case should follow this structure:

| Field | Description | Example |
|-------|-------------|---------|
| **Test ID** | Unique test identifier | `R10-TC-001` |
| **Test Name** | Descriptive test name | `Mass Publish Failure → Circuit Breaker Activation` |
| **Scenario** | R10 scenario reference | Scenario 1 (Section 3.1) |
| **Priority** | Test priority | High / Medium / Low |
| **Prerequisites** | Required setup | E2E environment, configs, baseline metrics |
| **Test Steps** | Step-by-step actions | 1. Start load, 2. Inject fault, 3. Observe... |
| **Expected Results** | What should happen | Circuit breaker opens, retries stop, metrics change |
| **Assertions** | Specific checks | `circuit_breaker_state == open`, `publish_attempts <= 3` |
| **Metrics to Verify** | Metrics to check | `router_nats_publish_failures_total`, `router_circuit_breaker_state_transitions_total` |
| **Logs to Verify** | Log events to check | `NATS_PUBLISH_ERROR`, `Circuit breaker opened` |
| **Cleanup** | Post-test actions | Disable faults, reset state, verify baseline |

## Test Case Template

### Test Case: [Test ID] - [Test Name]

**Scenario**: [R10 Scenario Reference]  
**Priority**: [High/Medium/Low]  
**Estimated Duration**: [X seconds/minutes]

#### Prerequisites

- [ ] E2E environment running (Router, Gateway, JetStream)
- [ ] Retry configuration set:
  - `maxAttempts`: [value]
  - `backoffStrategy`: [exponential/linear]
  - `backoffBase`: [value]ms
  - `backoffMax`: [value]ms
  - `jitter`: [value]%
  - `timeoutPerAttempt`: [value]s
  - `totalDeadline`: [value]s
- [ ] Circuit breaker configuration set:
  - `failureThreshold`: [value]
  - `errorRateThreshold`: [value]%
  - `errorRateWindow`: [value]s
  - `latencyThreshold`: [value]s (if applicable)
  - `openTimeout`: [value]s
  - `halfOpenMaxAttempts`: [value]
  - `successThreshold`: [value]
- [ ] Metrics/logging enabled
- [ ] Baseline metrics captured:
  - `router_nats_publish_total`: [baseline]
  - `router_nats_publish_failures_total`: [baseline]
  - `router_circuit_breaker_state_transitions_total`: [baseline]
  - System CPU: [baseline]%
  - System memory: [baseline]MB

#### Test Steps

| Step | Action | Expected Behavior | Verification |
|------|--------|-------------------|-------------|
| 1 | Start high load | N parallel clients, RPS: [value] | Load generator confirms |
| 2 | Wait for baseline | Normal operation for [X]s | Metrics stable |
| 3 | Inject fault | `router_nats_fault_injection:enable_fault(publish, [fault_type])` | Fault injection confirmed |
| 4 | Observe retry behavior | Monitor publish attempts | Retries occur, ≤ maxAttempts |
| 5 | Observe circuit breaker | Monitor breaker state | Breaker transitions to `open` |
| 6 | Verify degradation | Check system health | No OOM, latency within bounds |
| 7 | [Additional steps] | [Description] | [Verification] |

#### Expected Results

**Retry Behavior**:
- [ ] Each request makes ≤ `maxAttempts` publish attempts
- [ ] Inter-attempt intervals match backoff model (within jitter tolerance)
- [ ] Retries stop after `maxAttempts` reached
- [ ] Total retry time ≤ `totalDeadline`

**Circuit Breaker**:
- [ ] Breaker transitions to `open` within expected time window
- [ ] No real publish attempts to broker in `open` state
- [ ] New requests receive "circuit open" response or are blocked

**System Health**:
- [ ] System remains alive (no OOM, no crashes)
- [ ] Input request latency ≤ [SLO] (p99)
- [ ] System CPU usage ≤ 80%
- [ ] System memory usage ≤ 90%

#### Assertions

**Quantitative Assertions**:
```erlang
%% Retry assertions
assert_publish_attempts_per_request(ExpectedMaxAttempts),
assert_retry_intervals_match_backoff(BackoffModel, JitterTolerance),
assert_total_retry_time(TotalDeadline),

%% Circuit breaker assertions
assert_circuit_breaker_state(ExpectedState),
assert_circuit_breaker_transition_time(ExpectedWindow),
assert_publish_attempts_in_open_state(MaxAllowed),

%% System health assertions
assert_system_resources(CPUThreshold, MemoryThreshold),
assert_input_latency(SLO),
```

**Qualitative Assertions**:
- [ ] Logs contain retry attempt entries with correct attempt numbers
- [ ] Logs contain circuit breaker state transition events
- [ ] Logs contain "circuit open" messages when breaker is open
- [ ] No unexpected error types in logs

#### Metrics to Verify

| Metric | Expected Change | Verification Method |
|--------|----------------|---------------------|
| `router_nats_publish_total` | Increases during load | Counter increases |
| `router_nats_publish_failures_total` | Increases after fault injection | Counter increases |
| `router_circuit_breaker_state_transitions_total{state="open"}` | Increments when breaker opens | Counter increments |
| `router_circuit_breaker_error_rate` | Exceeds threshold | Gauge > errorRateThreshold |
| `router_nats_pending_operations_count` | May increase, then stabilize | Gauge within bounds |
| System CPU | Stays ≤ 80% | System metric |
| System memory | Stays ≤ 90% | System metric |

#### Logs to Verify

| Log Event | Expected Content | Verification Method |
|-----------|------------------|---------------------|
| `NATS_PUBLISH_ERROR` | Error code, subject, reason | JSON log parsing |
| Circuit breaker opened | State change, reason | JSON log parsing |
| Retry attempt | Attempt number, max attempts | JSON log parsing (if implemented) |
| Circuit breaker open blocking | Blocked publish message | JSON log parsing |

#### Cleanup

- [ ] Disable fault injection: `router_nats_fault_injection:disable_fault(publish)`
- [ ] Wait for recovery: [X]s
- [ ] Reset circuit breaker state (if needed)
- [ ] Verify system returns to baseline metrics
- [ ] Clear test data

#### Test Data

**Load Configuration**:
- Target RPS: [value]
- Concurrent clients: [value]
- Load duration: [value]s
- Request types: [mixed/batches/streams]

**Fault Configuration**:
- Fault type: [error/timeout/delay/close_connection]
- Fault start: After [X]s of normal operation
- Fault rate: [100% / partial]

#### Pass/Fail Criteria

**Test PASSES if**:
- ✅ All assertions pass
- ✅ All expected metrics change observed
- ✅ All expected log events present
- ✅ System health within bounds
- ✅ Cleanup successful

**Test FAILS if**:
- ❌ Any assertion fails
- ❌ Circuit breaker doesn't activate
- ❌ Retries exceed maxAttempts
- ❌ System resources exceed thresholds
- ❌ Unexpected errors occur

## Test Case Examples

### Example 1: R10-TC-001 - Mass Publish Failure → Circuit Breaker Activation

**Scenario**: Scenario 1 (Section 3.1)  
**Priority**: High  
**Estimated Duration**: 120 seconds

#### Prerequisites

- [ ] E2E environment running
- [ ] Retry configuration: `maxAttempts=3`, `backoffStrategy=exponential`, `backoffBase=100ms`, `backoffMax=5000ms`, `jitter=20%`, `timeoutPerAttempt=2s`, `totalDeadline=10s`
- [ ] Circuit breaker configuration: `failureThreshold=5`, `errorRateThreshold=50%`, `errorRateWindow=30s`, `openTimeout=30s`, `halfOpenMaxAttempts=3`, `successThreshold=2`
- [ ] Metrics enabled
- [ ] Baseline metrics captured

#### Test Steps

| Step | Action | Expected Behavior |
|------|--------|-------------------|
| 1 | Start 50 parallel clients, 200 RPS | Load generator starts |
| 2 | Wait 10s | Normal operation, metrics stable |
| 3 | `router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable})` | All publishes fail |
| 4 | Monitor for 60s | Retries occur, breaker opens |
| 5 | Verify breaker state | Breaker is `open` |
| 6 | Verify system health | CPU ≤ 80%, memory ≤ 90% |

#### Expected Results

- Retries: ≤ 3 attempts per request, backoff intervals match model
- Circuit breaker: Opens within 5-10s after fault injection
- System: No OOM, latency p99 ≤ 2s

#### Assertions

```erlang
assert_publish_attempts_per_request(3),
assert_circuit_breaker_state(open),
assert_publish_attempts_in_open_state(1),  % Only probes allowed
assert_system_resources(80, 90),
assert_input_latency(2000),  % 2s p99
```

#### Metrics to Verify

| Metric | Expected Change |
|--------|----------------|
| `router_nats_publish_failures_total` | Increases significantly |
| `router_circuit_breaker_state_transitions_total{state="open"}` | Increments |
| `router_circuit_breaker_error_rate` | > 50% |

#### Pass/Fail Criteria

**PASS**: All assertions pass, breaker opens, system healthy  
**FAIL**: Breaker doesn't open, retries exceed limit, system unhealthy

### Example 2: R10-TC-002 - Recovery After Failure (Half-open → Closed)

**Scenario**: Scenario 2 (Section 3.2)  
**Priority**: High  
**Estimated Duration**: 90 seconds

#### Prerequisites

- [ ] Scenario 1 completed, breaker in `open` state
- [ ] JetStream ready to be restored

#### Test Steps

| Step | Action | Expected Behavior |
|------|--------|-------------------|
| 1 | `router_nats_fault_injection:disable_fault(publish)` | Broker accepts publishes |
| 2 | Wait 30s (openTimeout) | Breaker transitions to `half_open` |
| 3 | Send 3 probe requests | Trial publishes reach broker |
| 4 | Verify breaker state | Breaker transitions to `closed` |
| 5 | Send normal load | Normal operation resumes |

#### Expected Results

- Breaker: Transitions `open → half_open → closed`
- Trial publishes: Limited to 3, all successful
- Normal operation: Resumes after breaker closes

#### Assertions

```erlang
assert_circuit_breaker_state_transition(open, half_open, 30000),  % Within 30s
assert_trial_publishes_count(3),
assert_circuit_breaker_state_transition(half_open, closed, 5000),  % After successful probes
assert_normal_operation_resumed(),
```

#### Metrics to Verify

| Metric | Expected Change |
|--------|----------------|
| `router_circuit_breaker_state_transitions_total{state="half_open"}` | Increments |
| `router_circuit_breaker_state_transitions_total{state="closed"}` | Increments |
| `router_nats_publish_total` | Increases (normal operation) |

## Test Case Matrix

| Test ID | Test Name | Scenario | Priority | Duration | Status |
|---------|-----------|----------|----------|----------|--------|
| R10-TC-001 | Mass Publish Failure → Circuit Breaker Activation | 3.1 | High | 120s | [ ] |
| R10-TC-002 | Recovery After Failure (Half-open → Closed) | 3.2 | High | 90s | [ ] |
| R10-TC-003 | Latency-Based Circuit Breaker Trigger | 3.3 | Medium | 60s | [ ] |
| R10-TC-004 | Partial Failures (Mixed Success/Failure) | Edge case | Medium | 90s | [ ] |
| R10-TC-005 | Thundering Herd on Recovery | Edge case | Low | 120s | [ ] |

## References

- [R10 Specification](./R10_PUBLISH_FAILURE_E2E_SPEC.md)
- [R10 Consistency Check](./R10_CONSISTENCY_CHECK.md)
- [R10 Metrics Review](./R10_METRICS_REVIEW.md)

