# R10 Detailed Test Cases: Retry Logic, Circuit Breaker, and Metrics

## Purpose

This document provides detailed test case specifications for R10 implementation:
- **Retry Logic**: Unit and integration tests for backoff, jitter, deadline, max attempts
- **Circuit Breaker**: Unit and integration tests for failure threshold, error rate, latency trigger, half-open
- **Metrics**: Unit tests for label validation and value verification

## Test Suite Structure

```
router_nats_publish_retry_SUITE.erl          # Unit tests for retry logic
router_circuit_breaker_SUITE.erl              # Unit tests for circuit breaker
router_publish_failure_e2e_SUITE.erl         # Integration/E2E tests (R10 scenarios)
router_metrics_r10_SUITE.erl                 # Metrics validation tests
```

## 1. Retry Logic Tests

### 1.1 Unit Tests: Backoff Strategies

#### Test: `test_exponential_backoff_calculation`

**Purpose**: Verify exponential backoff calculation with different attempt numbers

**Type**: Unit test

**Module**: `router_nats_publish_retry_SUITE.erl`

**Prerequisites**:
- Module `router_nats_publish_retry` loaded
- Mock publish function available

**Test Steps**:
```erlang
test_exponential_backoff_calculation(_Config) ->
    Base = 100,  % 100ms
    Max = 5000,  % 5000ms
    Jitter = 0,  % No jitter for deterministic test
    
    %% Test attempt 1: should be ~100ms
    Delay1 = router_nats_publish_retry:calculate_backoff(1, exponential, Base, Max, Jitter),
    ?assertEqual(100, Delay1),
    
    %% Test attempt 2: should be ~200ms (2^1 * 100)
    Delay2 = router_nats_publish_retry:calculate_backoff(2, exponential, Base, Max, Jitter),
    ?assertEqual(200, Delay2),
    
    %% Test attempt 3: should be ~400ms (2^2 * 100)
    Delay3 = router_nats_publish_retry:calculate_backoff(3, exponential, Base, Max, Jitter),
    ?assertEqual(400, Delay3),
    
    %% Test attempt 6: should be capped at Max (2^5 * 100 = 3200, but capped at 5000)
    Delay6 = router_nats_publish_retry:calculate_backoff(6, exponential, Base, Max, Jitter),
    ?assertEqual(3200, Delay6),
    
    %% Test attempt 10: should be capped at Max
    Delay10 = router_nats_publish_retry:calculate_backoff(10, exponential, Base, Max, Jitter),
    ?assert(Delay10 =< Max),
    ?assert(Delay10 >= 100).
```

**Expected Results**:
- Attempt 1: 100ms
- Attempt 2: 200ms
- Attempt 3: 400ms
- Attempt 6: 3200ms (not capped yet)
- Attempt 10: Capped at 5000ms

**Assertions**:
- Delays follow exponential progression: `delay(n) = base * 2^(n-1)`
- Delays are capped at `max`
- Delays are non-negative

---

#### Test: `test_linear_backoff_calculation`

**Purpose**: Verify linear backoff calculation

**Type**: Unit test

**Test Steps**:
```erlang
test_linear_backoff_calculation(_Config) ->
    Base = 100,
    Max = 5000,
    Jitter = 0,
    
    %% Test attempt 1: should be 100ms
    Delay1 = router_nats_publish_retry:calculate_backoff(1, linear, Base, Max, Jitter),
    ?assertEqual(100, Delay1),
    
    %% Test attempt 2: should be 200ms (2 * 100)
    Delay2 = router_nats_publish_retry:calculate_backoff(2, linear, Base, Max, Jitter),
    ?assertEqual(200, Delay2),
    
    %% Test attempt 50: should be capped at Max (50 * 100 = 5000)
    Delay50 = router_nats_publish_retry:calculate_backoff(50, linear, Base, Max, Jitter),
    ?assertEqual(5000, Delay50),
    
    %% Test attempt 100: should be capped at Max
    Delay100 = router_nats_publish_retry:calculate_backoff(100, linear, Base, Max, Jitter),
    ?assertEqual(5000, Delay100).
```

**Expected Results**:
- Attempt 1: 100ms
- Attempt 2: 200ms
- Attempt 50: 5000ms (capped)
- Attempt 100: 5000ms (capped)

---

#### Test: `test_backoff_with_jitter`

**Purpose**: Verify jitter adds randomness within specified range

**Type**: Unit test

**Test Steps**:
```erlang
test_backoff_with_jitter(_Config) ->
    Base = 100,
    Max = 5000,
    JitterPercent = 20,  % ±20%
    
    %% Run multiple times to verify jitter range
    Delays = [router_nats_publish_retry:calculate_backoff(2, exponential, Base, Max, JitterPercent) 
              || _ <- lists:seq(1, 100)],
    
    %% Base delay for attempt 2: 200ms
    BaseDelay = 200,
    JitterRange = trunc(BaseDelay * JitterPercent / 100),  % ±40ms
    
    %% All delays should be within [160, 240]ms
    lists:foreach(fun(Delay) ->
        ?assert(Delay >= BaseDelay - JitterRange),
        ?assert(Delay =< BaseDelay + JitterRange),
        ?assert(Delay >= 0),
        ?assert(Delay =< Max)
    end, Delays),
    
    %% Verify we have some variation (not all same value)
    UniqueDelays = sets:from_list(Delays),
    ?assert(sets:size(UniqueDelays) > 1).
```

**Expected Results**:
- All delays within `[base - jitter, base + jitter]` range
- Delays are non-negative and ≤ max
- Some variation in delays (not all identical)

---

### 1.2 Unit Tests: Max Attempts

#### Test: `test_max_attempts_enforced`

**Purpose**: Verify retry stops after max attempts

**Type**: Unit test

**Test Steps**:
```erlang
test_max_attempts_enforced(_Config) ->
    MaxAttempts = 3,
    AttemptCount = make_ref(),
    AttemptCountRef = put(attempt_count, AttemptCount),
    
    %% Mock publish function that always fails
    PublishFun = fun(_Subject, _Payload) ->
        Count = get(attempt_count),
        put(attempt_count, Count + 1),
        {error, timeout}
    end,
    
    %% Mock metrics function
    MetricsFun = fun(Attempt, _Max) -> ok end,
    
    Config = #{
        <<"max_attempts">> => MaxAttempts,
        <<"backoff_strategy">> => exponential,
        <<"backoff_base_ms">> => 10,  % Small for fast test
        <<"backoff_max_ms">> => 100,
        <<"jitter_percent">> => 0,
        <<"timeout_per_attempt_ms">> => 1000,
        <<"total_deadline_ms">> => 10000
    },
    
    put(attempt_count, 0),
    Result = router_nats_publish_retry:publish_with_retry(
        <<"test.subject">>, <<"payload">>, PublishFun, MetricsFun, #{}, Config
    ),
    
    FinalCount = get(attempt_count),
    
    %% Should have attempted exactly MaxAttempts times
    ?assertEqual(MaxAttempts, FinalCount),
    ?assertMatch({error, max_attempts_exceeded}, Result).
```

**Expected Results**:
- Exactly `max_attempts` publish attempts made
- Function returns `{error, max_attempts_exceeded}`
- No additional attempts after max reached

---

### 1.3 Unit Tests: Deadline Management

#### Test: `test_deadline_exceeded_before_max_attempts`

**Purpose**: Verify retry stops when deadline exceeded, even if max attempts not reached

**Type**: Unit test

**Test Steps**:
```erlang
test_deadline_exceeded_before_max_attempts(_Config) ->
    MaxAttempts = 10,  % High max attempts
    TotalDeadline = 500,  % 500ms deadline (short)
    BackoffBase = 200,  % 200ms base delay (will exceed deadline quickly)
    
    PublishFun = fun(_Subject, _Payload) ->
        timer:sleep(50),  % Simulate some processing time
        {error, timeout}
    end,
    
    MetricsFun = fun(_Attempt, _Max) -> ok end,
    
    Config = #{
        <<"max_attempts">> => MaxAttempts,
        <<"backoff_strategy">> => exponential,
        <<"backoff_base_ms">> => BackoffBase,
        <<"backoff_max_ms">> => 5000,
        <<"jitter_percent">> => 0,
        <<"timeout_per_attempt_ms">> => 1000,
        <<"total_deadline_ms">> => TotalDeadline
    },
    
    StartTime = erlang:system_time(millisecond),
    Result = router_nats_publish_retry:publish_with_retry(
        <<"test.subject">>, <<"payload">>, PublishFun, MetricsFun, #{}, Config
    ),
    Elapsed = erlang:system_time(millisecond) - StartTime,
    
    %% Should fail with deadline_exceeded
    ?assertMatch({error, deadline_exceeded}, Result),
    
    %% Should have taken approximately TotalDeadline (with some tolerance)
    ?assert(Elapsed >= TotalDeadline),
    ?assert(Elapsed < TotalDeadline + 200),  % Allow 200ms tolerance
    
    %% Should have made fewer attempts than max (deadline hit first)
    %% This is verified by the deadline_exceeded error
    ok.
```

**Expected Results**:
- Returns `{error, deadline_exceeded}`
- Total time ≈ `total_deadline_ms` (within tolerance)
- Fewer attempts than `max_attempts` (deadline hit first)

---

#### Test: `test_deadline_not_exceeded_with_fast_success`

**Purpose**: Verify deadline not exceeded when publish succeeds quickly

**Type**: Unit test

**Test Steps**:
```erlang
test_deadline_not_exceeded_with_fast_success(_Config) ->
    TotalDeadline = 10000,  % 10s deadline
    
    %% Mock publish that succeeds on 2nd attempt
    AttemptRef = make_ref(),
    put(attempt_ref, AttemptRef),
    put(attempt_count, 0),
    
    PublishFun = fun(_Subject, _Payload) ->
        Count = get(attempt_count),
        put(attempt_count, Count + 1),
        case Count of
            0 -> {error, timeout};  % First attempt fails
            _ -> ok  % Second attempt succeeds
        end
    end,
    
    MetricsFun = fun(_Attempt, _Max) -> ok end,
    
    Config = router_nats_publish_retry:get_default_config(),
    Config1 = Config#{<<"total_deadline_ms">> => TotalDeadline},
    
    StartTime = erlang:system_time(millisecond),
    Result = router_nats_publish_retry:publish_with_retry(
        <<"test.subject">>, <<"payload">>, PublishFun, MetricsFun, #{}, Config1
    ),
    Elapsed = erlang:system_time(millisecond) - StartTime,
    
    %% Should succeed
    ?assertMatch({ok, 2}, Result),
    
    %% Should complete well before deadline
    ?assert(Elapsed < TotalDeadline),
    
    %% Should have made 2 attempts
    ?assertEqual(2, get(attempt_count)).
```

**Expected Results**:
- Returns `{ok, AttemptNumber}`
- Completes before deadline
- Makes expected number of attempts

---

### 1.4 Unit Tests: Error Classification

#### Test: `test_retryable_errors`

**Purpose**: Verify retryable errors trigger retries

**Type**: Unit test

**Test Steps**:
```erlang
test_retryable_errors(_Config) ->
    RetryableErrors = [
        timeout,
        connection_refused,
        connection_closed,
        nats_unavailable,
        not_connected,
        {error, timeout},
        {error, connection_refused}
    ],
    
    lists:foreach(fun(Error) ->
        ?assert(router_nats_publish_retry:is_retryable_error(Error),
                "Error ~p should be retryable", [Error])
    end, RetryableErrors).
```

**Expected Results**:
- All listed errors return `true` for `is_retryable_error/1`

---

#### Test: `test_non_retryable_errors`

**Purpose**: Verify non-retryable errors don't trigger retries

**Type**: Unit test

**Test Steps**:
```erlang
test_non_retryable_errors(_Config) ->
    NonRetryableErrors = [
        invalid_payload,
        authorization_failed,
        {error, invalid_payload},
        {error, authorization_failed}
    ],
    
    lists:foreach(fun(Error) ->
        ?assertNot(router_nats_publish_retry:is_retryable_error(Error),
                   "Error ~p should NOT be retryable", [Error])
    end, NonRetryableErrors).
```

**Expected Results**:
- All listed errors return `false` for `is_retryable_error/1`

---

### 1.5 Integration Tests: Retry Flow

#### Test: `test_retry_flow_with_fault_injection`

**Purpose**: Verify retry flow with actual fault injection

**Type**: Integration test

**Module**: `router_publish_failure_e2e_SUITE.erl`

**Prerequisites**:
- Router application started
- NATS fault injection available
- Metrics collection enabled

**Test Steps**:
```erlang
test_retry_flow_with_fault_injection(_Config) ->
    %% Enable retry
    application:set_env(beamline_router, publish_retry_enabled, true),
    application:set_env(beamline_router, publish_retry_max_attempts, 3),
    application:set_env(beamline_router, publish_retry_backoff_base_ms, 100),
    
    %% Enable fault injection (fails first 2 attempts, succeeds on 3rd)
    AttemptCount = put(attempt_count, 0),
    router_nats_fault_injection:enable_fault(publish, fun(_) ->
        Count = get(attempt_count),
        put(attempt_count, Count + 1),
        case Count < 2 of
            true -> {true, {error, nats_unavailable}};
            false -> false  % Succeed on 3rd attempt
        end
    end),
    
    %% Capture baseline metrics
    BaselineAttempts = get_metric_value(router_nats_publish_attempts_total, #{status => <<"success">>, retry_count => <<"2">>}),
    
    %% Publish
    Result = router_nats:publish(<<"test.subject">>, <<"payload">>),
    
    %% Verify success
    ?assertEqual(ok, Result),
    
    %% Verify metrics
    FinalAttempts = get_metric_value(router_nats_publish_attempts_total, #{status => <<"success">>, retry_count => <<"2">>}),
    ?assertEqual(BaselineAttempts + 1, FinalAttempts),
    
    %% Verify retry delay metric
    RetryDelay = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"1">>}),
    ?assert(RetryDelay > 0),
    
    %% Cleanup
    router_nats_fault_injection:disable_fault(publish),
    ok.
```

**Expected Results**:
- Publish succeeds after retries
- Metrics show correct retry count
- Retry delay metrics recorded

---

## 2. Circuit Breaker Tests

### 2.1 Unit Tests: Failure Threshold

#### Test: `test_circuit_breaker_opens_on_failure_threshold`

**Purpose**: Verify circuit breaker opens when failure threshold exceeded

**Type**: Unit test

**Module**: `router_circuit_breaker_SUITE.erl`

**Test Steps**:
```erlang
test_circuit_breaker_opens_on_failure_threshold(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    FailureThreshold = 5,
    
    Config = #{
        <<"failure_threshold">> => FailureThreshold,
        <<"error_rate_threshold">> => 1.0,  % High to avoid triggering
        <<"latency_threshold_ms">> => undefined
    },
    
    %% Initialize circuit breaker
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Record failures up to threshold
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, FailureThreshold)),
    
    %% Verify circuit is open
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    %% Verify should_allow returns error
    ?assertMatch({error, circuit_open}, router_circuit_breaker:should_allow(TenantId, ProviderId)),
    
    %% Verify metrics
    StateMetric = get_metric_value(router_circuit_breaker_state, #{tenant_id => TenantId, provider_id => ProviderId, state => <<"open">>}),
    ?assertEqual(1.0, StateMetric),
    
    TriggerReason = get_metric_value(router_circuit_breaker_trigger_reason, #{tenant_id => TenantId, provider_id => ProviderId, reason => <<"failure_threshold_exceeded">>}),
    ?assertEqual(1, TriggerReason).
```

**Expected Results**:
- Circuit breaker state = `open`
- `should_allow` returns `{error, circuit_open}`
- State metric = 1.0
- Trigger reason = "failure_threshold_exceeded"

---

### 2.2 Unit Tests: Error Rate Threshold

#### Test: `test_circuit_breaker_opens_on_error_rate_threshold`

**Purpose**: Verify circuit breaker opens when error rate threshold exceeded

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_opens_on_error_rate_threshold(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    ErrorRateThreshold = 0.5,  % 50%
    WindowSeconds = 30,
    
    Config = #{
        <<"failure_threshold">> => 100,  % High to avoid triggering
        <<"error_rate_threshold">> => ErrorRateThreshold,
        <<"error_rate_window_seconds">> => WindowSeconds,
        <<"latency_threshold_ms">> => undefined
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Record 6 failures and 4 successes (60% error rate)
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 6)),
    
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 4)),
    
    %% Wait a bit for window calculation
    timer:sleep(100),
    
    %% Verify circuit is open
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    %% Verify trigger reason
    TriggerReason = get_metric_value(router_circuit_breaker_trigger_reason, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        reason => <<"error_rate_threshold_exceeded">>
    }),
    ?assertEqual(1, TriggerReason).
```

**Expected Results**:
- Circuit breaker opens when error rate > 50%
- Trigger reason = "error_rate_threshold_exceeded"

---

### 2.3 Unit Tests: Latency Trigger

#### Test: `test_circuit_breaker_opens_on_latency_threshold`

**Purpose**: Verify circuit breaker opens when latency threshold exceeded

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_opens_on_latency_threshold(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    LatencyThreshold = 5000,  % 5 seconds
    
    Config = #{
        <<"failure_threshold">> => 100,
        <<"error_rate_threshold">> => 1.0,
        <<"latency_threshold_ms">> => LatencyThreshold
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Simulate high latency by setting metric
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 6.0}, #{}),
    
    %% Record a success (to trigger state check)
    router_circuit_breaker:record_success(TenantId, ProviderId),
    
    %% Wait for latency check
    timer:sleep(100),
    
    %% Verify circuit is open
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    %% Verify trigger reason
    TriggerReason = get_metric_value(router_circuit_breaker_trigger_reason, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        reason => <<"latency_threshold_exceeded">>
    }),
    ?assertEqual(1, TriggerReason).
```

**Expected Results**:
- Circuit breaker opens when latency > threshold
- Trigger reason = "latency_threshold_exceeded"

---

### 2.4 Unit Tests: Half-Open State

#### Test: `test_circuit_breaker_half_open_after_timeout`

**Purpose**: Verify circuit breaker transitions to half-open after timeout

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_half_open_after_timeout(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    TimeoutMs = 1000,  % 1 second for fast test
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => TimeoutMs,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Wait for timeout
    timer:sleep(TimeoutMs + 100),
    
    %% Trigger state check (by recording a state)
    router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Verify half-open
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Verify state metric
    StateMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"half_open">>
    }),
    ?assertEqual(2.0, StateMetric).
```

**Expected Results**:
- Circuit transitions `open → half_open` after timeout
- State metric = 2.0 (half_open)

---

#### Test: `test_circuit_breaker_closes_after_success_threshold`

**Purpose**: Verify circuit breaker closes after success threshold in half-open

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_closes_after_success_threshold(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    SuccessThreshold = 2,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => SuccessThreshold
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(1100),
    router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Record successes to meet threshold
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, SuccessThreshold)),
    
    %% Verify closed
    {ok, ClosedState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(closed, ClosedState),
    
    %% Verify state metric
    StateMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"closed">>
    }),
    ?assertEqual(0.0, StateMetric).
```

**Expected Results**:
- Circuit transitions `half_open → closed` after success threshold
- State metric = 0.0 (closed)

---

#### Test: `test_circuit_breaker_reopens_on_half_open_failure`

**Purpose**: Verify circuit breaker reopens immediately on any failure in half-open

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_reopens_on_half_open_failure(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    Config = #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000,
        <<"half_open_max_calls">> => 3,
        <<"success_threshold">> => 2
    },
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, Config),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(1100),
    router_circuit_breaker:record_state(TenantId, ProviderId),
    
    {ok, HalfOpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(half_open, HalfOpenState),
    
    %% Record failure in half-open
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    %% Verify reopened
    {ok, OpenState} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, OpenState),
    
    %% Verify trigger reason
    TriggerReason = get_metric_value(router_circuit_breaker_trigger_reason, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        reason => <<"half_open_failure">>
    }),
    ?assertEqual(1, TriggerReason).
```

**Expected Results**:
- Circuit transitions `half_open → open` on any failure
- Trigger reason = "half_open_failure"

---

## 3. Metrics Tests

### 3.1 Unit Tests: Label Validation

#### Test: `test_publish_attempts_metrics_labels`

**Purpose**: Verify `router_nats_publish_attempts_total` has correct labels

**Type**: Unit test

**Module**: `router_metrics_r10_SUITE.erl`

**Test Steps**:
```erlang
test_publish_attempts_metrics_labels(_Config) ->
    %% Clear metrics
    router_metrics:ensure(),
    
    %% Simulate publish attempts with different retry counts
    router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
        status => <<"success">>,
        retry_count => <<"0">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
        status => <<"success">>,
        retry_count => <<"1">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_attempts_total, #{count => 1}, #{
        status => <<"error">>,
        retry_count => <<"2">>
    }),
    
    %% Verify metrics exist with correct labels
    Success0 = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"success">>,
        retry_count => <<"0">>
    }),
    ?assertEqual(1, Success0),
    
    Success1 = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"success">>,
        retry_count => <<"1">>
    }),
    ?assertEqual(1, Success1),
    
    Error2 = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"error">>,
        retry_count => <<"2">>
    }),
    ?assertEqual(1, Error2).
```

**Expected Results**:
- Metrics exist with correct label combinations
- Values match emitted counts

---

#### Test: `test_publish_errors_metrics_labels`

**Purpose**: Verify `router_nats_publish_errors_total` has correct error_type labels

**Type**: Unit test

**Test Steps**:
```erlang
test_publish_errors_metrics_labels(_Config) ->
    router_metrics:ensure(),
    
    %% Emit errors with different types
    router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
        error_type => <<"timeout">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
        error_type => <<"connection">>
    }),
    
    router_metrics:emit_metric(router_nats_publish_errors_total, #{count => 1}, #{
        error_type => <<"nack">>
    }),
    
    %% Verify labels
    Timeout = get_metric_value(router_nats_publish_errors_total, #{error_type => <<"timeout">>}),
    ?assertEqual(1, Timeout),
    
    Connection = get_metric_value(router_nats_publish_errors_total, #{error_type => <<"connection">>}),
    ?assertEqual(1, Connection),
    
    Nack = get_metric_value(router_nats_publish_errors_total, #{error_type => <<"nack">>}),
    ?assertEqual(1, Nack).
```

**Expected Results**:
- All error types have correct labels
- Values match emitted counts

---

#### Test: `test_circuit_breaker_state_metrics_labels`

**Purpose**: Verify `router_circuit_breaker_state` has correct state labels

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_state_metrics_labels(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, #{}),
    
    %% Verify closed state
    ClosedMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"closed">>
    }),
    ?assertEqual(0.0, ClosedMetric),
    
    %% Open circuit
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    %% Verify open state
    OpenMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"open">>
    }),
    ?assertEqual(1.0, OpenMetric),
    
    %% Wait for timeout and transition to half-open
    timer:sleep(61000),  % Default timeout is 60s
    router_circuit_breaker:record_state(TenantId, ProviderId),
    
    %% Verify half-open state
    HalfOpenMetric = get_metric_value(router_circuit_breaker_state, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        state => <<"half_open">>
    }),
    ?assertEqual(2.0, HalfOpenMetric).
```

**Expected Results**:
- Closed state: value = 0.0
- Open state: value = 1.0
- Half-open state: value = 2.0
- All have correct tenant_id and provider_id labels

---

#### Test: `test_circuit_breaker_transitions_metrics_labels`

**Purpose**: Verify `router_circuit_breaker_state_transitions_total` has from/to labels

**Type**: Unit test

**Test Steps**:
```erlang
test_circuit_breaker_transitions_metrics_labels(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, #{
        <<"failure_threshold">> => 5,
        <<"timeout_ms">> => 1000
    }),
    
    %% Open circuit (closed → open)
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 5)),
    
    ClosedToOpen = get_metric_value(router_circuit_breaker_state_transitions_total, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => <<"closed">>,
        to => <<"open">>
    }),
    ?assertEqual(1, ClosedToOpen),
    
    %% Wait for timeout (open → half_open)
    timer:sleep(1100),
    router_circuit_breaker:record_state(TenantId, ProviderId),
    
    OpenToHalfOpen = get_metric_value(router_circuit_breaker_state_transitions_total, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => <<"open">>,
        to => <<"half_open">>
    }),
    ?assertEqual(1, OpenToHalfOpen),
    
    %% Close circuit (half_open → closed)
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_success(TenantId, ProviderId)
    end, lists:seq(1, 2)),
    
    HalfOpenToClosed = get_metric_value(router_circuit_breaker_state_transitions_total, #{
        tenant_id => TenantId,
        provider_id => ProviderId,
        from => <<"half_open">>,
        to => <<"closed">>
    }),
    ?assertEqual(1, HalfOpenToClosed).
```

**Expected Results**:
- All transitions have correct `from` and `to` labels
- Values increment correctly

---

### 3.2 Unit Tests: Value Verification

#### Test: `test_publish_latency_metrics_values`

**Purpose**: Verify `router_nats_publish_latency_seconds` values are correct

**Type**: Unit test

**Test Steps**:
```erlang
test_publish_latency_metrics_values(_Config) ->
    router_metrics:ensure(),
    
    %% Emit latency values
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 0.1}, #{}),
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 0.5}, #{}),
    router_metrics:emit_metric(router_nats_publish_latency_seconds, #{value => 2.0}, #{}),
    
    %% Verify values (should be last emitted value for gauge)
    Latency = get_metric_value(router_nats_publish_latency_seconds, #{}),
    ?assertEqual(2.0, Latency).
```

**Expected Results**:
- Latency values stored correctly
- Values in seconds (not milliseconds)

---

#### Test: `test_retry_delay_metrics_values`

**Purpose**: Verify `router_nats_publish_retry_delay_seconds` values are correct

**Type**: Unit test

**Test Steps**:
```erlang
test_retry_delay_metrics_values(_Config) ->
    router_metrics:ensure(),
    
    %% Emit retry delays for different attempts
    router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, #{value => 0.1}, #{
        attempt => <<"1">>
    }),
    router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, #{value => 0.2}, #{
        attempt => <<"2">>
    }),
    router_metrics:emit_metric(router_nats_publish_retry_delay_seconds, #{value => 0.4}, #{
        attempt => <<"3">>
    }),
    
    %% Verify values
    Delay1 = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"1">>}),
    ?assertEqual(0.1, Delay1),
    
    Delay2 = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"2">>}),
    ?assertEqual(0.2, Delay2),
    
    Delay3 = get_metric_value(router_nats_publish_retry_delay_seconds, #{attempt => <<"3">>}),
    ?assertEqual(0.4, Delay3).
```

**Expected Results**:
- Retry delays stored with correct attempt labels
- Values in seconds

---

## 4. Integration Tests: Combined Scenarios

### 4.1 E2E Test: Retry + Circuit Breaker

#### Test: `test_retry_with_circuit_breaker_activation`

**Purpose**: Verify retry logic works correctly with circuit breaker

**Type**: Integration/E2E test

**Module**: `router_publish_failure_e2e_SUITE.erl`

**Prerequisites**:
- Router application started
- Retry enabled
- Circuit breaker configured
- Fault injection available

**Test Steps**:
```erlang
test_retry_with_circuit_breaker_activation(_Config) ->
    TenantId = <<"tenant1">>,
    ProviderId = <<"provider1">>,
    
    %% Configure retry
    application:set_env(beamline_router, publish_retry_enabled, true),
    application:set_env(beamline_router, publish_retry_max_attempts, 3),
    
    %% Configure circuit breaker
    router_circuit_breaker:record_state_with_config(TenantId, ProviderId, #{
        <<"failure_threshold">> => 5,
        <<"error_rate_threshold">> => 0.5,
        <<"timeout_ms">> => 30000
    }),
    
    %% Enable fault injection (all publishes fail)
    router_nats_fault_injection:enable_fault(publish, {error, nats_unavailable}),
    
    %% Make multiple publish attempts to trigger circuit breaker
    lists:foreach(fun(_) ->
        router_nats:publish(<<"test.subject">>, <<"payload">>)
    end, lists:seq(1, 10)),
    
    %% Wait for circuit breaker to open
    timer:sleep(2000),
    
    %% Verify circuit breaker is open
    {ok, State} = router_circuit_breaker:get_state(TenantId, ProviderId),
    ?assertEqual(open, State),
    
    %% Verify retry attempts were made (before circuit opened)
    Attempts = get_metric_value(router_nats_publish_attempts_total, #{
        status => <<"error">>,
        retry_count => <<"2">>
    }),
    ?assert(Attempts > 0),
    
    %% Cleanup
    router_nats_fault_injection:disable_fault(publish),
    ok.
```

**Expected Results**:
- Retries occur before circuit breaker opens
- Circuit breaker opens
- Retry attempts are recorded in metrics
- Circuit breaker eventually opens and blocks new requests

---

## 5. Test Helper Functions

### Helper: `get_metric_value/2`

```erlang
%% @doc Get metric value from ETS with labels
get_metric_value(MetricName, Labels) ->
    router_metrics:ensure(),
    LabelsKey = router_metrics:normalize_labels(Labels),
    Key = {MetricName, LabelsKey},
    case ets:lookup(router_metrics, Key) of
        [{Key, Value}] -> Value;
        [] -> 0
    end.
```

---

## 6. Test Suite Structure

### `router_nats_publish_retry_SUITE.erl`

```erlang
-module(router_nats_publish_retry_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        {group, unit_tests},
        {group, integration_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_exponential_backoff_calculation,
            test_linear_backoff_calculation,
            test_backoff_with_jitter,
            test_max_attempts_enforced,
            test_deadline_exceeded_before_max_attempts,
            test_deadline_not_exceeded_with_fast_success,
            test_retryable_errors,
            test_non_retryable_errors
        ]},
        {integration_tests, [sequence], [
            test_retry_flow_with_fault_injection
        ]}
    ].
```

### `router_circuit_breaker_SUITE.erl`

```erlang
-module(router_circuit_breaker_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        {group, unit_tests},
        {group, integration_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel], [
            test_circuit_breaker_opens_on_failure_threshold,
            test_circuit_breaker_opens_on_error_rate_threshold,
            test_circuit_breaker_opens_on_latency_threshold,
            test_circuit_breaker_half_open_after_timeout,
            test_circuit_breaker_closes_after_success_threshold,
            test_circuit_breaker_reopens_on_half_open_failure
        ]},
        {integration_tests, [sequence], [
            test_retry_with_circuit_breaker_activation
        ]}
    ].
```

### `router_metrics_r10_SUITE.erl`

```erlang
-module(router_metrics_r10_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        {group, label_tests},
        {group, value_tests}
    ].

groups() ->
    [
        {label_tests, [parallel], [
            test_publish_attempts_metrics_labels,
            test_publish_errors_metrics_labels,
            test_circuit_breaker_state_metrics_labels,
            test_circuit_breaker_transitions_metrics_labels
        ]},
        {value_tests, [parallel], [
            test_publish_latency_metrics_values,
            test_retry_delay_metrics_values
        ]}
    ].
```

---

## 7. Test Execution

### Running Tests

```bash
# Run all R10 tests
rebar3 ct --suite test/router_nats_publish_retry_SUITE
rebar3 ct --suite test/router_circuit_breaker_SUITE
rebar3 ct --suite test/router_metrics_r10_SUITE
rebar3 ct --suite test/router_publish_failure_e2e_SUITE

# Run specific group
rebar3 ct --suite test/router_nats_publish_retry_SUITE --group unit_tests

# Run specific test
rebar3 ct --suite test/router_nats_publish_retry_SUITE --case test_exponential_backoff_calculation
```

---

## 8. Test Coverage Goals

| Component | Unit Tests | Integration Tests | Total |
|-----------|------------|-------------------|-------|
| Retry Logic | 8 | 1 | 9 |
| Circuit Breaker | 6 | 1 | 7 |
| Metrics | 6 | 0 | 6 |
| **Total** | **20** | **2** | **22** |

---

## References

- [R10 Specification](./R10_PUBLISH_FAILURE_E2E_SPEC.md)
- [R10 Implementation Complete](./R10_IMPLEMENTATION_COMPLETE.md)
- [R10 Test Cases Template](./R10_TEST_CASES_TEMPLATE.md)
- [Common Test Documentation](http://erlang.org/doc/apps/common_test/index.html)

