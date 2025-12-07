# Testing Guide

This document provides comprehensive guidance on test execution procedures, test data requirements, and testing best practices for the router project.

## Test Execution Procedures

### Running Tests Locally

#### Single Test Suite

Run a specific test suite:
```bash
cd apps/otp/router
rebar3 as test ct --suite router_circuit_breaker_SUITE
```

#### Multiple Test Suites

Run multiple test suites:
```bash
rebar3 as test ct --suite router_circuit_breaker_SUITE --suite router_metrics_r10_SUITE
```

#### All Tests

Run all test suites:
```bash
rebar3 as test ct
```

#### Specific Test Case

Run a specific test case:
```bash
rebar3 as test ct --suite router_circuit_breaker_SUITE --case test_circuit_breaker_opens_on_failures
```

#### Test Groups

Run a specific test group:
```bash
rebar3 as test ct --suite router_circuit_breaker_SUITE --group integration
```

### Test Configuration

#### Environment Variables

Set test environment variables:
```bash
export ROUTER_TEST_TENANT_ID="test_tenant_1"
export ROUTER_TEST_PROVIDER_ID="test_provider_1"
rebar3 as test ct --suite router_circuit_breaker_SUITE
```

#### Application Configuration

Test-specific configuration is set in `router_test_utils.erl`:
- `grpc_port = 0` (disabled)
- `grpc_enabled = false`
- `nats_mode = mock`
- `tracing_enabled = false`
- `disable_heir = true`

### Test Lifecycle

#### Suite Lifecycle

**`init_per_suite/1`**:
```erlang
init_per_suite(Config) ->
    ok = router_test_utils:start_router_app(),
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    ok = router_r10_metrics:clear_metrics(),
    Config.
```

**`end_per_suite/1`**:
```erlang
end_per_suite(_Config) ->
    ok = router_test_utils:stop_router_app(),
    ok.
```

#### Test Case Lifecycle

**`init_per_testcase/2`**:
```erlang
init_per_testcase(_TestCase, Config) ->
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    ok = router_test_utils:reset_circuit_breaker(),
    ok = router_r10_metrics:clear_metrics(),
    Config.
```

**`end_per_testcase/2`**:
```erlang
end_per_testcase(_TestCase, _Config) ->
    ok.
```

### Test Utilities

#### Application Lifecycle

**`router_test_utils:start_router_app/0`**:
- Starts beamline_router application
- Idempotent (safe to call multiple times)
- Fails immediately if application cannot start
- Verifies supervisor and children are running

**`router_test_utils:stop_router_app/0`**:
- Stops beamline_router application
- Cleans up resources

#### Process Verification

**`router_test_utils:ensure_circuit_breaker_alive/0`**:
- Verifies circuit breaker process is running
- Waits up to 2 seconds for process to start
- Fails if process is not running

**`router_test_utils:ensure_router_nats_alive/0`**:
- Verifies router_nats process is running
- Fails immediately if process is not running

#### State Reset

**`router_test_utils:reset_circuit_breaker/0`**:
- Resets circuit breaker state
- Clears ETS tables
- Safe to call between test cases

**`router_test_utils:reset_rbac/0`**:
- Resets RBAC state
- Clears ETS tables

**`router_test_utils:reset_idem/0`**:
- Resets idempotency state
- Clears ETS tables

#### Metric Waiters

**`router_test_utils:wait_for_metric/3`**:
```erlang
ok = router_test_utils:wait_for_metric(
    fun() -> router_r10_metrics:get_publish_attempts_total() end,
    1,  % Expected value
    200 % Timeout ms
).
```

**`router_test_utils:wait_for_breaker_state/4`**:
```erlang
ok = router_test_utils:wait_for_breaker_state(
    <<"t1">>, <<"p1">>,
    open,  % Expected state
    3000   % Timeout ms
).
```

#### Debugging Utilities

**`router_test_utils:dump_metrics/0`**:
- Dumps all metrics for debugging
- Delegates to risk theme metrics module

**`router_test_utils:dump_supervisor_children/0`**:
- Dumps supervisor children for debugging
- Useful for diagnosing startup issues

## Test Data Requirements

### Tenant and Provider IDs

#### Standard Test IDs

Use consistent test IDs across test suites:
```erlang
-define(TEST_TENANT_ID, <<"test_tenant_1">>).
-define(TEST_PROVIDER_ID, <<"test_provider_1">>).
```

#### Dynamic Test IDs

For tests that need unique IDs:
```erlang
TenantId = <<"test_tenant_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
ProviderId = <<"test_provider_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.
```

### Request Data

#### Standard Request Map

```erlang
RequestMap = #{
    <<"tenant_id">> => <<"t1">>,
    <<"request_id">> => <<"req_1">>,
    <<"message">> => #{
        <<"content">> => <<"test message">>
    },
    <<"context">> => #{
        <<"user_id">> => <<"user_1">>
    }
}.
```

#### Route Request Record

```erlang
RouteRequest = #route_request{
    message = RequestMap,
    context = ContextMap
}.
```

### Policy Data

#### Standard Policy

```erlang
Policy = #policy{
    policy_id = <<"policy_1">>,
    tenant_id = <<"t1">>,
    providers = [
        #provider{
            provider_id = <<"p1">>,
            weight = 100
        }
    ],
    pre = [],
    validators = [],
    post = []
}.
```

### Metrics Data

#### Clearing Metrics

Always clear metrics before test cases:
```erlang
ok = router_r10_metrics:clear_metrics().
```

#### Reading Metrics

Use metrics access layer:
```erlang
Value = router_r10_metrics:get_metric_value(
    router_circuit_breaker_state,
    #{tenant_id => <<"t1">>, provider_id => <<"p1">>}
).
```

#### Waiting for Metrics

Use waiters for deterministic tests:
```erlang
ok = router_r10_metrics:wait_for_trigger_reason(
    <<"t1">>, <<"p1">>,
    [router_r10_metrics:trigger_reason_failure_threshold()],
    3000
).
```

## Test Patterns

### Deterministic Assertions

#### Use Waiters

**Bad** (non-deterministic):
```erlang
Value = router_r10_metrics:get_metric_value(MetricName, Labels),
?assertEqual(1, Value).  % May fail due to timing
```

**Good** (deterministic):
```erlang
ok = router_test_utils:wait_for_metric(
    fun() -> router_r10_metrics:get_metric_value(MetricName, Labels) end,
    1,
    200
).
```

### Error Handling

#### Expected Errors

```erlang
Result = router_circuit_breaker:get_state(TenantId, ProviderId),
case Result of
    {ok, State} ->
        ?assertEqual(open, State);
    {error, Reason} ->
        ct:fail({unexpected_error, Reason})
end.
```

### Test Isolation

#### Reset State Between Tests

```erlang
init_per_testcase(_TestCase, Config) ->
    ok = router_test_utils:reset_circuit_breaker(),
    ok = router_r10_metrics:clear_metrics(),
    Config.
```

### Common Test Patterns

#### Circuit Breaker State Tests

```erlang
test_circuit_breaker_opens(_Config) ->
    TenantId = <<"t1">>,
    ProviderId = <<"p1">>,
    
    %% Initial state should be closed
    ok = router_test_utils:wait_for_breaker_state(TenantId, ProviderId, closed, 1000),
    
    %% Trigger failures
    lists:foreach(fun(_) ->
        router_circuit_breaker:record_failure(TenantId, ProviderId)
    end, lists:seq(1, 10)),
    
    %% Wait for open state
    ok = router_test_utils:wait_for_breaker_state(TenantId, ProviderId, open, 5000),
    
    ok.
```

#### Metrics Tests

```erlang
test_metrics_increment(_Config) ->
    TenantId = <<"t1">>,
    ProviderId = <<"p1">>,
    
    %% Clear metrics
    ok = router_r10_metrics:clear_metrics(),
    
    %% Perform action
    router_circuit_breaker:record_failure(TenantId, ProviderId),
    
    %% Wait for metric
    ok = router_test_utils:wait_for_metric(
        fun() ->
            router_r10_metrics:get_metric_value(
                router_circuit_breaker_state,
                #{tenant_id => TenantId, provider_id => ProviderId}
            )
        end,
        1.0,  % open state
        2000
    ),
    
    ok.
```

## Troubleshooting

### Application Won't Start

**Symptoms**: `start_router_app/0` fails

**Solutions**:
1. Check supervisor children: `router_test_utils:dump_supervisor_children/0`
2. Verify application configuration
3. Check for port conflicts
4. Review application logs

### Process Not Found

**Symptoms**: `ensure_*_alive/0` fails

**Solutions**:
1. Verify application started: `router_test_utils:start_router_app/0`
2. Check supervisor: `router_test_utils:dump_supervisor_children/0`
3. Review supervisor configuration
4. Check for process crashes

### Metrics Not Updating

**Symptoms**: Metrics remain at 0

**Solutions**:
1. Verify metrics table exists: `router_r10_metrics:metrics_table_exists/0`
2. Dump metrics: `router_r10_metrics:dump_metrics/0`
3. Check metric names match implementation
4. Verify labels are normalized correctly

### Non-Deterministic Test Failures

**Symptoms**: Tests fail intermittently

**Solutions**:
1. Use waiters instead of immediate checks
2. Increase timeout values
3. Add delays between actions
4. Verify test isolation (reset state between tests)

## Test Utilities

### Test Utility Patterns

The `router_test_utils` module provides standardized test utility patterns for all test suites.

#### Lifecycle Functions

**Pattern**: `start_*/0`, `stop_*/0`, `ensure_*_alive/0`, `reset_*/0`

```erlang
%% Start application (idempotent)
ok = router_test_utils:start_router_app(),

%% Stop application
ok = router_test_utils:stop_router_app(),

%% Ensure component is alive (with retries)
ok = router_test_utils:ensure_circuit_breaker_alive(),

%% Reset component state (for testing)
ok = router_test_utils:reset_circuit_breaker().
```

#### Waiters

**Pattern**: `wait_for_*/N` with timeout support

```erlang
%% Wait for metric to reach expected value
ok = router_test_utils:wait_for_metric(
    fun() -> router_r10_metrics:get_metric_value(MetricName, Labels) end,
    ExpectedValue,
    TimeoutMs
).

%% Wait for circuit breaker state
ok = router_test_utils:wait_for_breaker_state(
    TenantId, ProviderId,
    ExpectedState,
    TimeoutMs
).
```

**Implementation Pattern**:
- Uses `erlang:monotonic_time(millisecond)` for timing
- Sleeps 50ms between retries
- Fails with diagnostic information on timeout

#### Helpers

**Pattern**: `get_*/N` functions return `{ok, Value} | {error, Reason}`

```erlang
%% Get component state
State = router_test_utils:get_breaker_state(TenantId, ProviderId).
```

#### Debugging

**Pattern**: `dump_*/0` functions for test debugging

```erlang
%% Dump metrics for debugging
Metrics = router_test_utils:dump_metrics(),

%% Dump supervisor children
Children = router_test_utils:dump_supervisor_children().
```

### Test Utility Template

For creating new test utility modules, use `test/router_test_utils_template.erl`:

```erlang
%% Copy template
cp test/router_test_utils_template.erl test/router_integration_test_utils.erl

%% Customize for your test category
%% - Update module name
%% - Implement lifecycle functions
%% - Add category-specific waiters
%% - Add category-specific helpers
```

### Standardized Patterns

All test utilities should follow these patterns:

1. **Lifecycle Functions**:
   - `start_*/0` - Start component (idempotent)
   - `stop_*/0` - Stop component
   - `ensure_*_alive/0` - Ensure component is alive (with retries)
   - `reset_*/0` - Reset component state (for testing)

2. **Waiters**:
   - `wait_for_*/N` - Wait for condition with timeout
   - `wait_for_*_loop/N` - Internal loop with retry logic
   - Uses `erlang:monotonic_time(millisecond)` for timing
   - Sleeps 50ms between retries

3. **Helpers**:
   - `get_*/N` - Get component state/values
   - Returns `{ok, Value} | {error, Reason}` for consistency

4. **Debugging**:
   - `dump_*/0` - Dump component state for debugging
   - Uses `ct:pal` for test output
   - Returns list of state information

### Usage Examples

#### Standard Test Suite Setup

```erlang
init_per_suite(Config) ->
    ok = router_test_utils:start_router_app(),
    ok = router_test_utils:ensure_circuit_breaker_alive(),
    Config.

end_per_suite(_Config) ->
    ok = router_test_utils:stop_router_app(),
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = router_test_utils:reset_circuit_breaker(),
    ok = router_r10_metrics:clear_metrics(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
```

#### Using Waiters

```erlang
test_deterministic_assertion(_Config) ->
    %% Use waiter instead of immediate check
    ok = router_test_utils:wait_for_metric(
        fun() -> router_r10_metrics:get_metric_value(MetricName, Labels) end,
        1,
        2000
    ),
    ok.
```

#### Using Debugging Functions

```erlang
test_with_debugging(_Config) ->
    try
        %% Test code
        perform_test_action(),
        ok
    catch
        Class:Reason ->
            %% Dump state on failure
            _ = router_test_utils:dump_metrics(),
            _ = router_test_utils:dump_supervisor_children(),
            erlang:raise(Class, Reason, erlang:get_stacktrace())
    end.
```

## Related Documentation

- `test/router_test_utils.erl` - Test utilities implementation
- `test/router_test_utils_template.erl` - Test utility template
- `OBSERVABILITY_CONVENTIONS.md` - Metrics access patterns
- `src/router_r10_metrics.erl` - Metrics access layer example
- `test/router_circuit_breaker_SUITE.erl` - Example test suite

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

