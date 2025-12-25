# Test Helpers Guide

**Last Updated**: 2025-01-27  
**Purpose**: Guide for using `test_helpers` module in Router test suites  
**Target Audience**: Developers writing Common Test suites

---

## Overview

The `test_helpers` module provides standardized bounded wait functions for all Router test suites. It replaces fixed `timer:sleep` calls with adaptive polling that waits only as long as needed.

**Location**: `apps/otp/router/test/test_helpers.erl`

---

## Key Functions

### 1. `wait_for_app_start/2`

Wait for an application process to be ready (bounded polling).

**Signature**:
```erlang
wait_for_app_start(ProcessName, MaxWaitMs) -> ok | no_return()
```

**Parameters**:
- `ProcessName` - Registered process name (atom)
- `MaxWaitMs` - Maximum wait time in milliseconds

**Example**:
```erlang
init_per_suite(Config) ->
    %% Wait for router_policy_store to start (max 1 second)
    test_helpers:wait_for_app_start(router_policy_store, 1000),
    Config.
```

**When to use**:
- In `init_per_suite/1` to wait for application processes
- Before test cases that depend on a process being ready

**Benefits**:
- Bounded wait (fails fast if process doesn't start)
- Adapts to timing variations
- No fixed sleeps

---

### 2. `wait_for_condition/2`

Wait for a condition to become true (bounded polling).

**Signature**:
```erlang
wait_for_condition(Fun, MaxWaitMs) -> ok | no_return()
```

**Parameters**:
- `Fun` - Function that returns `true` when condition is met
- `MaxWaitMs` - Maximum wait time in milliseconds

**Example**:
```erlang
test_circuit_breaker_opens(_Config) ->
    %% Wait for circuit breaker to open (max 2 seconds)
    test_helpers:wait_for_condition(
        fun() ->
            State = router_circuit_breaker:get_state(~"tenant", ~"provider"),
            State =:= open
        end,
        2000
    ),
    %% Verify state
    ?assertEqual(open, router_circuit_breaker:get_state(~"tenant", ~"provider")),
    ok.
```

**When to use**:
- Waiting for state changes (circuit breaker state, metrics, etc.)
- Waiting for conditions that may take variable time
- Replacing fixed `timer:sleep` calls

**Benefits**:
- Adapts to timing variations
- Fails fast if condition never becomes true
- No fixed sleeps

---

### 3. `wait_for_meck_call/4`

Wait for a meck function to be called (bounded polling).

**Signature**:
```erlang
wait_for_meck_call(Module, Function, Args, MaxWaitMs) -> ok | no_return()
```

**Parameters**:
- `Module` - Module name (atom)
- `Function` - Function name (atom)
- `Args` - Expected arguments (`'_'` for any, or list of arguments)
- `MaxWaitMs` - Maximum wait time in milliseconds

**Example**:
```erlang
test_publish_message(_Config) ->
    %% Mock router_nats:publish/2
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    %% Trigger publish
    router_decide_consumer:handle_message(Message),
    
    %% Wait for publish to be called (max 1 second)
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    %% Verify call
    ?assert(meck:called(router_nats, publish, '_', 2)),
    ok.
```

**When to use**:
- Verifying that mocked functions are called
- Waiting for async operations to complete
- Replacing fixed `timer:sleep` after async calls

**Benefits**:
- Verifies function was called (not just waits)
- Adapts to timing variations
- Fails fast if function is never called

---

### 4. `wait_for_process/2`

Wait for a process to exist and be alive (bounded polling).

**Signature**:
```erlang
wait_for_process(ProcessName, MaxWaitMs) -> ok | no_return()
```

**Parameters**:
- `ProcessName` - Registered process name (atom)
- `MaxWaitMs` - Maximum wait time in milliseconds

**Example**:
```erlang
test_process_restart(_Config) ->
    %% Kill process
    Pid = whereis(router_circuit_breaker),
    exit(Pid, kill),
    
    %% Wait for process to restart (max 2 seconds)
    test_helpers:wait_for_process(router_circuit_breaker, 2000),
    
    %% Verify process is alive
    NewPid = whereis(router_circuit_breaker),
    ?assert(is_pid(NewPid)),
    ?assert(is_process_alive(NewPid)),
    ok.
```

**When to use**:
- Waiting for process restarts
- Verifying process is alive after operations
- Replacing fixed `timer:sleep` after process operations

---

## Migration Guide

### Before (Fixed Sleeps)

```erlang
%% ❌ BAD: Fixed sleep
test_something(_Config) ->
    %% Fixed sleep - may be too short or too long
    timer:sleep(1000),
    
    %% Direct check - may fail intermittently
    Pid = whereis(router_circuit_breaker),
    ?assert(is_pid(Pid)),
    ok.
```

### After (Bounded Waits)

```erlang
%% ✅ GOOD: Bounded wait
test_something(_Config) ->
    %% Wait for process with bounded timeout
    test_helpers:wait_for_process(router_circuit_breaker, 2000),
    
    %% Verify process
    Pid = whereis(router_circuit_breaker),
    ?assert(is_pid(Pid)),
    ok.
```

---

## Common Patterns

### Pattern 1: Wait for Process Start

```erlang
init_per_suite(Config) ->
    application:ensure_all_started(beamline_router),
    %% Wait for process to be ready
    test_helpers:wait_for_app_start(router_policy_store, 1000),
    Config.
```

### Pattern 2: Wait for State Change

```erlang
test_state_change(_Config) ->
    %% Trigger state change
    router_circuit_breaker:record_failure(~"tenant", ~"provider"),
    
    %% Wait for state to change
    test_helpers:wait_for_condition(
        fun() ->
            State = router_circuit_breaker:get_state(~"tenant", ~"provider"),
            State =:= open
        end,
        2000
    ),
    
    %% Verify state
    ?assertEqual(open, router_circuit_breaker:get_state(~"tenant", ~"provider")),
    ok.
```

### Pattern 3: Wait for Async Operation

```erlang
test_async_operation(_Config) ->
    %% Mock async operation
    meck:new(router_nats, [passthrough]),
    meck:expect(router_nats, publish, fun(_Subject, _Payload) -> ok end),
    
    %% Trigger async operation
    router_decide_consumer:handle_message(Message),
    
    %% Wait for async operation to complete
    test_helpers:wait_for_meck_call(router_nats, publish, '_', 1000),
    
    %% Verify operation
    ?assert(meck:called(router_nats, publish, '_', 2)),
    ok.
```

---

## Best Practices

1. **Use bounded waits**: Always use `test_helpers` functions instead of fixed `timer:sleep`
2. **Set reasonable timeouts**: Use appropriate `MaxWaitMs` values (e.g., 1000ms for process start, 2000ms for state changes)
3. **Fail fast**: If condition never becomes true, test fails immediately (no infinite waits)
4. **Use in init_per_suite**: Wait for processes to be ready before tests start
5. **Combine with assertions**: Always verify the condition after waiting

---

## Troubleshooting

### Timeout Errors

If `wait_for_*` functions timeout:

1. **Check if process/condition is actually happening**: Add debug logging
2. **Increase timeout**: If condition takes longer than expected
3. **Check for errors**: Process may be crashing instead of starting
4. **Verify dependencies**: Process may depend on other processes being ready

**Example**:
```erlang
%% Add debug logging
test_helpers:wait_for_condition(
    fun() ->
        State = router_circuit_breaker:get_state(~"tenant", ~"provider"),
        ct:log("Current state: ~p", [State]),  %% Debug logging
        State =:= open
    end,
    2000
).
```

---

## See Also

- `test/test_helpers.erl` - Full implementation
- `docs/DESIGN_PATTERNS.md` - Design patterns documentation
- `docs/TESTING_RECOMMENDATIONS.md` - Testing best practices

