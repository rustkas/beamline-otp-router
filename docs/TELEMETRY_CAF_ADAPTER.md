# Telemetry Events: CAF Adapter

## Overview

This document describes telemetry events emitted by `router_caf_adapter` module for monitoring ExecAssignment publishing to CAF.

## Events

### `[router_caf_adapter, publish_assignment]` (Span)

**Type**: Span (start/stop)

**Purpose**: Tracks the duration of ExecAssignment publishing operation

**Start Metadata**:
- `assignment_id`: Assignment ID (binary, UUID v4)
- `request_id`: Request ID from DecideRequest (binary | undefined)
- `tenant_id`: Tenant ID (binary | undefined)
- `subject`: NATS subject for publishing (binary)
- `expected_latency_ms`: Expected latency from route decision (integer)
- `deadline_ms`: Calculated deadline (integer)

**Stop Metadata**:
- Same as start metadata (assignment_id, request_id, tenant_id, subject, expected_latency_ms, deadline_ms)
- `duration_us`: Duration in microseconds (automatically added by telemetry)
- `result`: `ok` or `error` (atom)
- `retries`: Number of retry attempts (non_neg_integer, 0 if no retries)
- `error_kind`: Error classification (atom, only if result = error)
  - NATS errors: `timeout`, `connection_failed`, `nats_unavailable`, `invalid_format`, `unknown_error`
  - Exceptions: `bad_argument`, `bad_match`, `function_clause`, `throw_exception`, `exit_exception`, `unknown_exception`
- `error`: Error details (term, only if result = error)

**Example**:
```erlang
telemetry:span(
    [router_caf_adapter, publish_assignment],
    #{
        assignment_id => <<"a1234567890-12345">>,
        request_id => <<"req-123">>,
        tenant_id => <<"acme">>,
        subject => <<"caf.exec.assign.v1">>
    },
    fun() -> ... end
)
```

### `[router_caf_adapter, assignments_published_total]` (Counter)

**Type**: Counter

**Purpose**: Increments when ExecAssignment is successfully published to NATS

**Measurement**:
- `count`: Always `1`

**Metadata**:
- `assignment_id`: Assignment ID (binary, UUID v4)
- `request_id`: Request ID (binary | undefined)
- `tenant_id`: Tenant ID (binary | undefined)
- `subject`: NATS subject (binary)
- `retries`: Number of retry attempts before success (non_neg_integer, 0 if first attempt succeeded)

**Example**:
```erlang
telemetry:execute(
    [router_caf_adapter, assignments_published_total],
    #{count => 1},
    #{
        assignment_id => <<"a1234567890-12345">>,
        request_id => <<"req-123">>,
        tenant_id => <<"acme">>,
        subject => <<"caf.exec.assign.v1">>
    }
)
```

### `[router_caf_adapter, assignments_retry_total]` (Counter)

**Type**: Counter

**Purpose**: Increments on each retry attempt for NATS publish failures

**Measurement**:
- `count`: Always `1`

**Metadata**:
- `assignment_id`: Assignment ID (binary, UUID v4)
- `request_id`: Request ID (binary | undefined)
- `tenant_id`: Tenant ID (binary | undefined)
- `subject`: NATS subject (binary)
- `retries`: Number of retry attempts so far (non_neg_integer)

**Example**:
```erlang
telemetry:execute(
    [router_caf_adapter, assignments_retry_total],
    #{count => 1},
    #{
        assignment_id => <<"a1234567890-12345">>,
        request_id => <<"req-123">>,
        tenant_id => <<"acme">>,
        subject => <<"caf.exec.assign.v1">>,
        retries => 1
    }
)
```

### `[router_caf_adapter, assignments_blocked_total]` (Counter)

**Type**: Counter

**Purpose**: Increments when assignment is blocked due to tenant not in allowlist

**Measurement**:
- `count`: Always `1`

**Metadata**:
- `tenant_id`: Tenant ID (binary)
- `reason`: `tenant_not_allowed` (atom)

**Example**:
```erlang
telemetry:execute(
    [router_caf_adapter, assignments_blocked_total],
    #{count => 1},
    #{
        tenant_id => <<"blocked-tenant">>,
        reason => tenant_not_allowed
    }
)
```

### `[router_caf_adapter, assignments_skipped_total]` (Counter)

**Type**: Counter

**Purpose**: Increments when assignment is skipped due to global disable flag

**Measurement**:
- `count`: Always `1`

**Metadata**:
- `reason`: `global_disabled` (atom)

**Example**:
```erlang
telemetry:execute(
    [router_caf_adapter, assignments_skipped_total],
    #{count => 1},
    #{
        reason => global_disabled
    }
)
```

### `[router_caf_adapter, assignments_failed_total]` (Counter)

**Type**: Counter

**Purpose**: Increments when ExecAssignment publishing fails (after retries exhausted or exception)

**Measurement**:
- `count`: Always `1`

**Metadata**:
- `assignment_id`: Assignment ID (binary, UUID v4)
- `request_id`: Request ID (binary | undefined)
- `tenant_id`: Tenant ID (binary | undefined)
- `subject`: NATS subject (binary)
- `error_kind`: Error classification (atom)
  - NATS errors: `timeout`, `connection_failed`, `nats_unavailable`, `invalid_format`, `unknown_error`, `max_retries_exceeded`
  - Exceptions: `bad_argument`, `bad_match`, `function_clause`, `throw_exception`, `exit_exception`, `unknown_exception`
- `error`: Error details (atom | tuple | binary)
- `retries`: Number of retry attempts before failure (non_neg_integer)

**Additional metadata for exceptions**:
- `reason`: Exception reason
- `stack`: Stack trace

**Example (NATS publish error)**:
```erlang
telemetry:execute(
    [router_caf_adapter, assignments_failed_total],
    #{count => 1},
    #{
        assignment_id => <<"a1234567890-12345">>,
        request_id => <<"req-123">>,
        tenant_id => <<"acme">>,
        subject => <<"caf.exec.assign.v1">>,
        error => nats_connection_failed
    }
)
```

**Example (Exception)**:
```erlang
telemetry:execute(
    [router_caf_adapter, assignments_failed_total],
    #{count => 1},
    #{
        assignment_id => <<"a1234567890-12345">>,
        request_id => <<"req-123">>,
        tenant_id => <<"acme">>,
        subject => <<"caf.exec.assign.v1">>,
        error => error,
        reason => badarg,
        stack => [...]
    }
)
```

## Usage

### Attaching Handlers

```erlang
%% Attach handler for span events
telemetry:attach_many(
    "router-caf-adapter-span-handler",
    [
        [router_caf_adapter, publish_assignment, start],
        [router_caf_adapter, publish_assignment, stop]
    ],
    fun(Event, Measurements, Metadata, Config) ->
        %% Handle span start/stop
        io:format("Event: ~p, Measurements: ~p, Metadata: ~p~n", 
                  [Event, Measurements, Metadata])
    end,
    #{}
).

%% Attach handler for counter events
telemetry:attach_many(
    "router-caf-adapter-counter-handler",
    [
        [router_caf_adapter, assignments_published_total],
        [router_caf_adapter, assignments_failed_total],
        [router_caf_adapter, assignments_retry_total],
        [router_caf_adapter, assignments_blocked_total],
        [router_caf_adapter, assignments_skipped_total]
    ],
    fun(Event, Measurements, Metadata, Config) ->
        %% Handle counter increment
        io:format("Counter: ~p, Count: ~p, Metadata: ~p~n",
                  [Event, Measurements, Metadata])
    end,
    #{}
).
```

### Metrics Aggregation

**Prometheus (future)**:
- `router_caf_adapter_assignments_published_total` - Counter
- `router_caf_adapter_assignments_failed_total` - Counter
- `router_caf_adapter_assignments_retry_total` - Counter
- `router_caf_adapter_assignments_blocked_total` - Counter
- `router_caf_adapter_assignments_skipped_total` - Counter
- `router_caf_adapter_publish_assignment_duration_seconds` - Histogram

**Metric Names (Fixed)**:
- Event: `[router_caf_adapter, assignments_published_total]` → Metric: `router_caf_adapter_assignments_published_total`
- Event: `[router_caf_adapter, assignments_failed_total]` → Metric: `router_caf_adapter_assignments_failed_total`
- Event: `[router_caf_adapter, publish_assignment]` → Metric: `router_caf_adapter_publish_assignment_duration_seconds`

**Attribute Schema (Fixed)**:
- `assignment_id`: binary (required) - Unique assignment identifier (UUID v4)
- `request_id`: binary | undefined (optional) - Request correlation ID
- `tenant_id`: binary | undefined (optional) - Tenant identifier
- `subject`: binary (required) - NATS subject for publishing
- `expected_latency_ms`: integer (required in span start) - Expected latency from decision
- `deadline_ms`: integer (required in span start/stop) - Calculated deadline
- `retries`: non_neg_integer (optional) - Number of retry attempts
- `error_kind`: atom (only for failed assignments) - Error classification
- `error`: atom | tuple | binary (only for failed assignments) - Error details
- `reason`: term (only for exceptions) - Exception reason
- `stack`: list (only for exceptions) - Stack trace
- `result`: atom (only in span stop) - `ok` or `error`

**Labels**:
- `tenant_id` (if present)
- `subject` (NATS subject)
- `error` (for failed assignments)

**Sampling/Thresholds (Future)**:
- Consider sampling for high-volume scenarios (e.g., `telemetry_sampling_rate: 1.0`)
- Add thresholds for alerting (e.g., failure rate > 5% → `telemetry_failure_threshold: 0.05`)

## Error Handling

### NATS Publish Errors

When `router_nats:publish/2` returns an error:
1. `assignments_failed_total` counter is incremented
2. Error is logged via `router_logger:error/2`
3. Function returns `error` atom

### Exceptions

When an exception occurs during ExecAssignment building or publishing:
1. Exception is caught
2. `assignments_failed_total` counter is incremented with exception details
3. Error is logged via `router_logger:error/2`
4. Function returns `error` atom

## References

- `src/router_caf_adapter.erl` - Implementation
- `docs/TELEMETRY_EVENTS.md` - General telemetry documentation
- `docs/API_CONTRACTS.md` - ExecAssignment format

