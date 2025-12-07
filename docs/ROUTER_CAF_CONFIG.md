# Router ↔ CAF Configuration

## Overview

This document describes configuration options for Router ↔ CAF integration.

## Application Environment Variables

### NATS Configuration

```erlang
{beamline_router, [
    {nats_url, "nats://localhost:4222"},
    {nats_mode, real},  %% or mock
    {nats_subject, <<"beamline.router.v1.decide">>},
    {nats_timeout_ms, 5000}
]}
```

### CAF Assignment Configuration

**Current Implementation** (CP1):
- `assignment_subject`: Configurable per request via `DecideRequest.assignment_subject` (default: `caf.exec.assign.v1`)
- `deadline_ms`: Calculated from `expected_latency_ms` (5x, minimum 5000ms)
- Retry: Default values (`max_attempts: 2`, `backoff_ms: 200`)

**Future Configuration** (Post-CP1):
```erlang
{beamline_router, [
    {caf_assignment_subject, <<"caf.exec.assign.v1">>},
    {caf_deadline_multiplier, 5},  %% deadline = expected_latency_ms * multiplier
    {caf_deadline_min_ms, 5000},
    {caf_retry_max_attempts, 2},
    {caf_retry_backoff_ms, 200},
    {caf_max_message_size, 1048576}  %% 1MB default
]}
```

## Configuration Parameters

### assignment_subject

**Type**: `binary()`

**Default**: `<<"caf.exec.assign.v1">>`

**Description**: NATS subject for ExecAssignment publication

**Override**: Can be overridden per request via `DecideRequest.assignment_subject`

**Example**:
```erlang
%% In DecideRequest
#{
    <<"push_assignment">> => true,
    <<"assignment_subject">> => <<"caf.exec.assign.v1.custom.queue">>
}
```

### deadline_ms Calculation

**Formula**: `max(caf_deadline_min_ms, expected_latency_ms * caf_deadline_multiplier)`

**Default Values**:
- `caf_deadline_multiplier`: `5`
- `caf_deadline_min_ms`: `5000`

**Override**: Can be overridden per request via `DecideRequest.options.deadline_ms`

**Example**:
```erlang
%% In DecideRequest
#{
    <<"options">> => #{
        <<"deadline_ms">> => 10000  %% Override calculated deadline
    }
}
```

### Retry Configuration

**Default Values**:
- `max_attempts`: `2`
- `backoff_ms`: `200`

**Override**: Can be overridden per request via `DecideRequest.options.retry`

**Example**:
```erlang
%% In DecideRequest
#{
    <<"options">> => #{
        <<"retry">> => #{
            <<"max_attempts">> => 3,
            <<"backoff_ms">> => 500
        }
    }
}
```

## Configuration Priority

1. **Request-level** (highest priority):
   - `DecideRequest.assignment_subject`
   - `DecideRequest.options.deadline_ms`
   - `DecideRequest.options.retry`

2. **Application environment** (future):
   - `caf_assignment_subject`
   - `caf_deadline_multiplier`, `caf_deadline_min_ms`
   - `caf_retry_max_attempts`, `caf_retry_backoff_ms`

3. **Default values** (lowest priority):
   - `caf.exec.assign.v1`
   - `deadline_ms = max(5000, expected_latency_ms * 5)`
   - `retry = {max_attempts: 2, backoff_ms: 200}`

## Security Configuration

### NATS Authentication

**mTLS** (recommended for production):
```erlang
{beamline_router, [
    {nats_tls_enabled, true},
    {nats_tls_cert_file, "/path/to/cert.pem"},
    {nats_tls_key_file, "/path/to/key.pem"},
    {nats_tls_ca_file, "/path/to/ca.pem"}
]}
```

**JWT** (alternative):
```erlang
{beamline_router, [
    {nats_jwt_enabled, true},
    {nats_jwt_token, "your-jwt-token"}
]}
```

### Subject Whitelist

**Future**: Add subject whitelist validation:
```erlang
{beamline_router, [
    {caf_allowed_subjects, [
        <<"caf.exec.assign.v1">>,
        <<"caf.exec.assign.v1.custom.queue">>
    ]}
]}
```

## Monitoring Configuration

### Telemetry

**Event Names** (fixed):
- `[router_caf_adapter, publish_assignment]` - Span
- `[router_caf_adapter, assignments_published_total]` - Counter
- `[router_caf_adapter, assignments_failed_total]` - Counter

**Sampling** (future):
```erlang
{beamline_router, [
    {telemetry_sampling_rate, 1.0},  %% 100% sampling
    {telemetry_failure_threshold, 0.05}  %% Alert if failure rate > 5%
]}
```

## References

- `docs/API_CONTRACTS.md` - Message format specifications
- `docs/NATS_SUBJECTS.md` - NATS subject specifications
- `docs/TELEMETRY_CAF_ADAPTER.md` - Telemetry events
- `src/router_caf_adapter.erl` - Implementation

