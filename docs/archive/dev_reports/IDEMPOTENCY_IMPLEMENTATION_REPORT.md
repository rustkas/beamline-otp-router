# Idempotency Implementation Report

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Implementation Complete**

## Overview

This report documents the complete implementation of ETS-based idempotency cache with support for `assignment_id`, `request_id`, `ack_id`, and `usage_id` keys, TTL, atomic checks, and hit/miss metrics.

## Implementation

### 1. ETS-Based Idempotency Cache

**Location**: `src/router_idempotency.erl`

**Features**:
- ✅ ETS table with `ordered_set` type for efficient lookups
- ✅ TTL-based expiration (configurable, default: 1 hour)
- ✅ Atomic check-and-mark operations via `gen_server:call`
- ✅ Automatic cleanup of expired entries
- ✅ Support for all key types: `assignment_id`, `request_id`, `ack_id`, `usage_id`

**Table Configuration**:
```erlang
Table = ets:new(router_idempotency, [
    ordered_set,
    named_table,
    public,  %% Allow other processes to read
    {write_concurrency, true},
    {read_concurrency, true}
])
```

**Key Format**:
- Key: `{KeyType, MessageId}`
- Value: `#{message_id, key_type, processed_at, expires_at, additional_data}`

### 2. Key Types Support

**Supported Key Types**:
1. ✅ `assignment_id`: Used in `router_result_consumer.erl` (line 322)
2. ✅ `request_id`: Used as fallback when `assignment_id` is missing
3. ✅ `ack_id`: Used in `router_ack_consumer.erl` (line 192)
4. ✅ `usage_id`: Used in `router_result_consumer.erl` for usage emission (line 181)

**Usage Examples**:
```erlang
%% Result processing by assignment_id
router_idempotency:check_and_mark(~"assignment_id", AssignmentId, #{
    status => Status,
    job_type => JobType,
    provider_id => ProviderId
})

%% ACK processing by ack_id
router_idempotency:check_and_mark(~"ack_id", AssignmentId, #{
    status => Status,
    message => Message
})

%% Usage emission by usage_id
router_idempotency:check_and_mark(~"usage_id", UsageId, #{
    tenant_id => ValidatedTenantId,
    provider_id => ProviderId,
    status => Status
})
```

### 3. Atomic Check-and-Mark Operations

**Implementation**:
- All operations go through `gen_server:call` for atomicity
- ETS `lookup` + `insert` operations are atomic within single `handle_call`
- No race conditions between concurrent requests

**Flow**:
1. Lookup key in ETS table
2. If not found → insert and return `{ok, not_seen}`
3. If found → check expiration:
   - If valid → return `{ok, seen}`
   - If expired → update and return `{ok, not_seen}`

### 4. TTL Configuration

**Configuration**:
- Parameter: `idempotency_ttl_seconds` (default: 3600 seconds = 1 hour)
- Location: `beamline_router.app.src`

**Cleanup**:
- Automatic cleanup timer runs every `min(TTL / 10, 60)` seconds
- Expired entries are removed from ETS table
- Cleanup logs debug messages when entries are removed

### 5. Hit/Miss Metrics

**Metrics Implemented**:
- ✅ `router_idempotency_hit_total{key_type, message_id}`: Emitted when message is already seen (cache hit)
- ✅ `router_idempotency_miss_total{key_type, message_id, reason}`: Emitted when message is not seen or expired (cache miss)

**Emission Points**:
- **Hit**: When `check_and_mark` finds valid (non-expired) entry
- **Miss**: When `check_and_mark` finds no entry or expired entry

**Telemetry Format**:
```erlang
telemetry:execute([router_idempotency, router_idempotency_hit_total], #{
    count => 1
}, #{
    key_type => KeyType,
    message_id => MessageId
})
```

### 6. Idempotent Usage Emission

**Location**: `src/router_result_consumer.erl`, lines 179-237

**Implementation**:
```erlang
%% Check idempotency for usage emission
UsageId = <<"usage-", (case AssignmentId of undefined -> RequestId; _ -> AssignmentId end)/binary>>,
case router_idempotency:check_and_mark(~"usage_id", UsageId, #{
    tenant_id => ValidatedTenantId,
    provider_id => ProviderId,
    status => Status
}) of
    {ok, seen} ->
        %% Usage already emitted - skip
        router_logger:info(~"Usage event already emitted (idempotency)", ...);
    {ok, not_seen} ->
        %% Not seen before - emit usage event
        emit_usage_event(...)
end
```

**Protection**:
- ✅ Prevents duplicate usage event emission
- ✅ Uses `usage_id` key based on `assignment_id` or `request_id`
- ✅ Stores additional data (tenant_id, provider_id, status) for audit

## Test Coverage

### Test Suite: `router_idempotency_SUITE.erl`

**Test Cases**:
1. ✅ `test_result_idempotency_by_assignment_id/1`: Verifies idempotency by `assignment_id`
2. ✅ `test_result_idempotency_by_request_id/1`: Verifies idempotency by `request_id`
3. ✅ `test_usage_event_idempotency/1`: Verifies usage emission idempotency
4. ✅ `test_ack_idempotency/1`: Verifies ACK processing idempotency
5. ✅ `test_assignment_publication_idempotency/1`: Verifies assignment publication idempotency
6. ✅ `test_concurrent_result_processing/1`: Verifies idempotency under concurrency
7. ✅ `test_concurrent_usage_emission/1`: Verifies usage emission idempotency under concurrency

**Concurrency Tests**:
- Tests spawn multiple processes to simulate race conditions
- Verifies that only one usage event is emitted even under concurrent processing
- Uses ETS tables to track publication counts

## Integration Points

### Result Processing

**Location**: `src/router_result_consumer.erl`

**Flow**:
1. Receive ExecResult message
2. Extract `assignment_id` or `request_id`
3. Check idempotency: `check_and_mark(~"assignment_id", IdempotencyKey, ...)`
4. If `{ok, seen}` → skip processing, acknowledge message
5. If `{ok, not_seen}` → process result, emit usage event

### ACK Processing

**Location**: `src/router_ack_consumer.erl`

**Flow**:
1. Receive ExecAssignmentAck message
2. Extract `assignment_id`
3. Check idempotency: `check_and_mark(~"ack_id", AssignmentId, ...)`
4. If `{ok, seen}` → skip processing, acknowledge message
5. If `{ok, not_seen}` → process ACK

### Usage Emission

**Location**: `src/router_result_consumer.erl`

**Flow**:
1. After processing result, generate `usage_id`
2. Check idempotency: `check_and_mark(~"usage_id", UsageId, ...)`
3. If `{ok, seen}` → skip emission
4. If `{ok, not_seen}` → emit usage event

## Metrics Summary

| Metric | When Emitted | Labels |
|--------|--------------|--------|
| `router_idempotency_hit_total` | Message already seen (cache hit) | `key_type`, `message_id` |
| `router_idempotency_miss_total` | Message not seen or expired (cache miss) | `key_type`, `message_id`, `reason` (optional) |

## Configuration

**Application Environment**:
```erlang
{idempotency_ttl_seconds, 3600}  %% Default: 1 hour
```

**Supervisor Integration**:
- `router_idempotency` is started as child worker in `beamline_router_sup.erl`
- Started before consumers to ensure availability

## Performance Considerations

**ETS Table**:
- `ordered_set` type for efficient range queries and lookups
- `write_concurrency` and `read_concurrency` enabled for concurrent access
- Public table allows read-only access from other processes

**Atomicity**:
- All operations go through `gen_server:call` for serialization
- No race conditions between concurrent `check_and_mark` calls
- ETS operations are atomic within single `handle_call`

**Memory**:
- TTL-based expiration prevents unbounded growth
- Automatic cleanup removes expired entries
- Cleanup interval: `min(TTL / 10, 60)` seconds

## Known Limitations

1. **Assignment Publication**: Currently doesn't enforce idempotency (test documents expected behavior)
2. **TTL Precision**: Cleanup runs periodically, not immediately on expiration
3. **Memory Growth**: Under high load, table may grow before cleanup runs

## References

- `src/router_idempotency.erl`: Core idempotency implementation
- `src/router_result_consumer.erl`: Result processing with idempotency
- `src/router_ack_consumer.erl`: ACK processing with idempotency
- `test/router_idempotency_SUITE.erl`: Test suite
- `docs/PROMETHEUS_ALERTS.md`: Metrics documentation

