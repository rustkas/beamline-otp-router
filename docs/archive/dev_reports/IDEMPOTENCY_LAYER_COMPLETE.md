# Idempotency Layer Complete Implementation

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ **Complete Implementation**

## Overview

This document describes the complete implementation of the **Idempotency Layer** (`router_idem.erl`) as a standalone subsystem layer, similar to the JetStream layer. The implementation includes:

1. ✅ **Functions in `router_idem.erl`** - Complete API with `check_or_register`, `mark_completed`, `extract_idempotency_key`
2. ✅ **ETS Structure** - Explicit table schema with `trace_id`, `span_id`, `result_snapshot`, `request_hash`
3. ✅ **SUITE Tests** - Comprehensive test scenarios including races, retries, and headers/trace integration
4. ✅ **Metrics and Headers/Trace Integration** - Full observability with `router_idem.conflict` metric and trace context

## Module: `router_idem.erl`

### ETS Table Structure

**Table Name**: `router_idem`

**Type**: `ordered_set` (for efficient range scans during cleanup)

**Key Format**:
- `{KeyType, IdempotencyKey}` | `IdempotencyKey` (binary)

**Value Format**:
```erlang
#{
  status => processing | completed | failed,
  expires_at => integer(),  %% milliseconds since epoch
  trace_id => binary() | undefined,
  span_id => binary() | undefined,
  request_hash => binary() | undefined,
  result_snapshot => map() | undefined,
  error_code => binary() | undefined,
  processed_at => integer(),  %% milliseconds since epoch
  completed_at => integer() | undefined,  %% milliseconds since epoch
  additional_data => map() | undefined
}
```

### Key Functions

#### `check_or_register/3` and `check_or_register/4`

**Purpose**: Check if message is already processed, register if not (atomic operation).

**Signature**:
```erlang
-spec check_or_register(binary(), integer(), map() | undefined) -> 
  {ok, not_seen} | {ok, seen, map()} | {error, term()}.

-spec check_or_register(binary(), integer(), map() | undefined, map() | undefined) -> 
  {ok, not_seen} | {ok, seen, map()} | {error, term()}.
```

**Parameters**:
- `Key`: Idempotency key (binary)
- `TTL`: Time-to-live in milliseconds
- `AdditionalData`: Optional additional data (map or undefined)
- `TraceContext`: Optional trace context `#{trace_id => binary(), span_id => binary()}`

**Returns**:
- `{ok, not_seen}` - Key not seen before, now registered as `processing`
- `{ok, seen, ExistingValue}` - Key already exists, returns existing value map
- `{error, Reason}` - Error (e.g., `invalid_ttl`)

**Example**:
```erlang
TraceContext = #{
  trace_id => ~"tr-123",
  span_id => ~"sp-456"
},
AdditionalData = #{
  tenant_id => ~"acme",
  provider_id => ~"openai:gpt-4o"
},
case router_idem:check_or_register(IdempotencyKey, 3600000, AdditionalData, TraceContext) of
  {ok, not_seen} ->
    %% Process message
    process_message();
  {ok, seen, ExistingValue} ->
    %% Return cached result or skip processing
    skip_processing(ExistingValue)
end.
```

#### `mark_completed/3` and `mark_completed/4`

**Purpose**: Mark operation as completed and save result snapshot.

**Signature**:
```erlang
-spec mark_completed(binary(), completed | failed, map() | undefined) -> 
  ok | {error, term()}.

-spec mark_completed(binary(), completed | failed, map() | undefined, binary() | undefined) -> 
  ok | {error, term()}.
```

**Parameters**:
- `Key`: Idempotency key (binary)
- `Status`: `completed` | `failed`
- `ResultSnapshot`: Optional result data to store (map or undefined)
- `ErrorCode`: Optional error code if status is `failed` (binary or undefined)

**Returns**:
- `ok` - Successfully marked as completed
- `{error, key_not_found}` - Key doesn't exist
- `{error, invalid_status}` - Invalid status value

**Example**:
```erlang
%% Mark as completed with result snapshot
ResultSnapshot = #{
  status => ~"success",
  latency_ms => 850,
  cost => 0.012
},
ok = router_idem:mark_completed(IdempotencyKey, completed, ResultSnapshot).

%% Mark as failed with error code
ok = router_idem:mark_completed(IdempotencyKey, failed, undefined, ~"timeout").
```

#### `extract_idempotency_key/2`

**Purpose**: Extract idempotency key from headers or payload (headers have priority).

**Signature**:
```erlang
-spec extract_idempotency_key(map(), map()) -> {ok, binary()} | {error, not_found}.
```

**Parameters**:
- `Headers`: Map of headers (from NATS or HTTP)
- `Payload`: Map of payload fields

**Returns**:
- `{ok, IdempotencyKey}` - Key found (from headers or payload)
- `{error, not_found}` - Key not found in headers or payload

**Header Priority**:
1. `idempotency_key` (lowercase)
2. `idempotency-key` (kebab-case)
3. `Idempotency-Key` (Pascal-Case)
4. Payload fallback: `idempotency_key`

**Example**:
```erlang
Headers = #{
  ~"idempotency_key" => ~"header-key-123"
},
Payload = #{
  ~"idempotency_key" => ~"payload-key-456"
},
{ok, ~"header-key-123"} = router_idem:extract_idempotency_key(Headers, Payload).
```

### Metrics

**Exported Metrics**:
- `router_idem_hits_total` - Duplicate detected (key already processed)
- `router_idem_miss_total` - New key (not seen before)
- `router_idem_conflict_total` - Race condition detected (two processes processing same key concurrently)

**Telemetry Events**:
- `[router, idempotency, hit]` - Duplicate detected
- `[router, idempotency, miss]` - New key
- `[router, idempotency, conflict]` - Race condition
- `[router, idempotency, remember]` - Key registered
- `[router, idempotency, expired]` - Key expired
- `[router, idempotency, completed]` - Operation completed
- `[router, idempotency, cleanup]` - Cleanup performed

**Telemetry Metadata**:
All events include:
- `key` - Idempotency key
- `trace_id` - Trace identifier (when available)
- `span_id` - Span identifier (when available)
- `status` - Processing status (when available)

## Test Suite: `router_idem_SUITE.erl`

### Test Scenarios

1. **Basic Operations**:
   - `test_basic_dup_check` - Basic duplicate detection
   - `test_remember_and_evict` - Remember and evict operations
   - `test_check_and_mark_atomic` - Atomic check-and-mark

2. **TTL and Expiration**:
   - `test_ttl_expiration` - TTL expiration behavior
   - `test_ttl_edge_case_zero` - Zero TTL rejection
   - `test_ttl_edge_case_negative` - Negative TTL rejection
   - `test_ttl_edge_case_very_large` - Very large TTL handling

3. **Cleanup**:
   - `test_evict_expired_mass_cleanup` - Mass cleanup of expired entries
   - `test_evict_all_emergency` - Emergency cleanup (evict all)
   - `test_cleanup_protects_ets_growth` - Cleanup prevents unbounded growth

4. **Concurrency**:
   - `test_concurrent_operations` - Concurrent operations
   - `test_zero_double_execution` - Prevents double execution under concurrency
   - `test_race_condition_conflict` - Race condition conflict detection

5. **Errors and Retries**:
   - `test_error_and_retry` - Error handling and retry scenarios

6. **Headers and Trace Integration**:
   - `test_check_or_register_with_trace` - Trace context storage
   - `test_extract_idempotency_key_from_headers` - Header extraction
   - `test_headers_trace_integration` - Full headers/trace integration

7. **Performance**:
   - `test_p95_duplicate_handling_latency` - p95 latency measurement

## Integration with Headers and Trace

### Headers Extraction

The layer integrates with NATS/HTTP headers to extract:
- `idempotency_key` - From headers (priority) or payload (fallback)
- `trace_id` - For distributed tracing
- `span_id` - For span correlation

### Trace Context Propagation

When processing messages:
1. Extract `idempotency_key` from headers or payload
2. Extract `trace_id` and `span_id` from headers
3. Store trace context in ETS along with idempotency key
4. Emit telemetry events with trace context
5. Link idempotency events to OpenTelemetry spans

**Example Flow**:
```erlang
%% Extract idempotency key from headers
{ok, IdempotencyKey} = router_idem:extract_idempotency_key(Headers, Payload),

%% Extract trace context from headers
TraceContext = #{
  trace_id => maps:get(~"trace_id", Headers),
  span_id => maps:get(~"span_id", Headers)
},

%% Register with trace context
case router_idem:check_or_register(IdempotencyKey, TTL, AdditionalData, TraceContext) of
  {ok, not_seen} ->
    %% Process message
    Result = process_message(),
    %% Mark as completed with result
    router_idem:mark_completed(IdempotencyKey, completed, Result);
  {ok, seen, ExistingValue} ->
    %% Return cached result
    maps:get(result_snapshot, ExistingValue)
end.
```

## Configuration

**Application Environment Variables**:
- `idempotency_ttl_seconds` - TTL in seconds (default: 3600 = 1 hour)
- `idempotency_max_size` - Maximum table size before warning (default: 1000000)

**Cleanup Interval**:
- Automatic cleanup every 10% of TTL or max 60 seconds (whichever is smaller)

## Usage in Consumers

### Example: Result Consumer

```erlang
%% Extract idempotency key from headers
{ok, IdempotencyKey} = router_idem:extract_idempotency_key(Headers, Payload),

%% Extract trace context
TraceContext = #{
  trace_id => maps:get(~"trace_id", Headers, undefined),
  span_id => maps:get(~"span_id", Headers, undefined)
},

%% Check or register
case router_idem:check_or_register(IdempotencyKey, 3600000, AdditionalData, TraceContext) of
  {ok, not_seen} ->
    %% Process result
    process_result(),
    %% Mark as completed
    router_idem:mark_completed(IdempotencyKey, completed, ResultSnapshot);
  {ok, seen, ExistingValue} ->
    %% Skip processing (already processed)
    skip_processing(ExistingValue)
end.
```

## Comparison with JetStream Layer

The Idempotency Layer is designed as a **standalone subsystem layer**, similar to the JetStream layer:

| Aspect | JetStream Layer | Idempotency Layer |
|--------|----------------|-------------------|
| **Module** | `router_jetstream.erl` | `router_idem.erl` |
| **Storage** | ETS table `router_jetstream_state` | ETS table `router_idem` |
| **Structure** | Explicit schema documented | Explicit schema documented |
| **Tests** | `router_jetstream_SUITE.erl` | `router_idem_SUITE.erl` |
| **Metrics** | JetStream-specific metrics | Idempotency-specific metrics |
| **Observability** | Headers/trace integration | Headers/trace integration |

## Status

✅ **Complete Implementation**:
- ✅ Functions in `router_idem.erl` with full API
- ✅ Explicit ETS structure with documented schema
- ✅ Comprehensive SUITE tests (races, retries, headers/trace)
- ✅ Metrics and observability integration
- ✅ Headers/trace context propagation

## Next Steps

1. **Integration**: Replace `router_idempotency.erl` usage with `router_idem.erl` in consumers
2. **Documentation**: Update consumer documentation with new API
3. **Monitoring**: Set up dashboards for idempotency metrics
4. **Performance**: Monitor p95 latency and optimize if needed

