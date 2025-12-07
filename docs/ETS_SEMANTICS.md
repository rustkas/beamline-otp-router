# ETS Semantics and Concurrency Guarantees

## Overview

`router_policy_store` uses ETS (Erlang Term Storage) for in-memory policy caching. This document describes the atomicity guarantees, concurrency semantics, and expected behavior under high load.

## ETS Table Configuration

**Main Table Type**: `set` (default)
- **Key**: `{tenant_id, policy_id}` (composite key)
- **Value**: `#policy{}` record
- **Access**: `protected` (only owner process can write, all processes can read)
- **Concurrency**: Safe for concurrent reads, writes serialized through gen_server

**Index Table Type**: `bag`
- **Key**: `tenant_id`
- **Value**: `policy_id` (stored as `{tenant_id, policy_id}` pairs)
- **Purpose**: Secondary index for O(k) tenant lookups instead of O(n) linear scan
- **Uniqueness**: Bag allows multiple entries per key, but each `{tenant_id, policy_id}` pair is unique
- **Deletion**: Uses `ets:delete_object/2` to remove specific `{tenant_id, policy_id}` pairs (not `ets:delete/2`)

## Ordering Guarantees

### ETS Set Tables and Order

**CRITICAL**: ETS tables of type `set` **do not guarantee order** of elements.

- `ets:match_object/2` may return results in any order
- Order can vary between calls even with the same data
- Order is **not deterministic** and should not be relied upon

### Deterministic Ordering in list_policies

To ensure deterministic ordering for clients, `list_policies` explicitly sorts results using binary comparison:

```erlang
%% Sort by policy_id for deterministic ordering (lexicographic binary comparison)
SortByPolicyId = fun(P1, P2) ->
    binary:compare(P1#policy.policy_id, P2#policy.policy_id) =:= less
end,
SortedResult = lists:sort(SortByPolicyId, Result).
```

**Guarantees:**
- Results are sorted by `policy_id` using `binary:compare/2` (lexicographic binary order)
- Order is **deterministic** and stable between calls
- Clients can rely on sorted order for predictable behavior
- Binary comparison ensures consistent ordering across different Erlang versions and platforms

## Atomic Operations

### Upsert Policy (`upsert_policy/2`)

**Operation**: Single atomic `ets:insert/2` call

**Semantics**:
- **Atomic**: The entire insert operation is atomic - either the policy is fully inserted or not at all
- **Idempotent**: Multiple calls with the same `{tenant_id, policy_id}` result in the same final state
- **Last Write Wins**: If two concurrent upserts target the same policy, the last write (by wall-clock time) wins
- **No Partial Updates**: Policy is validated before insertion; invalid policies are rejected atomically

**Implementation**:
```erlang
do_upsert_policy(TenantId, PolicyId, Policy, State) ->
    %% Validation happens before ETS operation
    case validate_policy(Policy) of
        ok ->
            %% Single atomic insert - no race conditions possible
            ets:insert(State#state.table, {{TenantId, PolicyId}, Policy}),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, invalid_policy, Reason}, State}
    end.
```

**Concurrency Behavior**:
- **Same Policy, Concurrent Updates**: Last write wins (expected behavior for ETS `set` tables)
- **Different Policies, Concurrent Updates**: All succeed independently
- **Read-Modify-Write**: Not applicable - direct insert replaces existing value atomically

### Delete Policy (`delete_policy/2`)

**Operation**: Single atomic `ets:delete/2` call

**Semantics**:
- **Atomic**: The entire delete operation is atomic - either the policy is fully deleted or not at all
- **Idempotent**: Deleting a non-existent policy returns `ok` (no error)
- **No Race Conditions**: Delete operation is atomic - no partial deletes possible

**Implementation**:
```erlang
do_delete_policy(TenantId, PolicyId, State) ->
    %% Single atomic delete
    ets:delete(State#state.table, {TenantId, PolicyId}),
    {reply, ok, State}.
```

**Concurrency Behavior**:
- **Concurrent Delete + Upsert**: 
  - If delete happens before upsert: policy is deleted, then upsert creates new policy
  - If upsert happens before delete: policy is updated, then deleted
  - Final state: policy does not exist (delete wins in both cases if it happens last)
- **Concurrent Deletes**: Both succeed (idempotent)

### Get Policy (`get_policy/2`)

**Operation**: Single atomic `ets:lookup/2` call

**Semantics**:
- **Atomic Read**: Returns consistent snapshot of policy at the moment of lookup
- **No Stale Reads**: ETS guarantees that reads see the most recent committed write
- **Consistent View**: All fields of the policy are read atomically (no partial reads)

**Concurrency Behavior**:
- **Read During Upsert**: Returns either old or new policy (never partial/corrupted)
- **Read During Delete**: Returns either existing policy or `not_found` (never partial state)

### List Policies (`list_policies/1`)

**Operation**: `ets:match_object/2` with pattern matching, followed by explicit sorting

**Semantics**:
- **Snapshot Consistency**: Returns all policies matching `{TenantId, _}` at the moment of call
- **No Partial Lists**: Either all matching policies are returned or none (atomic operation)
- **Consistent View**: All policies in the list are from the same point in time
- **Deterministic Order**: Results are sorted by `policy_id` (lexicographic order) for stable ordering

**Ordering**:
- ETS `set` tables do not guarantee order
- Results are explicitly sorted by `policy_id` to ensure deterministic ordering
- Order is stable between calls (same policies return in same order)

**Concurrency Behavior**:
- **List During Upsert**: May or may not include the newly inserted policy (depends on timing)
- **List During Delete**: May or may not include the deleted policy (depends on timing)

## Concurrency Guarantees

### Serialization Through gen_server

**Critical**: All write operations (`upsert_policy`, `delete_policy`) are serialized through the gen_server process.

**Implications**:
- **No Concurrent Writes**: Only one write operation can execute at a time
- **Sequential Processing**: Operations are processed in the order they arrive at the gen_server
- **No Lock Contention**: No need for explicit locking - gen_server mailbox provides natural serialization

**Example**:
```erlang
%% Process A: upsert_policy(tenant1, policy1, ...)
%% Process B: upsert_policy(tenant1, policy1, ...)  (same policy)
%% Process C: delete_policy(tenant1, policy1)

%% Execution order (determined by gen_server mailbox):
%% 1. Process A's upsert completes
%% 2. Process B's upsert completes (overwrites A's policy)
%% 3. Process C's delete completes (removes B's policy)

%% Final state: policy1 does not exist
```

### Read Operations

**Concurrent Reads**: Multiple processes can read from ETS simultaneously without blocking.

**Guarantees**:
- **No Read Locks**: Reads never block writes or other reads
- **Consistent Snapshots**: Each read sees a consistent state (no partial updates)
- **No Stale Reads**: ETS guarantees that reads see the most recent committed write

## High Load Scenarios

### Scenario 1: Concurrent Upserts to Same Policy

**Setup**: 100 concurrent processes all call `upsert_policy(tenant1, policy1, ...)` with different values.

**Expected Behavior**:
- All 100 operations succeed (return `ok`)
- Final state: Policy contains the value from the last operation (by gen_server processing order)
- **No Corruption**: Policy is never in a partial/invalid state
- **No Lost Updates**: All updates are applied (though only the last one is visible)

**Test**: `test_concurrent_upsert_same_policy`

### Scenario 2: Concurrent Upserts to Different Policies

**Setup**: 100 concurrent processes call `upsert_policy(tenant1, policy_N, ...)` where N = 1..100.

**Expected Behavior**:
- All 100 operations succeed
- Final state: All 100 policies exist with their respective values
- **No Interference**: Updates to different policies do not affect each other
- **No Lost Updates**: All policies are correctly stored

**Test**: `test_concurrent_upsert_different_policies`

### Scenario 3: Concurrent Delete and Upsert

**Setup**: Process A calls `delete_policy(tenant1, policy1)`, Process B calls `upsert_policy(tenant1, policy1, ...)`.

**Expected Behavior**:
- Both operations succeed
- Final state depends on execution order:
  - If delete happens first: policy is deleted, then upsert creates it → policy exists
  - If upsert happens first: policy is created/updated, then delete removes it → policy does not exist
- **No Race Condition**: Final state is deterministic based on gen_server processing order
- **No Corruption**: Policy is never in a partial/invalid state

**Test**: `test_concurrent_delete_and_upsert`

### Scenario 4: Concurrent List and Upsert

**Setup**: Process A calls `list_policies(tenant1)`, Process B calls `upsert_policy(tenant1, policy1, ...)`.

**Expected Behavior**:
- Both operations succeed
- List may or may not include the newly inserted policy (depends on timing)
- **No Inconsistency**: List returns a consistent snapshot (all policies from the same point in time)
- **No Partial Lists**: List never contains corrupted or partial entries

**Test**: `test_concurrent_list_and_upsert`

## "Last Write Wins" Semantics

### Definition

When multiple concurrent operations target the same policy, the last operation (by gen_server processing order) determines the final state.

### When It Applies

- **Same Policy, Concurrent Upserts**: Last upsert wins
- **Upsert + Delete**: Delete always wins (if it happens after upsert)

### When It Does NOT Apply

- **Different Policies**: Operations on different policies are independent
- **Read Operations**: Reads do not modify state, so "last write wins" is not applicable

### Why It's Acceptable

1. **Expected Behavior**: For policy management, "last write wins" is the expected semantics
2. **No Data Loss**: All operations are applied (though only the last one is visible)
3. **Deterministic**: Final state is deterministic based on gen_server processing order
4. **No Corruption**: Policy is never in a partial/invalid state

## Testing Strategy

### Unit Tests

- Test atomicity of individual operations
- Test validation before insertion
- Test error handling

### Concurrency Tests

- Test concurrent operations on same policy
- Test concurrent operations on different policies
- Test race conditions (delete + upsert, list + upsert)
- Test high load (100+ concurrent operations)

### Validation

- Verify final state matches expected semantics
- Verify no corruption or partial updates
- Verify all operations complete successfully

## Performance Considerations

### ETS Performance

- **Insert**: O(1) average case
- **Delete**: O(1) average case
- **Lookup**: O(1) average case
- **Match**: O(N) where N is the number of matching entries

### gen_server Serialization

- **Bottleneck**: All writes are serialized through gen_server
- **Throughput**: Limited by gen_server message processing speed
- **Latency**: Each write operation waits for previous operations to complete

### Optimization Opportunities

- **Batch Operations**: Group multiple operations into a single gen_server call
- **Async Operations**: Use `gen_server:cast/2` for fire-and-forget operations (if acceptable)
- **Sharding**: Partition policies across multiple ETS tables (if needed for very high load)

## Telemetry and Observability

### Standardized Events

**router_policy_store:**
- `[router_policy_store, upsert|delete|get|list|rebuild_index|transfer_attempt|transfer_success|transfer_timeout]`

**router_admin:**
- `[router_admin, upsert|delete|get|list]`

### Standardized Metadata

- `tenant_id` - tenant identifier
- `policy_id` - policy identifier (if applicable)
- `correlation_id` - correlation identifier for end-to-end operation linking (optional, from gRPC metadata)
  - **Format**: UUID/ULID string, generated by client
  - **Source**: gRPC metadata via `x-correlation-id` or `correlation-id` header
  - **Extraction**: `router_grpc.erl` and `router_admin_grpc.erl` extract via `extract_correlation_id/1`
  - **Propagation**: Passed to `router_policy_store` operations and included in telemetry for end-to-end tracing
- `table` - table name (for `router_policy_store` events)
- `count` - number of policies in response (for `list` operations)
- `result` - operation result (`ok` or `error`) - standardized across all events
- `error` - error details (only when `result = error`)

### Standardized Measurements

- `duration_us` - operation duration in microseconds (using `erlang:monotonic_time()` and `erlang:convert_time_unit/3`)
- `queue_len` - gen_server mailbox size at operation completion
- `wait_duration_us` - ownership wait duration for transfer events (only for `transfer_success` and `transfer_timeout`)

## Fault Tolerance and Recovery

### ETS Table Ownership and Heir

**CRITICAL**: When the owner process (gen_server) dies, the ETS table is automatically deleted **unless** an heir is configured.

**Current Implementation**:
- ETS tables (main + index) are created with `{heir, HeirPid, none}` option
- `router_policy_store_heir`: Dedicated `gen_server` process that acts as heir
- If `router_policy_store` process crashes, the heir receives both tables via `{'ETS-TRANSFER', Tab, FromPid, GiftData}` messages
- On restart, `init/1` checks if tables exist and claims them via `router_policy_store_heir:claim(self())`

**Recovery Process**:
1. **Process Crash**: `router_policy_store` gen_server crashes
2. **Heir Transfer**: Both tables (main + index) are transferred to `router_policy_store_heir` process
   - Event: `[router_policy_store, transferred_to_heir]` emitted with table metadata
3. **Heir Storage**: Heir stores table information and waits for claim
4. **Restart**: New `router_policy_store` process starts and calls `router_policy_store_heir:claim(self())`
5. **Ownership Wait with Retry**: New process waits for ownership with `wait_for_table_owner_with_retry/3`:
   - First wait: `TRANSFER_TIMEOUT_MS` (default: 5000ms)
   - If timeout: short retry `TRANSFER_RETRY_MS` (default: 100ms)
   - Events: `transfer_attempt`, `transfer_success`, or `transfer_timeout` emitted
   - Measurements: `wait_duration_us` included in `transfer_success` and `transfer_timeout` events to track transfer delays
6. **Write Access Verification**: After successful claim, write operations to `protected` tables are verified
   - Both main and index tables must be successfully claimed
   - Write access is tested to ensure tables are not read-only
7. **Fixture Reload**: Fixtures are reloaded to ensure consistency (even if table has data)
8. **Index Rebuild**: Index is rebuilt from main table to ensure consistency

**Limitations**:
- **In-Memory Only**: ETS tables are not persistent - data is lost on full node restart
- **Heir Transfer**: Heir mechanism preserves table during process crash, but supervisor restart may still recreate it
- **No External Backup**: Policies are not backed up to external storage (PostgreSQL, etc.)

**Future Improvements**:
- **Persistent Storage**: Back policies to PostgreSQL or file system
- **Recovery from Backup**: Restore policies from external source on startup
- **Health Checks**: Monitor table size and trigger recovery if data loss detected

### Process Crash Scenarios

#### Scenario 1: Normal Process Termination

**Setup**: `router_policy_store` process terminates normally (e.g., `exit(normal)`).

**Behavior**:
- ETS table is deleted (no heir transfer for normal termination)
- Supervisor restarts process
- New process creates fresh table and loads fixtures

**Data Loss**: All runtime policies are lost (only fixtures remain)

#### Scenario 2: Process Crash with Heir

**Setup**: `router_policy_store` process crashes (e.g., `exit(kill)`).

**Behavior**:
- Both ETS tables (main + index) are transferred to `router_policy_store_heir` process (heir)
- Supervisor restarts `router_policy_store` process
- New process checks table ownership via `ets:info(?TABLE, owner)` and `ets:info(?INDEX_TABLE, owner)`
- If tables owned by heir, new process calls `router_policy_store_heir:claim(self())`
- Heir process transfers both tables to new owner via `ets:give_away/3`
- New process waits for ownership with `wait_for_table_owner/3` (handles race conditions)
- Telemetry events are emitted: `transfer_attempt`, `transfer_success`, or `transfer_timeout`
- New process verifies ownership and rebuilds index
- Fixtures are reloaded to ensure consistency

**Data Loss**: Minimal - policies in table are preserved (heir mechanism works correctly)
**Write Access**: Restored - new process becomes owner and can write to `protected` tables

**Race Condition Handling**:
- **Problem**: New owner may call `claim/1` before heir receives `ETS-TRANSFER`
- **Solution**: `wait_for_table_owner_with_retry/3` implements two-stage waiting:
  1. Primary wait: polls `ets:info(Tab, owner)` with timeout (1000ms) and backoff (25ms)
  2. Additional retry: if primary wait times out, waits additional 500ms before fallback
- **Benefits**: Reduces probability of unnecessary table recreation due to ETS-TRANSFER message delivery delays
- **Fallback**: If both waits timeout, new owner creates empty table and reloads fixtures

#### Scenario 3: Node Restart

**Setup**: Entire Erlang node is restarted.

**Behavior**:
- All ETS tables are lost (in-memory only)
- All processes are restarted
- New `router_policy_store` process creates fresh table
- Only fixtures are loaded (runtime policies are lost)

**Data Loss**: All runtime policies are lost (only fixtures remain)

### Protected Table and Write Access

**CRITICAL**: ETS tables with `protected` access mode allow writes **only from the owner process**.

**Implications**:
- If table is transferred to heir, new `router_policy_store` process **cannot write** until ownership is transferred back
- `ets:give_away/3` must be called by current owner (heir) to transfer ownership
- Until transfer completes, write operations will fail with `badarg`

**Solution**:
- Heir process receives table and stores it in state
- New owner requests transfer via `claim/1` API
- Heir process calls `ets:give_away/3` to transfer ownership
- New owner verifies ownership before proceeding with operations

### Recovery Recommendations

1. **For Development**: Fixtures are sufficient for testing
2. **For Production**: 
   - Implement persistent storage (PostgreSQL, Redis, etc.)
   - Periodically backup policies to external storage
   - Restore policies from backup on startup
   - Monitor table size and trigger alerts if unexpected data loss
   - Monitor heir process health (it must be running for fault tolerance)

## Metrics and Monitoring

### Key Metrics to Track

1. **Queue Length** (`message_queue_len`):
   - **Normal**: < `queue_warn` (default: 10 messages)
   - **Warning**: `queue_warn`-`queue_crit` (default: 10-100 messages)
   - **Critical**: > `queue_crit` (default: 100 messages)
   - **Alert Threshold**: > `queue_crit` for > 1 minute
   - **Configuration**: Set via `application:get_env(beamline_router, queue_warn, 10)` and `queue_crit`

2. **Operation Latency** (configurable thresholds):
   - **upsert_policy**: P95 < `latency_warn_ms` (default: 10ms), P99 < `latency_crit_ms` (default: 50ms)
   - **delete_policy**: P95 < `latency_warn_ms` (default: 10ms), P99 < `latency_crit_ms` (default: 50ms)
   - **get_policy**: P95 < `latency_warn_ms` (default: 10ms), P99 < `latency_crit_ms` (default: 50ms)
   - **list_policies**: P95 < `list_policies_latency_warn_ms` (default: 5ms), P99 < `latency_crit_ms` (default: 50ms)
   - **Configuration**: Set via `application:get_env(beamline_router, latency_warn_ms, 10)`, `latency_crit_ms`, `list_policies_latency_warn_ms`

3. **Throughput (TPS)**:
   - **Target**: > 1000 ops/sec for single tenant
   - **Peak**: > 5000 ops/sec for multiple tenants
   - **Alert Threshold**: < 500 ops/sec for > 5 minutes

4. **Success/Error Rate**:
   - **Success Rate**: > 99.9%
   - **Error Rate**: < 0.1%
   - **Alert Threshold**: Error rate > 1% for > 1 minute

5. **Table Size**:
   - **Normal**: < 10,000 policies
   - **Warning**: 10,000-50,000 policies
   - **Critical**: > 50,000 policies
   - **Alert Threshold**: Unexpected size drop (potential data loss)

### Telemetry Events

All operations emit standardized telemetry events via `exec_with_telemetry/3`:

**Event Names:**
- `[router_policy_store, upsert]` - policy upsert operation
- `[router_policy_store, delete]` - policy deletion
- `[router_policy_store, list]` - list policies for tenant
- `[router_policy_store, get_policy]` - get single policy
- `[router_policy_store, rebuild_index]` - index rebuild operation
- `[router_policy_store, transfer_attempt]` - ETS table transfer attempt
- `[router_policy_store, transfer_success]` - successful table transfer (includes `wait_duration_us`)
- `[router_policy_store, transfer_timeout]` - table transfer timeout (includes `wait_duration_us`)

**Measurements:**
- `duration_us` - operation duration in microseconds (using `erlang:monotonic_time()` and `erlang:convert_time_unit/3`)
- `queue_len` - gen_server mailbox length at operation completion
- `wait_duration_us` - time spent waiting for table ownership transfer (only for `transfer_success` and `transfer_timeout` events)

**Metadata:**
- `tenant_id` - tenant identifier
- `table` - table name (`policy_store` or `policy_store_index`)
- `policy_id` - policy identifier (if applicable)
- `result` - operation result (`ok` or `error`)
- `error` - error details (if `result = error`)
- `count` - number of policies (for `list` operations)
- `correlation_id` - correlation identifier for end-to-end operation linking (optional, from gRPC metadata)
  - Format: UUID/ULID string, generated by client
  - Passed in gRPC metadata via `x-correlation-id` or `correlation-id` header
  - Propagated to telemetry for end-to-end tracing

**Time Measurement:**
- Uses `erlang:monotonic_time()` for accurate, monotonic time measurements
- Converts from native time units to microseconds via `erlang:convert_time_unit/3`
- Avoids `os:timestamp()` or `erlang:now()` which can cause inaccuracies

### Alerting Recommendations

1. **Queue Length Alert**:
   - Condition: `message_queue_len > 100` for > 1 minute
   - Action: Investigate gen_server bottleneck, consider sharding

2. **Latency Alert**:
   - Condition: P95 latency > 50ms for > 5 minutes
   - Action: Check table size, consider pagination for list_policies

3. **Error Rate Alert**:
   - Condition: Error rate > 1% for > 1 minute
   - Action: Check validation logic, investigate invalid policies

4. **Data Loss Alert**:
   - Condition: Table size drops unexpectedly
   - Action: Check process crashes, verify heir mechanism

5. **Throughput Alert**:
   - Condition: Throughput < 500 ops/sec for > 5 minutes
   - Action: Check system load, investigate gen_server queue

## Summary

- **Atomicity**: All ETS operations are atomic
- **Consistency**: Reads see consistent snapshots
- **Isolation**: Writes are serialized through gen_server
- **Durability**: In-memory only (not persistent)
- **Last Write Wins**: Expected behavior for concurrent updates to same policy
- **No Corruption**: Policy is never in a partial/invalid state
- **Fault Tolerance**: Heir mechanism preserves table during process crash (with limitations)
- **Recovery**: Fixtures are reloaded on restart to ensure consistency
- **Performance**: Secondary index (bag) for O(k) tenant lookups where k = policies per tenant, read_concurrency for concurrent reads
- **Monitoring**: Comprehensive metrics and telemetry for operations, latency, and errors

