# Performance Considerations

## router_policy_store Architecture

### Current Implementation

`router_policy_store` uses **gen_server** to serialize all write/read operations:

- **All operations** (`upsert_policy`, `delete_policy`, `get_policy`, `list_policies`) go through `gen_server:call/2`
- **Serialization**: gen_server processes messages sequentially (one after another)
- **ETS table**: `set`, `protected`, `read_concurrency = true` - atomic operations, access serialized through gen_server for writes
- **Secondary index**: `bag` table for O(1) lookup by `tenant_id`

### Potential Bottlenecks

#### 1. Write Bottleneck

**Problem:**
- All write operations (`upsert_policy`, `delete_policy`) are serialized through a single gen_server
- Under peak loads, the gen_server mailbox may grow
- Operation latency may increase under high contention

**Symptoms:**
- Growth in gen_server mailbox size (`process_info(Pid, message_queue_len)`)
- Increase in operation latency (time from call to response)
- Timeouts under high loads

**Monitoring:**
- Log when `queue_len > 10` or `latency > 10ms`
- Telemetry for successful/failed operations

#### 2. Reading Through gen_server

**Current Behavior:**
- Read operations (`get_policy`, `list_policies`) also go through gen_server
- This ensures consistency but adds overhead

**Optimization:**
- ETS table with `read_concurrency = true` allows parallel reads
- Secondary index for O(1) lookup by `tenant_id` instead of O(n) linear scan

**Alternative:**
- ETS table `protected` allows direct reads without gen_server
- But this may lead to race conditions with concurrent read/write

#### 3. list_policies and Linear Scan

**Problem (RESOLVED):**
- ~~`list_policies` uses `ets:match_object/2` to find all tenant policies~~
- ~~This is a **linear scan** through the entire table (O(n))~~
- ✅ **RESOLVED**: Uses secondary index (bag) for O(k) lookup by `tenant_id`

**Current Implementation:**
```erlang
do_list_policies(TenantId, Table, IndexTable) ->
    %% Use secondary index (bag) to get policy IDs for tenant (O(k) where k = policies for tenant)
    %% Bag table allows multiple entries per key, so we get all {TenantId, PolicyId} pairs
    IndexEntries = ets:match_object(IndexTable, {TenantId, '_'}),
    PolicyIds = [PolicyId || {_TenantId, PolicyId} <- IndexEntries],
    %% Fetch policies using policy IDs (O(k) where k = number of policies for tenant)
    Policies = [fetch_policy(Table, TenantId, PolicyId) || PolicyId <- PolicyIds],
    %% Sort by policy_id for deterministic ordering (O(k log k))
    SortByPolicyId = fun(P1, P2) ->
        binary:compare(P1#policy.policy_id, P2#policy.policy_id) =:= less
    end,
    SortedResult = lists:sort(SortByPolicyId, Policies),
    {ok, SortedResult}.
```

**Complexity:**
- **Without index**: O(n) - linear scan through entire table, where n = total number of policies
- **With index (bag)**: O(k) + O(k log k) where k = number of policies for a single tenant
  - Finding all entries for tenant in bag index: O(k) (bag stores all {TenantId, PolicyId} pairs)
  - Getting all policy IDs: O(k)
  - Loading policies: O(k)
  - Sorting: O(k log k) - deterministic sort by policy_id
  - **Total**: O(k) + O(k log k) = O(k log k) - significantly better than O(n) when k << n
  - **Note**: k is usually much smaller than n (number of policies for one tenant << total number of policies)
  
**Sorting trade-off:**
- Explicit sorting by `policy_id` adds O(k log k) to complexity
- Uses strict lexicographic order via `binary:compare/2`:
  ```erlang
  SortByPolicyId = fun(P1, P2) ->
      binary:compare(P1#policy.policy_id, P2#policy.policy_id) =:= less
  end,
  SortedResult = lists:sort(SortByPolicyId, Result).
  ```
- This is necessary for deterministic result ordering
- In most cases k << n, so overall O(k log k) complexity is acceptable
- Thresholds for `list_policies` are configured separately via `list_policies_latency_warn_ms` (default: 5ms)

**Result Ordering:**
- ETS table type `set` **does not guarantee** element order
- Results **are sorted by `policy_id`** for deterministic ordering
- Clients can rely on sorting by `policy_id` (lexicographic order)

**Symptoms:**
- High latency for `list_policies` with large number of policies (improved via index)
- Growth in gen_server mailbox with multiple `list_policies` calls
- Impact on latency of other operations (upsert, delete, get)

**Critical Values:**
- `< 100 policies per tenant`: acceptable (< 1ms)
- `100-1000 policies`: requires attention (1-10ms)
- `> 1000 policies`: potential issue (> 10ms) - **improved via index**

## Performance Monitoring

### Metrics to Track

#### 1. Mailbox Size

```erlang
%% Get router_policy_store mailbox size
{message_queue_len, QueueLen} = process_info(whereis(router_policy_store), message_queue_len),
io:format("Message queue length: ~p~n", [QueueLen]).
```

**Critical Values (configurable via `application:get_env/3`):**
- `QueueLen < queue_warn` (default: 10): normal load
- `QueueLen queue_warn-queue_crit` (default: 10-100): increased load, monitor
- `QueueLen > queue_crit` (default: 100): potential issue, optimization required

**Thresholds are configured in `beamline_router.app.src`:**
```erlang
{env, [
    {queue_warn, 10},
    {queue_crit, 100},
    {latency_warn_ms, 10},  %% Converted to us on load
    {latency_crit_ms, 50},  %% Converted to us on load
    {list_policies_latency_warn_ms, 5},  %% Converted to us on load
    {policy_count_warn, 100}
]}
```

**Important:** All thresholds are loaded from `beamline_router` application via `application:get_env(beamline_router, ...)`. Values in milliseconds are converted to microseconds once on load in `load_thresholds/0`.

#### 2. Operation Latency

```erlang
%% Measure operation latency
StartTime = erlang:monotonic_time(microsecond),
Result = router_policy_store:upsert_policy(TenantId, Policy),
EndTime = erlang:monotonic_time(microsecond),
LatencyUs = EndTime - StartTime,
io:format("Operation latency: ~p microseconds (~p ms)~n", [LatencyUs, LatencyUs / 1000]).
```

**Expected Values (configurable via `application:get_env/3`):**
- `Latency < latency_warn_ms` (default: 10ms): normal performance
- `Latency latency_warn_ms-latency_crit_ms` (default: 10-50ms): acceptable for most cases
- `Latency > latency_crit_ms` (default: 50ms): requires attention under peak loads

**For `list_policies` a separate threshold is used:**
- `list_policies_latency_warn_ms` (default: 5ms) - stricter threshold due to O(k log k) sorting

#### 3. Throughput (operations per second)

```erlang
%% Measure throughput
NumOps = 1000,
StartTime = erlang:monotonic_time(microsecond),
[router_policy_store:upsert_policy(TenantId, Policy) || _ <- lists:seq(1, NumOps)],
EndTime = erlang:monotonic_time(microsecond),
DurationUs = EndTime - StartTime,
Throughput = (NumOps * 1000000) / DurationUs,
io:format("Throughput: ~p ops/sec~n", [Throughput]).
```

**Expected Values:**
- `Throughput > 10000 ops/sec`: excellent performance
- `Throughput 1000-10000 ops/sec`: acceptable
- `Throughput < 1000 ops/sec`: may be a bottleneck under high loads

#### 4. Operation Telemetry

All operations use standardized telemetry via `exec_with_telemetry/3`:

**Events:**

**router_policy_store:**
- `[router_policy_store, upsert]` - create/update policy
- `[router_policy_store, delete]` - delete policy
- `[router_policy_store, list]` - get list of policies
- `[router_policy_store, get]` - get single policy
- `[router_policy_store, rebuild_index]` - rebuild index
- `[router_policy_store, transfer_attempt]` - table transfer attempt
- `[router_policy_store, transfer_success]` - successful table transfer (with `wait_duration_us`)
- `[router_policy_store, transfer_timeout]` - table transfer timeout (with `wait_duration_us`)

**router_admin:**
- `[router_admin, upsert]` - create/update policy via Admin API
- `[router_admin, delete]` - delete policy via Admin API
- `[router_admin, list]` - get list of policies via Admin API
- `[router_admin, get]` - get single policy via Admin API

**Measurements:**
- `duration_us` - operation duration in microseconds (uses `erlang:monotonic_time()` and `erlang:convert_time_unit/3`)
- `queue_len` - gen_server mailbox size at operation completion
- `wait_duration_us` - time waiting for table owner for transfer events (only for `transfer_success` and `transfer_timeout`)

**Metadata:**
- `tenant_id` - tenant identifier
- `table` - table name (`policy_store` or `policy_store_index`) - only for `router_policy_store`
- `policy_id` - policy identifier (if applicable)
- `result` - operation result (`ok` or `error`) - standardized for all events
- `error` - error details (if `result = error`)
- `count` - number of entities in response (for `list` operations: number of policies in response)
- `correlation_id` - correlation identifier for linking operations end-to-end (optional, from gRPC metadata)
  - **Format**: UUID/ULID string, generated by client
  - **Source**: gRPC metadata via `x-correlation-id` or `correlation-id` header
  - **Extraction**: `router_grpc.erl` and `router_admin_grpc.erl` extract via `extract_correlation_id/1`
  - **Propagation**: passed to `router_policy_store` operations and included in telemetry for end-to-end tracing
  - **Appearance in Telemetry**: present in all `router_policy_store` and `router_admin` events if provided by client

### Integration with Observability

**Recommended to add metrics in `router_logger`:**

```erlang
%% In router_policy_store.erl after handle_call
LatencyUs = erlang:monotonic_time(microsecond) - StartTime,
router_logger:info("Policy store operation", [
    {operation, Operation},
    {latency_us, LatencyUs},
    {queue_len, process_info(self(), message_queue_len)},
    {status, case Result of {ok, _} -> success; _ -> error end}
]).
```

## Optimization Recommendations

### Short-term (Current Architecture)

1. **Mailbox Monitoring:**
   - Add alert when `message_queue_len > 100`
   - Log queue size on each operation

2. **Operation Optimization:**
   - Minimize work inside `handle_call`
   - Use `gen_server:cast/2` for fire-and-forget operations (if acceptable)

3. **Timeouts:**
   - Set reasonable timeouts for `gen_server:call/3`
   - Handle timeouts gracefully

### Medium-term (Architectural Improvements)

1. **list_policies Optimization (IMPLEMENTED):**
   - ✅ **Secondary Index**: Separate ETS table for indexing by `tenant_id`
   - ✅ **O(1) Lookup**: Fast policy lookup by tenant instead of O(n) linear scan
   - **Pagination**: Add `limit` and `offset` parameters (future)
   - **Caching**: Cache results for frequently requested tenants (future)

2. **Read Concurrency (IMPLEMENTED):**
   - ✅ `read_concurrency = true` for main table
   - ✅ `read_concurrency = true` for index table
   - Allows multiple parallel reads without locks

3. **Read/Write Separation:**
   - Read operations directly from ETS (without gen_server)
   - Write operations through gen_server (for atomicity)
   - **Risk**: Race conditions with concurrent read/write

4. **Sharding:**
   - Split policies across multiple gen_server processes
   - Sharding by `tenant_id` or `policy_id`
   - **Advantage**: Parallel processing of different tenants

5. **Async Writes:**
   - Use `gen_server:cast/2` for write operations
   - Return `{ok, queued}` immediately
   - **Risk**: Loss of synchronous guarantees

6. **Write Batching:**
   - Group multiple operations into a single gen_server call
   - Reduces number of system calls
   - **Example**: `batch_upsert_policies/2` for multiple upserts

### Long-term (Alternative Solutions)

1. **Mnesia Instead of ETS:**
   - Distributed storage
   - Built-in replication
   - **Overhead**: Higher overhead

2. **External Storage:**
   - PostgreSQL, Redis, etc.
   - More complex, but more scalable
   - **Overhead**: Network latency

## Performance Testing

### Benchmark Test

Create `apps/otp/router/test/router_policy_store_bench_SUITE.erl`:

```erlang
test_throughput_benchmark(Config) ->
    NumOps = 10000,
    TenantId = ~"bench_tenant",
    
    %% Measure throughput
    StartTime = erlang:monotonic_time(microsecond),
    [router_policy_store:upsert_policy(TenantId, create_test_policy(N)) || N <- lists:seq(1, NumOps)],
    EndTime = erlang:monotonic_time(microsecond),
    
    DurationUs = EndTime - StartTime,
    Throughput = (NumOps * 1000000) / DurationUs,
    
    ct:log("Throughput: ~p ops/sec", [Throughput]),
    ?assert(Throughput > 1000, "Throughput should be > 1000 ops/sec").

test_list_policies_performance(Config) ->
    TenantId = ~"perf_tenant",
    
    %% Create varying numbers of policies
    TestCases = [10, 100, 500, 1000],
    
    [begin
        %% Create N policies
        [router_policy_store:upsert_policy(TenantId, create_test_policy(N)) || N <- lists:seq(1, NumPolicies)],
        
        %% Measure list_policies latency
        StartTime = erlang:monotonic_time(microsecond),
        {ok, Policies} = router_policy_store:list_policies(TenantId),
        EndTime = erlang:monotonic_time(microsecond),
        
        LatencyUs = EndTime - StartTime,
        LatencyMs = LatencyUs / 1000,
        
        ct:log("list_policies with ~p policies: ~p ms (~p us)", [NumPolicies, LatencyMs, LatencyUs]),
        
        %% Verify all policies returned
        ?assertEqual(NumPolicies, length(Policies)),
        
        %% Performance assertions
        case NumPolicies of
            N when N =< 100 ->
                ?assert(LatencyMs < 5, "Latency should be < 5ms for <= 100 policies");
            N when N =< 500 ->
                ?assert(LatencyMs < 20, "Latency should be < 20ms for <= 500 policies");
            _ ->
                ?assert(LatencyMs < 50, "Latency should be < 50ms for <= 1000 policies")
        end
    end || NumPolicies <- TestCases].
```

### Load Test (Burst)

```erlang
test_burst_load(Config) ->
    NumConcurrent = 100,
    TenantId = ~"burst_tenant",
    
    %% Spawn concurrent operations
    Pids = [spawn(fun() ->
        StartTime = erlang:monotonic_time(microsecond),
        router_policy_store:upsert_policy(TenantId, create_test_policy(N)),
        EndTime = erlang:monotonic_time(microsecond),
        self() ! {latency, EndTime - StartTime}
    end) || N <- lists:seq(1, NumConcurrent)],
    
    %% Collect latencies
    Latencies = [receive {latency, L} -> L end || _ <- Pids],
    
    MaxLatency = lists:max(Latencies),
    AvgLatency = lists:sum(Latencies) / length(Latencies),
    
    ct:log("Burst load - Max latency: ~p us, Avg latency: ~p us", [MaxLatency, AvgLatency]),
    
    %% Check message queue length
    {message_queue_len, QueueLen} = process_info(whereis(router_policy_store), message_queue_len),
    ct:log("Message queue length after burst: ~p", [QueueLen]),
    
    ?assert(QueueLen < 50, "Queue should not grow too much").
```

## Usage Recommendations

### In Production

1. **Monitoring:**
   - Track gen_server mailbox size
   - Alerts when `message_queue_len > 100`
   - Latency metrics for all operations
   - **Specifically for list_policies:**
     - Alert when latency > 10ms
     - Alert when policy count > 100
     - Monitor `list_policies` call frequency

2. **Rate Limiting:**
   - Limit number of concurrent operations per client
   - Use throttling to protect against burst loads
   - **For list_policies:** Limit call frequency (e.g., max 10 req/sec per tenant)

3. **Graceful Degradation:**
   - Under high load return `{error, overloaded}`
   - Clients should use retry with backoff
   - **For list_policies:** Limit maximum number of returned policies (e.g., 100)

4. **list_policies Optimization:**
   - Use pagination instead of getting all policies
   - Cache results for frequently requested tenants
   - Avoid frequent `list_policies` calls with large number of policies

### Under Peak Loads

1. **Horizontal Scaling:**
   - Run multiple Router instances
   - Load balancer distributes load

2. **Vertical Scaling:**
   - Increase number of Erlang processes
   - Optimize VM settings

3. **gen_server Optimization:**
   - Consider operation batching
   - Use async writes for fire-and-forget operations
   - Sharding by tenant_id for parallel processing
