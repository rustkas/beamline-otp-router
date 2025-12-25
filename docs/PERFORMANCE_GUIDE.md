# Performance Guide

This guide provides comprehensive information on performance tuning, benchmarking, and monitoring for the router project.

## Table of Contents

1. [Performance Tuning](#performance-tuning)
2. [Performance Benchmarks](#performance-benchmarks)
3. [Performance Monitoring](#performance-monitoring)
4. [Optimization Strategies](#optimization-strategies)
5. [Troubleshooting](#troubleshooting)

## Performance Tuning

### ETS Table Optimization

#### Table Configuration

ETS tables should be configured for performance:

```erlang
%% Use read_concurrency and write_concurrency for high-traffic tables
ets:new(router_metrics, [
    named_table,
    public,
    {read_concurrency, true},
    {write_concurrency, true}
]).
```

#### Table Size Management

Monitor ETS table sizes:

```erlang
%% Get table info
Info = ets:info(router_metrics),
Size = proplists:get_value(size, Info),
Memory = proplists:get_value(memory, Info).
```

**Recommendations**:
- Keep table sizes under 1M entries
- Implement cleanup for old entries
- Use `ordered_set` for range queries
- Use `bag` or `duplicate_bag` for multi-value lookups

### Policy Lookup Optimization

#### Caching Strategies

Cache frequently accessed policies:

```erlang
%% In router_policy_applier.erl
get_policy(TenantId, PolicyId) ->
    Key = {TenantId, PolicyId},
    case ets:lookup(policy_cache, Key) of
        [{Key, Policy}] ->
            Policy;
        [] ->
            Policy = load_policy_from_store(TenantId, PolicyId),
            ets:insert(policy_cache, {Key, Policy}),
            Policy
    end.
```

#### Lookup Patterns

- Use `ets:lookup/2` for single-key lookups
- Use `ets:match/2` for pattern matching
- Use `ets:select/2` for complex queries
- Avoid `ets:tab2list/1` for large tables

### Metrics Collection Optimization

#### Batch Updates

Batch metric updates when possible:

```erlang
%% Instead of multiple individual updates
lists:foreach(fun(Metric) ->
    router_metrics:inc(Metric)
end, Metrics),

%% Use batch update
router_metrics:batch_inc(Metrics).
```

#### Label Cardinality

Limit label cardinality to prevent metric explosion:

```erlang
%% Good: Limited cardinality
router_metrics:emit_metric(router_requests_total, #{count => 1}, #{
    tenant_id => TenantId,
    status => ~"success"
}).

%% Bad: High cardinality (user_id changes frequently)
router_metrics:emit_metric(router_requests_total, #{count => 1}, #{
    tenant_id => TenantId,
    user_id => UserId  % High cardinality!
}).
```

**Recommendations**:
- Limit labels to < 10 unique combinations per metric
- Use aggregation instead of per-entity metrics
- Monitor cardinality: `router_metrics:get_cardinality/1`

### Logging Performance

#### Structured Logging

Use structured logging for better performance:

```erlang
%% Good: Structured logging
router_logger:info(~"Request processed", #{
    ~"tenant_id" => TenantId,
    ~"request_id" => RequestId
}).

%% Bad: String formatting (slower)
router_logger:info(~"Request processed: ~p", [RequestId]).
```

#### Log Level Configuration

Configure appropriate log levels:

```erlang
%% In application config
{router_logger, [
    {level, info},  % Production: info, Development: debug
    {structured, true}
]}.
```

**Recommendations**:
- Use `info` level in production
- Use `debug` level only for troubleshooting
- Disable verbose logging in high-traffic scenarios

### Circuit Breaker Performance

#### State Lookup Optimization

Optimize circuit breaker state lookups:

```erlang
%% Use ets:lookup for single-key lookups
case ets:lookup(circuit_breaker_state, {TenantId, ProviderId}) of
    [{Key, State}] ->
        State;
    [] ->
        closed  % Default state
end.
```

#### Window Event Management

Limit window event history:

```erlang
%% Prune old events from sliding window
prune_old_events(Events, WindowSeconds) ->
    Now = erlang:system_time(second),
    Cutoff = Now - WindowSeconds,
    lists:filter(fun({Timestamp, _}) ->
        Timestamp >= Cutoff
    end, Events).
```

## Performance Benchmarks

### Baseline Metrics

#### Request Throughput

**Target**: >= 1000 requests/second per core

**Measurement**:
```erlang
%% In performance test
StartTime = erlang:monotonic_time(millisecond),
lists:foreach(fun(_) ->
    router_grpc:decide(Request, Context)
end, lists:seq(1, 1000)),
EndTime = erlang:monotonic_time(millisecond),
Throughput = 1000 / ((EndTime - StartTime) / 1000).
```

#### Latency Percentiles

**Targets**:
- P50: < 10ms
- P95: < 50ms
- P99: < 100ms

**Measurement**:
```erlang
%% Collect latencies
Latencies = lists:map(fun(_) ->
    Start = erlang:monotonic_time(millisecond),
    router_grpc:decide(Request, Context),
    End = erlang:monotonic_time(millisecond),
    End - Start
end, lists:seq(1, 1000)),

%% Calculate percentiles
P50 = lists:nth(500, lists:sort(Latencies)),
P95 = lists:nth(950, lists:sort(Latencies)),
P99 = lists:nth(990, lists:sort(Latencies)).
```

#### Memory Usage

**Target**: < 100MB per 1000 active circuits

**Measurement**:
```erlang
%% Get memory info
Memory = erlang:memory(),
Total = proplists:get_value(total, Memory),
Processes = proplists:get_value(processes, Memory),
ETS = proplists:get_value(ets, Memory).
```

### Benchmark Scripts

#### Load Test Script

```bash
#!/bin/bash
# scripts/benchmark_load.sh

REQUESTS=1000
CONCURRENT=10

for i in $(seq 1 $CONCURRENT); do
    (
        for j in $(seq 1 $REQUESTS); do
            # Send request
            curl -X POST http://localhost:9000/router.Decide/Decide \
                -H "Content-Type: application/json" \
                -d '{"request": {...}}'
        done
    ) &
done

wait
```

#### Stress Test

```bash
#!/bin/bash
# scripts/benchmark_stress.sh

DURATION=3600  # 1 hour
RATE=100       # requests/second

START=$(date +%s)
while [ $(($(date +%s) - START)) -lt $DURATION ]; do
    # Send requests at specified rate
    for i in $(seq 1 $RATE); do
        # Send request
        curl -X POST http://localhost:9000/router.Decide/Decide \
            -H "Content-Type: application/json" \
            -d '{"request": {...}}' &
    done
    sleep 1
done
```

### Performance Regression Tests

#### Automated Benchmarks

Create regression tests that fail if performance degrades:

```erlang
%% In test/router_performance_SUITE.erl
performance_regression_test(_Config) ->
    %% Run benchmark
    {Throughput, P95} = run_benchmark(),
    
    %% Check against baseline
    ?assert(Throughput >= 1000, "Throughput below baseline"),
    ?assert(P95 < 50, "P95 latency above threshold").
```

#### Baseline Storage

Store baseline metrics for comparison:

```erlang
%% Store baseline
Baseline = #{
    throughput => 1200,
    p50 => 8,
    p95 => 45,
    p99 => 90
},
store_baseline(Baseline).
```

## Performance Monitoring

### Metrics to Monitor

#### Request Metrics

- `router_requests_total`: Total request count
- `router_request_duration_seconds`: Request latency histogram
- `router_request_errors_total`: Error count

#### Circuit Breaker Metrics

- `router_circuit_breaker_state`: Current state (0=closed, 1=open)
- `router_circuit_breaker_state_transitions_total`: Transition count
- `router_circuit_breaker_error_rate`: Current error rate

#### Resource Metrics

- `router_ets_table_size`: ETS table sizes
- `router_process_count`: Process count
- `router_memory_usage_bytes`: Memory usage

### Monitoring Tools

#### Prometheus Integration

Export metrics to Prometheus:

```erlang
%% In router_metrics_http.erl
metrics(SessionId, _Env, _Input) ->
    Metrics = collect_all_metrics(),
    PrometheusFormat = format_prometheus(Metrics),
    {200, [{"content-type", "text/plain"}], PrometheusFormat}.
```

#### Grafana Dashboards

Create dashboards for:
- Request throughput over time
- Latency percentiles (P50, P95, P99)
- Error rates
- Circuit breaker states
- Resource usage (memory, CPU, ETS)

### Alerting

#### Performance Alerts

Configure alerts for:
- **High Latency**: P95 > 100ms for > 5 minutes
- **Low Throughput**: < 500 req/s for > 5 minutes
- **High Error Rate**: > 5% for > 5 minutes
- **Memory Growth**: > 10MB/hour for > 2 hours

#### Alert Configuration

```yaml
# prometheus_alerts.yml
groups:
  - name: router_performance
    rules:
      - alert: HighLatency
        expr: histogram_quantile(0.95, router_request_duration_seconds) > 0.1
        for: 5m
        annotations:
          summary: "High request latency detected"
```

## Optimization Strategies

### Profiling

#### fprof Profiling

Profile code execution:

```erlang
%% Start profiling
fprof:trace([start, {file, "fprof.trace"}]),

%% Run code
router_grpc:decide(Request, Context),

%% Stop profiling
fprof:trace([stop]),

%% Analyze
fprof:profile(),
fprof:analyse([{dest, "fprof.analysis"}]).
```

#### eprof Profiling

Profile with eprof:

```erlang
%% Start profiling
eprof:start(),
eprof:start_profiling([self()]),

%% Run code
router_grpc:decide(Request, Context),

%% Stop profiling
eprof:stop_profiling(),
eprof:log("eprof.log"),
eprof:analyze(),
eprof:stop().
```

### Hot Code Paths

#### Identify Hot Paths

Use profiling to identify hot code paths:

1. Run `fprof` or `eprof` during load test
2. Analyze results to find functions with highest CPU time
3. Optimize identified functions

#### Common Hot Paths

- **Policy lookup**: Cache policies
- **Metrics emission**: Batch updates
- **Logging**: Use appropriate log levels
- **ETS lookups**: Optimize table configuration

### Memory Optimization

#### Process Memory

Monitor process memory:

```erlang
%% Get process memory
ProcessInfo = erlang:process_info(Pid, [memory, message_queue_len]),
Memory = proplists:get_value(memory, ProcessInfo),
QueueLen = proplists:get_value(message_queue_len, ProcessInfo).
```

#### ETS Memory

Monitor ETS memory:

```erlang
%% Get ETS memory
Info = ets:info(Table, [memory, size]),
Memory = proplists:get_value(memory, Info),
Size = proplists:get_value(size, Info).
```

### CPU Optimization

#### Reduce Computations

- Cache expensive computations
- Use lookup tables for calculations
- Avoid unnecessary list operations
- Use `lists:keyfind/3` instead of `lists:filter/2` + `hd/1`

#### Process Scheduling

- Keep processes responsive (small message queues)
- Avoid blocking operations in hot paths
- Use `gen_server:cast/2` for non-critical operations

## Troubleshooting

### Performance Issues

#### High Latency

**Symptoms**: P95 latency > 100ms

**Investigation**:
1. Check ETS table sizes
2. Profile hot code paths
3. Check process message queues
4. Review circuit breaker states
5. Check external dependencies (NATS, CAF)

**Solutions**:
- Optimize ETS lookups
- Cache frequently accessed data
- Reduce logging verbosity
- Optimize hot code paths

#### Low Throughput

**Symptoms**: < 500 req/s

**Investigation**:
1. Check CPU usage
2. Check process count
3. Check ETS table contention
4. Review locking mechanisms

**Solutions**:
- Increase concurrency
- Optimize ETS table configuration
- Reduce contention
- Profile and optimize hot paths

#### Memory Growth

**Symptoms**: Memory growing > 10MB/hour

**Investigation**:
1. Check ETS table sizes
2. Check process memory
3. Look for memory leaks
4. Review cleanup procedures

**Solutions**:
- Implement cleanup for old data
- Limit cache sizes
- Fix memory leaks
- Review data retention policies

### Performance Tools

#### Observer

Use `observer` for real-time monitoring:

```erlang
%% Start observer
observer:start().
```

Monitor:
- Process tree
- ETS tables
- Memory usage
- CPU usage

#### Recon

Use `recon` for advanced debugging:

```erlang
%% Get process info
recon:info(Pid).

%% Get memory info
recon:memory(used).

%% Get ETS info
recon:ets_count().
```

## Related Documentation

- `DEVELOPER_GUIDE.md` - Development workflow
- `TESTING_GUIDE.md` - Test execution procedures
- `OBSERVABILITY_CONVENTIONS.md` - Metrics patterns
- `ARCHITECTURE_DOCUMENTATION.md` - System architecture

---

**Last Updated**: 2025-01-27  
**Maintainer**: Router Team

