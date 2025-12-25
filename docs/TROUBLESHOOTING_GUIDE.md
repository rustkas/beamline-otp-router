# Router Troubleshooting Guide

**Version**: 1.0  
**Last Updated**: 2025-01-27

## Overview

This guide provides step-by-step troubleshooting procedures for common Router issues.

## Diagnostic Commands

### Check Application Status

```erlang
%% Check if application is running
application:which_applications().

%% Get all configuration
application:get_all_env(beamline_router).

%% Check supervisor status
supervisor:which_children(beamline_router_sup).
```

### Check Process Health

```erlang
%% Check specific processes
whereis(router_circuit_breaker).
is_process_alive(whereis(router_circuit_breaker)).

whereis(router_nats).
is_process_alive(whereis(router_nats)).

whereis(router_policy_store).
is_process_alive(whereis(router_policy_store)).
```

### Check ETS Tables

```erlang
%% List all ETS tables
ets:all().

%% Check table info
ets:info(router_metrics).
ets:info(router_policies).
ets:info(router_extensions).

%% Check table size
ets:info(router_metrics, size).
```

### Check Metrics

```erlang
%% Dump all R10 metrics
router_r10_metrics:dump_metrics().

%% Check specific metric
router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => ~"tenant-123",
    provider_id => ~"provider-1"
}).

%% Check rate limit metrics
router_metrics:get_metric_value(router_rate_limit_exceeded_total, #{}).
```

## Common Issues

### Issue: Requests Failing with NOT_FOUND

**Symptoms**:
- gRPC returns `NOT_FOUND` (5)
- Error message: "policy not found"

**Diagnosis**:
```erlang
%% Check if policy exists
router_policy_store:list_policies(~"tenant-123").

%% Check policy store table
ets:tab2list(router_policies).
```

**Resolution**:
1. Verify tenant_id is correct
2. Check if policy was created: `router_policy_store:list_policies/1`
3. Verify policy store process is running: `whereis(router_policy_store)`
4. Reload policies if needed

### Issue: Requests Failing with RESOURCE_EXHAUSTED

**Symptoms**:
- gRPC returns `RESOURCE_EXHAUSTED` (8)
- Error message: "rate limit exceeded"

**Diagnosis**:
```erlang
%% Check rate limit metrics
router_metrics:get_metric_value(router_rate_limit_exceeded_total, #{
    tenant_id => ~"tenant-123"
}).

%% Check rate limiter process
whereis(router_rate_limiter).
```

**Resolution**:
1. Review rate limit configuration
2. Check for traffic spikes
3. Adjust `rate_limit_per_tenant` or `rate_limit_per_user`
4. Verify rate limiter process is running

### Issue: Circuit Breaker Open

**Symptoms**:
- All requests to provider fail immediately
- Circuit breaker state = open

**Diagnosis**:
```erlang
%% Check circuit breaker state
router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => ~"tenant-123",
    provider_id => ~"provider-1",
    state => ~"open"
}).

%% Check trigger reason
router_r10_metrics:get_latest_trigger_reason(~"tenant-123", ~"provider-1").
```

**Resolution**:
1. Check provider health
2. Review circuit breaker thresholds
3. Wait for half-open transition
4. Manually reset if needed (requires admin API or code change)

### Issue: NATS Connection Issues

**Symptoms**:
- NATS publish failures
- Connection state = disconnected

**Diagnosis**:
```erlang
%% Check NATS connection status
router_nats:get_connection_status().

%% Check NATS metrics
router_metrics:get_metric_value(router_nats_connection_lost_total, #{}).
router_metrics:get_metric_value(router_nats_publish_failures_total, #{}).
```

**Resolution**:
1. Check NATS server availability
2. Verify NATS URL: `application:get_env(beamline_router, nats_url)`
3. Check network connectivity
4. Review NATS logs
5. Manually reconnect: `router_nats:reconnect()`

### Issue: High Latency

**Symptoms**:
- Request latency > 5 seconds
- Timeout errors

**Diagnosis**:
```erlang
%% Check latency metrics
router_metrics:get_metric_value(router_grpc_request_duration_seconds, #{}).

%% Check for backpressure
router_intake_backpressure:get_backpressure_status(~"beamline.router.v1.decide").
```

**Resolution**:
1. Check ETS table sizes (may indicate memory pressure)
2. Check CPU usage
3. Review processing latency
4. Check for backpressure conditions
5. Review circuit breaker states (may be causing delays)

### Issue: Memory Issues

**Symptoms**:
- High memory usage
- ETS table size warnings

**Diagnosis**:
```erlang
%% Check ETS table sizes
ets:info(router_metrics, memory).
ets:info(router_policies, memory).
ets:info(router_extensions, memory).

%% Check table sizes
ets:info(router_metrics, size).
```

**Resolution**:
1. Clear old metrics: `router_r10_metrics:clear_metrics()`
2. Review ETS table retention policies
3. Check for memory leaks
4. Restart application if needed

## Performance Tuning

### Reduce Latency

1. **Optimize ETS Operations**:
   - Use read_concurrency for read-heavy tables
   - Use write_concurrency for write-heavy tables

2. **Circuit Breaker Tuning**:
   - Adjust latency thresholds
   - Review failure thresholds

3. **Rate Limiting**:
   - Adjust rate limits based on capacity
   - Use per-user limits for fine-grained control

### Increase Throughput

1. **Connection Pooling**:
   - Optimize NATS connection pooling
   - Review gRPC connection handling

2. **Batch Operations**:
   - Batch policy lookups where possible
   - Batch metric updates

3. **Caching**:
   - Review policy cache hit rates
   - Optimize extension registry cache

## Log Analysis

### Common Log Patterns

```bash
# Check for errors
grep ERROR /var/log/router/router.log

# Check for circuit breaker events
grep "circuit breaker" /var/log/router/router.log

# Check for rate limit events
grep "rate limit" /var/log/router/router.log

# Check for NATS issues
grep "NATS" /var/log/router/router.log | grep -i error
```

### Log Levels

- **ERROR**: Critical errors requiring immediate attention
- **WARN**: Warnings that may indicate issues
- **INFO**: Informational messages
- **DEBUG**: Detailed debugging information

## See Also

- [Operational Runbook](./OPERATIONAL_RUNBOOK.md)
- [Architecture Documentation](./ARCHITECTURE_DOCUMENTATION.md)
- [Configuration Reference](./CONFIGURATION_REFERENCE.md)

