# Router Operational Runbook

**Version**: 1.0  
**Last Updated**: 2025-01-27

## Overview

This runbook provides operational procedures for the Router service.

## Health Checks

### gRPC Health Check

```bash
grpc_health_probe -addr=localhost:9090
```

### Metrics Endpoint

```bash
curl http://localhost:9001/metrics
```

### Process Status

```erlang
%% Check supervisor status
supervisor:which_children(beamline_router_sup).

%% Check specific process
whereis(router_circuit_breaker).
is_process_alive(whereis(router_circuit_breaker)).
```

## Common Issues

### Issue: Circuit Breaker Stuck Open

**Symptoms**:
- All requests to a provider fail
- Circuit breaker metrics show state=open

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
2. Verify circuit breaker thresholds
3. Manually reset if needed (requires code change or admin API)

### Issue: Rate Limiting Too Aggressive

**Symptoms**:
- Requests return `RESOURCE_EXHAUSTED` (8)
- High rate limit error metrics

**Diagnosis**:
```erlang
%% Check rate limit metrics
router_metrics:get_metric_value(router_rate_limit_exceeded_total, #{}).
```

**Resolution**:
1. Review rate limit configuration
2. Adjust `rate_limit_per_tenant` or `rate_limit_per_user`
3. Check for traffic spikes

### Issue: NATS Connection Lost

**Symptoms**:
- NATS publish failures
- Connection state = disconnected

**Diagnosis**:
```erlang
%% Check NATS connection status
router_nats:get_connection_status().

%% Check NATS metrics
router_metrics:get_metric_value(router_nats_connection_lost_total, #{}).
```

**Resolution**:
1. Check NATS server availability
2. Verify NATS URL configuration
3. Check network connectivity
4. Review reconnect logs

### Issue: Policy Not Found

**Symptoms**:
- Requests return `NOT_FOUND` (5)
- Error: "policy not found"

**Diagnosis**:
```erlang
%% Check policy store
router_policy_store:list_policies(~"tenant-123").
```

**Resolution**:
1. Verify policy exists for tenant
2. Check policy store ETS table
3. Reload policies if needed

## Troubleshooting Guide

### Check Application Status

```erlang
%% List all applications
application:which_applications().

%% Check router application
application:get_all_env(beamline_router).
```

### Check ETS Tables

```erlang
%% List all ETS tables
ets:all().

%% Check specific table
ets:info(router_metrics).
ets:info(router_policies).
```

### Check Metrics

```erlang
%% Dump all metrics
router_r10_metrics:dump_metrics().

%% Check specific metric
router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => ~"tenant-123",
    provider_id => ~"provider-1"
}).
```

### Check Logs

```bash
# Application logs
tail -f /var/log/router/router.log

# Error logs
grep ERROR /var/log/router/router.log
```

## Incident Response Procedures

### 1. Service Unavailable

**Immediate Actions**:
1. Check process status: `supervisor:which_children(beamline_router_sup)`
2. Check logs for errors
3. Verify NATS connectivity
4. Check circuit breaker states

**Escalation**:
- If processes are down: Restart application
- If NATS unavailable: Check NATS infrastructure
- If circuit breakers open: Check provider health

### 2. High Error Rate

**Immediate Actions**:
1. Check error metrics: `router_metrics:get_metric_value(router_grpc_errors_total, #{})`
2. Check circuit breaker states
3. Review recent logs
4. Check provider health

**Escalation**:
- If provider issues: Contact provider team
- If internal errors: Review error logs and stack traces
- If rate limiting: Adjust rate limits or scale

### 3. Performance Degradation

**Immediate Actions**:
1. Check latency metrics: `router_metrics:get_metric_value(router_grpc_request_duration_seconds, #{})`
2. Check ETS table sizes
3. Review CPU/memory usage
4. Check for backpressure

**Escalation**:
- If memory issues: Review ETS table sizes, consider cleanup
- If CPU issues: Profile hot code paths
- If backpressure: Check queue sizes and processing latency

## Maintenance Procedures

### Restart Application

```bash
# Stop application
application:stop(beamline_router).

# Start application
application:start(beamline_router).
```

### Clear Metrics

```erlang
%% Clear R10 metrics
router_r10_metrics:clear_metrics().

%% Clear all metrics
router_metrics:clear_all().
```

### Reload Configuration

```erlang
%% Update configuration
application:set_env(beamline_router, rate_limit_per_tenant, 5000).

%% Restart affected processes (may require application restart)
```

## Monitoring

### Key Metrics to Monitor

1. **Request Rate**: `router_grpc_requests_total`
2. **Error Rate**: `router_grpc_errors_total`
3. **Latency**: `router_grpc_request_duration_seconds`
4. **Circuit Breaker States**: `router_circuit_breaker_state`
5. **Rate Limit Exceeded**: `router_rate_limit_exceeded_total`
6. **NATS Connection**: `router_nats_connection_status`

### Alert Thresholds

- Error rate > 1%: Warning
- Error rate > 5%: Critical
- Latency P95 > 5s: Warning
- Latency P95 > 10s: Critical
- Circuit breaker open: Warning
- NATS disconnected: Critical

## See Also

- [Architecture Documentation](./ARCHITECTURE_DOCUMENTATION.md)
- [Configuration Reference](./CONFIGURATION_REFERENCE.md)
- [API Documentation](./API_DOCUMENTATION.md)

