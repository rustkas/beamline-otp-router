# Complete Operational Runbook

**Version**: 1.0  
**Last Updated**: 2025-01-27  
**Status**: Complete

## Overview

This document provides comprehensive operational runbook for Router component, including troubleshooting guide and incident response procedures.

## Pre-Production Checklist

### 1. Configuration Verification

- [ ] All required configuration options set
- [ ] NATS TLS enabled and certificates configured
- [ ] Admin API key set (via environment variable)
- [ ] Circuit breaker thresholds configured appropriately
- [ ] Rate limits configured appropriately
- [ ] Metrics port accessible (9001)
- [ ] gRPC port accessible (9000)

### 2. Health Checks

- [ ] Application starts successfully
- [ ] All supervisors start children
- [ ] gRPC server starts on configured port
- [ ] Metrics server starts on configured port
- [ ] NATS connection established (if `nats_mode = real`)
- [ ] Circuit breaker process running
- [ ] Policy store initialized

### 3. Monitoring Setup

- [ ] Prometheus scraping `/metrics` endpoint
- [ ] Alerts configured for critical metrics
- [ ] Dashboards created for key metrics
- [ ] Log aggregation configured
- [ ] Distributed tracing configured (if enabled)

## Health Checks

### Application Health

**Endpoint**: `GET /_health`  
**Expected**: `200 OK` with JSON response

```json
{
  "status": "healthy",
  "timestamp": "2025-01-27T12:00:00Z",
  "checks": {
    "circuit_breaker": {
      "status": "ok",
      "message": "Process running"
    },
    "policy_store": {
      "status": "ok",
      "message": "Initialized"
    },
    "nats": {
      "status": "ok",
      "message": "Connected"
    }
  }
}
```

### Metrics Health

**Endpoint**: `GET /metrics`  
**Expected**: `200 OK` with Prometheus text format

**Key Metrics to Monitor**:
- `router_grpc_requests_total` - Total gRPC requests
- `router_grpc_request_duration_seconds` - Request latency
- `router_circuit_breaker_state` - Circuit breaker states
- `router_circuit_breaker_trigger_reason` - Trigger reasons
- `router_nats_publish_attempts_total` - NATS publish attempts
- `router_nats_publish_errors_total` - NATS publish errors

### Process Health

**Check via Erlang shell**:
```erlang
%% Check if application is running
lists:keyfind(beamline_router, 1, application:which_applications()).

%% Check supervisor
whereis(beamline_router_sup).

%% Check circuit breaker
whereis(router_circuit_breaker).

%% Check policy store
whereis(router_policy_store).

%% Check NATS subscriber
whereis(router_nats_subscriber).
```

## Troubleshooting Guide

### Issue: Application Won't Start

**Symptoms**:
- Application fails to start
- Supervisor crashes
- Children fail to start

**Diagnosis**:
1. Check application logs for errors
2. Verify configuration is valid
3. Check port availability (9000, 9001)
4. Verify ETS table creation

**Solutions**:
- Fix configuration errors
- Free up ports if in use
- Check supervisor child specs
- Verify dependencies are available

### Issue: Circuit Breaker Not Working

**Symptoms**:
- Circuit breaker always closed
- Circuit breaker never opens
- Metrics not updating

**Diagnosis**:
1. Check circuit breaker process: `whereis(router_circuit_breaker)`
2. Check metrics: `router_r10_metrics:dump_metrics/0`
3. Verify configuration: `router_circuit_breaker:get_status/2`
4. Check for process crashes

**Solutions**:
- Restart circuit breaker: `router_test_utils:reset_circuit_breaker/0`
- Verify thresholds are configured correctly
- Check metrics emission
- Review circuit breaker logs

### Issue: NATS Connection Failures

**Symptoms**:
- NATS publish failures
- Connection errors in logs
- Messages not delivered

**Diagnosis**:
1. Check NATS server availability
2. Verify NATS URL configuration
3. Check TLS certificates (if enabled)
4. Review connection logs

**Solutions**:
- Verify NATS server is running
- Check NATS URL is correct
- Verify TLS certificates are valid
- Review NATS connection resilience settings

### Issue: Policy Not Found

**Symptoms**:
- `policy_not_found` errors
- Policies not loading
- Policy store errors

**Diagnosis**:
1. Check policy exists: `router_policy_store:get_policy/2`
2. Verify tenant_id is correct
3. Check policy store process: `whereis(router_policy_store)`
4. Review policy store logs

**Solutions**:
- Create policy via RouterAdmin API
- Verify tenant_id matches
- Restart policy store if needed
- Check policy store cache

### Issue: Rate Limiting Issues

**Symptoms**:
- Requests rejected with `rate_limit_exceeded`
- Rate limits too strict
- Rate limits not working

**Diagnosis**:
1. Check rate limiter process: `whereis(router_rate_limiter)`
2. Verify rate limit configuration
3. Check rate limit counters
4. Review rate limiter logs

**Solutions**:
- Adjust rate limit configuration
- Clear rate limit counters if needed
- Verify rate limiter is running
- Check for counter overflow

### Issue: Extension Pipeline Failures

**Symptoms**:
- Extension timeouts
- Extension errors
- Pipeline not executing

**Diagnosis**:
1. Check extension registry: `router_extension_registry:list/0`
2. Verify extension subjects
3. Check NATS connectivity for extensions
4. Review extension logs

**Solutions**:
- Verify extensions are registered
- Check extension subjects are correct
- Verify NATS connectivity
- Review extension timeout settings

## Incident Response Procedures

### Severity Levels

**P0 - Critical**:
- Service completely down
- All requests failing
- Data loss or corruption

**P1 - High**:
- Partial service degradation
- High error rates
- Performance degradation

**P2 - Medium**:
- Minor issues
- Non-critical errors
- Performance warnings

**P3 - Low**:
- Cosmetic issues
- Documentation updates
- Non-blocking improvements

### P0 Incident Response

**Immediate Actions**:
1. **Assess Impact**:
   - Check service health endpoints
   - Review error rates
   - Check circuit breaker states
   - Review recent deployments

2. **Mitigate**:
   - Enable fail-open mode if NATS unavailable: `application:set_env(beamline_router, nats_fail_open_mode, true)`
   - Disable CAF publishing if needed: `application:set_env(beamline_router, caf_push_assignment_enabled, false)`
   - Restart application if needed: `application:stop(beamline_router), application:start(beamline_router)`

3. **Investigate**:
   - Review application logs
   - Check supervisor status
   - Review metrics for anomalies
   - Check for recent configuration changes

4. **Communicate**:
   - Notify stakeholders
   - Update status page
   - Document incident timeline

### P1 Incident Response

**Actions**:
1. **Identify Affected Components**:
   - Check circuit breaker states
   - Review error rates by component
   - Check NATS connection status
   - Review policy store status

2. **Mitigate**:
   - Adjust circuit breaker thresholds if needed
   - Increase rate limits if needed
   - Restart affected components
   - Clear caches if needed

3. **Investigate**:
   - Review component logs
   - Check metrics for patterns
   - Review recent changes
   - Check for resource constraints

### Recovery Procedures

**Application Restart**:
```erlang
application:stop(beamline_router),
timer:sleep(1000),
application:start(beamline_router).
```

**Circuit Breaker Reset**:
```erlang
router_test_utils:reset_circuit_breaker().
```

**Policy Store Rebuild**:
```erlang
router_policy_store:rebuild_index().
```

**Rate Limiter Reset**:
```erlang
router_rate_limiter:clear_all_counters().
```

## Monitoring and Alerts

### Key Metrics to Monitor

**Request Metrics**:
- `router_grpc_requests_total` - Total requests
- `router_grpc_request_duration_seconds` - Request latency (p50, p95, p99)
- `router_grpc_errors_total` - Total errors

**Circuit Breaker Metrics**:
- `router_circuit_breaker_state` - Current states (closed/open/half_open)
- `router_circuit_breaker_state_transitions_total` - State transitions
- `router_circuit_breaker_trigger_reason` - Trigger reasons

**NATS Metrics**:
- `router_nats_publish_attempts_total` - Publish attempts
- `router_nats_publish_errors_total` - Publish errors
- `router_nats_publish_latency_seconds` - Publish latency

**Policy Metrics**:
- `router_policy_loads_total` - Policy loads
- `router_policy_cache_hits_total` - Cache hits
- `router_policy_cache_misses_total` - Cache misses

### Alert Rules

**High Error Rate**:
```
router_grpc_errors_total / router_grpc_requests_total > 0.05
```

**High Latency**:
```
histogram_quantile(0.95, router_grpc_request_duration_seconds) > 1.0
```

**Circuit Breaker Open**:
```
router_circuit_breaker_state{state="open"} > 0
```

**NATS Connection Failures**:
```
rate(router_nats_publish_errors_total[5m]) > 10
```

## Maintenance Procedures

### Regular Maintenance

**Daily**:
- Review error rates and latency
- Check circuit breaker states
- Review NATS connection status
- Check policy store health

**Weekly**:
- Review metrics trends
- Check for configuration drift
- Review audit logs
- Verify backup procedures

**Monthly**:
- Review performance metrics
- Check for resource constraints
- Review security updates
- Update documentation

### Backup Procedures

**Policy Store Backup**:
- Policies stored in PostgreSQL (if configured)
- ETS cache can be rebuilt from database
- Regular database backups required

**Configuration Backup**:
- Configuration stored in version control
- Environment variables documented
- Secrets stored in secure vault

## References

- `docs/R10_RUNBOOK.md` - R10 circuit breaker runbook
- `docs/OPERATIONAL_GUIDE.md` - Operational guide
- `docs/PROMETHEUS_ALERTS.md` - Prometheus alerts
- `test/R10_MAINTENANCE_CHECKLIST.md` - R10 maintenance checklist

