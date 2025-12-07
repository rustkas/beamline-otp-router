# Router Incident Response Procedures

**Version**: 1.0  
**Last Updated**: 2025-01-27

## Overview

This document provides standardized incident response procedures for Router service incidents.

## Incident Severity Levels

### P0 - Critical

**Definition**: Complete service outage or data loss  
**Response Time**: Immediate  
**Examples**:
- All requests failing
- Application crash
- Data corruption

### P1 - High

**Definition**: Significant service degradation  
**Response Time**: < 15 minutes  
**Examples**:
- High error rate (> 5%)
- Circuit breakers open for multiple providers
- NATS connection lost

### P2 - Medium

**Definition**: Moderate service impact  
**Response Time**: < 1 hour  
**Examples**:
- Increased latency
- Single provider circuit breaker open
- Rate limiting issues

### P3 - Low

**Definition**: Minor service impact  
**Response Time**: < 4 hours  
**Examples**:
- Non-critical metrics issues
- Configuration questions
- Performance optimization

## Incident Response Workflow

### 1. Detection

**Sources**:
- Monitoring alerts
- User reports
- Log analysis
- Metrics anomalies

**Initial Assessment**:
1. Check service status
2. Review recent metrics
3. Check logs for errors
4. Determine severity level

### 2. Triage

**Immediate Actions**:
1. **Check Application Status**:
   ```erlang
   application:which_applications().
   supervisor:which_children(beamline_router_sup).
   ```

2. **Check Error Metrics**:
   ```erlang
   router_metrics:get_metric_value(router_grpc_errors_total, #{}).
   router_r10_metrics:dump_metrics().
   ```

3. **Check Logs**:
   ```bash
   tail -100 /var/log/router/router.log | grep ERROR
   ```

4. **Check Process Health**:
   ```erlang
   whereis(router_circuit_breaker).
   whereis(router_nats).
   is_process_alive(whereis(router_circuit_breaker)).
   ```

### 3. Containment

**Immediate Containment**:
- Isolate affected components
- Enable fail-open mode if needed
- Disable problematic features
- Scale resources if needed

**Examples**:
- Disable circuit breaker for specific provider
- Increase rate limits temporarily
- Enable NATS fail-open mode

### 4. Resolution

**Resolution Steps**:
1. Identify root cause
2. Implement fix
3. Verify fix
4. Monitor recovery

**Common Fixes**:
- Restart failed processes
- Clear stuck state
- Adjust configuration
- Rollback changes

### 5. Recovery

**Recovery Verification**:
1. Check error rates return to normal
2. Verify all processes running
3. Confirm metrics are healthy
4. Monitor for 15-30 minutes

### 6. Post-Incident

**Post-Incident Actions**:
1. Document incident
2. Root cause analysis
3. Update runbooks
4. Implement preventive measures

## Common Incident Scenarios

### Scenario 1: Complete Service Outage

**Symptoms**:
- All requests failing
- Application not responding
- Processes crashed

**Response**:
1. Check supervisor status
2. Review crash logs
3. Restart application
4. Verify recovery
5. Investigate root cause

### Scenario 2: High Error Rate

**Symptoms**:
- Error rate > 5%
- Multiple providers failing
- Circuit breakers open

**Response**:
1. Check circuit breaker states
2. Review provider health
3. Check NATS connectivity
4. Review recent changes
5. Implement fixes

### Scenario 3: Performance Degradation

**Symptoms**:
- High latency
- Timeout errors
- Backpressure active

**Response**:
1. Check latency metrics
2. Review ETS table sizes
3. Check CPU/memory usage
4. Review processing queues
5. Optimize or scale

### Scenario 4: NATS Connection Lost

**Symptoms**:
- NATS publish failures
- Connection state = disconnected
- JetStream consumers stopped

**Response**:
1. Check NATS server status
2. Verify network connectivity
3. Check NATS configuration
4. Manually reconnect
5. Verify recovery

## Escalation Procedures

### Level 1: On-Call Engineer

**Responsibilities**:
- Initial triage
- Basic troubleshooting
- Incident documentation

**Escalation Triggers**:
- Cannot resolve in 30 minutes
- Requires code changes
- Data loss risk

### Level 2: Senior Engineer

**Responsibilities**:
- Advanced troubleshooting
- Code changes
- Configuration changes

**Escalation Triggers**:
- Cannot resolve in 2 hours
- Requires architecture changes
- Multiple services affected

### Level 3: Engineering Lead

**Responsibilities**:
- Architecture decisions
- Resource allocation
- Post-incident review

## Communication

### Internal Communication

- **Slack Channel**: #router-incidents
- **Status Page**: Update during incidents
- **Email**: Notify team for P0/P1 incidents

### External Communication

- **Status Page**: Public status updates
- **Customer Notifications**: For P0/P1 incidents affecting customers

## Incident Documentation Template

```markdown
## Incident: [Title]

**Date**: YYYY-MM-DD
**Severity**: P0/P1/P2/P3
**Duration**: HH:MM
**Status**: Resolved/Investigating

### Summary
[Brief description]

### Timeline
- HH:MM: Detection
- HH:MM: Triage started
- HH:MM: Root cause identified
- HH:MM: Fix implemented
- HH:MM: Resolved

### Root Cause
[Root cause analysis]

### Resolution
[Steps taken to resolve]

### Prevention
[Preventive measures implemented]
```

## See Also

- [Operational Runbook](./OPERATIONAL_RUNBOOK.md)
- [Troubleshooting Guide](./TROUBLESHOOTING_GUIDE.md)
- [Architecture Documentation](./ARCHITECTURE_DOCUMENTATION.md)

