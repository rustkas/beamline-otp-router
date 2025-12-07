# NATS Publish Failure Metrics & Alerts - SRE Review Template

## Purpose

This document provides a template for SRE team review of metrics and alerts for `router_nats` publish and publish_with_ack failures.

## Review Checklist

### 1. Alert Thresholds

**Document**: `apps/otp/router/docs/NATS_PUBLISH_FAILURE_METRICS_ALERTS.md`

**Questions for SRE**:

- [ ] **Critical Alert Threshold**: Is 10% failure rate appropriate for `router_nats_publish_with_ack_failures_total`?
  - Current recommendation: `rate(...) / rate(...) > 0.1` for 5 minutes
  - Alternative suggestions: ___________

- [ ] **Warning Alert Threshold**: Is 5% failure rate appropriate?
  - Current recommendation: `rate(...) / rate(...) > 0.05` for 10 minutes
  - Alternative suggestions: ___________

- [ ] **Queue Full Threshold**: Is 1000 operations appropriate?
  - Current recommendation: `router_nats_pending_operations_count >= 1000` for 2 minutes
  - Alternative suggestions: ___________

- [ ] **Queue High Threshold**: Is 80% capacity (800 operations) appropriate?
  - Current recommendation: `router_nats_pending_operations_count >= 800` for 5 minutes
  - Alternative suggestions: ___________

- [ ] **Connection Down Duration**: Is 1 minute appropriate?
  - Current recommendation: `router_nats_connection_status == 0` for 1 minute
  - Alternative suggestions: ___________

**SRE Feedback**:
```
[Space for SRE comments]
```

### 2. Dashboard Recommendations

**Document**: `apps/otp/router/docs/NATS_PUBLISH_FAILURE_METRICS_ALERTS.md`

**Questions for SRE**:

- [ ] Are the recommended dashboard panels sufficient?
  - Current panels: Failure rates, connection status, queue metrics, retry success rate
  - Additional panels needed: ___________

- [ ] Are the PromQL queries correct?
  - Review queries in dashboard section
  - Corrections needed: ___________

- [ ] Should we add breakdown by error reason?
  - Requires metric labels (see enhancement plan)
  - Priority: High / Medium / Low

**SRE Feedback**:
```
[Space for SRE comments]
```

### 3. Operational Procedures

**Document**: `apps/otp/router/docs/NATS_PUBLISH_FAILURE_METRICS_ALERTS.md`

**Questions for SRE**:

- [ ] Are the operational procedures complete?
  - Current procedures: Connection status check, failure rate check, logs review, NATS server check, queue status check
  - Additional procedures needed: ___________

- [ ] Are the diagnostic commands correct?
  - Review commands in operational procedures section
  - Corrections needed: ___________

- [ ] Should we add runbook integration?
  - Link to operational runbooks
  - Priority: High / Medium / Low

**SRE Feedback**:
```
[Space for SRE comments]
```

### 4. Metric Labels Enhancement

**Document**: `apps/otp/router/docs/dev/NATS_PUBLISH_FAILURE_METRICS_ENHANCEMENT_PLAN.md`

**Questions for SRE**:

- [ ] Are the proposed labels appropriate?
  - Proposed: `reason`, `error_type`, `mode`
  - Additional labels needed: ___________

- [ ] What label values should be supported?
  - `reason`: nats_unavailable, timeout, connection_closed, not_connected
  - `error_type`: operation_error, timeout, connection_lost
  - `mode`: fail_open, queueing
  - Additional values needed: ___________

- [ ] What is the priority for label implementation?
  - Priority: High / Medium / Low
  - Timeline: ___________

**SRE Feedback**:
```
[Space for SRE comments]
```

### 5. Alert Routing and Notification

**Questions for SRE**:

- [ ] Who should receive critical alerts?
  - On-call engineer: ___________
  - Escalation path: ___________

- [ ] Who should receive warning alerts?
  - Team channel: ___________
  - Escalation path: ___________

- [ ] What notification channels should be used?
  - PagerDuty: ___________
  - Slack: ___________
  - Email: ___________

**SRE Feedback**:
```
[Space for SRE comments]
```

### 6. Integration with Existing Monitoring

**Questions for SRE**:

- [ ] How should these alerts integrate with existing NATS monitoring?
  - Existing NATS alerts: ___________
  - Integration approach: ___________

- [ ] Should we create a dedicated dashboard or integrate into existing?
  - Dedicated dashboard: Yes / No
  - Existing dashboard: ___________

- [ ] Are there existing runbooks we should reference?
  - Runbook links: ___________

**SRE Feedback**:
```
[Space for SRE comments]
```

## Review Sign-off

**Reviewer**: ___________

**Date**: ___________

**Status**: 
- [ ] Approved - Ready for implementation
- [ ] Approved with changes - See feedback above
- [ ] Needs revision - Major changes required

**Next Steps**:
```
[Space for next steps]
```

## Action Items

After review, create action items:

1. [ ] Update alert thresholds based on SRE feedback
2. [ ] Update dashboard recommendations
3. [ ] Update operational procedures
4. [ ] Prioritize metric labels enhancement
5. [ ] Integrate with existing monitoring
6. [ ] Create/update runbooks

## References

- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_METRICS_ALERTS.md` - Metrics and alerts documentation
- `apps/otp/router/docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Behavior specification
- `apps/otp/router/docs/dev/NATS_PUBLISH_FAILURE_METRICS_ENHANCEMENT_PLAN.md` - Enhancement plan
- `apps/otp/router/docs/NATS_CONNECTION_RESILIENCE.md` - Connection resilience documentation

