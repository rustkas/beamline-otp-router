# Router Alert Rules: MR Summary

**Type**: Feature / Observability  
**Component**: Router (JetStream/NATS)  
**Priority**: High  
**Status**: ✅ Ready for Review

## Summary

Implemented comprehensive alert rules for Router JetStream and NATS operations with production-tuned thresholds, proper routing labels, and complete documentation.

## Changes

### 1. Alert Rules File

**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml`

**Added**: 14 alert rules across 3 groups:
- **JetStream Alerts** (4 rules):
  - `RouterJetStreamHighRedeliveryRate` (warning)
  - `RouterJetStreamHighRedeliveryFromSource` (warning)
  - `RouterJetStreamMaxDeliverExhausted` (critical)
  - `RouterJetStreamGrowingRedeliveryQueue` (warning)

- **NATS Connection Alerts** (4 rules):
  - `RouterNatsFrequentReconnects` (warning)
  - `RouterNatsConnectionFailures` (critical)
  - `RouterNatsReconnectionExhausted` (critical)
  - `RouterNatsHighReconnectFailureRate` (warning)

- **NATS Operation Alerts** (6 rules):
  - `RouterNatsHighPublishFailureRate` (warning)
  - `RouterNatsHighPublishWithAckFailureRate` (warning)
  - `RouterNatsHighAckFailureRate` (critical)
  - `RouterNatsHighNakFailureRate` (warning)
  - `RouterNatsHighSubscribeFailureRate` (critical)
  - `RouterNatsPendingOperationsQueueFull` (warning)

**Key Features**:
- All alerts use real metrics from `router_jetstream.erl` and `router_nats.erl`
- Production-tuned thresholds (reduced false positives)
- Proper routing labels (`team: platform`, `service: router`, `component`, `severity`)
- Comprehensive annotations with runbook links

### 2. Thresholds Tuning

**Rationale**: Adjusted thresholds for production stability while maintaining effective monitoring.

**Changes**:
- Warning alerts: Duration increased to 10-15m (from 5-10m)
- Critical alerts: Duration optimized to 1-5m (faster response)
- Failure rate thresholds: Lowered to 1-2% (from 5%) for more sensitive detection
- Queue thresholds: Increased to 1000 ops (from 500) to reduce false positives

**Details**: See `docs/archive/dev/ALERT_THRESHOLDS_REVIEW.md`

### 3. Routing Labels

**Added to all alerts**:
- `service: router`
- `component: jetstream | nats`
- `team: platform`
- `severity: warning | critical`

**External labels** (via Prometheus `external_labels`):
- `env: staging | production`
- `cluster: cluster-name`

### 4. Smoke Test Script

**File**: `apps/otp/router/scripts/smoke_test_alerts.sh`

**Features**:
- Tests 3 critical scenarios:
  1. MaxDeliver exhaustion (critical alert)
  2. NATS connection failure (critical alert)
  3. High redelivery rate (warning alert)
- Verifies alerts fire in Alertmanager
- Checks alert labels
- Provides manual trigger instructions

### 5. Documentation

**Created**:
- `docs/observability/PROMETHEUS_SETUP.md` - Prometheus configuration guide
- `docs/observability/ALERT_ROUTING_VERIFICATION.md` - Routing verification guide
- `docs/archive/dev/ALERT_THRESHOLDS_REVIEW.md` - Threshold tuning documentation
- `docs/observability/QUICK_START.md` - Quick reference guide
- `docs/archive/dev/ALERT_SETUP_COMPLETE.md` - Complete setup report

### 6. Prometheus Configuration

**File**: `tools/observability/prometheus.yml`

**Changes**:
- Changed `environment` → `env` for consistency
- Added `rule_files` section for alert rules
- Documented external labels usage

## Testing

### Validation

- ✅ YAML syntax validated
- ✅ Prometheus syntax validated (promtool)
- ✅ All 14 alerts present
- ✅ All alerts use correct metric names
- ✅ All alerts have routing labels
- ✅ Smoke test script syntax validated

### Staging Testing Required

Before merging to production:
1. Deploy to staging environment
2. Run smoke test: `./scripts/smoke_test_alerts.sh`
3. Verify alerts fire correctly
4. Verify routing to Slack/email/PagerDuty
5. Monitor for false positives (adjust thresholds if needed)

## Deployment Plan

### Staging

1. Update Prometheus config with staging `external_labels`
2. Reload Prometheus: `curl -X POST http://prometheus-staging:9090/-/reload`
3. Run smoke test
4. Verify alert routing
5. Monitor for 24-48 hours

### Production

1. Update Prometheus config with production `external_labels`
2. Configure Alertmanager routing (PagerDuty for critical)
3. Deploy during low-traffic window
4. Monitor alert frequency
5. Adjust thresholds based on production experience

## Rollback Plan

If issues occur:
1. Remove `rule_files` from Prometheus config
2. Reload Prometheus: `curl -X POST http://prometheus:9090/-/reload`
3. Alerts will stop firing immediately
4. No impact on metrics collection

**See**: `docs/archive/dev/ALERT_ROLLBACK_CHECKLIST.md`

## Impact

### Positive
- Early detection of JetStream/NATS issues
- Reduced mean time to detection (MTTD)
- Proper alert routing to on-call team
- Comprehensive documentation for operations

### Risks
- False positives if thresholds need adjustment
- Alert fatigue if not properly tuned
- **Mitigation**: Start with conservative thresholds, adjust based on production experience

## Metrics Used

All alerts use existing metrics from:
- `router_jetstream.erl`: `router_jetstream_redelivery_total`, `router_jetstream_maxdeliver_exhausted_total`, `router_jetstream_ack_total`
- `router_nats.erl`: `router_nats_connection_*`, `router_nats_publish_*`, `router_nats_ack_*`, etc.

**No new metrics required** - all metrics already exist in codebase.

## Related Issues/PRs

- Implements alerting requirements for Router JetStream/NATS operations
- Addresses observability gaps identified in CP2 planning

## Reviewers

- @platform-team - For alert routing and thresholds
- @router-team - For metric usage and alert logic
- @sre-team - For production deployment and monitoring

## Checklist

- [x] Alert rules file created and validated
- [x] Thresholds tuned for production
- [x] Routing labels added
- [x] Smoke test script created
- [x] Documentation created
- [x] Prometheus config updated
- [ ] Staging deployment tested
- [ ] Alert routing verified in staging
- [ ] Production deployment plan reviewed

## Files Changed

### Modified
- `apps/otp/router/docs/observability/router-alert-rules.yaml` (updated thresholds and labels)
- `tools/observability/prometheus.yml` (added rule_files and external_labels)

### Created
- `apps/otp/router/scripts/smoke_test_alerts.sh`
- `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md`
- `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`
- `apps/otp/router/docs/observability/ALERT_ROUTING_VERIFICATION.md`
- `apps/otp/router/docs/dev/ALERT_SETUP_COMPLETE.md`
- `apps/otp/router/docs/observability/QUICK_START.md`

## References

- Alert rules: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- Setup guide: `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`
- Quick start: `apps/otp/router/docs/observability/QUICK_START.md`
- Rollback plan: `apps/otp/router/docs/dev/ALERT_ROLLBACK_CHECKLIST.md`

