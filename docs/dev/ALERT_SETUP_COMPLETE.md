# Alert Setup Complete Report

**Date**: 2025-11-30  
**Status**: ✅ **All Steps Complete**  
**Purpose**: Summary of alert rules setup, threshold tuning, and verification

## Summary

All alert rules have been reviewed, thresholds adjusted for production stability, routing labels added, and verification scripts created.

## Completed Tasks

### ✅ 1. Alert Rules File Verified

**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml`

**Verification**:
- ✓ YAML syntax is valid
- ✓ Prometheus syntax validated (if promtool available)
- ✓ All 14 required alerts present
- ✓ All 3 alert groups present (router-jetstream, router-nats-connection, router-nats-operations)

**Alert Groups**:
- `router-jetstream.rules`: 4 alerts
- `router-nats-connection.rules`: 4 alerts
- `router-nats-operations.rules`: 6 alerts

**Total**: 14 alerts

### ✅ 2. Thresholds Adjusted

All thresholds have been tuned for production stability:

**Key Changes**:
- Warning alerts: Duration increased to 10-15m (reduced false positives)
- Critical alerts: Duration optimized to 1-5m (faster response)
- Thresholds: Lowered for more sensitive detection (2% instead of 5% for failures)
- Queue thresholds: Increased to reduce false positives (1000 instead of 500)

**Details**: See `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md`

### ✅ 3. Routing Labels Added

All alerts now include:
- ✓ `service: router` - Service identifier
- ✓ `component: jetstream | nats` - Component identifier
- ✓ `team: platform` - Team for Alertmanager routing
- ✓ `severity: warning | critical` - Severity level

**External Labels** (added by Prometheus):
- `env: staging | production` - Environment identifier
- `cluster: cluster-name` - Cluster identifier

**Configuration**: See `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`

### ✅ 4. Smoke Test Script Created

**File**: `apps/otp/router/scripts/smoke_test_alerts.sh`

**Features**:
- Tests 3 key alert scenarios:
  1. MaxDeliver exhaustion (critical)
  2. NATS connection failure (critical)
  3. High redelivery rate (warning)
- Verifies alerts fire in Alertmanager
- Checks alert labels
- Provides manual trigger instructions

**Usage**:
```bash
./scripts/smoke_test_alerts.sh \
  --router-url http://router-staging:9001 \
  --prometheus-url http://prometheus-staging:9090 \
  --alertmanager-url http://alertmanager-staging:9093 \
  --wait-time 120
```

### ✅ 5. Documentation Created

**Files Created**:
1. `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md`
   - Complete threshold change log
   - Production tuning guidelines
   - SLO-based tuning recommendations

2. `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`
   - Prometheus configuration guide
   - External labels setup
   - Alertmanager routing configuration
   - Environment-specific setup

3. `apps/otp/router/docs/observability/ALERT_ROUTING_VERIFICATION.md`
   - Step-by-step verification guide
   - Label checking commands
   - Common issues and solutions
   - Testing procedures

### ✅ 6. Prometheus Configuration Updated

**File**: `tools/observability/prometheus.yml`

**Changes**:
- Changed `environment` → `env` for consistency
- Added `rule_files` section for alert rules
- Documented external labels usage

## Next Steps for Deployment

### 1. Staging Deployment

1. **Update Prometheus config** in staging:
   ```yaml
   global:
     external_labels:
       cluster: 'staging-cluster'
       env: 'staging'
   
   rule_files:
     - "apps/otp/router/docs/observability/router-alert-rules.yaml"
   ```

2. **Reload Prometheus**:
   ```bash
   curl -X POST http://prometheus-staging:9090/-/reload
   ```

3. **Run smoke test**:
   ```bash
   ./scripts/smoke_test_alerts.sh \
     --router-url http://router-staging:9001 \
     --prometheus-url http://prometheus-staging:9090 \
     --alertmanager-url http://alertmanager-staging:9093
   ```

4. **Verify routing**:
   - Check Slack channel receives alerts
   - Verify alert labels are correct
   - Confirm no false positives

### 2. Production Deployment

1. **Update Prometheus config** in production:
   ```yaml
   global:
     external_labels:
       cluster: 'production-cluster'
       env: 'production'
   
   rule_files:
     - "apps/otp/router/docs/observability/router-alert-rules.yaml"
   ```

2. **Configure Alertmanager routing**:
   - Critical alerts → PagerDuty
   - Warning alerts → Slack
   - Production alerts → On-call rotation

3. **Monitor alert frequency**:
   - Track false positive rate
   - Adjust thresholds if needed
   - Document SLO violations

### 3. Ongoing Maintenance

1. **Monitor alert health**:
   - Track alert firing frequency
   - Review false positive rate
   - Adjust thresholds based on SLOs

2. **Review alert effectiveness**:
   - Are alerts catching real issues?
   - Are alerts actionable?
   - Are runbooks up to date?

3. **Update thresholds**:
   - Based on production experience
   - Based on SLO requirements
   - Based on team feedback

## Verification Checklist

- [x] Alert rules file syntax valid
- [x] All 14 alerts present
- [x] Thresholds adjusted for production
- [x] Routing labels added (team, service, component)
- [x] External labels documented (env, cluster)
- [x] Smoke test script created
- [x] Documentation created
- [x] Prometheus config updated
- [ ] Staging deployment tested
- [ ] Alert routing verified in staging
- [ ] Production deployment ready

## Files Modified/Created

### Modified
- `apps/otp/router/docs/observability/router-alert-rules.yaml` - Thresholds and labels updated
- `tools/observability/prometheus.yml` - External labels and rule_files added

### Created
- `apps/otp/router/scripts/smoke_test_alerts.sh` - Smoke test script
- `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md` - Threshold review
- `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md` - Setup guide
- `apps/otp/router/docs/observability/ALERT_ROUTING_VERIFICATION.md` - Verification guide
- `apps/otp/router/docs/dev/ALERT_SETUP_COMPLETE.md` - This file

## References

- Alert rules: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- Thresholds review: `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md`
- Prometheus setup: `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`
- Routing verification: `apps/otp/router/docs/observability/ALERT_ROUTING_VERIFICATION.md`
- Smoke test: `apps/otp/router/scripts/smoke_test_alerts.sh`
- Alert validation: `apps/otp/router/scripts/check_alert_rules.sh`

