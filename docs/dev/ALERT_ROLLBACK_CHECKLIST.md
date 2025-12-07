# Alert Rules Rollback Checklist

**Purpose**: Step-by-step rollback plan if alert rules cause issues in production.

## Quick Rollback (5 minutes)

### Step 1: Remove Alert Rules from Prometheus

**Option A: Remove rule_files from config**

Edit `prometheus.yml`:
```yaml
# Comment out or remove:
# rule_files:
#   - "apps/otp/router/docs/observability/router-alert-rules.yaml"
```

**Option B: Move/rename alert rules file**

```bash
# Move alert rules file (Prometheus won't find it)
mv apps/otp/router/docs/observability/router-alert-rules.yaml \
   apps/otp/router/docs/observability/router-alert-rules.yaml.disabled
```

### Step 2: Reload Prometheus

```bash
# Reload Prometheus configuration
curl -X POST http://prometheus:9090/-/reload

# Or restart Prometheus service
systemctl reload prometheus
# or
docker-compose restart prometheus
```

### Step 3: Verify Alerts Stopped

```bash
# Check Prometheus rules (should be empty for router groups)
curl http://prometheus:9090/api/v1/rules | \
  jq '.data.groups[] | select(.name | contains("router"))'

# Check Alertmanager (alerts should resolve)
curl http://alertmanager:9093/api/v2/alerts | \
  jq '.[] | select(.labels.service == "router")'
```

**Expected**: No router alerts should be firing after reload.

## Detailed Rollback Steps

### Pre-Rollback Verification

1. **Identify the issue**:
   - [ ] Too many false positives?
   - [ ] Alerts not firing when they should?
   - [ ] Alert routing issues?
   - [ ] Performance impact?

2. **Document current state**:
   ```bash
   # Save current alert state
   curl http://alertmanager:9093/api/v2/alerts | \
     jq '.[] | select(.labels.service == "router")' > /tmp/router_alerts_before_rollback.json
   
   # Save Prometheus rules
   curl http://prometheus:9090/api/v1/rules > /tmp/prometheus_rules_before_rollback.json
   ```

### Rollback Execution

#### Option 1: Complete Rollback (Remove All Rules)

**Time**: ~5 minutes  
**Impact**: All Router alerts stop firing

1. **Remove rule_files from Prometheus config**:
   ```yaml
   # prometheus.yml
   # rule_files:
   #   - "apps/otp/router/docs/observability/router-alert-rules.yaml"
   ```

2. **Reload Prometheus**:
   ```bash
   curl -X POST http://prometheus:9090/-/reload
   ```

3. **Verify**:
   ```bash
   # Should return empty or no router groups
   curl http://prometheus:9090/api/v1/rules | \
     jq '.data.groups[] | select(.name | contains("router"))'
   ```

#### Option 2: Selective Rollback (Disable Specific Alerts)

**Time**: ~10 minutes  
**Impact**: Only problematic alerts disabled

1. **Edit alert rules file**:
   ```yaml
   # Comment out problematic alert
   # - alert: RouterJetStreamHighRedeliveryRate
   #   expr: ...
   ```

2. **Reload Prometheus**:
   ```bash
   curl -X POST http://prometheus:9090/-/reload
   ```

#### Option 3: Threshold Adjustment (Not Rollback, but Fix)

**Time**: ~15 minutes  
**Impact**: Alerts still fire but with adjusted thresholds

1. **Edit alert rules file**:
   ```yaml
   # Increase threshold to reduce false positives
   - alert: RouterJetStreamHighRedeliveryRate
     expr: |
       ... > 0.15  # Changed from 0.1
     for: 15m      # Changed from 10m
   ```

2. **Reload Prometheus**:
   ```bash
   curl -X POST http://prometheus:9090/-/reload
   ```

### Post-Rollback Verification

1. **Check Prometheus**:
   ```bash
   # Verify rules are removed/adjusted
   curl http://prometheus:9090/api/v1/rules | \
     jq '.data.groups[] | select(.name | contains("router"))'
   ```

2. **Check Alertmanager**:
   ```bash
   # Verify alerts are resolved
   curl http://alertmanager:9093/api/v2/alerts | \
     jq '.[] | select(.labels.service == "router" and .status.state == "active")'
   ```

3. **Check Notifications**:
   - [ ] Verify no new alerts sent to Slack/email/PagerDuty
   - [ ] Check that existing alerts are resolved
   - [ ] Confirm alert fatigue stops

4. **Monitor Metrics**:
   ```bash
   # Metrics should still be collected (rollback doesn't affect metrics)
   curl 'http://prometheus:9090/api/v1/query?query=router_jetstream_ack_total'
   ```

## Rollback Scenarios

### Scenario 1: Too Many False Positives

**Symptoms**:
- Alerts firing constantly
- Alert fatigue in on-call team
- Alerts resolving quickly without action

**Action**:
1. Use **Option 3** (Threshold Adjustment)
2. Increase thresholds by 50-100%
3. Increase `for:` duration by 50%
4. Monitor for 24 hours
5. Adjust further if needed

**Example**:
```yaml
# Before (too sensitive)
- alert: RouterJetStreamHighRedeliveryRate
  expr: ... > 0.1
  for: 10m

# After (less sensitive)
- alert: RouterJetStreamHighRedeliveryRate
  expr: ... > 0.2  # Doubled threshold
  for: 20m          # Doubled duration
```

### Scenario 2: Alerts Not Firing

**Symptoms**:
- Real issues occur but no alerts
- Metrics show problems but alerts don't fire

**Action**:
1. Check alert expressions in Prometheus UI
2. Verify metrics are being collected
3. Use **Option 2** (Selective Rollback) to disable problematic alerts
4. Investigate root cause (metric names, label matching, etc.)

### Scenario 3: Alert Routing Issues

**Symptoms**:
- Alerts fire but don't reach correct channels
- Wrong team receives alerts
- Alerts go to wrong Slack channel

**Action**:
1. **DO NOT rollback alert rules** (rules are fine)
2. Fix Alertmanager routing configuration
3. Verify labels match routing rules
4. Test with smoke test script

### Scenario 4: Performance Impact

**Symptoms**:
- Prometheus CPU/memory usage increased
- Slow query performance
- Alert evaluation taking too long

**Action**:
1. Check Prometheus metrics:
   ```bash
   curl 'http://prometheus:9090/api/v1/query?query=prometheus_rule_evaluation_duration_seconds'
   ```
2. If impact is significant, use **Option 1** (Complete Rollback)
3. Investigate rule complexity
4. Optimize alert expressions
5. Re-deploy with optimized rules

## Recovery After Rollback

### If Rollback Was Due to False Positives

1. **Analyze false positive patterns**:
   - What conditions triggered false positives?
   - What time of day?
   - What traffic patterns?

2. **Adjust thresholds**:
   - Increase thresholds by 50-100%
   - Increase `for:` duration
   - Add additional conditions to reduce noise

3. **Re-deploy**:
   - Deploy to staging first
   - Run smoke test
   - Monitor for 48 hours
   - Deploy to production

### If Rollback Was Due to Missing Alerts

1. **Investigate root cause**:
   - Check metric names match code
   - Verify label matching
   - Test alert expressions in Prometheus UI

2. **Fix alert rules**:
   - Update metric names if needed
   - Fix label matching
   - Adjust expressions

3. **Re-deploy**:
   - Deploy to staging first
   - Run smoke test
   - Verify alerts fire correctly
   - Deploy to production

## Prevention

### Before Production Deployment

1. **Staging Testing**:
   - [ ] Run smoke test in staging
   - [ ] Monitor for 48 hours
   - [ ] Verify no false positives
   - [ ] Verify alerts fire when expected

2. **Threshold Review**:
   - [ ] Review thresholds with SRE team
   - [ ] Compare with production traffic patterns
   - [ ] Adjust if needed

3. **Routing Verification**:
   - [ ] Test alert routing in staging
   - [ ] Verify Slack/email/PagerDuty integration
   - [ ] Confirm correct team receives alerts

### Monitoring After Deployment

1. **First 24 Hours**:
   - Monitor alert frequency
   - Track false positive rate
   - Collect team feedback

2. **First Week**:
   - Review all alerts fired
   - Identify patterns
   - Adjust thresholds if needed

3. **Ongoing**:
   - Monthly review of alert effectiveness
   - Adjust based on SLO requirements
   - Remove unused alerts

## Emergency Contacts

- **Platform Team**: @platform-team (Slack)
- **SRE On-Call**: Check PagerDuty schedule
- **Router Team**: @router-team (Slack)

## References

- Alert rules: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- Thresholds review: `apps/otp/router/docs/dev/ALERT_THRESHOLDS_REVIEW.md`
- Setup guide: `apps/otp/router/docs/observability/PROMETHEUS_SETUP.md`
- Quick start: `apps/otp/router/docs/observability/QUICK_START.md`

