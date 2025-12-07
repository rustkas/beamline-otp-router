# Alert Rules Implementation Complete

**Date**: 2025-11-30  
**Status**: ✅ **Complete**  
**Purpose**: Implementation of Prometheus alert rules for Router JetStream and NATS monitoring

## Executive Summary

Complete set of Prometheus alert rules has been created for monitoring Router JetStream and NATS operations. All alerts are based on actual metrics exported by `router_jetstream` and `router_nats` modules. Validation scripts and testing tools have been created to ensure alert rules are properly configured and working.

## Implementation

### 1. Alert Rules File

**File**: `apps/otp/router/docs/observability/router-alert-rules.yaml`

**Structure**:
- 3 alert groups
- 14 total alerts
- All alerts use real metrics from code

**Alert Groups**:

1. **router-jetstream.rules** (4 alerts):
   - `RouterJetStreamHighRedeliveryRate` - High redelivery rate (>10%)
   - `RouterJetStreamHighRedeliveryFromSource` - High redelivery from specific source (>5/sec)
   - `RouterJetStreamMaxDeliverExhausted` - Messages exhausting MaxDeliver (>0.1/sec, critical)
   - `RouterJetStreamGrowingRedeliveryQueue` - Growing redelivery queue (not recovering)

2. **router-nats-connection.rules** (4 alerts):
   - `RouterNatsFrequentReconnects` - Frequent reconnects (>0.5/sec)
   - `RouterNatsConnectionFailures` - Connection failures (critical)
   - `RouterNatsReconnectionExhausted` - Reconnection attempts exhausted (critical)
   - `RouterNatsHighReconnectFailureRate` - High reconnect failure rate (>50%)

3. **router-nats-operations.rules** (6 alerts):
   - `RouterNatsHighPublishFailureRate` - High publish failure rate (>5%)
   - `RouterNatsHighPublishWithAckFailureRate` - High publish_with_ack failure rate (>5%)
   - `RouterNatsHighAckFailureRate` - High ACK failure rate (>5%, critical)
   - `RouterNatsHighNakFailureRate` - High NAK failure rate (>5%)
   - `RouterNatsHighSubscribeFailureRate` - High subscribe failure rate (>10%, critical)
   - `RouterNatsPendingOperationsQueueFull` - Pending operations queue full (>500)

### 2. Validation Scripts

**Created Scripts**:

1. **`scripts/check_alert_rules.sh`** (enhanced):
   - Validates YAML syntax
   - Checks all 14 alerts are present
   - Verifies metric usage
   - Checks severity levels
   - Validates runbook links
   - Supports `--promtool` and `--verbose` flags

2. **`scripts/validate_alert_rules_promtool.sh`** (new):
   - Validates Prometheus syntax using `promtool`
   - Checks alert expressions are valid
   - Requires `promtool` to be installed

3. **`scripts/test_alert_rules.sh`** (new):
   - Tests alert rules in staging environment
   - Validates YAML and Prometheus syntax
   - Checks Prometheus connectivity
   - Verifies rules are loaded
   - Checks Alertmanager connectivity
   - Verifies metrics are queryable
   - Supports `--prometheus-url` and `--alertmanager-url` flags

### 3. Runbook Links

**Updated**: All runbook links in alert rules now point to:
- `../../docs/OPS_RUNBOOK_ROUTER_INTAKE.md` with appropriate anchors

**Runbook Sections Referenced**:
- DLQ Growth: `#dlq-growth`
- Common Symptoms: `#common-symptoms`
- Backpressure Active: `#backpressure-active`
- NATS Connection Failures: `#nats-connection-failures`

## Metrics Used

### JetStream Metrics (from `router_jetstream.erl`):
- `router_jetstream_ack_total` - Counter of successful ACKs
- `router_jetstream_redelivery_total` - Counter of redeliveries (with labels: `assignment_id`, `request_id`, `reason`, `source`)
- `router_dlq_total` - Counter of messages sent to DLQ (MaxDeliver exhausted)

### NATS Metrics (from `router_nats.erl`):
- `router_nats_connection_*` - Connection metrics (established, failures, lost, restored, status)
- `router_nats_reconnect_*` - Reconnect metrics (attempts, failures, exhausted)
- `router_nats_publish_*` - Publish metrics (total, failures)
- `router_nats_publish_with_ack_*` - Publish with ACK metrics (total, failures)
- `router_nats_ack_*` - ACK metrics (total, failures)
- `router_nats_nak_*` - NAK metrics (total, failures)
- `router_nats_subscribe_*` - Subscribe metrics (total, failures)
- `router_nats_pending_operations_count` - Gauge of pending operations queue size

## Thresholds

**Default Thresholds** (can be adjusted based on traffic patterns):

- **Redelivery Rate**: 10% (warning)
- **Redelivery from Source**: 5/sec (warning)
- **MaxDeliver Exhausted**: 0.1/sec (critical)
- **Reconnects**: 0.5/sec (warning)
- **Connection Failures**: 0.1/sec (critical)
- **Publish/ACK/NAK Failures**: 5% (warning/critical)
- **Subscribe Failures**: 10% (critical)
- **Pending Queue**: 500 operations (warning)

## Usage

### Validate Alert Rules

```bash
# Basic validation
./scripts/check_alert_rules.sh

# With Prometheus syntax validation (requires promtool)
./scripts/check_alert_rules.sh --promtool

# Verbose output
./scripts/check_alert_rules.sh --verbose
```

### Validate with promtool

```bash
# Requires promtool to be installed
./scripts/validate_alert_rules_promtool.sh
```

### Test in Staging

```bash
# Default URLs (localhost:9090, localhost:9093)
./scripts/test_alert_rules.sh

# Custom URLs
./scripts/test_alert_rules.sh \
  --prometheus-url http://prometheus.example.com:9090 \
  --alertmanager-url http://alertmanager.example.com:9093
```

## Integration

### Prometheus Configuration

Add to `prometheus.yml`:

```yaml
rule_files:
  - "apps/otp/router/docs/observability/router-alert-rules.yaml"
```

Or use relative path from Prometheus working directory.

### Alertmanager Configuration

Alert rules are automatically loaded by Prometheus and forwarded to Alertmanager. No additional configuration needed.

### CI/CD Integration

Add to CI/CD pipeline:

```bash
# Validate alert rules in CI
./scripts/check_alert_rules.sh --promtool
```

## Next Steps

1. **Deploy to Staging**:
   - Copy `router-alert-rules.yaml` to Prometheus rules directory
   - Reload Prometheus: `curl -X POST http://prometheus:9090/-/reload`
   - Verify rules are loaded: `curl http://prometheus:9090/api/v1/rules`

2. **Test Alerts**:
   - Run `./scripts/test_alert_rules.sh` to verify connectivity
   - Monitor alerts in Alertmanager UI
   - Test alert expressions in Prometheus UI

3. **Adjust Thresholds** (if needed):
   - Review alert thresholds based on actual traffic patterns
   - Update thresholds in `router-alert-rules.yaml`
   - Re-validate and redeploy

4. **Monitor and Tune**:
   - Monitor alert frequency and adjust thresholds
   - Review false positives and adjust `for` intervals
   - Update runbook links if documentation changes

## Files Created/Modified

### New Files

1. `apps/otp/router/docs/observability/router-alert-rules.yaml` - Alert rules file
2. `apps/otp/router/scripts/validate_alert_rules_promtool.sh` - Prometheus syntax validation
3. `apps/otp/router/scripts/test_alert_rules.sh` - Staging testing script
4. `apps/otp/router/docs/dev/ALERT_RULES_IMPLEMENTATION_COMPLETE.md` - This document

### Modified Files

1. `apps/otp/router/scripts/check_alert_rules.sh` - Enhanced with full validation

## Verification

### Validation Results

```bash
$ ./scripts/check_alert_rules.sh
=== Checking Alert Rules Configuration ===

✓ Alert rules file found: /path/to/router-alert-rules.yaml

1. Checking YAML syntax...
   ✓ YAML syntax is valid

3. Checking alert groups...
   ✓ Group 'router-jetstream.rules' found
   ✓ Group 'router-nats-connection.rules' found
   ✓ Group 'router-nats-operations.rules' found

4. Checking required alerts...
   ✓ All 14 alerts found

5. Checking metric usage...
   ✓ All metrics verified

7. Checking severity levels...
   ✓ Critical alerts: 5
   ✓ Warning alerts: 9

8. Checking runbook links...
   ✓ Found 14 runbook reference(s)

=== Alert Rules Check Complete ===
```

## References

- **Alert Rules**: `apps/otp/router/docs/observability/router-alert-rules.yaml`
- **Runbook**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
- **Metrics Documentation**: `docs/OBSERVABILITY_ROUTER_DASHBOARD.md`
- **Validation Scripts**: `apps/otp/router/scripts/check_alert_rules.sh`

## Status

✅ **All steps completed**:
- ✅ Alert rules file created with 14 alerts
- ✅ Validation scripts created and tested
- ✅ Runbook links updated
- ✅ Testing script created for staging validation
- ✅ Documentation created

