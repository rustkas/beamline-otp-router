# R10 Circuit Breaker Runbook

## Purpose

Quick reference for diagnosing and resolving circuit breaker issues in production based on R10 metrics and test scenarios.

## Identifying Circuit Breaker Issues

### Key Metrics to Check

1. **Circuit Breaker State** (`router_circuit_breaker_state`):
   - `closed` (0.0) - Normal operation
   - `open` (1.0) - Circuit is open, requests blocked
   - `half_open` (2.0) - Testing recovery

2. **Trigger Reason** (`router_circuit_breaker_trigger_reason`):
   - `failure_threshold_exceeded` - Too many consecutive failures
   - `error_rate_threshold_exceeded` - Error rate too high
   - `latency_threshold_exceeded` - Latency exceeded threshold
   - `half_open_failure` - Failure during half-open probe
   - `timeout_elapsed` - Timeout period expired

3. **State Transitions** (`router_circuit_breaker_state_transitions_total`):
   - Count of state changes (indicates instability if high)

### How to Check Metrics

**Via Prometheus/Grafana**:
```promql
# Current state for tenant/provider
router_circuit_breaker_state{tenant_id="<tenant>", provider_id="<provider>"}

# Latest trigger reason
router_circuit_breaker_trigger_reason{tenant_id="<tenant>", provider_id="<provider>"}

# State transitions (rate)
rate(router_circuit_breaker_state_transitions_total[5m])
```

**Via Local Test**:
```bash
# Reproduce scenario locally
rebar3 ct --suite test/router_publish_failure_e2e_SUITE \
  --case scenario_mass_failure_opens_breaker \
  --config test/ct.config
```

## Common Scenarios

### Scenario 1: Circuit Breaker Opened Due to Failures

**Symptoms**:
- `router_circuit_breaker_state = 1.0` (open)
- `trigger_reason = failure_threshold_exceeded` or `error_rate_threshold_exceeded`
- Requests returning `{error, circuit_open}`

**Diagnosis**:
1. Check provider health (external service status)
2. Check error logs for provider-specific errors
3. Verify failure threshold configuration

**Resolution**:
1. Fix underlying provider issue
2. Wait for timeout period (circuit will transition to half_open)
3. If issue persists, adjust thresholds:
   - Increase `failure_threshold` if false positives
   - Increase `error_rate_threshold` if temporary spikes
   - Adjust `error_rate_window_seconds` for calculation window

**Local Reproduction**:
```bash
# Run mass failure scenario
rebar3 ct --suite test/router_publish_failure_e2e_SUITE \
  --case scenario_mass_failure_opens_breaker
```

### Scenario 2: Circuit Breaker Opened Due to Latency

**Symptoms**:
- `router_circuit_breaker_state = 1.0` (open)
- `trigger_reason = latency_threshold_exceeded`
- Requests timing out or very slow

**Diagnosis**:
1. Check provider latency metrics
2. Check network conditions
3. Verify latency threshold configuration

**Resolution**:
1. Investigate provider performance degradation
2. Check network connectivity
3. Adjust `latency_threshold_ms` if threshold too aggressive
4. Consider increasing timeout values

**Local Reproduction**:
```bash
# Run latency-based trigger scenario
rebar3 ct --suite test/router_publish_failure_e2e_SUITE \
  --case scenario_latency_based_trigger
```

### Scenario 3: Circuit Breaker Stuck in Half-Open

**Symptoms**:
- `router_circuit_breaker_state = 2.0` (half_open) for extended period
- Frequent transitions between half_open and open

**Diagnosis**:
1. Check if probe requests are failing
2. Verify `half_open_max_attempts` configuration
3. Check for intermittent provider issues

**Resolution**:
1. Fix underlying provider instability
2. Increase `half_open_max_attempts` if probes too strict
3. Adjust `success_threshold` if recovery criteria too high

**Local Reproduction**:
```bash
# Run recovery scenario
rebar3 ct --suite test/router_publish_failure_e2e_SUITE \
  --case scenario_recovery_after_failure
```

## Configuration Tuning

### When to Adjust Thresholds

**Increase `failure_threshold`**:
- Circuit opens too frequently on temporary issues
- Provider has occasional but recoverable failures

**Increase `error_rate_threshold`**:
- Error rate calculation too sensitive
- Provider has acceptable error rate but circuit opens

**Increase `latency_threshold_ms`**:
- Circuit opens on acceptable latency
- Provider has variable but acceptable performance

**Increase `open_timeout_ms`**:
- Circuit stays open too long
- Need faster recovery attempts

**Increase `half_open_max_attempts`**:
- Circuit can't recover from half_open
- Probes too strict

**Increase `success_threshold`**:
- Circuit can't transition from half_open to closed
- Recovery criteria too strict

### Configuration Files

**Test Configuration**: `test/ct.config`
- Default CI profile: 10 clients × 20 requests
- Heavy profile: 50 clients × 100 requests

**Application Configuration**: Check application environment variables or config files for:
- `circuit_breaker.failure_threshold`
- `circuit_breaker.error_rate_threshold`
- `circuit_breaker.latency_threshold_ms`
- `circuit_breaker.open_timeout_ms`
- `circuit_breaker.half_open_max_attempts`
- `circuit_breaker.success_threshold`

## Quick Diagnostic Commands

### Check Circuit Breaker State

```erlang
% In Erlang shell
router_circuit_breaker:get_state(<<"tenant_id">>, <<"provider_id">>).
```

### Check Metrics via router_r10_metrics

```erlang
% Get latest trigger reason
router_r10_metrics:get_latest_trigger_reason(<<"tenant_id">>, <<"provider_id">>).

% Get current state metric
router_r10_metrics:get_metric_value(router_circuit_breaker_state, #{
    tenant_id => <<"tenant_id">>,
    provider_id => <<"provider_id">>,
    state => <<"open">>
}).
```

### Dump All Metrics

```erlang
% In test environment
router_r10_metrics:dump_metrics().
```

## Local Testing Workflow

1. **Identify the issue** from production metrics
2. **Reproduce locally** using appropriate E2E scenario:
   ```bash
   rebar3 ct --suite test/router_publish_failure_e2e_SUITE \
     --case <scenario_name> \
     --config test/ct.config
   ```
3. **Adjust configuration** if needed
4. **Verify fix** by running scenario again
5. **Deploy** and monitor metrics

## References

- `R10_P0_COMPLETE_FINAL.md` - R10 Metrics Access Layer
- `QA_TEST_PLAN.md` - R10 E2E Test Suite documentation
- `OBSERVABILITY_CONVENTIONS.md` - R10 Circuit Breaker Metrics conventions
- `test/ct.config` - Test configuration profiles

