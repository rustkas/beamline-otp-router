# Beamline Router: Full System Integration Guide

> **Version:** 1.0  
> **Date:** December 2024  
> **Status:** Complete

This document provides a unified view of all subsystems, their integration points, and operational procedures.

---

## Executive Summary

| Subsystem | Status | Key Files |
|-----------|--------|-----------|
| Circuit Breaker | ✅ Fixed + Monitored | `router_circuit_breaker.erl`, `prometheus_circuit_breaker_alerts.yml` |
| NATS Publishing | ✅ Fault-tolerant | `router_nats.erl`, `router_nats_fault_injection.erl` |
| CAF Adapter | ✅ Tested + Telemetry | `router_caf_adapter.erl`, `router_caf_test_helper.erl` |
| Test Infrastructure | ✅ Deterministic + CI Enforced | `router_nats_test_helper.erl`, `docs/testing/TEST_NOTES.md` |
| Observability | ✅ Prometheus + Grafana | `router_prometheus.erl`, alerts YAML |

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          BEAMLINE ROUTER                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐       │
│  │ router_caf_      │───▶│ router_circuit_  │───▶│ router_nats      │       │
│  │ adapter          │    │ breaker          │    │ (publish with    │       │
│  │                  │    │                  │    │  fault injection)│       │
│  │ - publish_assign │    │ - should_allow   │    │ - publish_with_  │       │
│  │ - telemetry emit │    │ - record_failure │    │   ack            │       │
│  │ - tenant check   │    │ - latency check  │    │ - retry logic    │       │
│  └────────┬─────────┘    └────────┬─────────┘    └────────┬─────────┘       │
│           │                       │                       │                  │
│           ▼                       ▼                       ▼                  │
│  ┌──────────────────────────────────────────────────────────────────────┐   │
│  │                        router_metrics / router_prometheus             │   │
│  │                                                                       │   │
│  │  Emitted metrics:                                                     │   │
│  │  - router_circuit_breaker_state{tenant_id, provider_id, state}       │   │
│  │  - router_circuit_breaker_trigger_reason{reason}                      │   │
│  │  - router_nats_publish_failures_total                                 │   │
│  │  - router_nats_publish_latency_seconds                                │   │
│  │  - router_assignment_skipped_total                                    │   │
│  │  - router_assignment_blocked_total                                    │   │
│  └──────────────────────────────────────────────────────────────────────┘   │
│                                    │                                         │
└────────────────────────────────────┼─────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          OBSERVABILITY STACK                                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐       │
│  │ Prometheus       │───▶│ Alertmanager     │───▶│ PagerDuty/Slack  │       │
│  │                  │    │                  │    │                  │       │
│  │ Alert rules:     │    │ Routing:         │    │ Escalation:      │       │
│  │ - circuit_breaker│    │ - severity       │    │ - critical→PD    │       │
│  │ - nats_publishing│    │ - component      │    │ - warning→Slack  │       │
│  │ - caf_adapter    │    │ - team           │    │                  │       │
│  └──────────────────┘    └──────────────────┘    └──────────────────┘       │
│                                                                              │
│  ┌──────────────────┐                                                        │
│  │ Grafana          │                                                        │
│  │                  │                                                        │
│  │ Dashboards:      │                                                        │
│  │ - CB State       │                                                        │
│  │ - NATS Health    │                                                        │
│  │ - CAF Throughput │                                                        │
│  └──────────────────┘                                                        │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 1. Circuit Breaker Subsystem

### Components
| File | Purpose |
|------|---------|
| `src/router_circuit_breaker.erl` | State machine, metric emission |
| `priv/prometheus_circuit_breaker_alerts.yml` | Alerting rules |
| `test/router_circuit_breaker_SUITE.erl` | Unit tests |
| `test/router_circuit_breaker_integration_SUITE.erl` | Integration tests |

### Key Metrics
```promql
# Current state (0=closed, 0.5=half_open, 1=open)
router_circuit_breaker_state{tenant_id, provider_id, state}

# Trigger reasons
router_circuit_breaker_trigger_reason{reason="latency_threshold_exceeded"}
router_circuit_breaker_trigger_reason{reason="failure_threshold_exceeded"}
router_circuit_breaker_trigger_reason{reason="error_rate_threshold_exceeded"}

# State transitions
router_circuit_breaker_state_transitions_total{from, to}
```

### Alerts
| Alert | Severity | Condition |
|-------|----------|-----------|
| `CircuitBreakerLatencyThresholdExceeded` | warning | CB opened due to latency |
| `CircuitBreakerFailureThresholdExceeded` | warning | CB opened due to failures |
| `CircuitBreakerHalfOpenFailures` | critical | Repeated half-open failures |
| `MultipleCircuitBreakersOpen` | critical | >3 CBs open for tenant |
| `CircuitBreakerFlapping` | warning | >10 transitions in 10min |

### Runbooks
- [Circuit Breaker Latency](https://wiki.example.com/runbooks/circuit-breaker-latency)
- [Circuit Breaker Failures](https://wiki.example.com/runbooks/circuit-breaker-failures)

---

## 2. NATS Publishing Subsystem

### Components
| File | Purpose |
|------|---------|
| `src/router_nats.erl` | NATS client, publish with retry |
| `src/router_nats_fault_injection.erl` | Test fault injection |
| `test/router_nats_test_helper.erl` | Centralized mocking |
| `test/router_intake_chaos_SUITE.erl` | Chaos tests |

### Key Metrics
```promql
# Publish operations
router_nats_publish_failures_total
router_nats_publish_latency_seconds
router_nats_publish_retries_total
router_nats_publish_attempts_total

# Connection status
router_nats_connection_status  # 1=connected, 0=disconnected
```

### Alerts
| Alert | Severity | Condition |
|-------|----------|-----------|
| `NATSPublishFailuresSpike` | warning | Failure rate > 0.1/sec |
| `NATSPublishLatencyHigh` | warning | P95 latency > 500ms |
| `NATSConnectionLost` | critical | Connection down |
| `NATSHighRetryRate` | warning | Retry rate > 20% |

### Recording Rules
```promql
router:nats_publish_success_rate:5m
router:nats_publish_latency_avg:5m
router:nats_publish_throughput:rate1m
```

---

## 3. CAF Adapter Subsystem

### Components
| File | Purpose |
|------|---------|
| `src/router_caf_adapter.erl` | Assignment publishing |
| `test/router_caf_adapter_enhanced_SUITE.erl` | Telemetry tests |
| `test/router_caf_integration_SUITE.erl` | Integration tests |
| `test/router_caf_test_helper.erl` | Test utilities |

### Key Metrics
```promql
# Assignment operations
router_assignment_skipped_total{tenant_id}
router_assignment_blocked_total{tenant_id}
router_assignment_published_total{tenant_id}
router_assignment_retry_total{tenant_id}
```

### Telemetry Events
| Event | Metadata |
|-------|----------|
| `[router_caf_adapter, router_assignment_published_total]` | `assignment_id, request_id, tenant_id, subject, retries, pub_ack_id` |
| `[router_caf_adapter, router_assignment_skipped_total]` | `request_id, tenant_id, reason` |
| `[router_caf_adapter, router_assignment_blocked_total]` | `request_id, tenant_id, policy` |

### Alerts
| Alert | Severity | Condition |
|-------|----------|-----------|
| `CAFAssignmentSkipRateHigh` | warning | Skip rate > 50% |
| `CAFAssignmentBlockedRateHigh` | warning | Blocked rate > 0.1/sec per tenant |

---

## 4. Test Infrastructure Subsystem

### Components
| File | Purpose |
|------|---------|
| `test/router_nats_test_helper.erl` | Centralized NATS mocking |
| `test/router_caf_test_helper.erl` | CAF test utilities |
| `docs/testing/TEST_NOTES.md` | Test documentation |
| `docs/testing/BUSINESS_PROBLEMS_MAP.md` | Problem tracking |
| `scripts/ci_check_chaos_mode.sh` | CI enforcement |
| `Makefile` | CI targets |

### Test Categories
| Category | Suites | Makefile Target |
|----------|--------|-----------------|
| Unit (mock) | `router_caf_*_SUITE` | `make test-unit` |
| Integration | `router_*_integration_SUITE` | `make test-integration` |
| Chaos (Docker) | `router_intake_chaos_SUITE` | `make test-chaos-docker` |
| Stress | `router_*_stress_SUITE` | `make test-stress` |
| Randomized | `router_*_randomized_SUITE` | `make test-randomized` |

### CI Enforcement
| Mechanism | Environment Variable | Effect |
|-----------|---------------------|--------|
| Require Docker | `CHAOS_REQUIRE_DOCKER=true` | Fail if no Docker |
| Allow Mock | `CHAOS_MOCK_ALLOWED=true` | Allow degraded coverage |
| Strict Mocks | `STRICT_MOCK_DISCIPLINE=true` | Fail on basic-only mocks |

### Discipline Helpers
```erlang
%% Check corner-case coverage
router_nats_test_helper:check_corner_case_coverage()

%% Verify specific arguments
router_nats_test_helper:verify_publish_called_with(#{
    subject => ~"caf.exec.assign.v1",
    payload_contains => ~"tenant_id"
})

%% Get call history for debugging
router_nats_test_helper:get_call_history(publish_with_ack)
```

---

## 5. Observability Stack Integration

### Prometheus Configuration

```yaml
# prometheus.yml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

rule_files:
  - "/etc/prometheus/rules/circuit_breaker_alerts.yml"

scrape_configs:
  - job_name: 'beamline-router'
    static_configs:
      - targets: ['router-host:8080']
    metrics_path: '/metrics'
```

### Alertmanager Routing

```yaml
# alertmanager.yml
route:
  group_by: ['alertname', 'component']
  routes:
    - match:
        severity: critical
      receiver: pagerduty-critical
    - match:
        severity: warning
        component: circuit_breaker
      receiver: slack-platform
    - match:
        severity: warning
        component: nats
      receiver: slack-platform
    - match:
        component: test_infrastructure
      receiver: slack-devops
```

### Grafana Dashboard Variables

```promql
# Tenant selector
label_values(router_circuit_breaker_state, tenant_id)

# Provider selector
label_values(router_circuit_breaker_state{tenant_id="$tenant_id"}, provider_id)

# Component selector
label_values({__name__=~"router_.*"}, component)
```

---

## 6. Operational Procedures

### 6.1 Deploying Alert Rules

```bash
# 1. Validate rules
promtool check rules priv/prometheus_circuit_breaker_alerts.yml

# 2. Copy to Prometheus
cp priv/prometheus_circuit_breaker_alerts.yml /etc/prometheus/rules/

# 3. Reload Prometheus
curl -X POST http://localhost:9090/-/reload

# 4. Verify loaded
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].name'
```

### 6.2 Running Tests

```bash
# Full test suite
make test-all

# CI pipeline (Docker required)
make test-chaos-ci

# Reproduce failed test
make reproduce-chaos SEED=1733511111,123456,789
```

### 6.3 Investigating Circuit Breaker Issues

1. **Check current state:**
   ```promql
   router_circuit_breaker_state{tenant_id="$tenant", provider_id="$provider"}
   ```

2. **Check trigger reason:**
   ```promql
   increase(router_circuit_breaker_trigger_reason{tenant_id="$tenant"}[1h])
   ```

3. **Check NATS latency:**
   ```promql
   router:nats_publish_latency_avg:5m
   ```

4. **Check logs:**
   ```bash
   # Erlang logs
   grep "circuit_breaker" /var/log/beamline-router/erlang.log
   ```

### 6.4 Silencing Alerts During Maintenance

```yaml
# alertmanager silence
{
  "matchers": [
    {"name": "component", "value": "circuit_breaker"},
    {"name": "tenant_id", "value": "maintenance-tenant"}
  ],
  "startsAt": "2024-12-08T00:00:00Z",
  "endsAt": "2024-12-08T02:00:00Z",
  "comment": "Planned maintenance"
}
```

---

## 7. Troubleshooting Guide

### Issue: Circuit Breaker Not Opening on Latency

**Symptoms:**
- High publish latency observed
- CB remains closed

**Root Cause:**
- `get_recent_latency()` was not handling integer metrics (FIXED)

**Verification:**
```bash
rebar3 ct --suite=router_circuit_breaker_SUITE --case=test_circuit_breaker_opens_on_latency_threshold
```

### Issue: Chaos Tests Silent-Skip in CI

**Symptoms:**
- Tests pass but coverage is low
- "CHAOS TESTS RUNNING IN MOCK MODE" in logs

**Solution:**
```bash
# Use CI enforcement
CHAOS_REQUIRE_DOCKER=true make test-chaos-ci
```

### Issue: Flaky Test Failures

**Symptoms:**
- Tests pass/fail randomly
- No seed logged

**Solution:**
```bash
# Reproduce with seed from logs
make reproduce-chaos SEED=<seed_from_failed_run>
```

---

## 8. Quick Reference

### Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `CHAOS_REQUIRE_DOCKER` | Fail if Docker unavailable | `false` |
| `CHAOS_MOCK_ALLOWED` | Explicitly allow mock mode | `false` |
| `CHAOS_USE_MOCK` | Force mock mode | `false` |
| `CHAOS_RAND_SEED` | Reproduce chaos test | random |
| `RANDOMIZED_TEST_SEED` | Reproduce randomized test | deterministic |
| `STRICT_MOCK_DISCIPLINE` | Fail on basic-only mocks | `false` |

### Makefile Targets

| Target | Description |
|--------|-------------|
| `make test` | Run all tests |
| `make test-chaos` | Chaos tests (auto-detect) |
| `make test-chaos-ci` | Chaos + CI enforcement |
| `make test-discipline` | Strict mock discipline |
| `make ci-chaos-pipeline` | Full CI chaos pipeline |
| `make reproduce-chaos SEED=A,B,C` | Reproduce by seed |

### Key Files

| Category | Files |
|----------|-------|
| Prod Code | `router_circuit_breaker.erl`, `router_nats.erl`, `router_caf_adapter.erl` |
| Alerting | `prometheus_circuit_breaker_alerts.yml`, `router_alert_rules.erl` |
| Test Helpers | `router_nats_test_helper.erl`, `router_caf_test_helper.erl` |
| Documentation | `docs/testing/TEST_NOTES.md`, `docs/testing/BUSINESS_PROBLEMS_MAP.md`, this file |
| CI Scripts | `ci_check_chaos_mode.sh`, `Makefile` |

---

## 9. Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2024-12 | Initial full integration: CB, NATS, CAF, Test Infrastructure, Observability |

---

## 10. Contributors

- **Platform Team**: Circuit Breaker, NATS, CAF Adapter
- **SRE Team**: Alerting, Dashboards
- **DevOps Team**: CI Enforcement, Test Infrastructure
