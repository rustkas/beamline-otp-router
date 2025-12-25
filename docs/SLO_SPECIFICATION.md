# Service Level Objectives (SLO) Specification

**Version**: 1.0  
**Last Updated**: 2025-12-21  
**Owner**: Router Team  
**Review Cycle**: Quarterly

## Overview

This document defines formal Service Level Objectives (SLOs) for the Beamline Router. SLOs establish quantitative targets for service reliability and performance, enabling data-driven operational decisions.

**Key Principles**:
- SLOs are **measurable** from existing metrics
- SLOs are **achievable** based on baseline measurements
- SLOs are **meaningful** to users and business
- SLOs drive **error budget** calculations

## SLO Summary

| SLO | Target | Measurement Window | Current Baseline |
|-----|--------|-------------------|------------------|
| **Latency (p95)** | < 100ms | 5-minute rolling | TBD |
| **Latency (p99)** | < 200ms | 5-minute rolling | TBD |
| **Availability** | 99.9% | 30-day rolling | TBD |
| **Error Budget** | 0.1% | 30-day rolling | TBD |

## 1. Latency SLOs

### 1.1 Definition

**Service Level Indicator (SLI)**: Routing decision latency measured from gRPC request receipt to response transmission.

**Measurement**: Prometheus histogram `router_grpc_request_duration_seconds{method="decide"}`.

### 1.2 Targets

- **p95 Latency**: 95% of requests complete within **100ms**
- **p99 Latency**: 99% of requests complete within **200ms**

**Rationale**:
- p95 target ensures good user experience for majority of requests
- p99 target prevents tail latency from impacting sensitive workloads
- Targets aligned with typical LLM routing decision time budget

### 1.3 Exclusions

Not included in latency SLO:
- Network time between Gateway and Router (out of Router control)
- Provider execution time (measured separately)
- TLS handshake time (amortized over connection lifetime)

### 1.4 Recording Rule

```yaml
- record: slo:router_latency_p95:5m
  expr: |
    histogram_quantile(0.95,
      sum(rate(router_grpc_request_duration_seconds_bucket{method="decide"}[5m])) by (le)
    )
```

### 1.5 Alert Thresholds

| Alert | Threshold | Duration | Severity |
|-------|-----------|----------|----------|
| RouterLatencyP95High | > 100ms | 5 minutes | warning |
| RouterLatencyP99Critical | > 200ms | 5 minutes | critical |

## 2. Availability SLO

### 2.1 Definition

**Service Level Indicator (SLI)**: Percentage of successful routing requests.

**Success**: gRPC response with status `OK` (code 0)  
**Failure**: Any gRPC error status (codes 1-16)

**Measurement**: Prometheus counter `router_grpc_requests_total{method="decide",status="success|error"}`.

### 2.2 Target

**Availability**: **99.9%** (three nines)

**Allowed Downtime**:
- **Per day**: 86.4 seconds (~1.4 minutes)
- **Per month**: 43.2 minutes
- **Per year**: 8.76 hours

**Rationale**:
- 99.9% is industry standard for internal services
- Allows for planned maintenance windows
- Realistic given dependency on external systems (NATS, providers)

### 2.3 Calculation

```
Availability = (Successful Requests) / (Total Requests)
```

**30-day rolling window** to smooth out short-term spikes.

### 2.4 Recording Rule

```yaml
- record: slo:router_availability:30d
  expr: |
    sum(increase(router_grpc_requests_total{method="decide",status="success"}[30d]))
    /
    sum(increase(router_grpc_requests_total{method="decide"}[30d]))
```

### 2.5 Alert Thresholds

| Alert | Threshold | Duration | Severity |
|-------|-----------|----------|----------|
| RouterAvailabilityLow | Error rate > 0.1% | 5 minutes | warning |
| RouterNatsDisconnected | NATS connection lost | 1 minute | critical |

## 3. Error Budget

### 3.1 Definition

**Error Budget**: The allowed amount of unreliability before SLO is violated.

**Formula**:
```
Error Budget = (1 - SLO Target) × Total Requests
```

For 99.9% SLO:
```
Error Budget = 0.001 × Total Requests (30-day window)
```

### 3.2 Error Budget Policy

#### When Error Budget is Exhausted (< 0%)

**Actions**:
1. **Feature Freeze**: Halt all non-critical feature development
2. **Reliability Focus**: Prioritize reliability improvements
3. **Postmortem Required**: Document root cause and prevention
4. **Stakeholder Communication**: Inform leadership of SLO violation

#### When Error Budget is Low (< 10%)

**Actions**:
1. **Caution Mode**: Code review rigor increased
2. **Deployment Freeze**: Non-emergency deploys blocked
3. **Monitoring**: Increase alert sensitivity
4. **Planning**: Prepare reliability backlog

#### When Error Budget is Healthy (> 50%)

**Actions**:
1. **Normal Operations**: Standard release cadence
2. **Innovation**: Safe to experiment with new features
3. **Tech Debt**: Allocate time for technical debt

### 3.3 Burn Rate Alerts

Error budget burn rate measures how fast we consume our budget:

| Burn Rate | Meaning | Budget Exhaustion Time | Alert Threshold |
|-----------|---------|------------------------|-----------------|
| 1x | Normal | 30 days | None |
| 2x | Elevated | 15 days | Slow burn (1 hour) |
| 6x | Critical | 5 days | Fast burn (5 minutes) |

**Fast Burn Alert**: Consuming **5% budget in 1 hour** (6x normal rate)  
**Slow Burn Alert**: Consuming **10% budget in 6 hours** (2x normal rate)

### 3.4 Recording Rules

```yaml
- record: slo:router_error_budget_remaining:30d
  expr: |
    1 - (
      (1 - slo:router_availability:30d)
      /
      0.001
    )

- record: slo:router_error_budget_burn_rate:1h
  expr: |
    (1 - slo:router_success_rate:5m) / 0.001
```

## 4. SLO Compliance Reporting

### 4.1 Daily Report

**Metrics** (automated PromQL queries):
- Current p95 latency (5-minute average)
- Current p99 latency (5-minute average)
- Current availability (30-day rolling)
- Error budget remaining (percentage)
- Error budget burn rate (1-hour)

**Distribution**: Slack #router-slo channel (daily at 9:00 AM)

### 4.2 Weekly Report

**Metrics**:
- SLO compliance trend (7 days)
- Top latency violations (count and duration)
- Top availability incidents (count and impact)
- Error budget forecast (projected exhaustion date)

**Distribution**: Engineering all-hands (Monday morning)

### 4.3 Quarterly Review

**Objectives**:
- Validate SLO targets are still appropriate
- Adjust based on user feedback and business needs
- Review error budget policy effectiveness
- Identify systemic reliability improvements

## 5. Baseline Measurement

### 5.1 Methodology

**Data Source**: Last 30 days of production metrics (if available) or performance test results.

**Queries**:
```promql
# p95 latency (last 7 days)
histogram_quantile(0.95,
  sum(rate(router_grpc_request_duration_seconds_bucket{method="decide"}[7d])) by (le)
)

# p99 latency (last 7 days)
histogram_quantile(0.99,
  sum(rate(router_grpc_request_duration_seconds_bucket{method="decide"}[7d])) by (le)
)

# Availability (last 30 days)
sum(increase(router_grpc_requests_total{method="decide",status="success"}[30d]))
/
sum(increase(router_grpc_requests_total{method="decide"}[30d]))
```

### 5.2 Baseline Report

**Location**: `_artifacts/slo_baseline_report.md` (generated by `scripts/slo_baseline.sh`)

**Contents**:
- Measured p95, p99 latency
- Measured availability
- SLO compliance status (PASS/FAIL)
- Recommended adjustments (if targets unrealistic)

## 6. SLO Dependencies

SLOs depend on:
- ✅ **NATS Availability**: Router cannot function without NATS
- ✅ **Circuit Breakers**: Open breakers reduce availability
- ✅ **Backpressure**: Active backpressure increases latency
- ✅ **Provider Health**: Degraded providers impact latency

**Monitoring**: Dedicated alerts for each dependency (see `prometheus/router_slo_alerts.yml`).

## 7. References

- **Prometheus Rules**: `prometheus/router_slo_rules.yml` (recording), `prometheus/router_slo_alerts.yml` (alerting)
- **Runbooks**: `docs/operations/RECOVERY_RUNBOOK.md`
- **Error Budget Policy**: Section 3.2 above
- **Google SRE Book**: https://sre.google/sre-book/service-level-objectives/

## 8. Appendix: Prometheus Query Examples

### 8.1 Current SLO Status (Last 5 Minutes)

```promql
# p95 latency
slo:router_latency_p95:5m

# p99 latency
slo:router_latency_p99:5m

# Error rate
slo:router_error_rate:5m

# Success rate
slo:router_success_rate:5m
```

### 8.2 Error Budget Status

```promql
# Remaining budget (30-day window)
slo:router_error_budget_remaining:30d

# Burn rate (1-hour window)
slo:router_error_budget_burn_rate:1h

# Days until exhaustion (if current burn rate continues)
30 / slo:router_error_budget_burn_rate:1h
```

### 8.3 SLO Compliance (Boolean)

```promql
# Latency SLO met?
slo:router_latency_p95:5m < 0.100 and slo:router_latency_p99:5m < 0.200

# Availability SLO met?
slo:router_availability:30d >= 0.999
```
