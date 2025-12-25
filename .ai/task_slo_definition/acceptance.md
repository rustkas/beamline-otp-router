# Acceptance Criteria: T-SLO-01

## Done When

### 1. SLO Specification Document Complete
- [ ] `docs/SLO_SPECIFICATION.md` created
- [ ] Latency SLO defined (p95 < 100ms, p99 < 200ms)
- [ ] Availability SLO defined (99.9% uptime)
- [ ] Error budget formula documented
- [ ] Measurement methodology explained
- [ ] SLO review process defined

### 2. Prometheus Recording Rules Deployed
- [ ] `prometheus/router_slo_rules.yml` created
- [ ] p95 latency recording rule: `slo:router_latency_p95:5m`
- [ ] p99 latency recording rule: `slo:router_latency_p99:5m`
- [ ] Error rate recording rule: `slo:router_error_rate:5m`
- [ ] 30-day availability: `slo:router_availability:30d`
- [ ] Rules syntax validated with `promtool`

### 3. Prometheus Alerting Rules Deployed
- [ ] `prometheus/router_slo_alerts.yml` created
- [ ] Latency p95 violation alert (> 100ms for 5min)
- [ ] Latency p99 violation alert (> 200ms for 5min)
- [ ] Availability violation alert (error rate > 0.1%)
- [ ] Error budget exhaustion alert (< 10% remaining)
- [ ] Fast burn alert (5% budget in 1 hour)
- [ ] Slow burn alert (10% budget in 6 hours)
- [ ] All alerts have proper annotations and runbook links

### 4. Baseline Measurement Complete
- [ ] Current p95 latency measured (from last 7 days)
- [ ] Current p99 latency measured (from last 7 days)
- [ ] Current availability measured (from last 30 days)
- [ ] Current error budget consumption rate calculated
- [ ] Baseline report: `_artifacts/slo_baseline_report.md`
- [ ] SLO compliance status: PASS/FAIL documented

### 5. Error Budget Calculation Automated
- [ ] Script or query to calculate remaining error budget
- [ ] Daily error budget report (manual or automated)
- [ ] Error budget policy documented (what to do when exhausted)

### 6. Documentation Complete
- [ ] SLO specification reviewed and approved
- [ ] Incident response runbook updated with SLO procedures
- [ ] Error budget policy communicated to team
- [ ] README updated with SLO section

## Validation Checklist

- [ ] Recording rules return valid values (not NaN)
- [ ] Alerts can be triggered in test environment
- [ ] Baseline measurement shows realistic values
- [ ] SLO targets are achievable based on baseline
- [ ] Prometheus rules pass `promtool check rules`
