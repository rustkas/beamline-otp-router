# Production Roadmap

**Baseline**: 2025-12-21  
**Target**: Production Readiness  
**Current Phase**: Pre-Production Foundation

---

## Gap → Task Mapping

| # | Gap (from gap_analysis.md) | Task | Priority | Domain |
|---|---------------------------|------|----------|--------|
| 1 | Staging NATS/JetStream | T-INFRA-01 | 🔴 BLOCKER | Infra |
| 2 | Heavy tests not green | T-TEST-01 | 🔴 BLOCKER | Test |
| 3 | Performance baseline unknown | T-PERF-01 | 🔴 BLOCKER | Perf |
| 4 | SLO/SLA not defined | T-SLO-01 | 🔴 BLOCKER | SLO |
| 5 | Rollback not verified | T-OPS-01 | 🔴 BLOCKER | Ops |
| 6 | NATS TLS not enabled | T-SEC-01 | 🔴 BLOCKER | Security |
| 7 | Alerts not fire-tested | T-OBS-01 | 🔴 BLOCKER | Observability |
| 8 | Capacity limits unknown | T-PERF-02 | 🟡 REQUIRED | Perf |
| 9 | Chaos tests not validated | T-TEST-02 | 🟡 REQUIRED | Test |
| 10 | E2E tests with external | T-TEST-03 | 🟡 REQUIRED | Test |
| 11 | Dashboard not validated | T-OBS-02 | 🟡 REQUIRED | Observability |
| 12 | Blue-green deployment | T-OPS-02 | 🟢 OPTIONAL | Ops |
| 13 | Security scan | T-SEC-02 | 🟢 OPTIONAL | Security |
| 14 | Distributed tracing E2E | T-OBS-03 | 🟢 OPTIONAL | Observability |

---

## Dependency Graph

```
                    ┌─────────────────┐
                    │  T-INFRA-01     │
                    │ Staging NATS    │
                    └────────┬────────┘
                             │
            ┌────────────────┼────────────────┐
            │                │                │
            ▼                ▼                ▼
    ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
    │  T-TEST-01    │ │  T-SEC-01     │ │  T-PERF-01    │
    │ Heavy tests   │ │ NATS TLS      │ │ Benchmark     │
    └───────┬───────┘ └───────────────┘ └───────┬───────┘
            │                                   │
            │                                   ▼
            │                           ┌───────────────┐
            │                           │  T-SLO-01     │
            │                           │ Define SLOs   │
            │                           └───────┬───────┘
            │                                   │
            ▼                                   ▼
    ┌───────────────┐                   ┌───────────────┐
    │  T-OBS-01     │                   │  T-PERF-02    │
    │ Alert fire-test│                  │ Capacity test │
    └───────────────┘                   └───────────────┘
            │
            ▼
    ┌───────────────┐
    │  T-OPS-01     │
    │ Rollback verify│
    └───────────────┘
```

---

## Task Definitions

### PHASE 0: Infrastructure (Blocker)

---

#### T-INFRA-01: Provision Staging NATS/JetStream

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | Infrastructure |
| **Depends On** | None |
| **Blocks** | T-TEST-01, T-SEC-01, T-PERF-01, T-OBS-01 |

**Objective**: Provision a NATS/JetStream cluster accessible from staging environment.

**Acceptance Criteria**:
- [ ] NATS server running with JetStream enabled
- [ ] Router can connect from staging
- [ ] Subjects `beamline.router.v1.*` and `caf.exec.*` accessible
- [ ] Connection verified via health check

**Artifacts**:
- Staging NATS connection URL
- Connection test script

---

### PHASE 1: Validation (Blockers)

---

#### T-TEST-01: Heavy Test Tier Green

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | Testing |
| **Depends On** | T-INFRA-01 |
| **Blocks** | T-OBS-01 |

**Objective**: Execute heavy test tier with staging NATS and achieve green status.

**Acceptance Criteria**:
- [ ] `ROUTER_TEST_LEVEL=heavy rebar3 ct` passes (0 failures)
- [ ] All JetStream E2E tests pass
- [ ] All chaos/fault injection tests pass
- [ ] Results documented in progress.md

**Artifacts**:
- Test log: `_artifacts/ct_heavy_green_*.log`
- Summary: Passed/Failed/Skipped counts

---

#### T-PERF-01: Establish Performance Baseline

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | Performance |
| **Depends On** | T-INFRA-01 |
| **Blocks** | T-SLO-01, T-PERF-02 |

**Objective**: Measure baseline performance metrics under controlled load.

**Acceptance Criteria**:
- [ ] Benchmark harness script created
- [ ] RPS (avg) measured and recorded
- [ ] Latency p50/p95/p99 measured and recorded
- [ ] Error rate measured and recorded
- [ ] Environment metadata captured (CPU, OTP, config)

**Artifacts**:
- Script: `scripts/bench_router.sh` or `.ai/task_benchmark_harness/`
- Baseline data: `_artifacts/perf_baseline_*.json`

---

#### T-SEC-01: Enable NATS TLS (Staging)

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | Security |
| **Depends On** | T-INFRA-01 |
| **Blocks** | None (parallel) |

**Objective**: Configure and verify TLS for NATS connections in staging.

**Acceptance Criteria**:
- [ ] NATS server TLS enabled
- [ ] Router connects via TLS (verified)
- [ ] Plaintext connection rejected (or documented as policy)
- [ ] Cert handling documented

**Artifacts**:
- Config evidence: env vars / config paths
- Connection test output

---

#### T-SLO-01: Define Initial SLOs

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | SLO |
| **Depends On** | T-PERF-01 |
| **Blocks** | None |

**Objective**: Define SLO targets based on measured baseline.

**Acceptance Criteria**:
- [ ] Latency SLO: p99 <= X ms (derived from baseline × factor)
- [ ] Availability SLO: >= Y% (stated with rationale)
- [ ] Error rate SLO: <= Z% (derived from baseline)
- [ ] Each SLO has documented rationale

**Artifacts**:
- SLO document: `docs/SLO.md` or in progress.md

---

#### T-OBS-01: Fire-Test Alerts

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | Observability |
| **Depends On** | T-INFRA-01, T-TEST-01 |
| **Blocks** | None |

**Objective**: Verify alerting pipeline end-to-end in staging.

**Acceptance Criteria**:
- [ ] 1-2 high-signal alerts triggered intentionally
- [ ] Alert delivered to configured channel (Slack/PagerDuty)
- [ ] Alert cleared after condition resolved
- [ ] Timestamps recorded

**Artifacts**:
- Fire-test log with timestamps
- Delivery evidence (screenshot or webhook log)

---

#### T-OPS-01: Verify Rollback Procedure

| Attribute | Value |
|-----------|-------|
| **Priority** | 🔴 BLOCKER |
| **Domain** | Operations |
| **Depends On** | T-INFRA-01 |
| **Blocks** | None |

**Objective**: Execute and verify rollback procedure in staging.

**Acceptance Criteria**:
- [ ] `scripts/rollback.sh` executed in staging
- [ ] Rollback to previous version successful
- [ ] Post-rollback health check passes
- [ ] `scripts/smoke.sh` passes after rollback

**Artifacts**:
- Rollback execution log
- Smoke test output

---

### PHASE 2: Extended Validation (Required)

---

#### T-PERF-02: Establish Capacity Limits

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟡 REQUIRED |
| **Domain** | Performance |
| **Depends On** | T-PERF-01 |
| **Blocks** | None |

**Objective**: Determine maximum capacity before degradation.

**Acceptance Criteria**:
- [ ] Max RPS at acceptable latency (p99 < SLO)
- [ ] Memory limit under sustained load
- [ ] CPU utilization at max load
- [ ] Breaking point documented

**Artifacts**:
- Capacity report

---

#### T-TEST-02: Validate Chaos Tests

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟡 REQUIRED |
| **Domain** | Testing |
| **Depends On** | T-TEST-01 |
| **Blocks** | None |

**Objective**: Validate chaos/fault injection test suite in staging.

**Acceptance Criteria**:
- [ ] router_ext_chaos_* suites pass
- [ ] Fault recovery validated
- [ ] Results documented

**Artifacts**:
- Chaos test log

---

#### T-TEST-03: E2E Tests with External Components

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟡 REQUIRED |
| **Domain** | Testing |
| **Depends On** | T-TEST-01 |
| **Blocks** | None |

**Objective**: Run E2E tests involving Gateway and/or CAF.

**Acceptance Criteria**:
- [ ] Gateway → Router integration tested
- [ ] Router → CAF assignment flow tested
- [ ] Result flow tested
- [ ] Trace propagation verified

**Artifacts**:
- E2E test report

---

#### T-OBS-02: Validate Dashboard

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟡 REQUIRED |
| **Domain** | Observability |
| **Depends On** | T-OBS-01 |
| **Blocks** | None |

**Objective**: Verify Prometheus/Grafana dashboard shows correct data.

**Acceptance Criteria**:
- [ ] Dashboard deployed
- [ ] Key metrics visible (RPS, latency, errors)
- [ ] Data matches reality
- [ ] Screenshot captured

**Artifacts**:
- Dashboard screenshot
- Dashboard JSON export

---

### PHASE 3: Polish (Optional)

---

#### T-OPS-02: Blue-Green Deployment

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟢 OPTIONAL |
| **Domain** | Operations |
| **Depends On** | T-OPS-01 |
| **Blocks** | None |

**Objective**: Implement blue-green deployment strategy.

**Acceptance Criteria**:
- [ ] Blue-green script or config created
- [ ] Traffic switch tested
- [ ] Rollback via blue-green tested

---

#### T-SEC-02: Security/Dependency Scan

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟢 OPTIONAL |
| **Domain** | Security |
| **Depends On** | None |
| **Blocks** | None |

**Objective**: Audit dependencies for known vulnerabilities.

**Acceptance Criteria**:
- [ ] Dependency audit run
- [ ] No critical vulnerabilities (or mitigated)
- [ ] Report generated

---

#### T-OBS-03: Distributed Tracing E2E Validation

| Attribute | Value |
|-----------|-------|
| **Priority** | 🟢 OPTIONAL |
| **Domain** | Observability |
| **Depends On** | T-OBS-01 |
| **Blocks** | None |

**Objective**: Verify distributed tracing works end-to-end.

**Acceptance Criteria**:
- [ ] Trace visible in OpenTelemetry backend
- [ ] Spans from Router visible
- [ ] Parent-child relationships correct

---

## Execution Order

| Order | Task | Depends On | Parallel With |
|-------|------|------------|---------------|
| 1 | T-INFRA-01 | - | - |
| 2 | T-SEC-01 | T-INFRA-01 | T-PERF-01, T-TEST-01 |
| 2 | T-PERF-01 | T-INFRA-01 | T-SEC-01, T-TEST-01 |
| 2 | T-TEST-01 | T-INFRA-01 | T-SEC-01, T-PERF-01 |
| 3 | T-SLO-01 | T-PERF-01 | T-OBS-01, T-OPS-01 |
| 3 | T-OBS-01 | T-TEST-01 | T-SLO-01, T-OPS-01 |
| 3 | T-OPS-01 | T-INFRA-01 | T-SLO-01, T-OBS-01 |
| 4 | T-PERF-02 | T-SLO-01 | T-TEST-02 |
| 4 | T-TEST-02 | T-TEST-01 | T-PERF-02 |
| 4 | T-TEST-03 | T-TEST-01 | T-OBS-02 |
| 4 | T-OBS-02 | T-OBS-01 | T-TEST-03 |
| 5 | T-OPS-02 | T-OPS-01 | T-SEC-02 |
| 5 | T-SEC-02 | - | T-OPS-02 |
| 5 | T-OBS-03 | T-OBS-01 | - |

---

## Summary

| Phase | Tasks | Blockers | Est. Effort |
|-------|-------|----------|-------------|
| **0: Infra** | 1 | 1 | 1-2 days |
| **1: Validation** | 6 | 6 | 3-5 days |
| **2: Extended** | 4 | 0 | 2-3 days |
| **3: Polish** | 3 | 0 | 2-3 days |
| **Total** | 14 | 7 | 8-13 days |

---

## Critical Path

```
T-INFRA-01 → T-PERF-01 → T-SLO-01 → Production Gate
              ↓
         T-TEST-01 → T-OBS-01
              ↓
         T-OPS-01
```

**Minimum path to production**: T-INFRA-01 + all Phase 1 tasks (7 blockers).

---

## Task File Index

When tasks are executed, create corresponding `.ai/task_*` directories:

| Task ID | Task Folder |
|---------|-------------|
| T-INFRA-01 | `.ai/task_staging_nats_setup/` |
| T-TEST-01 | `.ai/task_heavy_tests_green/` |
| T-PERF-01 | `.ai/task_perf_baseline/` |
| T-SEC-01 | `.ai/task_nats_tls_staging/` |
| T-SLO-01 | `.ai/task_slo_definition/` |
| T-OBS-01 | `.ai/task_alert_fire_test/` |
| T-OPS-01 | `.ai/task_rollback_verification/` |
| T-PERF-02 | `.ai/task_capacity_limits/` |
| T-TEST-02 | `.ai/task_chaos_tests_validation/` |
| T-TEST-03 | `.ai/task_e2e_external/` |
| T-OBS-02 | `.ai/task_dashboard_validation/` |
| T-OPS-02 | `.ai/task_blue_green_deploy/` |
| T-SEC-02 | `.ai/task_security_scan/` |
| T-OBS-03 | `.ai/task_otel_tracing_e2e/` |
