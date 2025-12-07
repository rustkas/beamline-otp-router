# R10 Integration Roadmap: From Code to Production

This document provides a prioritized roadmap for integrating R10 (Retry Logic + Circuit Breaker) into production operations and monitoring.

## Current Status

✅ **Code Complete**: R10 architecture, tests, and documentation are production-ready  
✅ **Artifacts Created**: Dashboard JSON, alert rules, CLI command, property tests  
✅ **P2 Tasks Complete**: Metrics cleanup, property tests, protective rails script  
⏳ **Integration Pending**: Deploy dashboard, deploy alerts, integrate CLI into router_ctl, validate CI jobs

## P1 — Production Integration (Required)

### 1. Grafana Dashboard Implementation

**Status**: ✅ **Artifact Created** - Dashboard JSON ready

**Location**: `apps/otp/router/observability/r10_dashboard.json`

**Required Panels** (all implemented):
1. Circuit Breaker State (Gauge) - aggregate + per tenant/provider
2. Trigger Reasons (Bar/Stacked Chart) - distribution by reason type
3. State Transitions (Stacked Area) - transition rates over time
4. Timeout Remaining (Time Series) - time until half-open transition
5. Sliding Error Rate (Time Series) - error rate over time

**Next Steps**:
- [ ] Import dashboard JSON into production Grafana
- [ ] Configure datasource variable
- [ ] Verify all panels display correctly
- [ ] Add dashboard link to `R10_RUNBOOK.md` (✅ already added)

**Status**: ✅ Dashboard JSON ready, documentation updated

**Estimated Effort**: 30 minutes (import and verification)

---

### 2. Production Alerts Implementation

**Status**: ✅ **Artifact Created** - Alert rules YAML ready

**Location**: `apps/otp/router/observability/r10_alerts.yaml`

**Required Alerts** (all implemented):
1. `R10CircuitOpenTooLong` - Breaker in open > 5 minutes
2. `R10HighErrorRate` - Error rate > 50% for > 5 minutes
3. `R10CircuitFlapping` - High transition rate (> 10/5m)
4. `R10LatencyTriggerDominating` - Latency trigger > 70% of openings

**All alerts include**:
- Runbook URLs pointing to `https://github.com/rustkas/orchestrator/blob/main/apps/otp/router/test/R10_RUNBOOK.md`
- Proper severity labels (warning/critical)
- Team and component labels

**Next Steps**:
- [ ] Add alert rules file to Prometheus configuration
- [ ] Include in `prometheus.yml` or alertmanager config
- [ ] Deploy to production monitoring system
- [ ] Test alert firing on sandbox environment

**Status**: ✅ Alert rules YAML ready with runbook URLs

**Estimated Effort**: 1-2 hours (deployment and testing)

---

### 3. Runbook Integration with SRE Tools

**Goal**: Make runbook part of operational workflow

**Current State**: `R10_RUNBOOK.md` exists as Markdown file

**Required Integration Points**:
1. Incident ticket templates (Jira/YouTrack/etc.) - add runbook link
2. Alert descriptions - add `runbook_url` field pointing to runbook
3. SRE consolidated runbook/index - add R10 runbook entry
4. On-call tools - verify runbook accessibility

**Deliverable**:
- Runbook links in all relevant operational tools
- Verified accessibility from on-call workflows

**Estimated Effort**: 1-2 hours

---

### 4. CI Jobs Dry-Run and Validation

**Goal**: Validate CI jobs work as expected with artifacts

**Current State**: Jobs added to `.gitlab-ci.yml` but not validated

**Required Validation**:
1. Test `router-r10-unit` in merge request pipeline
2. Test `router-r10-e2e-ci` in merge request pipeline
3. Test `router-r10-e2e-heavy` in nightly/scheduled pipeline
4. Verify artifact saving:
   - CT HTML reports
   - `router_r10_metrics:dump_metrics()` output on failures
   - Router application logs
5. Document artifact locations and retention

**Deliverable**:
- Validated CI pipeline
- Working artifact collection
- Documentation of artifact locations

**Estimated Effort**: 2-3 hours

---

## P2 — Stability Improvements (Important)

### 5. Automatic Test Metrics Cleanup

**Status**: ✅ **Complete** - Function implemented and integrated

**Implementation**:
- ✅ `prune_old_test_metrics/1` added to `router_r10_metrics.erl`
- ✅ Removes all metrics with `tenant_*` / `provider_*` patterns
- ✅ Integrated in `init_per_suite` for both test suites
- ✅ Integrated in property test suite
- ✅ Added to CI heavy profile job

**Deliverable**: ✅ Complete
- Cleanup function in `router_r10_metrics.erl`
- Integration in test lifecycle
- Documentation in `R10_MAINTENANCE_CHECKLIST.md`

**Estimated Effort**: ✅ Completed

---

### 6. Light Property-Based Testing

**Status**: ✅ **Artifact Created** - Property test suite ready

**Location**: `apps/otp/router/test/router_circuit_breaker_prop_SUITE.erl`

**Implementation**:
- Light mode: 20 random sequences per run
- Basic invariants:
  - No transition to open from pure successes
  - Transition to open after failure threshold
  - `should_allow` consistency with state
- Uses only public API (no direct ETS access)

**Next Steps**:
- [ ] Add property profile to `ct.config` (if needed) - ✅ not needed, works with default config
- [ ] Integrate into nightly pipeline - ✅ CI job added
- [ ] Document in `R10_CI_PROFILES.md`
- [ ] Verify execution time < 0.5 seconds

**Status**: ✅ Property test suite ready, CI job added

**Estimated Effort**: 30 minutes (verification)

---

## P3 — Operational Safety (Desirable)

### 7. Automated Protective Rails Validation

**Status**: ✅ **Complete** - Script created and CI job added

**Implementation**:
- ✅ `scripts/check_r10_protective_rails.sh` created
- ✅ Prohibits:
  - `ets:lookup(router_provider_circuit_breaker`
  - `ets:lookup(router_metrics` (except cleanup patterns)
  - Hardcoded trigger reason binaries
- ✅ Requires use of `router_r10_metrics:trigger_reason_*()` constants
- ✅ Added to CI pipeline as `router-r10-protective-rails` job

**Deliverable**: ✅ Complete
- Validation script (`scripts/check_r10_protective_rails.sh`)
- CI integration (`.gitlab-ci.yml`)
- Clear error messages with fixes

**Estimated Effort**: ✅ Completed

---

### 8. R10 Health-Check CLI Command

**Status**: ✅ **Artifact Created** - CLI module ready

**Location**: `apps/otp/router/src/router_ctl_r10.erl`

**Implementation**:
- Command: `router_ctl r10 status <tenant> <provider>`
- Output includes:
  - Current breaker state (closed/open/half_open)
  - Latest trigger reason
  - Error rate (percentage)
  - Timeout remaining (if in open state)
  - Should allow check
  - Runbook URL

**Next Steps**:
- [ ] Integrate into existing `router_ctl` escript (see `docs/R10_CLI_INTEGRATION.md`)
- [ ] Add command routing in `router_ctl` main function
- [ ] Test CLI command locally
- [ ] Document in `R10_RUNBOOK.md` (✅ already documented) and router admin docs

**Status**: ✅ CLI module ready, integration guide created

**Estimated Effort**: 1-2 hours (integration and testing)

---

## P4 — Pattern Replication (Optional)

### 9. R10 Template for Future Risk Modules

**Goal**: Formalize R10 pattern for R11/R12/etc.

**Content**:
- State machine module structure
- Metrics module pattern (`*_rN_metrics.erl`)
- Unit test suite structure
- E2E test suite structure
- Observability specification template
- Maintenance checklist template

**Deliverable**:
- `docs/dev/R10_TEMPLATE_FOR_RISK_MODULES.md`
- Reference in `OBSERVABILITY_CONVENTIONS.md`

**Estimated Effort**: 2-3 hours

---

## Implementation Order

### Phase 1: Production Readiness (P1)
1. Grafana Dashboard (2-4h)
2. Production Alerts (2-3h)
3. Runbook Integration (1-2h)
4. CI Validation (2-3h)

**Total**: 7-12 hours

### Phase 2: Stability (P2)
5. Test Metrics Cleanup (1-2h)
6. Property Tests (3-4h)

**Total**: 4-6 hours

### Phase 3: Operational Safety (P3)
7. Protective Rails Automation (2-3h)
8. CLI Health Check (2-3h)

**Total**: 4-6 hours

### Phase 4: Pattern Replication (P4)
9. Template Documentation (2-3h)

**Total**: 2-3 hours

---

## Success Criteria

### P1 Complete When:
- ✅ Dashboard visible in production Grafana
- ✅ All 4 alerts deployed and tested
- ✅ Runbook accessible from incident tools
- ✅ CI jobs validated with working artifacts

### P2 Complete When:
- ✅ Test metrics automatically cleaned in CI
- ✅ Property tests running in nightly pipeline

### P3 Complete When:
- ✅ Protective rails validated automatically in CI
- ✅ CLI command available for operators

### P4 Complete When:
- ✅ Template documentation available for future risk modules

---

## Notes

- **P1 is required** before R10 can be considered fully integrated into production
- **P2 improves stability** and reduces maintenance burden
- **P3 enhances operational safety** but can be deferred if needed
- **P4 is optional** and can be done when needed for R11/R12

