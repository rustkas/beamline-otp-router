# E2E Fault Injection - Next Tasks

**Status**: ✅ Base implementation complete (S1-S3, CI integration, documentation)  
**Focus**: Monitoring, maintenance, and regression control

## Tactical Tasks (Immediate)

### [monitor_e2e_stability] Monitor CI Stability

**Goal**: Track test stability in CI/CD pipelines

**Frequency**: 
- Review CI job results **once per sprint** (or on-demand)
- **Mandatory review** when changes are made to `router_nats`, `router_result_consumer`, or JetStream-related modules

**Actions**:
- Monitor `router_jetstream_e2e_SUITE` and `router_delivery_count_tracking_SUITE` results in GitHub/GitLab CI
- On first flakiness signs:
  - Document specific test cases/conditions in `E2E_FAULT_INJECTION_IMPLEMENTATION_REPORT.md` (add "Known flaky cases" section)
  - Create separate ticket to fix flakiness factors (timeouts, overly strict expectations, etc.)

**Success Criteria**:
- CI runs stable (no flakiness for 2+ weeks)
- Known flaky cases documented (if any)

---

### [sanity_check_local_run] One-time Local Sanity Check

**Goal**: Verify test script works on clean developer machine

**Type**: **One-time task** (not recurring)

**Definition of Done**:
- ✅ Script `scripts/run_fault_injection_e2e_tests.sh` successfully executed on typical dev machine (3 runs)
- ✅ All runs completed without errors
- ✅ Any issues/observations documented in implementation report
- ✅ Local run instructions added to documentation (if missing)

**Actions**:
- Run `scripts/run_fault_injection_e2e_tests.sh` locally (3 runs)
- Verify script works on clean environment
- If needed, add "How to run locally" section to documentation

**Success Criteria**:
- Script runs successfully on clean machine
- Local run instructions documented (if missing)

---

## Maintenance Tasks (Ongoing)

### [maintain_spec_and_matrix] Keep Spec and Coverage Matrix Up-to-Date

**Goal**: Maintain documentation sync when business requirements change

**Triggers** (execute when any of the following changes):
- **JETSTREAM fault-injection behavior** (new fault types, changed failure semantics)
- **Retry/MaxDeliver policy** (retry logic, MaxDeliver limits, backoff strategies)
- **JetStream metrics set** (new metrics, changed metric labels, removed metrics)
- **New scenarios S4+** added to specification

**Actions**:
1. Update `JETSTREAM_FAULT_INJECTION_TESTS.md` (spec)
2. Synchronously update:
   - `E2E_FAULT_INJECTION_COVERAGE_PLAN.md`
   - `E2E_FAULT_INJECTION_IMPLEMENTATION_REPORT.md`
   - `E2E_FAULT_INJECTION_FINAL_STATUS.md`
3. For new scenarios S4+:
   - Follow existing template: spec → plan → implementation → matrix update

**Success Criteria**:
- All documentation files stay in sync
- Coverage matrix reflects current test state

---

### [regression_review_fault_injection_suites] Regression Review on Key Module Changes

**Goal**: Ensure fault injection tests remain valid when core modules change

**Triggers**: Changes to:
- `router_nats` (ACK/NAK implementation)
- `router_result_consumer` (message processing)
- Metrics/telemetry subsystem
- `router_jetstream` (MaxDeliver logic)

**Checklist**:
- [ ] Review if changes break:
  - ACK/NAK call points
  - MaxDeliver behavior
  - Metric semantics (`*_redelivery_total`, `*_maxdeliver_exhausted_total`)
- [ ] If needed:
  - Update existing tests (metric expectations, labels, timeouts)
  - Update scenario descriptions in documentation

**Success Criteria**:
- Tests remain valid after module changes
- Test expectations match new implementation

---

## Completed Tasks ✅

- ✅ Analysis: Spec review, SUITE review, gap analysis
- ✅ Planning: Coverage plan with requirements
- ✅ Implementation: S1-S3 tests in both SUITEs
- ✅ CI Integration: GitHub Actions, GitLab CI updated
- ✅ Documentation: Coverage matrix, implementation report, final status
- ✅ Scripts: Test execution script with stability verification
- ✅ File Management: Removed `.skip` extension

---

## Task Tracking

| Task ID | Priority | Status | Owner | Notes |
|---------|----------|--------|-------|-------|
| `monitor_e2e_stability` | High | ⏳ Pending | - | Start after first CI runs |
| `sanity_check_local_run` | Medium | ⏳ Pending | - | One-time task |
| `maintain_spec_and_matrix` | Medium | 🔄 Ongoing | - | As needed |
| `regression_review_fault_injection_suites` | High | 🔄 Ongoing | - | On module changes |

---

## Quick Reference

**Test Execution**:
```bash
cd apps/otp/router
bash scripts/run_fault_injection_e2e_tests.sh
```

**CI Monitoring**:
- GitHub Actions: `.github/workflows/validate.yml.template` (CP2 profile)
- GitLab CI: `.gitlab-ci.yml` (router-observability-tests job)

**Documentation**:
- Spec: `docs/archive/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- Coverage Plan: `docs/archive/dev/E2E_FAULT_INJECTION_COVERAGE_PLAN.md`
- Implementation Report: `docs/archive/dev/E2E_FAULT_INJECTION_IMPLEMENTATION_REPORT.md`
- Final Status: `docs/archive/dev/E2E_FAULT_INJECTION_FINAL_STATUS.md`

---

**Last Updated**: 2025-11-30  
**Next Review**: After first CI runs (monitor stability)

