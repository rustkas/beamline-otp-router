# E2E Fault Injection - Tasks Summary

**Base Status**: ✅ **Complete** (S1-S3 implemented, CI integrated, 100% coverage)

## Quick Task List

### 🔴 Immediate (Next 1-2 weeks)

1. **[monitor_e2e_stability]** Monitor CI stability
   - **When**: At least once per sprint and after changes in `router_nats` / fault-injection infrastructure
   - Track test results in GitHub/GitLab CI
   - Document flaky cases if any → create fix ticket

2. **[sanity_check_local_run]** One-time local sanity check (single execution)
   - **When**: Once, after initial implementation
   - Run `scripts/run_fault_injection_e2e_tests.sh` locally
   - Verify script works, document if needed
   - ✅ Mark as done after first successful run

### 🟡 Ongoing Maintenance

3. **[maintain_spec_and_matrix]** Keep docs in sync
   - **When**: On any change to fault-injection behavior, retry/MaxDeliver policy, or JetStream metrics
   - Update spec + coverage matrix when requirements change
   - Follow template: spec → plan → implementation → matrix

4. **[regression_review_fault_injection_suites]** Regression review
   - **When**: Modifying `router_nats`, `router_result_consumer`, or metrics/telemetry definitions
   - Review: ACK/NAK points, MaxDeliver, metric semantics
   - Update tests/expectations if needed

---

## Task Details

See full details: [`E2E_FAULT_INJECTION_NEXT_TASKS.md`](./E2E_FAULT_INJECTION_NEXT_TASKS.md)

---

## Completed ✅

- ✅ S1-S3 implementation (5 tests total)
- ✅ CI integration (GitHub Actions, GitLab CI)
- ✅ Documentation (coverage matrix, reports)
- ✅ Test execution script
- ✅ File management (removed `.skip`)

---

**Last Updated**: 2025-11-30

