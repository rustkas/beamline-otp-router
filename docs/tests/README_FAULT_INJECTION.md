# Fault Injection Test Suite - Quick Reference

## Overview

This directory contains comprehensive fault injection and recovery tests for router consumers (`router_result_consumer` and `router_decide_consumer`). These tests verify system resilience, state consistency, and recovery mechanisms under various fault conditions.

## 🎯 Quick Navigation by Use Case

**I want to...**

- **Add a new fault scenario** → See [`FAULT_INJECTION_MAINTENANCE_PROCESS.md`](FAULT_INJECTION_MAINTENANCE_PROCESS.md#process-1-traceability-maintenance) (Process 1: Traceability Maintenance)
- **Fix a failing test** → See [`FAULT_INJECTION_TEST_STABILITY.md`](FAULT_INJECTION_TEST_STABILITY.md) (Flaky Test Tracking)
- **Update requirements/TZ** → See [`FAULT_INJECTION_MAINTENANCE_PROCESS.md`](FAULT_INJECTION_MAINTENANCE_PROCESS.md#process-1-traceability-maintenance) (Update Checklist)
- **Understand what tests guarantee** → See [`FAULT_INJECTION_TEST_SCENARIOS.md`](FAULT_INJECTION_TEST_SCENARIOS.md) (Test Scenarios)
- **Run smoke tests for production incident** → See [`FAULT_INJECTION_SMOKE_TESTS.md`](FAULT_INJECTION_SMOKE_TESTS.md) (Minimal Critical Smoke Test Set)
- **Extend tests to new backend (Kafka, Redis)** → See [`FAULT_INJECTION_BACKEND_EXTENSIBILITY.md`](FAULT_INJECTION_BACKEND_EXTENSIBILITY.md) (Extension Guide)
- **Update runbook with test references** → See [`FAULT_INJECTION_RUNBOOK_INTEGRATION.md`](FAULT_INJECTION_RUNBOOK_INTEGRATION.md) (Runbook Update Checklist)
- **Review test coverage and traceability** → See [`FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`](FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md) (Requirements to Test Mapping)

## 🚀 Typical Workflows (5-10 minutes)

### Workflow 1: Investigating a Production Incident

**Scenario**: Production alert shows `router_jetstream_redelivery_total` increasing rapidly.

**Steps**:
1. **Check observability docs** → `docs/PROMETHEUS_ALERTS.md` (see alert definition and thresholds)
2. **Find related test scenario** → `FAULT_INJECTION_TEST_SCENARIOS.md` (search for "redelivery" or "NAK")
3. **Run smoke test** → `FAULT_INJECTION_SMOKE_TESTS.md` (quick validation: `test_nats_connection_loss_recovery`)
4. **Check runbook** → `docs/OPS_RUNBOOK_ROUTER_INTAKE.md` (troubleshooting steps)
5. **Verify test guarantees** → `FAULT_INJECTION_TEST_SCENARIOS.md` (what the test verifies)

**Time**: ~5-7 minutes to understand the scenario and find relevant tests/runbooks.

### Workflow 2: Adding a New Fault Scenario

**Scenario**: Need to test a new failure pattern (e.g., Kafka connection loss).

**Steps**:
1. **Check extensibility guide** → `FAULT_INJECTION_BACKEND_EXTENSIBILITY.md` (if new backend) or `FAULT_INJECTION_MAINTENANCE_PROCESS.md` (Process 1) if existing backend
2. **Update traceability** → `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md` (add requirement, map to test)
3. **Document scenario** → `FAULT_INJECTION_TEST_SCENARIOS.md` (add scenario description)
4. **Write test** → Add to appropriate `*_SUITE.erl` file
5. **Update smoke tests** → `FAULT_INJECTION_SMOKE_TESTS.md` (if scenario becomes critical)

**Time**: ~10 minutes to understand the process and start implementation.

### Workflow 3: Understanding Test Guarantees for a Specific Scenario

**Scenario**: Need to know what `test_max_delivery_count_exhaustion` guarantees.

**Steps**:
1. **Find test scenario** → `FAULT_INJECTION_TEST_SCENARIOS.md` (search for "Max Delivery Count Exhaustion")
2. **Check traceability** → `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md` (see requirement R4.1)
3. **Check runbook integration** → `FAULT_INJECTION_RUNBOOK_INTEGRATION.md` (see runbook mapping)
4. **Review test code** → `router_result_consumer_SUITE.erl` or `router_decide_consumer_SUITE.erl` (see actual test implementation)

**Time**: ~3-5 minutes to understand what the test verifies.

## 📚 Documentation Map

**When to read which document:**

| Document | When to Read | Key Content |
|----------|--------------|-------------|
| **`FAULT_INJECTION_MAINTENANCE_PROCESS.md`** | When requirements/system change, adding tests, quarterly reviews | Maintenance processes, update checklists, review schedules |
| **`FAULT_INJECTION_TEST_STABILITY.md`** | Test fails in CI, investigating flakiness, monthly review | Stability metrics, flaky test tracking, monitoring process |
| **`FAULT_INJECTION_TEST_SCENARIOS.md`** | Understanding what tests cover, writing new tests | Detailed test scenarios, test differentiation, coverage |
| **`FAULT_INJECTION_SMOKE_TESTS.md`** | Production incident, quick validation, CI smoke runs | Minimal critical test set, quick smoke test commands |
| **`FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`** | Requirements change, verifying coverage, quarterly audit | Requirements → tests mapping, coverage summary |
| **`FAULT_INJECTION_RUNBOOK_INTEGRATION.md`** | Updating ops runbooks, linking tests to incidents | Runbook → test mapping, update checklist |
| **`FAULT_INJECTION_BACKEND_EXTENSIBILITY.md`** | Adding support for new backend (Kafka, Redis, etc.) | Extension guide, reusable abstractions, migration steps |
| **`FAULT_INJECTION_TEST_AUDIT.md`** | Understanding test overlap, quarterly review | Test differentiation, overlap analysis |
| **`FAULT_INJECTION_MAINTENANCE_DRY_RUN.md`** | Quarterly maintenance validation | Dry-run results, process validation, recommendations |

## Quick Links

### Documentation

- **Test Scenarios**: [`FAULT_INJECTION_TEST_SCENARIOS.md`](FAULT_INJECTION_TEST_SCENARIOS.md) - Detailed test scenarios documentation
- **Smoke Tests**: [`FAULT_INJECTION_SMOKE_TESTS.md`](FAULT_INJECTION_SMOKE_TESTS.md) - Minimal critical smoke test set
- **Requirements Traceability**: [`FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`](FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md) - Requirements to test mapping
- **Test Audit**: [`FAULT_INJECTION_TEST_AUDIT.md`](FAULT_INJECTION_TEST_AUDIT.md) - Test differentiation and overlap analysis
- **Maintenance Process**: [`FAULT_INJECTION_MAINTENANCE_PROCESS.md`](FAULT_INJECTION_MAINTENANCE_PROCESS.md) - Maintenance and evolution processes
- **Test Stability**: [`FAULT_INJECTION_TEST_STABILITY.md`](FAULT_INJECTION_TEST_STABILITY.md) - Stability tracking and monitoring
- **Backend Extensibility**: [`FAULT_INJECTION_BACKEND_EXTENSIBILITY.md`](FAULT_INJECTION_BACKEND_EXTENSIBILITY.md) - Guide for extending to other backends
- **Runbook Integration**: [`FAULT_INJECTION_RUNBOOK_INTEGRATION.md`](FAULT_INJECTION_RUNBOOK_INTEGRATION.md) - Integration with operational runbooks
- **Maintenance Dry Run**: [`FAULT_INJECTION_MAINTENANCE_DRY_RUN.md`](FAULT_INJECTION_MAINTENANCE_DRY_RUN.md) - Quarterly maintenance process validation report

### Test Suites

- **`router_result_consumer_SUITE.erl`** - Result consumer fault injection tests
- **`router_decide_consumer_SUITE.erl`** - Decide consumer fault injection tests
- **`router_jetstream_fault_injection_SUITE.erl`** - NATS/JetStream integration fault tests

## Running Tests

### All Fault Injection Tests

```bash
cd apps/otp/router
rebar3 ct --suite test/router_result_consumer_SUITE \
         --suite test/router_decide_consumer_SUITE \
         --suite test/router_jetstream_fault_injection_SUITE
```

### Smoke Tests Only

```bash
cd apps/otp/router
rebar3 ct \
  --suite test/router_jetstream_fault_injection_SUITE --case test_nats_connection_loss_recovery \
  --suite test/router_result_consumer_SUITE --case test_max_delivery_count_exhaustion,test_prolonged_fault_period_recovery_no_router_restart \
  --suite test/router_decide_consumer_SUITE --case test_decide_max_delivery_count_exhaustion,test_decide_prolonged_fault_period_recovery_no_router_restart
```

### Via Makefile

```bash
make test-slow  # Includes all fault injection tests
```

## Test Coverage

### Requirements Coverage: 100%

- **R1: Concurrent Fault Handling** - 4 requirements, 4 covered
- **R2: Prolonged Fault Periods** - 1 requirement, 1 covered
- **R3: ETS and Delivery Count Integrity** - 3 requirements, 3 covered
- **R4: Max Delivery Count Exhaustion** - 1 requirement, 1 covered
- **R5: Multiple Fault → Recovery Cycles** - 1 requirement, 1 covered
- **R6: NATS/JetStream Restart** - 3 requirements, 3 covered

### Critical Invariants

- **I1: No Router Restart** - Router/consumer processes remain alive throughout all fault scenarios
- **I2: Tracking State Consistency** - ETS delivery counts remain consistent before, during, and after faults
- **I3: Recovery** - System recovers from faults without requiring manual intervention
- **I4: Absence of Infinite Retries** - Messages do not retry indefinitely

## Maintenance Schedule

### Regular Reviews

- **Weekly**: Monitor CI/CD for test failures → See [`FAULT_INJECTION_TEST_STABILITY.md`](FAULT_INJECTION_TEST_STABILITY.md#monitoring-process)
- **Monthly**: Review test stability metrics → See [`FAULT_INJECTION_TEST_STABILITY.md`](FAULT_INJECTION_TEST_STABILITY.md#review-schedule)
- **Quarterly**: Review smoke test set and traceability → See [`FAULT_INJECTION_MAINTENANCE_PROCESS.md`](FAULT_INJECTION_MAINTENANCE_PROCESS.md#process-3-smoke-test-review)

### When Requirements Change

**Quick checklist** (detailed process in [`FAULT_INJECTION_MAINTENANCE_PROCESS.md`](FAULT_INJECTION_MAINTENANCE_PROCESS.md#process-1-traceability-maintenance)):

1. Update `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md` (add requirements, map to tests)
2. Update `FAULT_INJECTION_TEST_SCENARIOS.md` (document new scenarios)
3. Update smoke tests if needed (see [`FAULT_INJECTION_SMOKE_TESTS.md`](FAULT_INJECTION_SMOKE_TESTS.md))
4. Verify test coverage remains 100%
5. Run all tests to verify no regressions

### When Metrics Contract Changes

**If `router_jetstream_redelivery_total` or `router_jetstream_maxdeliver_exhausted_total` contract changes**:

1. Update metric contract constants in test files (see `router_jetstream_fault_injection_SUITE.erl`)
2. Update `docs/PROMETHEUS_ALERTS.md` if documentation changed
3. Update runbook references if metrics are mentioned (see [`FAULT_INJECTION_RUNBOOK_INTEGRATION.md`](FAULT_INJECTION_RUNBOOK_INTEGRATION.md))
4. Update fault injection documentation if metrics are referenced
5. Run metric validation tests to verify contract compliance

See [`FAULT_INJECTION_MAINTENANCE_PROCESS.md`](FAULT_INJECTION_MAINTENANCE_PROCESS.md#process-6-metrics-contract-synchronization) for detailed metrics contract maintenance process.

## CI Integration

### Extended CI (Merge to main/staging)

✅ **Integrated**: Fault injection tests run in extended CI:
- Drone CI: `nats-fault-injection-tests` step
- GitLab CI: `router-observability-tests` job

### Nightly CI (Scheduled)

✅ **Integrated**: All fault injection tests run in nightly CI.

### Fast CI (PR Checks)

❌ **Not included**: Fault injection tests excluded to keep PR feedback quick (< 5 minutes).

## User Feedback & Improvements

### How to Provide Feedback

If you've used the fault injection documentation and workflows, please share feedback:

- **What was helpful**: Which workflows or sections were most useful?
- **What was unclear**: Which steps or links were confusing or missing?
- **What could be improved**: Suggestions for better navigation or documentation structure

**Where to provide feedback**:
- Create an issue or PR with improvements
- Update this section directly with your feedback
- Contact the team lead

### Recent Feedback

**2025-11-30 - Initial Setup**:
- Documentation and workflows created
- Awaiting first real usage feedback

**Feedback Collection Process**:
1. After next real incident or on-call simulation, team members should:
   - Use `Typical Workflows` section to navigate
   - Use `From Alert to Test` workflow when investigating alerts
   - Note any unclear steps, missing links, or unnecessary complexity
2. Provide feedback via:
   - Update this section directly
   - Create issue/PR with improvements
   - Share in team chat/meeting

**What to report**:
- ✅ Which workflows were actually used?
- ✅ Which steps were helpful vs. confusing?
- ✅ What was missing that you expected to find?
- ✅ What seemed unnecessary or redundant?

**First Feedback Cycle Template** (for next incident/simulation):

**Date**: [YYYY-MM-DD]  
**Context**: [Real incident / On-call simulation / Training session]  
**Participants**: [Names/roles]

**Workflows Used**:
- [ ] Workflow 1: Investigating a Production Incident
- [ ] Workflow 2: Adding a New Fault Scenario
- [ ] Workflow 3: Understanding Test Guarantees
- [ ] From Alert to Test (from `FAULT_INJECTION_TEST_SCENARIOS.md`)

**What Worked Well**:
- [List specific steps/links/examples that were helpful]

**What Was Confusing or Missing**:
- [List unclear steps, missing links, or expected but absent information]

**Suggestions for Improvement**:
- [Specific, actionable suggestions - e.g., "Add example for X scenario", "Clarify step Y wording"]

**Action Items** (if any):
- [ ] [Specific improvement to make]

**Last updated**: 2025-11-30  
**Next review**: After next on-call simulation or major incident

## References

- **Test Classification**: `apps/otp/router/docs/TEST_CLASSIFICATION.md`
- **Operational Runbooks**: `docs/OPS_RUNBOOK_ROUTER_INTAKE.md`
- **Router Operational Guide**: `apps/otp/router/docs/OPERATIONAL_GUIDE.md`
- **Observability & Metrics**: 
  - `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert definitions and metric contracts
  - `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` - Dashboard queries and monitoring
  - See [`FAULT_INJECTION_TEST_SCENARIOS.md`](FAULT_INJECTION_TEST_SCENARIOS.md#observability-integration) for links between fault injection scenarios and observability metrics/alerts

