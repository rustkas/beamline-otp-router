# Cleanup Plan

Generated: 2025-12-20

## Phase 1: DELETE - Execution Logs (92 files)

All `TODO_EXECUTION_*` files are session logs with no canonical value.

**Action: DELETE ALL**

```bash
rm -f docs/dev/TODO_EXECUTION_*.md
```

Files to delete:
- TODO_EXECUTION_FINAL_REPORT.md
- TODO_EXECUTION_PROGRESS.md
- TODO_EXECUTION_SESSION*.md (all variants)
- TODO_EXECUTION_SUMMARY.md
- TODO_IMPLEMENTATION_PLAN.md

---

## Phase 2: ARCHIVE - Status/Report Files

Move to `docs/archive/` (create if not exists)

### docs/ top-level (15 files)
```
NATS_RESILIENCE_COMPLETE.md
NATS_RESILIENCE_NEXT_STEPS.md  
NATS_RESILIENCE_STATUS.md
NATS_RESILIENCE_SUMMARY.md
CHANGELOG_NATS_PUBLISH_FAILURE.md
MR_DESCRIPTION_NATS_PUBLISH_FAILURE.md
API_DOCUMENTATION_COMPLETE.md
CONFIGURATION_REFERENCE_COMPLETE.md
OPERATIONAL_RUNBOOK_COMPLETE.md
OBSERVABILITY_CP2_PLANNING.md
CP1_TESTING.md
QA_TEST_PLAN.md
RELEASE_NOTES_GRPC_API.md
```

### docs/dev/ - All *_REPORT.md files (~50 files)
```bash
mv docs/dev/*_REPORT*.md docs/archive/dev_reports/
```

### docs/dev/ - All *_COMPLETE*.md, *_STATUS*.md, *_SUMMARY*.md
```bash
mv docs/dev/*_COMPLETE*.md docs/archive/
mv docs/dev/*_STATUS*.md docs/archive/
mv docs/dev/*_SUMMARY*.md docs/archive/
```

---

## Phase 3: MERGE - Consolidate Documents

### 3.1 NATS Documentation → NATS_INTEGRATION_GUIDE.md

Merge these into `NATS_INTEGRATION_GUIDE.md`:
- NATS_BEST_PRACTICES.md
- NATS_CONNECTION_RESILIENCE.md
- NATS_PUBLISH_FAILURE_BEHAVIOR.md

After merge, delete originals.

### 3.2 Metrics Documentation → METRICS_DOCUMENTATION.md

Merge:
- NATS_METRICS_COMPLIANCE.md
- ROUTER_NATS_METRICS.md

### 3.3 Alerts → PROMETHEUS_ALERTS.md

Merge:
- NATS_METRICS_ALERTS.md
- NATS_PUBLISH_FAILURE_METRICS_ALERTS.md

### 3.4 Configuration → CONFIG.md

Merge:
- ROUTER_CAF_CONFIG.md

### 3.5 Testing → TESTING_GUIDE.md

Merge:
- TEST_ENVIRONMENT.md
- TESTING_RECOMMENDATIONS.md
- CONCURRENCY_TESTS_ETS_AND_MECK.md

### 3.6 Operations → OPERATIONAL_RUNBOOK.md

Merge:
- OPERATIONAL_GUIDE.md
- NATS_PRODUCTION_MONITORING.md
- NATS_PUBLISH_FAILURE_MONITORING.md

### 3.7 R10 → R10_RUNBOOK.md

Merge:
- R10_CLI_EXAMPLES.md
- R10_CLI_INTEGRATION.md
- R10_HOW_TO_OPERATE.md
- R10_SRE_OPERATIONAL_CHECKLIST.md

### 3.8 Performance → PERFORMANCE_GUIDE.md

Merge:
- PERFORMANCE.md

### 3.9 Extended Recovery → EXTENDED_RECOVERY.md (new)

Merge:
- EXTENDED_RECOVERY_IMPLEMENTATION_SUMMARY.md
- EXTENDED_RECOVERY_PRODUCTION_SCALE.md
- EXTENDED_RECOVERY_RESOURCE_LIMITS.md

### 3.10 Security → SECURITY_GUIDE.md

Merge:
- EXTENSIONS_SECURITY_GUIDE.md

### 3.11 Observability → OBSERVABILITY.md

Merge:
- OBSERVABILITY_DASHBOARD.md
- TELEMETRY_CAF_ADAPTER.md → TELEMETRY_EVENTS.md

### 3.12 ELP → ELP_SETUP.md

Merge:
- ELP_QUICK_FIX.md

---

## Phase 4: docs/dev/ Cleanup

After Phase 1-3, review remaining docs/dev/ files:

### Keep (Canonical dev docs)
- DEVELOPER_GUIDE.md
- DEPLOYMENT_GUIDE.md
- CONFIGURATION_REFERENCE.md
- CT_SUITE_CONVENTIONS.md
- GEN_SERVER_RESET_LIFECYCLE_PATTERN.md
- TEST_HELPERS_GUIDE.md
- METRICS_CONTRACT_SPECIFICATION.md
- METRICS_MODULE_TEMPLATE.md
- NETWORK_PARTITION_TESTING.md
- STRESS_RUN_GUIDE.md

### Archive (Historical/Report)
- All *_VERIFICATION*.md
- All *_IMPLEMENTATION*.md (except guides)
- All CP1_*, CP2_* files
- All FAULT_INJECTION_* status files
- All TASK*_REPORT.md files

---

## Phase 5: Create docs/README.md Index

Create navigation index:
```markdown
# Router Documentation

## Quick Links
- [Architecture](ARCHITECTURE_DOCUMENTATION.md)
- [API Contracts](API_CONTRACTS.md)
- [Configuration](CONFIG.md)
- [Testing Guide](TESTING_GUIDE.md)
- [Operational Runbook](OPERATIONAL_RUNBOOK.md)

## By Domain
- Architecture & Design
- API & Contracts
- NATS & Messaging
- Observability & Metrics
- Security
- Testing
- Operations
- Developer Guide
```

---

## Execution Order

1. ✅ Create archive directory structure
2. 🔄 Phase 1: Delete TODO_EXECUTION_* (92 files)
3. 🔄 Phase 2: Archive status/report files (~100 files)
4. 🔄 Phase 3: Merge documents (~25 merges)
5. 🔄 Phase 4: Review docs/dev/
6. 🔄 Phase 5: Create README.md index
7. 🔄 Final verification

---

## Expected Results

| Metric | Before | After | Reduction |
|--------|--------|-------|-----------|
| Total files | 481 | ~100 | 79% |
| docs/ top-level | 75 | ~25 | 67% |
| docs/dev/ | 258 | ~30 | 88% |
| Canonical docs | unclear | ~25 | - |
