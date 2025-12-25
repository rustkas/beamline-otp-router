# Acceptance Criteria

## Functional Requirements
- [x] Tests marked with `{quarantine, [...]}` are excluded from PR/CI test runs
- [x] Quarantined tests are included in nightly/heavy test runs
- [x] Existing quarantine mechanism continues to work as before
- [x] Clear error messages when quarantine group is misused

## Non-Functional Requirements
- [x] No performance regression in test execution
- [x] Backward compatibility maintained
- [x] Documentation updated
- [x] Test coverage for new functionality

## Success Metrics
- [x] 100% of existing quarantined tests can be migrated to use the new group
- [x] Zero impact on test execution time for non-quarantined tests
- [x] No test failures introduced by the change
- [x] All CI/CD pipelines pass with the new implementation

## Verification
- [x] Manual verification of test execution with quarantine group
- [x] Automated tests for quarantine group behavior
- [x] CI/CD pipeline verification (ct-heavy.sh verified)
- [x] Documentation review (TEST_GOVERNANCE.md updated)

## Verification Evidence

### Functional Verification
1. **Full tier excludes quarantine**: `ROUTER_TEST_LEVEL=full` runs 8 tests for alerts, 9 for nats (no quarantine)
2. **Heavy tier includes quarantine**: `ROUTER_TEST_LEVEL=heavy` runs 9 tests for alerts, 11 for nats (includes quarantine)
3. **Existing mechanism works**: `config/quarantine/quarantined_suites.txt` still used by ct-full.sh

### Log Paths
- CT logs: `_build/test/logs/ct_run.nonode@nohost.2025-12-16_11.05.12/`
- Latest: `_build/test/logs/last/`

### Status: COMPLETE ✅
