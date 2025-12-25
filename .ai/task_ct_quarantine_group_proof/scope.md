# Scope Definition

## In Scope
- Verifying quarantine group exclusion in full test runs
- Verifying quarantine group inclusion in heavy test runs
- Providing executable proof of test execution behavior
- Documenting the test execution flow

## Out of Scope
- Modifying `ct-full.sh` or any other runner scripts
- Changing test implementation
- Fixing test failures (only proving the execution flow)
- Modifying the test environment setup

## Key Files to Reference
- `scripts/ct-full.sh` (read-only)
- `scripts/ct-heavy.sh`
- `test/router_alerts_test_SUITE.erl`
- `.ai/task_ct_quarantine_group/` (previous task artifacts)
