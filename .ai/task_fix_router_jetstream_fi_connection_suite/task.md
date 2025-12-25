# Task: Fix router_jetstream_fi_connection_SUITE

## Objective
Make `router_jetstream_fi_connection_SUITE` reliably pass in deterministic runs.

## Context
This suite likely tests Fault Injection (FI) scenarios for JetStream connections. It is currently unstable or failing. We need to apply deterministic fixes, preferring test-side changes (mocks, waits, sequencing) over production changes.

## Constraints
1. **Quarantine Governance**: Do NOT touch `scripts/check_quarantine_policy.sh` or `config/quarantine/quarantined_suites.txt`.
2. **Test Isolation**: Prefer fixes in the test suite itself.
3. **Scope**: Only `router_jetstream_fi_connection_SUITE`.
