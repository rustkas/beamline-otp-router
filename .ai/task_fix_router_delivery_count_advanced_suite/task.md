# Task: Fix router_delivery_count_advanced_SUITE

## Objective
Make `router_delivery_count_advanced_SUITE` reliably pass in its intended tier(s).

## Context
This suite likely tests advanced delivery count logic (e.g. JetStream redelivery counts, max deliver attempts, DLQ routing). It is currently unstable or failing. We need to apply deterministic fixes.

## Constraints
1. **Quarantine Governance**: Do NOT touch `scripts/check_quarantine_policy.sh` or `config/quarantine/quarantined_suites.txt`.
2. **Test Isolation**: Prefer fixes in the test suite itself.
3. **Scope**: Only `router_delivery_count_advanced_SUITE`.
