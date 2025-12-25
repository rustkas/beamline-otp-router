# Acceptance Criteria

## Functional
1. Every quarantined suite/test MUST have:
   - owner
   - reason
   - date (ISO-8601 or epoch)
2. Missing metadata is detected automatically.

## Enforcement
3. A quarantine older than configured TTL (e.g. 30 days):
   - produces a CI warning (non-fatal), OR
   - fails CI (configurable, default: warning).

## Verification
4. A single command exists to validate quarantine policy, e.g.:
   - scripts/check_quarantine_policy.sh
5. Command exits non-zero on policy violation (per config).

## Documentation
6. Governance rules are documented and discoverable.
7. `progress.md` contains evidence of:
   - command output,
   - at least one passing case,
   - at least one failing (simulated) case.
