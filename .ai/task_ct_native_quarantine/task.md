Title:
CT-native quarantine using Common Test groups, with staged suite migration

Goal:
Implement a true Common Test quarantine mechanism using CT groups (group name: quarantine) so that:
- Full/PR tier excludes quarantine
- Heavy/nightly tier includes quarantine

Single source of truth:
- config/quarantine/quarantined_suites.txt remains authoritative (suite | owner | reason).

IMPORTANT: We accept mass changes to suites if required for correctness.
