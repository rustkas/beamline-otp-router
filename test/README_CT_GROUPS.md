# Common Test Groups Policy

## quarantine

- `quarantine` is a native CT group
- Quarantined suites MUST define `group(quarantine, ...)`
- Inclusion/exclusion is controlled by CT flags and suite-declared groups
- Runners do NOT filter by name

## Source of truth
- `config/quarantine/quarantined_suites.txt`

RULES:
- QUARANTINED_SUITES.TXT — GOVERNANCE ONLY.
- EXECUTION TRUTH = COMMON TEST GROUPS.
- CONSISTENCY ENFORCED BY LINT.
