## Acceptance

- There is an explicit quarantine metadata file (e.g., `config/quarantine/quarantined_suites.txt`) whose format is documented and which lists at least one suite with a reason/owner for the new policy.
- `bash scripts/ct-full.sh --list` outputs a “Quarantined suites” block, excludes each listed suite from execution, and indicates the associated reason so reviewers can understand the exception without scanning code.
- `ROUTER_TEST_LEVEL=heavy bash scripts/ct-heavy.sh` (or the equivalent nightly rebar run) still runs every quarantined suite, and the heavy runner logs that it is covering those suites so that no coverage is lost.
- `docs/testing/TEST_GOVERNANCE.md` (or a linked note) explains how to add/remove entries, describes when a suite enters/leaves quarantine, and clarifies that quarantined suites are deferred to nightly/heavy tiers to preserve signal.
