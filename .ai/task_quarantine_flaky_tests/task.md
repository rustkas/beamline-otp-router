## Task: Introduce quarantine mechanism for truly flaky tests

- Introduce an explicit quarantine metadata source (tag/group/file) so truly flaky suites are easy to spot and maintain.
- Ensure quarantined suites are excluded from PR/full tiers (`scripts/ct-full.sh`, `ROUTER_TEST_LEVEL=full`) but still execute in nightly/heavy coverage runs (`scripts/ct-heavy.sh` / `ROUTER_TEST_LEVEL=heavy`).
- Keep the mechanism visible in CI tooling (listing/annotations in the full-tier runner) rather than silently skipping suites.
- Enable clear verification: a quarantined suite should not appear in a `scripts/ct-full.sh --list` run but should run when the heavy tier is invoked.
