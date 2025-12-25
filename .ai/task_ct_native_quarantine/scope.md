IN SCOPE:
- A canonical helper module that suites call to define tier-aware CT groups in a uniform way.
- Migrating suites to a standard groups/all pattern if necessary.
- Updating scripts/ct-full.sh and scripts/ct-heavy.sh to pass CT group include/exclude flags.
- Keeping quarantine metadata list authoritative.
- A staged rollout approach: migrate in batches, keeping full tier stable.

OUT OF SCOPE:
- Fixing the flaky tests themselves beyond making quarantine mechanics correct.
- CI scheduling redesign.
