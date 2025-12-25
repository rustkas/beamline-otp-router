## Execution Plan

1. Inventory all `*_SUITE.erl` files and classify them:
   - already grouped
   - ungrouped
   - heavy-only (soak/chaos/load/stress by name)

2. Define and agree on mapping rules between group names and levels.

3. Apply the standard pattern:
   - normalize `all/0`
   - introduce `groups_for_level/1`
   - refactor `groups/0` if needed

4. Validate behavior per suite with:
   - fast
   - full
   - heavy

5. Record progress per suite in `progress.md`.
