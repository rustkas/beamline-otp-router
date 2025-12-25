# Scope

## In Scope
- **Legacy Suite Migration:** Suites currently using `groups_for_level/1` where the logic is identical across tiers (sanity, fast, full, heavy) will be converted to use a static `all/0` and `groups/0`.
- **Cleanup:** Removal of unused exports (`groups_for_level/1`, `meta_all/0`) exposed by the structural changes.
- **Verification:** Running `rebar3 ct` for each migrated suite to ensure correctness.
- **Heavy Verification:** Verification of migrated suites under `ROUTER_TEST_LEVEL=heavy` if applicable.

## Out of Scope
- **New Quarantine:** Adding any new entries to `config/quarantine/quarantined_suites.txt`.
- **Policy Changes:** Modifying CI gates or quarantine policies.
- **Deep Bug Fixes:** Solving complex functional bugs unrelated to test structure (e.g., RBAC consistency, Logger issues). These will be documented as skips/debt.
- **Production Code Refactoring:** Changes are limited to `test/` directory.

## Constraints
- **One Suite at a Time:** strict iterative process.
- **Progress Truth:** `progress.md` is the single source of truth.
- **No Magic:** No `ROUTER_TEST_LEVEL` branching inside suites; runners handle level decisions (if any needed), but ideally suites are just "dumb" collections of tests.
