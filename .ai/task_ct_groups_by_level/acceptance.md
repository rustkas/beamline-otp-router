## Acceptance Criteria

1. Every `*_SUITE.erl` defines:
   - `all/0`
   - `groups/0`
   - `groups_for_level/1`

2. `all/0`:
   - Reads `ROUTER_TEST_LEVEL`
   - Maps it to one of: fast | full | heavy
   - Delegates ONLY to `groups_for_level(Level)`

3. `groups_for_level/1`:
   - Explicitly defines behavior for fast, full, heavy
   - Returns only `{group, GroupName}` tuples

4. `groups/0`:
   - Contains ALL test cases
   - No test case exists outside a group

5. Behavior verification:
   - `ROUTER_TEST_LEVEL=fast` runs only fast groups
   - `ROUTER_TEST_LEVEL=full` excludes heavy-only groups
   - `ROUTER_TEST_LEVEL=heavy` runs the superset

6. All suites compile and run under `rebar3 ct`.
