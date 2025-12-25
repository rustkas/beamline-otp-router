## In scope
- All `*_SUITE.erl` files under `apps/otp/router/test`.
- Refactoring `all/0`, `groups/0`, and adding `groups_for_level/1`.
- Purely structural changes to test grouping and selection.

## Out of scope
- Changing test logic or assertions.
- Renaming test cases unless required by grouping.
- Introducing new test cases.
- Performance optimizations of tests.

## Constraints
- Must remain Common Test compliant.
- No suppression of compiler or linter warnings.
- No dynamic test selection outside `groups_for_level/1`.
