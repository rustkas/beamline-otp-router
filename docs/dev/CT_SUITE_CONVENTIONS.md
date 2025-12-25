# Common Test Suite Conventions

This document codifies the invariants that every `*_SUITE.erl` test module must satisfy so that the Common Test framework and the suite linter behave deterministically.

## Required lifecycle callbacks
Every suite must explicitly define and export the following callbacks, even if they do nothing more than pass the configuration through:

```erlang
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
```

- These callbacks must be exported in the same module (see next section).
- When no setup/teardown is needed, simply return the incoming `Config` or `ok` so that suites remain stateless.
- Do not rely on global state or introduce delays such as `timer:sleep/1` inside these callbacks.

## Export ordering rules
`
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-module(...).
-export([... init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, ...]).
`

- The export attribute should list the above callbacks along with whichever test-specific functions the suite exposes. Keeping this list in one place prevents the suite linter from rejecting files due to missing exports.
- Avoid duplicating exports or defining callbacks conditionally; the suite linter reads these attributes statically.

## Test semantics expectations
- The lifecycle callbacks should behave deterministically. Do not introduce side effects, sleeps, or randomness without justification. (Those belong inside actual test functions.)
- When the suite implements helper modules (e.g., `router_jetstream_recovery_helpers`), keep their usage outside the lifecycle definitions so that callback signatures stay minimal.

## Automation guard
A lightweight guard script (`scripts/lint/check_ct_suite_structure.sh`) runs before Common Test executions to ensure changed suites explicitly define and export `init_per_suite/1`, `end_per_suite/1`, `init_per_testcase/2`, and `end_per_testcase/2`. The script inspects the affected `_SUITE.erl` files (tracked modifications or new suites) and fails fast if the callbacks are absent, preventing regressions before invoking the suite linter or rebar3 `ct`.

### Guard scope
By default the guard only evaluates the `*_SUITE.erl` files changed in your working tree (`git diff` / untracked files) so it stays fast yet protective; CI runs will always include the guard prior to invoking `rebar3 ct`. You can also supply explicit directory roots (e.g., fixtures under `scripts/lint/test_check_ct_suite_structure.sh`) to exercise the guard logic outside the main tree.

Document the guard and this file in the project README or developer onboarding guides so that future contributors can easily verify their changes.
