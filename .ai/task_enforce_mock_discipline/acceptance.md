## Acceptance Criteria

1. The suite linter includes counts for `meck:new(..., [passthrough])` and `gen_server:call(router_nats, ...)`.
2. All `test/*_SUITE.erl` files now sample zero occurrences of the new rules (baseline enforces zero tolerance).
3. Direct `gen_server:call(router_nats, ...)` is only permitted inside the approved helpers (`router_nats_test_helper` canonical, `router_mock_helpers` secondary); any other file triggers a lint error.
4. A deliberately introduced violation causes the linter to fail (proof via a temporary `_SUITE` file during verification).
5. Documented progress includes the helper whitelist, lint rule status, violation list, and verification runs.
