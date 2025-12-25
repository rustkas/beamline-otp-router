## Plan

1. Extend `test_support/router_suite_linter.erl` to count:
   - `meck:new(..., [passthrough])` occurrences.
   - `gen_server:call(router_nats, ...)` usage.
   - Only compare the latter against non-approved modules (canonical `router_nats_test_helper`, secondary `router_mock_helpers`).
2. Update `test_support/router_suite_linter_baseline.eterm` so the new rules start from zero tolerance.
3. Run the linter to confirm the clean tree still passes.
4. Introduce a temporary violation (a throwaway `_SUITE.erl` that uses passthrough and directly calls `gen_server:call(router_nats,…)`), run the linter to verify it now fails, then remove the file.
5. Record progress (rule implementation status, verification run + artificial violation, helper whitelist) and update the task context docs.
