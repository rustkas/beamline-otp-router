## Prompts

### Fix the lint if it reports a violation
“The new `router_suite_linter` rule reported either a `meck_passthrough` or `gen_server_router_nats_call` violation; fix the offending suite (or wrap the call in `router_nats_test_helper`/`router_mock_helpers`) and rerun `erl -noshell -pa test_support -s router_suite_linter run -s init stop`.”

### Add another helper to the approved list
“Identify the next helper module that legitimately wraps router_nats interactions and add it to the whitelist documented under `.ai/task_enforce_mock_discipline/scope.md`, keeping the linter rule from rejecting it.”

### Verify lint failure on violation
“Temporarily drop in `test/router_mock_discipline_violation_SUITE.erl` (or your own mock) and run `erl -noshell -pa test_support -s router_suite_linter run -s init stop` to confirm the new rules indeed fail the build. Remove the temporary file afterwards.”
