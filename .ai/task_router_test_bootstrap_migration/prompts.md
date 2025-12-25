## Prompts (for short chats)

### Continue migration (next group)
“Migrate the next batch of suites that still call `router_suite_helpers:start_router_suite()` to `router_test_bootstrap`. Keep suite-specific mocks/metrics/fault logic intact. After changes, run `./bin/rebar3 ct --suite ...` for each modified suite.”

### Find remaining suites
“List remaining CT suites in `test/` still using `router_suite_helpers:start_router_suite/0` (or manual `application:ensure_all_started`) and propose the next grouping for migration.”

### Validate full tier
“Run `erl -noshell -pa test_support -s router_suite_linter run -s init stop` and then `./scripts/ct-full.sh`. If failures occur, fix them with minimal behavior change.”

### Infra-mode correctness
“Audit `router_test_bootstrap` infra-mode detection and skip behavior; ensure docker-required suites skip when not in docker mode, without slowing ct-full.”

