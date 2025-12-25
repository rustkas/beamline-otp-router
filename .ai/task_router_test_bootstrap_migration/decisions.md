## Decisions

- Keep bootstrap conservative: nothing “global” unless explicitly enabled by options.
- Default `common_env` sets `grpc_port => 0`, `grpc_enabled => false` to avoid binding ports in most test contexts; suites can override via `common_env => false` + `app_env`.
- Avoid `ct:pal` or heavy logging inside bootstrap to not affect `ct-full` timing.
- Keep suite-specific logic inside the suite modules (before/after bootstrap calls), not in `router_test_bootstrap`.
- Use `persistent_term` caching for infra-mode detection to avoid repeated expensive detection per suite.

