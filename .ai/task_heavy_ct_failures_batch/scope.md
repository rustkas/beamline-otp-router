# Scope

## In Scope
- `test/router_deployment_SUITE.erl`
- `test/router_headers_propagation_e2e_SUITE.erl`
- `test/router_jetstream_recovery_ext_SUITE.erl`
- `test/router_jetstream_recovery_helpers.erl` (if missing, create)
- `test/router_health_integration_SUITE.erl` (compilation issues)
- `router_mock_helpers` hygiene (cleanup leaks)
- Test helper modules directly used by failing suites

## Out of Scope
- `scripts/ct-full.sh` (FORBIDDEN to edit)
- Unrelated test suites
- Production code changes (unless strictly required for correctness)
- Semantic test behavior changes (only fix infrastructure/setup issues)

## Constraints
- Max 3 fix attempts per failing test/suite
- Never leave repo in non-compiling state
- Apply 3-strikes rollback + quarantine protocol
