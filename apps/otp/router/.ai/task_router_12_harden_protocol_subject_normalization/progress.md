# Progress (facts only)

## 2025-12-25
- Added strict subject allowlist via `router_control_protocol:is_known_subject/1` and return `invalid_subject` for unknown subjects.
- Updated `router_control_protocol:validate_subject_fields/2` to reject unknown subjects with `invalid_subject`.
- Added `test_unknown_subject_rejected` in `apps/router_control_api/test/router_control_contract_SUITE.erl`.
- Verification:
  - `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_contract_SUITE --retry` (PASS, 9 tests).
