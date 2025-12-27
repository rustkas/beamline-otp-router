# Progress (facts only)

## 2025-12-25
- Added contract tests in `apps/router_control_api/test/router_control_contract_SUITE.erl`:
  - `test_token_header_ide_token_accepted`
  - `test_token_header_precedence`
- Updated `docs/CONTROL_API.md` to note header token precedence over body.
- Verification:
  - `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_contract_SUITE --retry` (PASS, 9 tests).
