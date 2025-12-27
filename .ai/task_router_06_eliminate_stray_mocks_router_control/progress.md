# Progress (facts only)

## 2025-12-25
- Added global `apps/beamline_router/include` to `rebar.config` so control app sources can include `beamline_router.hrl`.
- Updated `apps/beamline_router/test/router_mock_helpers.erl` so `cleanup_and_verify/0` unloads mocks before checking for leftovers, preventing the stray mock warning.
- Sequence 1 (control suites):
  - `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_boot_SUITE --retry`
  - `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_contract_SUITE --retry`
  - `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_config_SUITE --retry`
  - All suites passed with no stray-mock warning.
- Sequence 2 (same three commands, all PASS; no warnings).
- Sequence 3 (same three commands, all PASS; no warnings).
