# Progress (facts only)

## 2025-12-25
- Updated `apps/router_control_api/src/router_control_protocol.erl` to normalize legacy `.ide.` subjects only when `ROUTER_CONTROL_SUBJECT_ALIAS=true`.
- Added alias-only contract test group in `apps/router_control_api/test/router_control_contract_SUITE.erl` (`test_alias_events_subscribe_inbox`).
- Docs updated to reflect canonical `beamline.router.control.v1.*` and legacy alias policy:
  - `docs/CONTROL_API.md`
  - `docs/CONTROL_API_NATS_SUBJECTS.md`
- Verification (canonical): `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_contract_SUITE --retry` (PASS).
- Verification (alias enabled): `ROUTER_CONTROL_SUBJECT_ALIAS=true rebar3 as test ct --dir apps/router_control_api/test --suite router_control_contract_SUITE --retry` (PASS, 6 tests).
