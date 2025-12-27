# Progress (facts only)

## 2025-12-25
- Confirmed control modules live under `apps/router_control_api/src`:
  - `router_control.erl`
  - `router_control_config.erl`
  - `router_control_nats.erl`
  - `router_control_protocol.erl`
  - `router_control_sup.erl`
  - `router_control_api_app.erl`
  - `router_control_api.app.src`
- Verified no `router_control` modules remain in `apps/beamline_router/src` or `apps/beamline_router/test` (rg returned no matches).
- Control CT verification (from Task 06): `rebar3 as test ct --dir apps/router_control_api/test --suite router_control_boot_SUITE --retry` (PASS).
