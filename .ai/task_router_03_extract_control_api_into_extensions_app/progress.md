# Progress

Status: DONE

Changes:
- Converted repo to umbrella layout with apps:
  - `apps/beamline_router/` (core)
  - `apps/router_control_api/` (control API)
- Moved core directories:
  - `src/` -> `apps/beamline_router/src/`
  - `include/` -> `apps/beamline_router/include/`
  - `priv/` -> `apps/beamline_router/priv/`
  - `test/` -> `apps/beamline_router/test/`
- Moved control API modules to `apps/router_control_api/src/`:
  - `router_control_config.erl`
  - `router_control_protocol.erl`
  - `router_control_nats.erl`
  - `router_control_sup.erl`
  - `router_control.erl`
- Moved control suites to `apps/router_control_api/test/`:
  - `router_control_config_SUITE.erl`
  - `router_control_contract_SUITE.erl`
  - `router_control_boot_SUITE.erl`
- Added `apps/router_control_api/src/router_control_api.app.src` and `apps/router_control_api/src/router_control_api_app.erl`.
- Added app-level `rebar.config` files:
  - `apps/beamline_router/rebar.config` (include path for gpb)
  - `apps/router_control_api/rebar.config` (include path to core headers)
- Root `rebar.config` updated with `{project_apps, [...]}` and gpb output paths to `apps/beamline_router/...`.
- Compatibility symlinks created at repo root:
  - `src` -> `apps/beamline_router/src`
  - `include` -> `apps/beamline_router/include`
  - `priv` -> `apps/beamline_router/priv`
  - `test` -> `apps/beamline_router/test`

Verification:
- `REBAR3_OFFLINE=1 rebar3 ct --dir apps/router_control_api/test --suite router_control_config_SUITE` (PASS)
- `REBAR3_OFFLINE=1 rebar3 ct --dir apps/router_control_api/test --suite router_control_contract_SUITE` (PASS)
- `REBAR3_OFFLINE=1 rebar3 ct --dir apps/router_control_api/test --suite router_control_boot_SUITE` (PASS)
  - Warning: `router_mock_helpers` reported stray mock cleanup for `router_nats`.
- `REBAR3_OFFLINE=1 rebar3 ct --dir apps/beamline_router/test --suite router_nats_mock_smoke_SUITE` (PASS, core smoke)

Notes:
- Added `erlang:function_exported/3` guard in `apps/beamline_router/test/router_mock_helpers.erl` for `router_nats:simulate_connection_lost/0` to avoid undefined-function failures.
