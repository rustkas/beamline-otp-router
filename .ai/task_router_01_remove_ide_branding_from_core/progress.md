# Progress

Status: DONE

Changes:
- Renamed modules/files: `src/router_ide_config.erl` -> `src/router_control_config.erl`,
  `src/router_ide_protocol.erl` -> `src/router_control_protocol.erl`,
  `src/router_ide_nats.erl` -> `src/router_control_nats.erl`,
  `src/router_ide_sup.erl` -> `src/router_control_sup.erl`,
  `src/router_ide.erl` -> `src/router_control.erl`.
- Renamed test suites: `test/router_ide_config_SUITE.erl` -> `test/router_control_config_SUITE.erl`,
  `test/router_ide_contract_SUITE.erl` -> `test/router_control_contract_SUITE.erl`,
  `test/router_ide_boot_SUITE.erl` -> `test/router_control_boot_SUITE.erl`.
- Updated config env/app keys: `IDE_MODE|IDE_TENANT_ID|IDE_TOKEN` -> `ROUTER_CONTROL_MODE|ROUTER_CONTROL_TENANT_ID|ROUTER_CONTROL_TOKEN`,
  app env keys `ide_*` -> `control_*`.
- Updated `src/beamline_router_sup.erl` wiring to `router_control_*` (control supervisor conditional kept for Task 2).

Verification (no IDE strings in core src):
- `rg -n "router_ide\\b|router_ide_" src` -> no matches.
- `rg -n "IDE_MODE|IDE_TOKEN|IDE_TENANT_ID|\\bide_mode\\b|\\bide_token\\b" src` -> no matches.

CT runs:
- `REBAR3_OFFLINE=1 rebar3 ct --suite test/router_control_config_SUITE.erl` -> PASS (2 tests).
- `REBAR3_OFFLINE=1 rebar3 ct --suite test/router_control_contract_SUITE.erl` -> PASS (5 tests).
- `REBAR3_OFFLINE=1 rebar3 ct --suite test/router_control_boot_SUITE.erl` -> PASS (1 test), warning: stray mocks [router_nats] cleaned.
