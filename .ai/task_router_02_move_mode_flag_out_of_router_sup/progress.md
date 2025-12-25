# Progress

Status: DONE

Changes:
- Removed control-mode conditional wiring from `src/beamline_router_sup.erl` (no control API children in core supervisor).

Verification:
- `REBAR3_OFFLINE=1 rebar3 ct --suite test/router_control_boot_SUITE.erl` -> PASS (1 test), warning: stray mocks [router_nats] cleaned.
