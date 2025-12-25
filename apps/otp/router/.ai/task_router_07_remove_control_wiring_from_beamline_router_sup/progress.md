# Progress (facts only)

## 2025-12-25
- Confirmed `apps/beamline_router/src/beamline_router_sup.erl` has no `router_control` or `ROUTER_CONTROL_` references after refactor.
- Verified canonical supervision tree still builds via `rebar3 as test ct --suite router_nats_mock_smoke_SUITE --retry` (all 6 tests passed, no control layer started).
- Verified Router core smoke `rebar3 as test ct --suite router_cp1_smoke_SUITE --retry` (11 tests passed, no control children).
