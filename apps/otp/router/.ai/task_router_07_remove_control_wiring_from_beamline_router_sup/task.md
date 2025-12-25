# Title
Remove control wiring from beamline_router_sup

# Context
- Router core must remain agnostic about optional control APIs because wiring them directly into the supervisor breaks separation of concerns.
- Any conditional startup logic in `beamline_router_sup` increases the chance that control code will “grow back” into the core.
- Maintaining a clean core supervision tree is critical for predictable, multi-profile releases.

# Goal (RU)
Удалить из `apps/otp/router/src/beamline_router_sup.erl` любые упоминания про `router_control_*` и `ROUTER_CONTROL_*`, чтобы Router всегда стартовал как раньше, а control слой оставался внешним.

# Goal (EN)
Remove every `router_control_*` and `ROUTER_CONTROL_*` reference from `beamline_router_sup.erl` so Router boots unchanged, and the control layer stays out of the core.

# Scope
- Remove conditional child startup of the control supervisor from `beamline_router_sup.erl`.
- Avoid introducing any new IDE/control-specific switches inside the core supervision tree.

# Non-goals
- Enrich the core with new control logic or stateful data for IDE features.
- Rewire the control API in this task; that will be handled by the separate control app once this wiring is removed.

# Acceptance Criteria
1) `apps/otp/router/src/beamline_router_sup.erl` contains no references to `router_control` or `ROUTER_CONTROL_`.
2) Router core starts the same way regardless of `ROUTER_CONTROL_*` environment variables.
3) Control functionality can still be started separately (covered in downstream tasks).

# Verification
- `rg -n \"router_control|ROUTER_CONTROL_\" apps/otp/router/src/beamline_router_sup.erl` – empty result.
- Run the canonical core smoke suites (e.g., `router_nats_mock_smoke_SUITE`, `router_cp1_smoke_SUITE`) and confirm they pass without starting the control layer.
- Ensure control API can still be started independently (documented later).

# Implementation Plan
1) Remove the conditional `router_control_sup` child from `beamline_router_sup.erl`.
2) Check for any remaining `ROUTER_CONTROL_*` usage, remove or document it.
3) Run the core smoke suites and capture logs.

# Risks / Notes
- Control behavior will be enabled via a separate app/bootstrap after this task; removing the wiring may transiently affect local dev scripts, which will be updated in follow-up tasks.
