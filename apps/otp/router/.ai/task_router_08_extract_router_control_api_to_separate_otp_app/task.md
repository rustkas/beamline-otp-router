# Title
Extract router_control_api into a separate OTP application

# Context
- Router core must stay pure and not depend on IDE/control components at compile or runtime.
- Housing control logic inside the main app makes the core harder to reason about and harder to test independently.
- A separate OTP application lets the control API evolve without touching the beams-level router.

# Goal (RU)
Перенести контрольный API в отдельный OTP-приложение (`apps/otp/router_control_api`), чтобы Router core не зависел от него.

# Goal (EN)
Extract the control layer into a standalone OTP application (e.g. `apps/otp/router_control_api`) so the Router core has no dependency on it.

# Scope
- Create the new OTP app (`router_control_api`) with its own `app.src`, source, and test tree.
- Move the control modules (`router_control_*`) and their suites into this app.
- Adjust build/rebar/release configs accordingly.

# Non-goals
- Changing public NATS contracts beyond namespace neutralization (handled elsewhere).
- Adding functionality while refactoring (focus remains separation).

# Acceptance Criteria
1) Router core app builds/tests without referencing the control app.
2) Control app builds/tests independently and can talk to Router when started.
3) `rebar3 ct` runs separately for both the core and control app.

# Verification
- `rebar3 ct` for the Router core only (core profile) passes.
- `rebar3 ct` for `apps/router_control_api` passes.
- Document the dependency graph showing Router core has no control dependencies.

# Implementation Plan
1) Create `apps/otp/router_control_api` directory with `src`, `test`, and `app.src`.
2) Move the `router_control_*` modules and suites into this app.
3) Update rebar & release configs to include the new app.
4) Run CT for both apps and capture logs.

# Risks / Notes
- Carefully maintain include paths/imports while moving modules out of the Router app.
