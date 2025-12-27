# Title
Define control API bootstrap via extension/app startup

# Context
- The control layer must be kernel-agnostic and start itself without the core supervisor’s involvement.
- Using extension or application hooks ensures control code can be toggled per profile without polluting beamline_router supervisors.
- Keeping startup logic within the control app prevents accidental wiring back into the Router core.

# Goal (RU)
Создать bootstrap (через приложение/extension registry), который запускает control API (`router_control_sup`), чтобы его можно было включать независимо от core.

# Goal (EN)
Define a bootstrap (via application or extension registry) that starts the control API (`router_control_sup`), ensuring it can be enabled independently of the Router core.

# Scope
- Implement `router_control_api_app.erl` (or similar) as the control application module.
- Allow enabling/disabling via app env or release profile.
- Avoid any changes to `beamline_router_sup`.

# Non-goals
- Adding IDE-specific logic to the core release.

# Acceptance Criteria
1) There is a dedicated application module that starts `router_control_sup`.
2) An app env/release profile toggles the control layer.
3) Starting the dev profile with control enabled shows the children in the supervision tree.

# Verification
- Start Router with control disabled (core only) and record children list.
- Start Router with control app enabled and show the control children.
- Document the env/profile switch.

# Implementation Plan
1) Add an application callback for the control app that starts `router_control_sup`.
2) Tie the control app’s start to a profile/env flag (e.g., `router_control_api, enabled`).
3) Update release configs or shell scripts to include the control app when desired.
4) Run `rebar3` to verify startup and log the supervision tree difference.

# Risks / Notes
- After removal of wiring from `beamline_router_sup`, scripts that assumed implicit control start may need updates.
