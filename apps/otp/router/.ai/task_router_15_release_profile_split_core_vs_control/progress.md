# Progress (facts only)

## 2025-12-25
- CREATED task_router_15_release_profile_split_core_vs_control
- rebar.config: default shell apps set to [beamline_router] (core-only); dev_control profile shell apps set to [beamline_router, router_control_api]
- docs/CONTROL_API.md: added Startup profiles section with core-only vs dev_control commands
- docs/README.md: added pointer to CONTROL_API startup section
- rebar3 shell --eval 'init:stop().' (core-only profile) -> PASS; compiled beamline_router/router_control_api with existing unused-function warnings; booted beamline_router then exited cleanly
