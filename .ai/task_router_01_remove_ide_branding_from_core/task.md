# Remove IDE-specific naming and entrypoints from Router core

## Goal (EN)
Router core must not contain IDE-specific modules/names. Move or rename all `router_ide_*` modules into a generic “orchestrator API” layer (non-IDE), or an extensions package.

## Цель (RU)
Убрать из ядра Router любые сущности/модули/точки входа, содержащие “IDE” (по имени и по смыслу). Всё, что нужно IDE, должно жить как внешний слой (extensions) и использовать Router как generic orchestrator.

## Scope
- Find all modules/files with `router_ide*` and any config flags `IDE_MODE`, `IDE_TOKEN`, `IDE_TENANT_ID`.
- Rename/move to a neutral namespace (e.g., `router_control_*` or `router_orch_*`).
- Ensure no string “IDE” exists in `apps/otp/router/src` after completion (except docs/examples outside core).

## Acceptance Criteria
- No `router_ide*.erl` remains under Router core module namespace.
- No `IDE_*` config keys are read by Router core.
- Unit/CT tests updated and pass.
- Public NATS API stays stable (subjects unchanged or version-bumped with alias compatibility).

## Implementation Notes
- Prefer creating a small “router control API” module set:
  - `router_control_config.erl`
  - `router_control_protocol.erl`
  - `router_control_nats.erl`
  - `router_control_sup.erl`
  - `router_control.erl`
- If keeping subjects, rename docs to “Control API” rather than IDE API.

## Verification
```
rebar3 ct --suite router_control_* --retry
rebar3 ct --suite router_* --retry
rg -n "IDE_|router_ide|IDE_MODE|IDE_TOKEN" apps/otp/router/src && exit 1 || true
```
