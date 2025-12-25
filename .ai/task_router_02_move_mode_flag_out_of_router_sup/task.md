# Remove IDE-mode conditional wiring from beamline_router_sup

## Goal (EN)
Router supervisor must not contain IDE-specific conditionals. Any optional API surface must be enabled via generic extension registration, not hardcoded in `beamline_router_sup`.

## Цель (RU)
Убрать из `beamline_router_sup` условный старт “IDE mode” (или эквивалент). Core sup запускает только core компоненты. Внешний API слой подключается через Extension Registry или отдельное приложение.

## Scope
- Remove conditional child start for `router_ide_sup` (or renamed control sup).
- Replace with one of:
  1. Start a generic extension host that discovers registered extensions and starts them.
  2. Or keep core sup unchanged, and start API layer as a separate OTP application (preferred).

## Acceptance Criteria
- `beamline_router_sup.erl` does not mention IDE/control API modules, flags, or modes.
- Router starts identical regardless of “IDE mode” env vars (because Router has no such env vars).
- API layer can be started independently for local dev.

## Verification
- Start Router without API layer: passes.
- Start Router + API layer: NATS subscriptions present, CT passes.
