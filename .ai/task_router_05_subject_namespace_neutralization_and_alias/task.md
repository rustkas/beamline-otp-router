# Neutralize NATS subject namespace (remove `.ide.`) with backward-compatible aliases

## Goal (EN)
Remove IDE-specific subject names. Provide neutral subjects and optional compatibility mapping.

## Цель (RU)
Перевести subjects с `beamline.router.ide.v1.*` на нейтральные `beamline.router.control.v1.*` (или `beamline.router.api.v1.*`). При необходимости — алиасы на старые subjects (soft-deprecate).

## Scope
- Implement subject mapping layer in control api:
  - Subscribe to new subjects primarily.
  - Optionally also subscribe to old `.ide.` subjects and reply with same handler.
- Update docs + contract suite to assert new subjects.
- Add `DEPRECATION.md` note with timeline/plan.

## Acceptance Criteria
- New neutral subjects documented and tested.
- Old subjects either removed (major bump) or supported via alias mode (default off/on).
- No “ide” appears in subjects in Router core.

## Verification
- Contract test suite validates both subject sets when alias enabled.
- Smoke script sends request to new subjects and gets expected replies.
