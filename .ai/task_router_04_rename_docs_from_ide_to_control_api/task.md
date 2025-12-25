# Rename IDE docs to Router Control API docs (no IDE wording in Router docs)

## Goal (EN)
Router documentation must not describe IDE features. Rename docs to generic “Control API over NATS”.

## Цель (RU)
Переименовать/перенести документацию: не “IDE_*”, а “CONTROL_API_*”. В доках Router не должно быть IDE-специфичных формулировок.

## Scope
- Rename:
  - `IDE_API.md` → `CONTROL_API.md`
  - `IDE_NATS_SUBJECTS.md` → `CONTROL_API_NATS_SUBJECTS.md`
- Update any pointers in `NATS_SUBJECTS.md` to refer to Control API docs.
- Ensure docs live outside Router core docs if you split into separate app.

## Acceptance Criteria
- No “IDE” word in Router docs tree (except optional historical changelog).
- Subjects remain `beamline.router.control.v1.*` OR (if you keep old) state deprecation/aliasing policy.

## Verification
```
rg -n "\bIDE\b|IDE_" docs apps/otp/router && exit 1 || true
```
