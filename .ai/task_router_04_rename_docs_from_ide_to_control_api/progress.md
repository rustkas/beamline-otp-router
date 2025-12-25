# Progress

Status: DONE

Changes:
- Renamed `docs/IDE_API.md` -> `docs/CONTROL_API.md`.
- Renamed `docs/IDE_NATS_SUBJECTS.md` -> `docs/CONTROL_API_NATS_SUBJECTS.md`.
- Updated `docs/NATS_SUBJECTS.md` to point to Control API docs and renamed IDE section to Control API.
- Updated config references in `docs/CONTROL_API.md` to `ROUTER_CONTROL_TOKEN` and `ROUTER_CONTROL_TENANT_ID`.
- Removed uppercase `IDE` mentions from core docs:
  - `docs/README.md` ("ELP/editor setup")
  - `docs/dev/DEVELOPER_GUIDE.md` ("Editor Configuration")
  - `docs/schemas/plugin.schema.json` ("editor support")

Verification:
- `rg -n "\\bIDE\\b|IDE_" docs` only matches `docs/archive/...` (historical).
