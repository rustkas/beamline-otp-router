# Progress (facts only)

## 2025-12-25
- Repo docs sweep using `rg -n "\\bIDE\\b|IDE_|beamline\\.router\\.ide\\.v1" docs` shows remaining `.ide.` references only in alias/deprecation sections:
  - `docs/CONTROL_API.md` (legacy alias note)
  - `docs/CONTROL_API_NATS_SUBJECTS.md` (alias note)
  - `docs/NATS_SUBJECTS.md` (alias note)
  - `docs/README.md` (compat note)
  - `docs/archive/dev_reports/JETSTREAM_OBS_DEVELOPMENT_PROCESS_INTEGRATION_COMPLETE.md` (archived report with legacy IDE workflow mention)
- No other IDE-branded mentions found in non-archive docs beyond the explicit alias/compatibility notes.
