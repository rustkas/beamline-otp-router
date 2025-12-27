# Title
Finalize subject alias policy for control API

# Context
- Canonical subjects must be always active, while legacy `.ide.` endpoints should only exist as explicit aliases controlled by a flag.
- Without a clear alias policy, clients risk talking to the wrong subjects or relying on undocumented behavior.
- Documenting and testing the alias ensures dependable migration from `.ide.` to `.control.`.

# Goal (RU)
Финализировать политику alias: `beamline.router.control.v1.*` — canonical и всегда включён, `beamline.router.ide.v1.*` — допустим только по флагу, с документированной стратегией deprecation.

# Goal (EN)
Finalize the alias policy: `beamline.router.control.v1.*` is canonical and always enabled, while `beamline.router.ide.v1.*` is allowed only behind a flag and carries an explicit deprecation plan.

# Scope
- Canonical subject subscriptions remain default.
- Alias subscriptions are gated by `ROUTER_CONTROL_SUBJECT_ALIAS=true`.
- Contract tests ensure canonical subjects always work; alias tests run only when the flag is enabled.

# Non-goals
- Removing `.ide.` subjects immediately (that’s a follow-up release decision).

# Acceptance Criteria
1) Canonical subjects are always enabled.
2) Alias subjects are only active when `ROUTER_CONTROL_SUBJECT_ALIAS=true`.
3) Tests cover canonical behavior and alias behavior under the flag.
4) Documentation explains canonical subjects, the alias flag, and the deprecation plan.

# Verification
- `rebar3 ct --suite router_control_contract_SUITE` and verify canonical tests pass.
- Run the alias tests under `ROUTER_CONTROL_SUBJECT_ALIAS=true` and confirm they pass.
- Document flag and deprecation plan in `docs/CONTROL_API.md`.

# Implementation Plan
1) Update NATS subscription logic to respect canonical-first and alias flag.
2) Split contract suites into canonical and alias groups.
3) Add doc entries describing the alias flag and timeline.
4) Run tests in both canonical and alias modes.

# Risks / Notes
- Alias mode should remain opt-in until we decide to retire `.ide.` subjects.
