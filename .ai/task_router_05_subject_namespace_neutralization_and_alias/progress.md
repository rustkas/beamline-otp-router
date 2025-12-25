# Progress

Status: IN PROGRESS

Changes:
- Added canonical control subjects `beamline.router.control.v1.*` and explicit `ROUTER_CONTROL_SUBJECT_ALIAS` toggle in `router_control_config.erl`.
- Updated `router_control_nats.erl` to subscribe to the new namespace while optionally keeping the old `.ide.` subjects when the alias flag is `true`.
- Reworked `router_control_protocol.erl` to validate requests against the new subjects, normalize legacy prefixes at runtime, and stream events on the new inbox namespace.
- Updated the control protocol contract suite to exercise the new subjects and canonical inbox prefix.
- Renamed the control API docs to explain the new namespace and alias behavior (`docs/CONTROL_API*.md`, `docs/NATS_SUBJECTS.md`).

Verification:
- `REBAR3_OFFLINE=1 rebar3 ct --dir apps/router_control_api/test --suite router_control_contract_SUITE` (PASS, ensures subjects & docs aligned)

Next:
- Document alias configuration in README/CONFIG and finish any remaining doc updates (Task 5 acceptance).
