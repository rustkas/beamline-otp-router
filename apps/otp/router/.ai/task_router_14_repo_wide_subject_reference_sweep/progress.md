# Progress (facts only)

## 2025-12-25
- CREATED task_router_14_repo_wide_subject_reference_sweep
- rg -n "beamline\\.router\\.ide\\.v1" -S . -> docs/CONTROL_API_NATS_SUBJECTS.md, docs/NATS_SUBJECTS.md, docs/README.md, docs/CONTROL_API.md, apps/router_control_api/test/router_control_contract_SUITE.erl
- rg -n "ROUTER_CONTROL_SUBJECT_ALIAS" -S . -> docs/CONTROL_API_NATS_SUBJECTS.md, docs/NATS_SUBJECTS.md, docs/README.md, docs/CONTROL_API.md, apps/router_control_api/test/router_control_contract_SUITE.erl, apps/router_control_api/src/router_control_config.erl
- Legacy subject references limited to docs compatibility notes and alias-mode test suite.
