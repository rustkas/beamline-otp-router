# Operating Instructions — T-INFRA-01

- Always run scripts from repo root.
- Every run must emit artifacts to `_artifacts/`.
- Never pipe `rebar3 ct` output to `tail` during execution; always `tee` to file.
- Scripts must be idempotent and must not leave zombie processes.
- Use `ss -tlnp` or `netstat -tlnp` checks in scripts (fallback if one is missing).
- Do not introduce TLS/auth here; baseline only.
- After each completed step, update `.ai/task_staging_nats_setup/progress.md` immediately with:
  - command executed
  - result (PASS/FAIL)
  - artifact links
