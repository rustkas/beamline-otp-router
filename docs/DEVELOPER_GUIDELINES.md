# Developer Guidelines: Router

## Agent Operations Rules

1.  **Working Directory**: Always run commands from the repository root (`/home/rustkas/aigroup/apps/otp/router`). Absolute paths should be used for file operations, but terminal commands expect the root CWD.
2.  **Heavy Test Execution**: 
    *   Always use `tee` to capture output to a file: `rebar3 as test ct ... 2>&1 | tee /tmp/ct_heavy_run.log`.
    *   Monitor the log file in a separate background command or use `command_status` frequently.
3.  **Concurrency Management**:
    *   For the Heavy CT tier or suites with shared resources (ETS, global Mocks), ALWAYS set `{num_concurrent_suites, 1}` in `rebar.config` or run them one by one.
    *   Avoid `[parallel]` testcase execution for suites using shared named ETS tables.
4.  **Contract Updates**: Any change to response semantics (e.g., adding/modifying fields in `DecideResponse` or Gateway responses) MUST be documented in `docs/API_CONTRACTS.md` before the task is considered done.
5.  **Evidence Collection**: Before declaring a stabilization task done, provide:
    *   The path to the full CT run log.
    *   A snippet of the fixed test demonstrating the resolution of the race/logic issue.
