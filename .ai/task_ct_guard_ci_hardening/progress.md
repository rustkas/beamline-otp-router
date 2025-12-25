# Progress

## Step 1 – Document guard scope
- Command: `none` (doc update); updated `docs/dev/CT_SUITE_CONVENTIONS.md` to describe guard scope (`apps/otp/router/test`) and reuse argument support for fixture testing.

## Step 2 – Guard script & tests
- Commands:
  - `scripts/lint/check_ct_suite_structure.sh` (clean tree) → exit 0; output `ct suite structure guard: no suites to check`.
  - `scripts/lint/test_check_ct_suite_structure.sh` → exit 0; exercise positive case plus missing callback/export scenarios.

## Step 3 – CI / runner coverage
- Guard wired into `rebar.config` via provider hooks so `rebar3 ct` runs `scripts/lint/check_ct_suite_structure.sh` before starting (in addition to the previous `ct-full` guard invocation).
- Command: `./scripts/ct-full.sh` (clean tree) → exit 1; guard ran but the run aborted later because `router_admin_grpc_rbac_SUITE` fails without valid API keys (missing or invalid API key errors in multiple test cases), so quality gates reported test failures.
