## Architecture (test bootstrap)

**Goal**
- Сделать CT hooks “тонкими”, а поведение — декларативным.

**Core flow**
- `router_test_bootstrap:init_per_suite/2`
  1) detect `infra_mode` (cached via `persistent_term`)
  2) optional skip if suite requires docker but runtime is not docker
  3) optional reset/load app
  4) apply env (`common_env` defaults + `app_env` override)
  5) start dependencies (`ensure_apps`)
  6) start strategy:
     - `router_suite` (default) => `router_suite_helpers:start_router_suite/0`
     - `ensure_all_started` => `application:ensure_all_started/1`
     - `router_app` => `router_test_utils:start_router_app/0`
     - `none`
  7) optional wait for internal apps to start (`test_helpers:wait_for_app_start/2`)
- `router_test_bootstrap:end_per_suite/2`
  - stop strategy: `router_suite` / `stop_app` / `none` (with `auto` resolving based on how it was started)
  - optional mock cleanup verification
- `router_test_bootstrap:init_per_testcase/3` / `end_per_testcase/3`
  - optional metrics setup/teardown and clearing
  - optional fault injection clearing
  - optional “ensure router nats alive”
  - optional mock cleanup verification

