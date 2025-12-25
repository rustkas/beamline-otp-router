# Progress

# Step 0 – Setup
- Created metadata files for the admin API key override task.

# Step 1 – Override logic
- Added a helper that detects `ROUTER_ADMIN_API_TEST_OVERRIDE` and returns `~"test-admin-key"` (or an explicit valid override) before falling back to the production config path in `router_admin_grpc:get_admin_key/0`.

# Step 2 – Suite wiring
- `router_admin_grpc_rbac_SUITE:init_per_suite/1` now enables the override flag, uses the fixed admin key, and `end_per_suite/1` disables the flag so other suites stay unaffected.

# Step 3 – Verification
- Command: `ROUTER_TEST_LEVEL=full rebar3 ct --suite test/router_admin_grpc_rbac_SUITE.erl` → exit 0; suite passes with all 9 tests, so API key validation no longer fails without shared credentials.
