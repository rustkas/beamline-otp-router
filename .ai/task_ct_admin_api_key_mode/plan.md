1. Introduce helper(s) in `router_admin_grpc` that detect a test-mode override flag and return a fixed test API key while leaving prod behavior untouched.
2. Wire the override check into `get_admin_key/0` so the suite can bypass the real secret when the flag is present.
3. Update `router_admin_grpc_rbac_SUITE:init_per_suite/1` to set the override flag, use the fixed key in metadata, and clean up the flag in `end_per_suite/1`.
4. Run the suite locally (`rebar3 ct --suite test/router_admin_grpc_rbac_SUITE.erl`) and confirm it no longer complains about missing API keys.
