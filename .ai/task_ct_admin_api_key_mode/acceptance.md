Acceptance:
- `router_admin_grpc:get_admin_key/0` returns a deterministic test API key whenever the override environment flag is set.
- `router_admin_grpc_rbac_SUITE` runs without requiring external credentials because it enables the override and keeps metadata aligned with the constant key.
- Targeted `rebar3 ct --suite test/router_admin_grpc_rbac_SUITE.erl` passes after the changes.
