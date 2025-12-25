Scope:
- Provide a test-only admin API key override hook in `router_admin_grpc:get_admin_key/0`.
- Enable `router_admin_grpc_rbac_SUITE` to opt in to the override (environment flag and deterministic key) without external secrets.
- Ensure the suite writes the override flag to the environment while the suite runs and clears it afterwards.
