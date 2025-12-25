# Progress (facts only)

## 2025-12-25
- `apps/router_control_api/src/router_control_api_app.erl` starts `router_control_sup` via application callback.
- `apps/router_control_api/src/router_control_api.app.src` registers `{mod, {router_control_api_app, []}}`.
- Control layer can be started independently as its own OTP app (no core supervisor wiring).
