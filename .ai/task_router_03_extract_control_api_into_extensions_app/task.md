# Extract Router Control API (NATS) into a separate OTP application

## Goal (EN)
Router core should not ship “IDE API”. Provide a separate OTP app (e.g., `router_control_api`) that depends on Router and registers NATS handlers.

## Цель (RU)
Вынести NATS Control API (то, что сейчас делалось для IDE) в отдельное OTP приложение/подпроект. Router остаётся ядром и ничего не знает про IDE.

## Scope
- Create new app directory (example): `apps/otp/router_control_api/`.
- Move protocol/config/nats handler/sup into that app.
- Keep contracts/suites inside that app.
- The app uses Router’s existing NATS client module (`router_nats`), core message fields conventions, extensions registry if needed.

## Acceptance Criteria
- Router app can be built/tested without `router_control_api`.
- `router_control_api` can be enabled in local/dev profile and runs against Router.
- No “IDE” naming in Router core; docs for control api live in `router_control_api/docs`.

## Verification
```
rebar3 as test ct --suite router_*   # router core
rebar3 as test ct --suite router_control_*  # new app
```
