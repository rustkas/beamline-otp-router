# Control API (Router control mode, v1)

This document defines the JSON contract for Router control mode commands and
responses over NATS. All fields use JSON string keys and UTF-8 values.

## Startup profiles

The control API runs as a separate OTP application (`router_control_api`).
Default shell starts core only; the control API is enabled via the
`dev_control` profile.

Core only:

```
rebar3 shell
```

Core + control API:

```
rebar3 as dev_control shell
```

## Common Request Fields

Required for all control commands:

```json
{
  "version": "1",
  "tenant_id": "local",
  "request_id": "req-123",
  "trace_id": "tr-123",
  "idempotency_key": "idem-123"
}
```

Optional:

```json
{
  "run_id": "run-123",
  "step_id": "step-123",
  "token": "local-dev-token"
}
```

Notes:
- `token` is required only if `ROUTER_CONTROL_TOKEN` is configured.
- Token may be provided in JSON payload (`token`) or via NATS headers:
  - `token: <token>`
  - `authorization: Bearer <token>`
- Header token takes precedence if both header and body are provided.
- `tenant_id` defaults to `ROUTER_CONTROL_TENANT_ID` when Router is configured for control mode.
- Subjects are `beamline.router.control.v1.*` by default; set `ROUTER_CONTROL_SUBJECT_ALIAS=true` to also
  listen on legacy `beamline.router.ide.v1.*` subjects (compatibility only).

## Common Response Fields

Success:
```json
{
  "ok": true,
  "result": { },
  "context": {
    "tenant_id": "local",
    "request_id": "req-123",
    "trace_id": "tr-123",
    "idempotency_key": "idem-123"
  }
}
```

Error:
```json
{
  "ok": false,
  "error": {
    "code": "invalid_request",
    "message": "Missing required fields: tenant_id, request_id"
  },
  "context": {
    "tenant_id": "local",
    "request_id": "req-123",
    "trace_id": "tr-123",
    "idempotency_key": "idem-123"
  }
}
```

## Commands

### project.open

**Subject**: `beamline.router.control.v1.project.open`

Required fields:
- `project_root` (string)

Optional fields:
- `project_id` (string)
- `project_config` (object)

### project.close

**Subject**: `beamline.router.control.v1.project.close`

Required fields:
- `project_id` (string)

### index.start

**Subject**: `beamline.router.control.v1.index.start`

Required fields:
- `project_id` (string)

Optional fields:
- `index_id` (string)
- `filters` (object)

### index.status

**Subject**: `beamline.router.control.v1.index.status`

Required fields:
- `index_id` (string)

### index.cancel

**Subject**: `beamline.router.control.v1.index.cancel`

Required fields:
- `index_id` (string)

### task.submit

**Subject**: `beamline.router.control.v1.task.submit`

Required fields:
- `job` (object)
- `job.type` (string)

Optional fields:
- `job.payload` (object)
- `job.payload_ref` (string)
- `job.metadata` (object)

### task.status

**Subject**: `beamline.router.control.v1.task.status`

Required fields:
- `task_id` (string)

### task.cancel

**Subject**: `beamline.router.control.v1.task.cancel`

Required fields:
- `task_id` (string)

### chat.start

**Subject**: `beamline.router.control.v1.chat.start`

Required fields:
- `project_id` (string)

Optional fields:
- `session_id` (string)
- `model` (string)

### chat.send

**Subject**: `beamline.router.control.v1.chat.send`

Required fields:
- `session_id` (string)
- `message` (string)

### chat.cancel

**Subject**: `beamline.router.control.v1.chat.cancel`

Required fields:
- `session_id` (string)

### events.subscribe

**Subject**: `beamline.router.control.v1.events.subscribe`

Optional fields:
- `event_types` (array of strings)

Response `result` payload:
```json
{
  "subscription_id": "sub-123",
  "inbox_subject": "beamline.router.control.v1.events.inbox.<subscription_id>"
}
```

## Error Codes

- `invalid_request` - Missing or invalid required fields.
- `invalid_subject` - Unknown or unsupported control subject.
- `unauthorized` - Missing or invalid `ide_token`.
- `not_implemented` - Command accepted but not implemented yet.

## Notes

- Legacy `.ide.` subjects are supported only when `ROUTER_CONTROL_SUBJECT_ALIAS=true`.
- Deprecation plan: remove `.ide.` alias support after the next CPx cut once clients migrate.
