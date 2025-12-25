# Control API NATS Subjects (Router control mode, v1)

## Overview

This document defines the NATS subjects for Router control mode.
Control mode is a local, NATS-only API surface and does not change existing
Router ↔ CAF CP1/CP2 subjects or contracts.

All control commands use request-reply (NATS inbox) and return JSON responses.
Control events are published to an inbox subject returned by `events.subscribe`.

## Subject Naming Convention

Control subjects currently use the namespace:
- `beamline.router.control.v1.*` for commands
- `beamline.router.control.v1.events.*` for events

Legacy alias mode:
- Enable `ROUTER_CONTROL_SUBJECT_ALIAS=true` to keep responding on the old `beamline.router.ide.v1.*` subjects for backward compatibility.
- Deprecation plan: `.ide.` aliases are temporary and will be removed after the next CPx cut once clients migrate.

## Command Subjects (Request-Reply)

| Subject | Description |
| --- | --- |
| `beamline.router.control.v1.project.open` | Open a project session |
| `beamline.router.control.v1.project.close` | Close a project session |
| `beamline.router.control.v1.index.start` | Start indexing a project |
| `beamline.router.control.v1.index.status` | Get index status |
| `beamline.router.control.v1.index.cancel` | Cancel indexing |
| `beamline.router.control.v1.task.submit` | Submit a task |
| `beamline.router.control.v1.task.status` | Get task status |
| `beamline.router.control.v1.task.cancel` | Cancel a task |
| `beamline.router.control.v1.chat.start` | Start a chat session |
| `beamline.router.control.v1.chat.send` | Send a chat message |
| `beamline.router.control.v1.chat.cancel` | Cancel a chat session |
| `beamline.router.control.v1.events.subscribe` | Subscribe to event stream |

## Event Subjects (Published by Router)

Events are published to the inbox subject returned by `events.subscribe`.
The subject is not fixed and is recommended by Router at subscription time.

Example event subjects:
- `beamline.router.control.v1.events.inbox.<subscription_id>`

## Message Format

All command requests and responses are JSON and follow the control API schema.
See `docs/CONTROL_API.md` for required fields, validation rules, and examples.
