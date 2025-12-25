# Scope — T-INFRA-01

## In scope
- Baseline single-node NATS Server with JetStream:
  - `--jetstream`
  - `--store_dir <path>`
  - `-p 4222`
  - `-m 8222`
- Start/stop/status scripts with pidfile + logs
- Health checks:
  - `GET http://localhost:8222/healthz`
  - `GET http://localhost:8222/varz`
- Log artifacts to `_artifacts/`
- Document “how to run heavy CT against real NATS” in a canonical doc section (or README update)

## Out of scope
- Clustered NATS (multi-node)
- TLS enablement (belongs to T-SEC-01)
- Auth/JWT/nkeys (optional, not part of baseline)
- Stream/consumer pre-provisioning (optional; can be separate follow-up)

## Constraints / conventions
- Scripts must be runnable from repo root.
- Scripts must be idempotent: repeated start should not spawn duplicates.
- All evidence must be captured into `_artifacts/`.
- progress.md is the only source of truth for completion.
