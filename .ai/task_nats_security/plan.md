# T-SEC-01 — Plan

## Step 1 — Certs
- Add `scripts/generate_certs.sh`
- Output to `_artifacts/certs/`
- Ensure idempotency (no overwrite if already present)

## Step 2 — NATS TLS config
- Add `config/nats_tls.conf`
- TLS enabled, verify client cert
- JetStream enabled with stable store_dir
- Expose HTTP monitor at `:8222`

## Step 3 — Router TLS sys.config
- Add `config/test_real_nats_tls.config`
- Set `nats_url = tls://localhost:4222`
- Provide TLS file paths via `application:get_env(beamline_router, ...)`

## Step 4 — Validation orchestration
- Add `scripts/validate_nats_tls.sh`
- Bounded waits, clear failure modes, always stops NATS

## Step 5 — Run + capture evidence
- Run: `./scripts/validate_nats_tls.sh`
- Capture:
  - `_artifacts/nats_tls_<ts>.log`
  - `_artifacts/ct_tls_<ts>.log`
- Update `progress_md` with PASS + evidence paths
