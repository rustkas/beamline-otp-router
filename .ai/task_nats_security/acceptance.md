# T-SEC-01 — NATS TLS Validation (CP1)

## Acceptance Criteria

1. **Cert generation is repeatable**
   - `scripts/generate_certs.sh` is idempotent (second run does not rotate/overwrite valid certs).
   - Artifacts go to `_artifacts/certs/`.

2. **NATS starts with TLS + JetStream**
   - `config/nats_tls.conf` enables TLS on `:4222` and HTTP monitor on `:8222`.
   - JetStream enabled with deterministic store dir.
   - Client certificate verification is enabled (`verify: true`).

3. **Router test config is explicit**
   - `config/test_real_nats_tls.config` forces `tls://localhost:4222`
   - TLS file paths are defined (CA + client cert + client key)
   - Timeouts are bounded (CI-safe).

4. **End-to-end validation script**
   - `scripts/validate_nats_tls.sh`:
     - generates certs
     - starts NATS with TLS
     - waits for `/healthz`
     - runs CT suites with TLS sys.config
     - always stops NATS (trap)
     - always writes `_artifacts/nats_tls_*.log` and `_artifacts/ct_tls_*.log`

5. **Evidence**
   - A successful run produces:
     - `_artifacts/nats_tls_*.log`
     - `_artifacts/ct_tls_*.log`
   - `progress.md` references exact artifact filenames from a real run.
