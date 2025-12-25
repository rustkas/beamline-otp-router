# Acceptance — T-INFRA-01

## Done criteria

### A) Scripts present and executable
- `scripts/nats_start.sh`
- `scripts/nats_stop.sh`
- `scripts/nats_status.sh`

### B) Start is deterministic
Running:
- `./scripts/nats_start.sh`

Must result in:
- NATS listening on :4222
- Monitor on :8222
- `curl -fsS http://localhost:8222/healthz` returns `{"status":"ok"}`

### C) Stop is deterministic
Running:
- `./scripts/nats_stop.sh`

Must result in:
- ports 4222/8222 are free (ss/netstat check)
- pidfile removed

### D) Logs and artifacts
- Start script writes log to `_artifacts/nats_<timestamp>.log`
- Status script prints pid + ports + healthz + varz header
- progress.md links at least one successful NATS log artifact

### E) Heavy tests command is documented
- Canonical command exists (in doc or progress.md), e.g.:
  - `ROUTER_TEST_LEVEL=heavy NATS_URL=nats://localhost:4222 rebar3 as test ct ...`
- Must produce heavy-tier log artifact in `_artifacts/`
