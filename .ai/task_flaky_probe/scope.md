## Scope

- **CI entrypoint**: there is no YAML/CI config in this repo; `scripts/ci_pipeline.sh` is the de facto pipeline entrypoint executed by the external scheduler, so the flaky-probe step is wired into this script.
- **Shortlist suites**:
  - Default list (5–10, includes NATS publish suites) lives in `config/flaky_probe_suites.txt`.
  - Override via `FLAKY_PROBE_SUITES` (comma or space-separated suite paths); or edit the config file for a longer term change.
  - Default list: `test/router_nats_publish_fail_open_SUITE.erl`, `test/router_nats_publish_queueing_SUITE.erl`, `test/router_nats_publish_metrics_SUITE.erl`, `test/router_nats_publish_retry_SUITE.erl`, `test/router_nats_conn_failure_basic_SUITE.erl`, `test/router_nats_conn_failure_recovery_SUITE.erl`, `test/router_nats_connection_failure_SUITE.erl`.
- **Iteration control**: `FLAKY_PROBE_ITERS` defaults to `10`; CI jobs (or manual reruns) may set it to `20` for deeper sweeps.
- **Nightly gating**:
  - Environment variable `NIGHTLY=1` signals a nightly run that should fail on any flaky outcome.
  - When `CI_PIPELINE_SOURCE=="schedule"` (common in GitLab) or `NIGHTLY=1`, the job exits non-zero on failures; otherwise it stays “unstable” but returns success.
- **Report output**: job writes `reports/flaky_probe.json` (and this path should be uploaded as a CI artifact) even if suites fail, so the team can inspect the failure details.

CI is managed externally; `scripts/ci_pipeline.sh` is the canonical integration point.
