## Task: Add automatic flaky-probe CI job for shortlist suites

- Introduce a reusable flaky-probe job that exercises 5–10 critical suites (at least the NATS publish family) multiple times via `scripts/ct-probe-flaky.sh`.
- Each suite should run 10 iterations by default, adjustable via `FLAKY_PROBE_ITERS`, and make it easy to swap the shortlist via `config/flaky_probe_suites.txt` or the `FLAKY_PROBE_SUITES` env var.
- The job must emit `reports/flaky_probe.json`, mark non-nightly runs as “unstable” on failures (exit 0) and fail nightly runs when any iteration breaks (`NIGHTLY=1` or a scheduled pipeline).
