## Prompts

### Adjusting the flaky-probe shortlist
“Add or remove suites from `config/flaky_probe_suites.txt`, keeping the list between 5 and 10 entries, and ensure at least the NATS publish suites remain. If you need a temporary override, set `FLAKY_PROBE_SUITES` before calling `scripts/run_flaky_probe_job.sh` or in the CI job’s environment.”

### Increasing iteration depth
“Need deeper flake coverage? Set `FLAKY_PROBE_ITERS=20` (or a higher number) before invoking the flaky-probe job; the script will run that many iterations per suite and still emit `reports/flaky_probe.json`.”

### Debugging nightly failures
“Nightly pipelines export `NIGHTLY=1` (or detect `CI_PIPELINE_SOURCE=schedule`), so `scripts/run_flaky_probe_job.sh` exits non-zero on any failure. Re-run manually with `bash scripts/run_flaky_probe_job.sh` and inspect `reports/flaky_probe.json` to see which suite/iteration failed before adjusting.”
