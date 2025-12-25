## Plan

1. Define the shortlist of flaky-probe suites in `config/flaky_probe_suites.txt` and allow overrides via `FLAKY_PROBE_SUITES`.
2. Create `scripts/run_flaky_probe_job.sh` to iterate each suite with `scripts/ct-probe-flaky.sh`, collect statuses, emit `reports/flaky_probe.json`, and treat `NIGHTLY`/`CI_PIPELINE_SOURCE=schedule` as fatal.
3. Hook the job into `scripts/ci_pipeline.sh` as the final step so the existing CI entrypoint can expose the new check and artifacts.
4. Document the new job, config knobs, nightly gating, and report location inside `.ai/task_flaky_probe/*` for future editors.
5. (Later) Validate by running `scripts/run_flaky_probe_job.sh` locally (limited short list via `FLAKY_PROBE_SUITES=...`) and check that `reports/flaky_probe.json` is created, then record the command/output.
