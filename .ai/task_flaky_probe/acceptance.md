## Acceptance Criteria

1. `scripts/run_flaky_probe_job.sh` iterates the shortlist (per `config/flaky_probe_suites.txt` or `FLAKY_PROBE_SUITES`) and invokes `scripts/ct-probe-flaky.sh` for each suite using `FLAKY_PROBE_ITERS` iterations.
2. The job records all outcomes (suite, iterations, status, failure iteration) into `reports/flaky_probe.json`, regardless of pass/fail, so CI artifacts can always inspect the history.
3. `scripts/ci_pipeline.sh` calls the flaky-probe job as step 6; it still reports “unstable but passing” for non-nightly failures while nightly runs (`NIGHTLY=1` or `CI_PIPELINE_SOURCE=schedule`) propagate the failure.
4. The shortlist includes the requested NATS publish suites, is easy to edit, and the job honours overrides.
