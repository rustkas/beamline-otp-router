## Chaos-enabled clean run

Use this script when you want to run Batch #1 with chaos tests enabled:

`scripts/run_ct_batch1_clean_chaos.sh`

It runs the same fast/full/heavy flow as the standard clean script, but sets:
- `RUN_CHAOS_TESTS=true`
- `RUN_JETSTREAM_SOAK=false`
- `STRESS_SOAK_DURATION_HOURS=0.01`
