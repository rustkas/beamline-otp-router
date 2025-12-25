# Progress

Status: DONE

Batch #1 clean verification completed via official entrypoint:
`scripts/ct-batch.sh --batch=1 --level=all`.

All suites ran fast/full/heavy from scratch with:
- `RUN_JETSTREAM_SOAK=false`
- `RUN_CHAOS_TESTS=false`
- `STRESS_SOAK_DURATION_HOURS=0.01`

Chaos engineering suite is skipped by design without `RUN_CHAOS_TESTS=true`.
