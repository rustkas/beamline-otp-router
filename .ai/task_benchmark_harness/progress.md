# Progress — T-PERF-01

Status: PASS

## Work log
- [x] Task requirements established
- [x] Create scripts/bench_router.sh
- [x] Verify benchmark against real NATS
- [x] Capture baseline results in _artifacts/
- [x] Final report generation

## Evidence
- Performance Summary: `_artifacts/perf_summary_20251221_120602.md`
- Performance JSON: `_artifacts/perf_baseline_20251221_120602.json`
- Log File: `_artifacts/bench_router_20251221_120602.log`

### Baseline Metrics (Real NATS)
- Sequential: ~24 req/s, P95 99ms
- Concurrent: ~62 req/s, P95 796ms
- Memory: ~94 MB
- ETS Store Size: ~759 entries
