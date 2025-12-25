## In scope
- Compare environment characteristics between baseline and promotion runs.
- CPU pinning and scheduler configuration impact.
- System load/noisy neighbor indicators during runs.
- Concurrency model (workers vs requests) in regression suite.
- Isolated reruns of `router_performance_regression_SUITE` with evidence capture.

## Out of scope
- Changing baselines or gates.
- Relaxing assertions or adding skips.
- Code optimizations unrelated to investigation.

## Constraints
- Baseline artifacts remain unchanged.
- Gates remain unchanged.
