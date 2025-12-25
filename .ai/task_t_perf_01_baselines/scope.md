## In scope
- Definition of baseline metrics (p50 / p95 / p99).
- Measurement methodology for Batch #3 performance suites.
- Storage format and location for baselines.
- Integration of baselines into existing perf test assertions.
- Documentation of variance and tolerance rules.

## Out of scope
- Performance optimizations of router code.
- Changes to load generation logic unrelated to measurement.
- CI scheduling changes.

## Constraints
- Baselines must be reproducible on the same class of environment.
- Baselines must not depend on transient CI load.
- No silent auto-adjustment of baselines.
