# T-PERF-01: Performance baselines (p50 / p95 / p99)

## Problem
Batch #3 heavy is now stable and reproducible, but performance assertions
are currently validated only as boolean thresholds inside test suites.

There is no single source of truth for:
- expected p50 / p95 / p99 latency baselines,
- how those baselines are measured,
- where they are stored and versioned,
- how regressions are detected vs accepted variance.

This makes future performance regressions harder to reason about and
forces ad-hoc decisions on threshold changes.

## Goal
Formalize performance baselines (p50 / p95 / p99) as first-class artifacts,
measured in a reproducible way and used consistently by performance tests.

## Expected Outcome
A documented, versioned performance baseline system that allows:
- deterministic perf validation in Batch #3 heavy,
- clear distinction between regression, noise, and expected variance.
