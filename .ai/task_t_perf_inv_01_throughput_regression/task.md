# T-PERF-INV-01: Throughput Regression Investigation (Batch #3)

## Problem
Batch #3 heavy promotion with regression throughput asserts enabled failed:
`router_performance_regression_SUITE` concurrent throughput degraded beyond the gate.

Baseline: 59.10 req/s  
Current: 34.55 req/s  
Degradation: 41.53% (threshold: 35%)

## Goal
Determine whether the throughput regression is caused by environment/infra
noise or by a real performance regression, without changing baselines
or gates.

## Expected Outcome
Root-cause analysis with reproducible evidence and a clear decision:
infra noise vs code regression.
