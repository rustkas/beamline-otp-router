# Task: T-CHAOS-01 — Controlled Chaos Testing

**Status**: Planning  
**Priority**: High  
**Category**: Testing, Reliability, Production Readiness

## Problem Statement

The Router lacks systematic chaos engineering validation to prove resilience under fault conditions. While unit tests exist for individual failures, we need integrated chaos tests that verify:
- System recovery from catastrophic failures (NATS/Router process death)
- Graceful degradation under partial failures
- SLO compliance during recovery (time-to-green)
- Data consistency after network partitions and redelivery

Without chaos testing, production incidents may reveal unknown failure modes.

## Goal

Create a comprehensive chaos testing suite that validates Router resilience by:
1. Killing NATS process mid-flight and verifying reconnection
2. Killing Router process and verifying clean restart
3. Inducing JetStream lag/redelivery and validating idempotency
4. Measuring recovery SLO (target: < 30s to operational state)

## Expected Outcomes

- ✅ Automated chaos test suite: `test/router_chaos_controlled_SUITE.erl`
- ✅ Chaos orchestration script: `scripts/chaos_test.sh`
- ✅ Recovery SLO verification with metrics
- ✅ Documentation of failure modes and recovery patterns
- ✅ Evidence that Router meets production resilience requirements

## Success Criteria

1. All chaos scenarios execute without manual intervention
2. Router recovers to operational state within SLO
3. No data loss or corruption after recovery
4. Metrics correctly reflect failure and recovery events
5. Circuit breakers and backpressure mechanisms activate appropriately
