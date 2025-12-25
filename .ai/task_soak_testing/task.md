# Task: T-SOAK-01 — Long-running Soak Testing

**Status**: Planning  
**Priority**: High  
**Category**: Production Readiness, Performance, Reliability

## Problem Statement

The Router lacks long-running stability validation. While unit and integration tests verify correctness, they don't detect:
- **Memory leaks**: Gradual memory growth over hours/days
- **ETS table bloat**: Unbounded growth of in-memory tables
- **Resource exhaustion**: File descriptors, processes, ports
- **JetStream consumer drift**: Consumer lag accumulation over time
- **Metric cardinality explosion**: Unbounded label combinations

Production systems often fail after hours or days of operation due to issues invisible in short tests.

## Goal

Execute long-running soak tests (6-24 hours) to validate Router stability under sustained load:
1. **Memory Stability**: No memory leaks or unbounded growth
2. **ETS Growth**: ETS tables remain bounded
3. **JetStream Consumer Stability**: No consumer lag drift
4. **Process Stability**: No process crashes or restarts
5. **Metric Stability**: No cardinality explosion

## Expected Outcomes

- ✅ Soak test suite: `test/router_soak_stability_SUITE.erl`
- ✅ Soak orchestration script: `scripts/soak_test.sh`
- ✅ Monitoring dashboard spec: `prometheus/soak_dashboard.json`
- ✅ Soak test report template: Evidence of 24-hour run
- ✅ Stability criteria document: Pass/fail thresholds

## Success Criteria

1. Router runs for 24 hours without crashes
2. Memory usage grows < 10% over 24 hours
3. ETS table count remains bounded (no runaway growth)
4. JetStream consumer lag stays < 100 messages
5. Process count remains stable
6. All soak tests pass with green status

## Business Impact

- **Production Confidence**: Validates long-term stability before deployment
- **Cost Optimization**: Prevents memory leaks that require frequent restarts
- **Incident Prevention**: Catches issues before they impact users
