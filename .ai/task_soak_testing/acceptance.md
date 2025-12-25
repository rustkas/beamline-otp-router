# Acceptance Criteria: T-SOAK-01

## Done When

### 1. Soak Test Suite Complete
- [ ] `test/router_soak_stability_SUITE.erl` created
- [ ] Test case: `test_memory_stability_6h` (6-hour soak)
- [ ] Test case: `test_memory_stability_24h` (24-hour soak)
- [ ] Test case: `test_ets_growth_monitoring` (ETS bounds check)
- [ ] Test case: `test_jetstream_consumer_stability` (lag monitoring)
- [ ] Suite compiles and runs successfully

### 2. Soak Orchestration Script Complete
- [ ] `scripts/soak_test.sh` created
- [ ] Supports duration parameter (6h, 24h, custom)
- [ ] Starts NATS if needed
- [ ] Starts Router application
- [ ] Generates sustained load via `router_load_generator`
- [ ] Collects metrics every 5 minutes
- [ ] Generates final report

### 3. Monitoring Infrastructure Ready
- [ ] `prometheus/soak_dashboard.json` created (Grafana dashboard spec)
- [ ] Panels for: memory, ETS size, JetStream lag, process count
- [ ] Real-time monitoring during soak test
- [ ] Automatic screenshot capture every hour

### 4. Memory Stability Validated
- [ ] 6-hour soak run completed (short validation)
- [ ] 24-hour soak run completed (long validation)
- [ ] Memory growth < 10% over 24 hours
- [ ] No OOM (Out of Memory) crashes
- [ ] Evidence: `_artifacts/soak_24h_report.md`

### 5. ETS Growth Validated
- [ ] All ETS tables measured at start and end
- [ ] No table exceeds 10,000 entries (or documented limit)
- [ ] No unbounded growth detected
- [ ] Evidence: ETS size graph in report

### 6. JetStream Consumer Validated
- [ ] Consumer lag stays < 100 messages (99th percentile)
- [ ] No consumer crashes or restarts
- [ ] ACK rate matches delivery rate (no drift)
- [ ] Evidence: JetStream lag graph in report

### 7. Process Stability Validated
- [ ] Process count remains stable (± 10%)
- [ ] No supervisor restart loops
- [ ] No runaway process creation
- [ ] Evidence: Process count graph in report

### 8. Metric Cardinality Validated
- [ ] Total unique series < 10,000
- [ ] No cardinality explosion
- [ ] Label combinations documented
- [ ] Evidence: Prometheus cardinality query results

### 9. Documentation Complete
- [ ] `docs/SOAK_TEST_GUIDE.md` - How to run soak tests
- [ ] Soak test pass/fail criteria documented
- [ ] Known issues and workarounds documented
- [ ] Baseline memory/ETS sizes documented

## Validation Checklist

### Before Execution
- [ ] NATS is running and healthy
- [ ] Prometheus is configured to scrape Router
- [ ] Sufficient disk space (> 10GB free)
- [ ] No other heavy processes running

### During Execution
- [ ] Monitor memory usage every hour
- [ ] Check for ERROR logs periodically
- [ ] Verify request rate is sustained
- [ ] Check Prometheus is collecting metrics

### After Execution
- [ ] Review final report for pass/fail
- [ ] Analyze memory trend (linear, logarithmic, stable?)
- [ ] Check ETS table final sizes
- [ ] Verify no crashes in logs

## Pass/Fail Criteria

### PASS ✅
- Memory growth < 10% over 24 hours
- All ETS tables < configured limits
- JetStream lag < 100 messages (p99)
- Zero process crashes
- Zero supervisor restarts
- Metric cardinality < 10,000 series

### FAIL ❌
- Memory growth > 10% over 24 hours
- Any ETS table unbounded growth
- JetStream lag > 1000 messages
- Any process crash
- Any supervisor restart loop
- Metric cardinality > 10,000 series
- OOM kill event
