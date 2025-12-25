# Soak Test Guide

This guide describes how to run and interpret long-running soak tests for the Beamline Router.

## Overview

Soak tests validate system stability under sustained load (6-24 hours) by monitoring:
- **Memory stability**: Detect memory leaks and unbounded growth
- **ETS growth**: Ensure in-memory tables stay bounded
- **JetStream consumer stability**: Verify consumer lag doesn't drift
- **Process stability**: No runaway process creation or crashes

## Running Soak Tests

### Quick Validation (10 minutes)

Validates basic stability before long-running tests:

```bash
# Start NATS if not running
./scripts/nats_start.sh

# Run short soak test
rebar3 ct --suite=router_soak_stability_SUITE --case=test_memory_stability_short

# Check results
cat _build/test/logs/*/run.*/suite.log.html
```

**Expected Result**: Memory growth < 5%, no crashes.

### 6-Hour Soak Test

```bash
# Uncomment test_memory_stability_6h in router_soak_stability_SUITE.erl (line ~41)
# Then run:
rebar3 ct --suite=router_soak_stability_SUITE --case=test_memory_stability_6h
```

**Expected Result**: Memory growth < 10%, ETS tables bounded.

### 24-Hour Soak Test

```bash
# Ensure you have:
# - Stable environment (no planned reboots)
# - Sufficient resources (4GB RAM, 20GB disk)
# - Monitoring configured

# Run 24-hour test
nohup rebar3 ct --suite=router_soak_stability_SUITE --case=test_memory_stability_24h > soak_24h.log 2>&1 &

# Monitor progress
tail -f soak_24h.log

# Check process is running
ps aux | grep beam
```

**Duration**: ~24 hours  
**Expected Result**: Memory growth < 10%, all stability criteria met.

## Monitoring During Soak

### Memory Usage

```bash
# Watch memory in real-time
watch -n 60 'ps aux | grep beam.smp'

# Erlang memory breakdown
erl -name monitor@localhost -setcookie beamline \
    -eval "erlang:memory() | io:format(\"~p~n\", [M])."
```

### ETS Tables

```bash
# Query ETS table sizes
erl -name monitor@localhost -setcookie beamline \
    -eval "ets:all() | lists:foreach(fun(T) -> io:format(\"~p: ~p~n\", [T, ets:info(T, size)]) end)"
```

### JetStream Lag

```bash
# Query consumer info (requires nats CLI)
nats consumer ls <STREAM_NAME>
nats consumer info <STREAM_NAME> <CONSUMER_NAME>
```

### Prometheus Metrics

```bash
# Query Router metrics
curl -s http://localhost:9001/metrics | grep router_

# Specific metrics:
# - process_memory_bytes
# - router_ets_table_size
# - router_jetstream_pending_messages
```

## Pass/Fail Criteria

### ✅ PASS

- Memory growth < 10% over 24 hours
- All ETS tables < 10,000 entries (or configured limit)
- JetStream lag < 100 messages (p99)
- Zero process crashes
- Zero supervisor restarts
- Metric cardinality < 10,000 series

### ❌ FAIL

- Memory growth > 10%
- Any ETS table shows unbounded growth (>1000% increase)
- JetStream lag > 1,000 messages
- Process crashes detected
- OOM (Out of Memory) event
- Metric cardinality explosion (>10,000 series)

## Interpreting Results

### Memory Trend Analysis

**Stable** (< 5% growth):
- System is healthy
- No memory leaks detected
- Safe for production

**Linear Growth** (5-15%):
- Investigate source of growth
- Check for caching without eviction
- May be acceptable if bounded

**Exponential Growth** (> 15%):
- ❌ CRITICAL: Memory leak detected
- Review recent code changes
- Use `recon` or `:observer` to profile
- DO NOT deploy to production

### Common Issues

#### High Memory Growth

**Symptoms**: Memory increases > 10% over 24h

**Possible Causes**:
- ETS table growth (unbounded cache)
- Binary accumulation (large messages not GC'd)
- Process dictionary growth
- Metric cardinality explosion

**Investigation**:
```erlang
% Check largest processes
recon:proc_count(memory, 10).

% Check binary memory
erlang:memory(binary).

% Check ETS memory
ets:all() |> Enum.map(fn t -> {t, :ets.info(t, :memory)} end) |> Enum.sort()
```

#### ETS Table Growth

**Symptoms**: ETS table size increases unbounded

**Possible Causes**:
- Cache without TTL or eviction
- Metrics with unbounded labels
- Session storage without cleanup

**Fix**:
- Implement TTL-based eviction
- Add max size limits
- Use LRU cache pattern

#### JetStream Lag

**Symptoms**: Consumer lag consistently > 100 messages

**Possible Causes**:
- Slow message processing
- Backpressure not activating
- Under-provisioned capacity

**Fix**:
- Scale horizontally (add Router instances)
- Optimize message processing
- Tune backpressure thresholds

## Troubleshooting

### Test Fails Immediately

**Symptoms**: Test exits within minutes

**Checks**:
1. Is NATS running? `./scripts/nats_status.sh`
2. Is Router started? `ps aux | grep beam`
3. Check logs: `logs/console.log`

### Out of Memory (OOM)

**Symptoms**: Process killed by OS

**Immediate Actions**:
1. Restart Router
2. Reduce load (RequestsPerSecond in test)
3. Increase system memory limits

**Root Cause Analysis**:
- Review memory snapshots before OOM
- Check for unbounded growth
- Profile with `:observer` or `recon`

### Abnormal Termination

**Symptoms**: Test exits with error

**Investigation**:
1. Check CT logs: `_build/test/logs/`
2. Check Router logs: `logs/`
3. Check for core dumps: `ls -la core.*`

## Generating Reports

### Manual Report

After soak test completion:

```bash
# Extract key metrics from CT log
grep "Memory growth" _build/test/logs/*/run.*/suite.log

# Generate markdown report
cat > _artifacts/soak_report_$(date +%Y%m%d).md <<EOF
# Soak Test Report

**Date**: $(date -Iseconds)
**Duration**: 24 hours
**Request Rate**: 50 req/s

## Results

- Memory Growth: X%
- ETS Growth: Bounded
- JetStream Lag: < 100 messages
- Process Stability: Stable

## Conclusion

PASS ✅
EOF
```

## Best Practices

1. **Run short test first**: Validate 10-minute test before long runs
2. **Monitor actively**: Don't let soak tests run unattended for first few runs
3. **Baseline first**: Establish baseline metrics before each soak
4. **Isolated environment**: Run on dedicated hardware (no other workloads)
5. **Document anomalies**: Record any unusual behavior for investigation

## Related Documentation

- Task Definition: `.ai/task_soak_testing/task.md`
- Acceptance Criteria: `.ai/task_soak_testing/acceptance.md`
- Test Source: `test/router_soak_stability_SUITE.erl`
- Helper Module: `test/router_soak_helper.erl`
