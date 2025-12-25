# Extended Stress and Soak Tests - Next Steps Complete

## Status

✅ **ALL NEXT STEPS COMPLETED**

## Date

**2025-11-30**

## Summary

All "Next Steps" from the initial implementation have been completed:

1. ✅ **Performance monitoring module created** (`router_stress_perf_monitor.erl`)
2. ✅ **CI/CD integration** (nightly and PR workflows)
3. ✅ **Comparison script** for previous runs
4. ✅ **Quick test script** for development

## Components Created

### 1. Performance Monitor Module

**File**: `apps/otp/router/src/router_stress_perf_monitor.erl`

**Features**:
- **Latency tracking**: P50, P95, P99, mean, min, max for operations
- **Throughput tracking**: Operations per second with time-series data
- **Queue size tracking**: Queue size statistics with percentiles
- **Performance degradation detection**: Automatic comparison with baseline
- **Configurable collection windows**: Default 1 minute, customizable

**API**:
```erlang
{ok, PerfMonitor} = router_stress_perf_monitor:start(),
router_stress_perf_monitor:record_latency(PerfMonitor, message_processing, 5.2),
router_stress_perf_monitor:record_throughput(PerfMonitor, publish, 100),
Stats = router_stress_perf_monitor:get_stats(PerfMonitor),
router_stress_perf_monitor:stop(PerfMonitor)
```

**Integration**: Integrated with `router_stress_monitor` via optional `enable_perf_monitor` option.

### 2. Comparison Script

**File**: `apps/otp/router/scripts/compare_stress_soak_runs.sh`

**Usage**:
```bash
./scripts/compare_stress_soak_runs.sh \
  stress_soak_results/test_baseline_normal_soak_20250126_120000 \
  stress_soak_results/test_baseline_normal_soak_20250127_120000
```

**Features**:
- Compares two test runs (baseline vs current)
- Generates comparison report (Markdown)
- Extracts resource summaries
- Detects regressions and improvements
- Creates detailed comparison files

**Output**:
- `comparison_report.md`: Main comparison report
- `summary_comparison.txt`: Summary comparison
- `resource_comparison.txt`: Resource usage comparison

### 3. Quick Test Script

**File**: `apps/otp/router/scripts/run_quick_stress_soak_test.sh`

**Usage**:
```bash
./scripts/run_quick_stress_soak_test.sh [test_case]
```

**Features**:
- Runs 1-hour tests (suitable for development)
- Fast validation before longer runs
- Default test case: `test_single_fault_connect_soak`
- Verbose output for debugging

**Example**:
```bash
# Quick test with default case
./scripts/run_quick_stress_soak_test.sh

# Quick test with specific case
./scripts/run_quick_stress_soak_test.sh test_baseline_normal_soak
```

### 4. CI/CD Integration

#### Nightly Workflow

**File**: `.github/workflows/router-stress-soak-nightly.yml`

**Schedule**: Runs every night at 2 AM UTC

**Features**:
- Configurable test case and duration (default: 4 hours)
- Manual trigger via `workflow_dispatch`
- 8-hour timeout
- Artifact upload (30-day retention)
- Failure notifications via GitHub Issues
- Summary generation in GitHub Actions

**Configuration**:
- Default test: `test_multi_fault_triple_soak`
- Default duration: 4 hours
- Can be overridden via workflow inputs

#### PR Workflow

**File**: `.github/workflows/router-stress-soak-pr.yml`

**Trigger**: Pull requests affecting router code

**Features**:
- Quick 1-hour tests on PRs
- 2-hour timeout
- Artifact upload (7-day retention)
- Fast feedback for developers

**Test**: Runs `test_single_fault_connect_soak` (quick validation)

### 5. Enhanced Resource Monitor

**File**: `apps/otp/router/src/router_stress_monitor.erl` (updated)

**Enhancements**:
- Optional performance monitor integration
- Perf monitor metrics included in snapshots
- Automatic perf monitor cleanup on termination

**Usage**:
```erlang
{ok, Monitor} = router_stress_monitor:start(#{
    collection_interval_ms => 300000,
    enable_perf_monitor => true  %% Enable perf monitoring
}),
```

## Integration Points

### Performance Monitor Integration

The performance monitor can be enabled in stress/soak tests:

```erlang
{ok, Monitor} = router_stress_monitor:start(#{
    collection_interval_ms => 300000,
    enable_perf_monitor => true
}),

%% In test code, record latencies:
router_stress_perf_monitor:record_latency(PerfMonitor, message_processing, LatencyMs),

%% Check for degradation:
Baseline = router_stress_perf_monitor:get_stats(PerfMonitor),
%% ... run test ...
Current = router_stress_perf_monitor:get_stats(PerfMonitor),
{ok, no_degradation} = router_stress_perf_monitor:check_performance_degradation(
    PerfMonitor, Baseline, #{
        p95_latency_multiplier => 2.0,
        throughput_drop_percent => 30.0
    }
)
```

### CI/CD Workflow Integration

**Nightly Runs**:
- Automatic execution every night
- Results stored as artifacts
- Failure notifications
- Summary in GitHub Actions

**PR Runs**:
- Automatic execution on router code changes
- Quick validation (1 hour)
- Fast feedback loop

## Usage Examples

### Development Workflow

1. **Quick local test**:
   ```bash
   ./scripts/run_quick_stress_soak_test.sh test_single_fault_connect_soak
   ```

2. **Medium validation**:
   ```bash
   ./scripts/run_stress_soak_test.sh test_multi_fault_triple_soak 4
   ```

3. **Compare with previous run**:
   ```bash
   ./scripts/compare_stress_soak_runs.sh \
     stress_soak_results/test_*_20250126_* \
     stress_soak_results/test_*_20250127_*
   ```

### CI/CD Workflow

**Automatic**:
- Nightly: 4-hour multi-fault test
- PR: 1-hour quick test

**Manual**:
- Trigger nightly workflow with custom parameters
- Override test case and duration

## Testing

### Manual Testing

**Quick Test**:
```bash
cd apps/otp/router
./scripts/run_quick_stress_soak_test.sh
```

**Expected**: Test completes in ~1 hour, generates report

**Comparison**:
```bash
# Run two tests
./scripts/run_stress_soak_test.sh test_baseline_normal_soak 2
# ... wait ...
./scripts/run_stress_soak_test.sh test_baseline_normal_soak 2

# Compare
./scripts/compare_stress_soak_runs.sh \
  stress_soak_results/test_baseline_normal_soak_*_1 \
  stress_soak_results/test_baseline_normal_soak_*_2
```

### CI/CD Testing

**Nightly Workflow**:
- Trigger manually via `workflow_dispatch`
- Verify test runs and artifacts are uploaded
- Check failure notifications

**PR Workflow**:
- Create test PR with router code changes
- Verify quick test runs automatically
- Check artifacts are uploaded

## Files Created/Updated

### New Files

1. `apps/otp/router/src/router_stress_perf_monitor.erl` - Performance monitor module
2. `apps/otp/router/scripts/compare_stress_soak_runs.sh` - Comparison script
3. `apps/otp/router/scripts/run_quick_stress_soak_test.sh` - Quick test script
4. `.github/workflows/router-stress-soak-nightly.yml` - Nightly CI workflow
5. `.github/workflows/router-stress-soak-pr.yml` - PR CI workflow
6. `apps/otp/router/docs/dev/STRESS_SOAK_NEXT_STEPS_COMPLETE.md` - This file

### Updated Files

1. `apps/otp/router/src/router_stress_monitor.erl` - Added perf monitor integration

## Next Steps (Future Enhancements)

### Recommended Future Enhancements

1. **JSON Report Generation**:
   - Machine-readable JSON reports
   - Structured data for automated analysis
   - Integration with monitoring systems

2. **Trend Visualization**:
   - Graph generation (memory, latency, throughput)
   - Historical trend analysis
   - Regression detection automation

3. **Advanced Performance Tracking**:
   - Per-operation detailed tracking
   - Correlation analysis (resource vs performance)
   - Anomaly detection

4. **Automated Baseline Management**:
   - Automatic baseline selection
   - Baseline versioning
   - Baseline comparison automation

5. **Enhanced CI/CD Integration**:
   - Slack/email notifications
   - Dashboard integration
   - Test result aggregation

## References

- `STRESS_SOAK_TESTS_SPEC.md`: Detailed specification
- `STRESS_SOAK_TESTS_GUIDE.md`: User guide
- `STRESS_SOAK_IMPLEMENTATION_SUMMARY.md`: Initial implementation summary
- `router_stress_monitor.erl`: Resource monitor implementation
- `router_stress_perf_monitor.erl`: Performance monitor implementation
- `router_stress_soak_SUITE.erl`: Test suite implementation

