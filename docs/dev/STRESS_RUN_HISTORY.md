# Stress Run History

This document tracks regular stress-run results for `router_concurrent_faults_SUITE` to identify trends and detect flaky tests over time.

## How to Update

After each weekly/nightly stress-run:
1. **Get results from CI artifacts**:
   - Download `stress-run-report-{OTP_VERSION}-{YYYYMMDD}` artifact
   - Or check local run: `stress_run_results_*/stress_run_report.md`

2. **Fill in the table**:
   - Add new row with date, OTP version, iterations, passed/failed counts, success rate
   - Add trigger type (weekly/nightly/manual)
   - Add brief notes

3. **Update trends section**:
   - Update success rate trend graph
   - Update current status
   - Note any significant changes

4. **Add detailed report**:
   - Create new section with date
   - Copy summary from generated report
   - Document any actions taken (investigations, fixes)

5. **Adjust threshold if needed**:
   - If success rate consistently ≥98% → consider raising threshold
   - If success rate <95% → investigate and fix before adjusting
   - Update threshold in workflow/scripts if changed

## Success Rate Threshold Configuration

**Current Threshold**: 95% (default)

**Recommended Threshold**: 98% (based on first run results)

**Rationale**: First run achieved 100% success rate, indicating tests are stable. Raising threshold to 98% will help catch any emerging flakiness earlier while still allowing for occasional transient failures.

**How to Adjust**:

### In CI Workflow

Edit `.github/workflows/router-stress-run.yml`:
```yaml
env:
  STRESS_RUN_SUCCESS_RATE_THRESHOLD: '98'  # Adjust based on first run results
```

### In Local Scripts

```bash
# Set environment variable
export STRESS_RUN_SUCCESS_RATE_THRESHOLD=98
./scripts/stress_run_concurrent_faults.sh 50
```

**Recommendation**: After first real run, analyze results and adjust threshold accordingly:
- **≥98% consistently** → Raise to 98%
- **95-97%** → Keep at 95%
- **<95%** → Fix flaky tests first, then adjust

## Report History

| Date | OTP Version | Iterations | Passed | Failed | Success Rate | Trigger | Notes |
|------|-------------|------------|--------|--------|--------------|---------|-------|
| 2025-11-29 | 27 | 5 | 5 | 0 | 100% | manual | First stress-run: all tests stable, no flakiness detected |

**Legend**:
- ✅ **100%**: All tests stable, no flakiness
- ⚠️ **95-99%**: Mostly stable, minor issues
- ❌ **<95%**: Potential flakiness, investigation needed

## Trends

### Success Rate Trend

```
100% ┤                                    ╭─
 95% ┤ [Threshold line] ───────────────────┴───
 90% ┤
     └──────────────────────────────────────────────→
     2025-11-29
```

**Current Status**: ✅ Stable (100% success rate)

**Success Rate Threshold**: 95% (default) → **Recommended: 98%** (see adjustment below)

**First Run Analysis**:
- **Result**: 100% success rate (5/5 iterations passed)
- **Timing**: Consistent ~15s average per run
- **Conclusion**: Tests are stable, no flakiness detected

**Threshold Adjustment Recommendation**:
- ✅ **First run shows 100% success rate** → Consider raising threshold to **98%**
- Rationale: With perfect first run, we can be more strict about flakiness detection
- Action: Update threshold to 98% after confirming stability in next few runs

### Flaky Test Analysis

**Status**: ✅ No flaky tests detected

**First Run Results**:
- All 5 iterations passed successfully
- No timing issues or race conditions observed
- Consistent execution time (~15s per run)

If flaky tests are detected in future runs:
- Document test name and failure pattern
- Link to investigation issue/PR
- Track fix status

## Detailed Reports

**Note**: Detailed reports will be added here after each weekly/nightly run.

### Template for New Report Entry

```markdown
### YYYY-MM-DD

**Report**: [CI Artifact: stress-run-report-{OTP_VERSION}-{YYYYMMDD}](./stress_run_report_{YYYYMMDD}.md)

**Summary**:
- [X/Y] iterations passed
- Average time: [X]s per run
- [Brief description of any issues]

**Status**: [✅ Stable / ⚠️ Minor issues / ❌ Flakiness detected]

**Actions Taken**: [Investigation/Fix/None]
```

### 2025-11-29 - First Stress Run

**Report**: [Local Run: stress_run_results_20251129_184723/stress_run_report.md](../../stress_run_results_20251129_184723/stress_run_report.md)

**Summary**:
- **5/5 iterations passed** (100% success rate)
- **Average time**: 15s per run
- **Total time**: 75s
- **No failures detected**: All tests stable
- **Timing consistency**: Run times varied between 10-20s (acceptable variance)

**Status**: ✅ Stable - All iterations passed, no flakiness detected

**Actions Taken**: 
- None required - tests are stable
- **Recommendation**: Monitor next few runs, then consider raising threshold to 98% if stability continues

**Test Coverage**:
- All concurrent fault scenarios (A-E) passed
- All edge cases (F-L) passed
- All backoff timing tests passed

**Observations**:
- Test execution is fast and consistent
- No resource leaks or timing issues observed
- All fault injection scenarios work as expected

## Action Items

- [x] Run first stress-run (2025-11-29) - ✅ Completed
- [x] Fill in STRESS_RUN_HISTORY.md with first run results - ✅ Completed
- [x] Analyze first run results and adjust threshold - ✅ Completed (recommended: 98%)
- [ ] Update CI workflow threshold to 98% after confirming stability
- [ ] Monitor success rate weekly
- [ ] Investigate any failures immediately
- [ ] Update this document after each run
- [ ] Track fixes and improvements

## Related Documentation

- `apps/otp/router/docs/dev/STRESS_RUN_GUIDE.md` - Usage guide
- `apps/otp/router/docs/dev/STRESS_RUN_SUMMARY.md` - Implementation summary
- `.github/workflows/router-stress-run.yml` - CI workflow

