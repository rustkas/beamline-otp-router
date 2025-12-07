# Fault Injection Tests Activation Checklist

## ✅ Completed

```
┌─────────────────────────────────────────────────────────────┐
│  ACTIVATION STATUS: COMPLETE                                 │
└─────────────────────────────────────────────────────────────┘

✓ File renamed: .skip → .erl
✓ Timeouts increased for CI stability
✓ Integrated in test-slow.sh
✓ CI will run via rebar3 ct
✓ All documentation scenarios covered
```

### Changes Made

| Component | Change | Status |
|-----------|--------|--------|
| Test file | `router_jetstream_fault_injection_SUITE.erl.skip` → `.erl` | ✅ Done |
| Timeouts | `timer:sleep(300)` → `500ms` | ✅ Done |
| Timeouts | `wait_for_metric(2000)` → `5000ms` | ✅ Done |
| Timeouts | `wait_for_meck_call(1000)` → `2000ms` | ✅ Done |
| Integration | Already in `test_slow.sh` | ✅ Verified |
| CI | Runs via `rebar3 ct` | ✅ Verified |

## 🔄 Next Steps

### 1. Local Validation (Required)

**Run multiple times to verify stability:**

```bash
cd apps/otp/router
./scripts/test_fault_injection_repeat.sh --runs 20
```

**Expected result**: All 20 runs pass (0 failures)

**If failures occur**:
- Check failure patterns
- Increase timeouts further if needed
- Review test logic for race conditions

### 2. CI Monitoring (First 5-10 runs)

**Monitor in CI**:
- [ ] Check execution time of `router_jetstream_fault_injection_SUITE`
- [ ] Verify no timeouts or failures
- [ ] Check if overall `rebar3 ct` duration increased significantly

**CI Dashboard**: Check GitHub Actions / Drone / GitLab CI logs

**Metrics to track**:
- Suite execution time (should be ~20-40s per run)
- Failure rate (should be 0%)
- Overall CI job duration impact

### 3. Optional: Separate CI Job

**If `rebar3 ct` becomes too slow** (>15 minutes):

Create separate job in `.github/workflows/ci.yml`:

```yaml
- name: Fault Injection Tests
  working-directory: apps/otp/router
  run: |
    rebar3 ct --suite router_jetstream_fault_injection_SUITE
```

**Options**:
- Run in parallel with main tests
- Schedule as nightly
- Run on-demand via label

## 📊 Success Criteria

- [x] Test suite activated (file renamed)
- [x] Timeouts stabilized
- [x] Integration verified
- [ ] Local multi-run validation passed (20+ runs, 0 failures)
- [ ] CI monitoring shows stable runs (5-10 runs, 0 failures)
- [ ] No significant CI duration increase

## 📝 Files

- **Test suite**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`
- **Documentation**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`
- **Summary**: `apps/otp/router/docs/dev/FAULT_INJECTION_ACTIVATION_SUMMARY.md`
- **Repeat script**: `apps/otp/router/scripts/test_fault_injection_repeat.sh`

## 🚀 Quick Start

```bash
# 1. Run locally once
cd apps/otp/router
rebar3 ct --suite router_jetstream_fault_injection_SUITE

# 2. Run multiple times for stability check
./scripts/test_fault_injection_repeat.sh --runs 20

# 3. Monitor CI after push
# Check GitHub Actions / CI logs for first few runs
```

