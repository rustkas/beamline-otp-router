# Fault Injection Tests - Monitoring Automation

**Date**: 2025-11-30  
**Status**: ✅ **Automation Complete**

## Overview

Automated monitoring infrastructure for fault injection tests has been created:
- ✅ Weekly stability checks (automated)
- ✅ Monthly trend analysis (automated)
- ✅ GitHub Actions workflows
- ✅ Reporting scripts

## Automated Workflows

### Weekly Stability Check

**Workflow**: `.github/workflows/fault-injection-monitoring.yml`

**Schedule**: Every Monday at 9 AM UTC

**Actions**:
1. Runs monitoring script
2. Generates weekly report
3. Checks thresholds
4. Uploads artifacts

**Artifacts**:
- `weekly_stability_report_<run_number>`
- Contains stability summary and weekly report JSON

### Monthly Review

**Workflow**: `.github/workflows/fault-injection-monitoring.yml`

**Schedule**: First Monday of each month

**Actions**:
1. Collects all weekly reports from month
2. Generates trend analysis
3. Calculates averages
4. Provides recommendations

**Artifacts**:
- `monthly_stability_report_<YYYY-MM>`
- Contains trend analysis and monthly summary

## Manual Scripts

### Weekly Check

**Script**: `scripts/weekly_stability_check.sh`

**Usage**:
```bash
cd apps/otp/router
./scripts/weekly_stability_check.sh
```

**Output**:
- `reports/fault-injection-weekly/weekly_report_<date>.json`
- Console output with threshold checks

### Monthly Review

**Script**: `scripts/monthly_stability_review.sh`

**Usage**:
```bash
cd apps/otp/router
./scripts/monthly_stability_review.sh
```

**Output**:
- `reports/fault-injection-monthly/monthly_report_<YYYY-MM>.json`
- `reports/fault-injection-monthly/trend_analysis_<YYYY-MM>.txt`
- Console output with recommendations

## Monitoring Workflow

### Weekly Process

1. **Automated** (GitHub Actions):
   - Runs every Monday
   - Analyzes CI logs from previous week
   - Generates stability report
   - Checks thresholds
   - Alerts if issues detected

2. **Manual** (if needed):
   - Run `scripts/weekly_stability_check.sh`
   - Review report
   - Take action if thresholds exceeded

### Monthly Process

1. **Automated** (GitHub Actions):
   - Runs first Monday of month
   - Collects all weekly reports
   - Generates trend analysis
   - Provides recommendations

2. **Manual** (if needed):
   - Run `scripts/monthly_stability_review.sh`
   - Review trend analysis
   - Update documentation
   - Plan improvements

## Thresholds and Alerts

### Weekly Thresholds

**Pass Rate**:
- ✅ Good: ≥ 99%
- ⚠️ Warning: 95-99%
- 🔴 Critical: < 95%

**Status**:
- ✅ Stable: All tests passing
- 🔴 Unstable: Failures detected

**Duration**:
- ✅ Good: < 5 minutes
- ⚠️ Warning: 5-15 minutes
- 🔴 Critical: > 15 minutes

### Monthly Thresholds

**Average Pass Rate**:
- ✅ Good: ≥ 99%
- ⚠️ Warning: 95-99%
- 🔴 Critical: < 95%

**Stable Weeks**:
- ✅ Good: All weeks stable
- ⚠️ Warning: 1-2 unstable weeks
- 🔴 Critical: > 2 unstable weeks

## Action Items

### High Priority (Fix within 1 week)

- Pass rate < 95%
- > 2 unstable weeks in month
- Tests blocking CI pipeline
- Duration > 15 minutes consistently

### Medium Priority (Fix within 1 month)

- Pass rate 95-99%
- 1-2 unstable weeks in month
- Tests causing noise but not blocking
- Duration 5-15 minutes

### Low Priority (Fix when convenient)

- Pass rate ≥ 99%
- All weeks stable
- Tests not blocking
- Duration < 5 minutes

## Integration

### GitHub Actions

Workflows are automatically triggered:
- Weekly: Every Monday at 9 AM UTC
- Monthly: First Monday of month

### Manual Trigger

Workflows can be manually triggered:
1. Go to Actions tab
2. Select "Fault Injection Tests Monitoring"
3. Click "Run workflow"

### Artifacts

All reports are stored as GitHub Actions artifacts:
- Retention: 90 days (weekly), 365 days (monthly)
- Download: Available in Actions tab
- Analysis: Can be downloaded for offline analysis

## Troubleshooting

### Workflow Not Running

**Check**:
1. Workflow file exists: `.github/workflows/fault-injection-monitoring.yml`
2. Schedule is correct
3. Repository has Actions enabled

**Fix**:
- Verify workflow syntax
- Check GitHub Actions settings
- Manually trigger workflow to test

### Scripts Failing

**Check**:
1. Scripts are executable: `chmod +x scripts/*.sh`
2. Dependencies installed: `jq`, `bc`
3. Report directories exist

**Fix**:
- Make scripts executable
- Install dependencies
- Create report directories manually

### No Data Available

**Check**:
1. CI logs exist in `_build/test/logs`
2. Monitoring script can find logs
3. Reports directory is writable

**Fix**:
- Run tests first to generate logs
- Check log directory permissions
- Verify report directory exists

## References

- Monitoring script: `scripts/monitor_fault_injection_ci.sh`
- Weekly check: `scripts/weekly_stability_check.sh`
- Monthly review: `scripts/monthly_stability_review.sh`
- Workflow: `.github/workflows/fault-injection-monitoring.yml`
- CI monitoring guide: `docs/dev/FAULT_INJECTION_CI_MONITORING_GUIDE.md`

