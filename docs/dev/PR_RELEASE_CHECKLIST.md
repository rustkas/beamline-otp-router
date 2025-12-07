# PR/Release Checklist: router_jetstream_redelivery_total

**Status**: ✅ **READY FOR PR/RELEASE**  
**Date**: 2025-11-30

## Quick Reference

### For PR Description
**Use**: `OBSERVABILITY_REDELIVERY_PR_READY.md`

**Sections to include**:
- Summary
- Changes Overview
- Impact
- Validation
- How to Validate in Staging

**Optional**: Shorten if needed, keep core information.

### For PR Links/References
Add links to:
- `OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md` - Detailed task completion
- `STAGING_VALIDATION_COMPLETE.md` - Validation results
- `OBSERVABILITY_REDELIVERY_FINAL_REPORT.md` - Final comprehensive report
- `docs/ADR/ADR-014-metrics-tracing.md#updates` - ADR update
- `docs/ADR/ADR-011-jetstream-e2e.md#updates` - ADR update

### For Release Notes
**Use**: `RELEASE_NOTES_REDELIVERY_METRICS.md`

**Sections**:
- Summary
- Changes
- Impact
- Migration (if needed)

## Checklist

### Pre-PR
- [x] Code implemented and tested
- [x] Tests pass
- [x] Documentation updated
- [x] ADR references updated
- [x] Validation scripts created
- [x] Staging validation guide provided

### PR Creation
- [ ] Copy `OBSERVABILITY_REDELIVERY_PR_READY.md` to PR description
- [ ] Add links to task completion and validation reports
- [ ] Reference ADR updates (ADR-014, ADR-011)
- [ ] Add reviewers (observability team, router team)
- [ ] Tag with labels: `observability`, `metrics`, `cp2`

### Pre-Release
- [ ] Staging validation passed (if not done)
- [ ] Production deployment plan reviewed
- [ ] Monitoring/alerting verified in staging

### Release
- [ ] Copy `RELEASE_NOTES_REDELIVERY_METRICS.md` to release notes
- [ ] Update CHANGELOG.md (if applicable)
- [ ] Tag release with version
- [ ] Deploy to production
- [ ] Monitor metrics and alerts

## Files Reference

**PR Materials**:
- `OBSERVABILITY_REDELIVERY_PR_READY.md` - PR description template
- `PR_RELEASE_CHECKLIST.md` - This file

**Release Materials**:
- `RELEASE_NOTES_REDELIVERY_METRICS.md` - Release notes template

**Reports**:
- `OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md` - Task completion
- `STAGING_VALIDATION_COMPLETE.md` - Validation results
- `OBSERVABILITY_REDELIVERY_FINAL_REPORT.md` - Final report

**ADR Updates**:
- `docs/ADR/ADR-014-metrics-tracing.md` - Metrics ADR (updated)
- `docs/ADR/ADR-011-jetstream-e2e.md` - JetStream ADR (updated)

**Validation**:
- `STAGING_VALIDATION_GUIDE.md` - Staging validation guide
- `scripts/run_complete_staging_validation.sh` - Validation automation

## Quick Commands

### Validate Before PR
```bash
cd apps/otp/router
./scripts/run_complete_staging_validation.sh --skip-prometheus
```

### Check Test Coverage
```bash
cd apps/otp/router
rebar3 ct --suite router_jetstream_redelivery_metrics_SUITE
rebar3 ct --suite router_jetstream_redelivery_runtime_SUITE
```

### Verify Alert Rules
```bash
cd apps/otp/router
./scripts/check_alert_rules.sh
```

## Notes

- **Backward Compatibility**: ✅ Maintained (no breaking changes)
- **Migration**: Not required (backward compatible)
- **Staging Validation**: Guide provided, ready for deployment
- **Production Ready**: ✅ Yes (after staging validation)

---

**Status**: ✅ **ALL MATERIALS READY - PROCEED WITH PR**

