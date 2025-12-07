# PR Creation Guide: router_jetstream_redelivery_total

**Date**: 2025-11-30  
**Status**: ✅ **READY FOR PR CREATION**

## Quick Start

### Option 1: Using GitHub CLI (gh)

```bash
cd /home/rustkas/aigroup

# Create a new branch (if not already created)
git checkout -b observability/redelivery-metrics-enhancement

# Stage all changes
git add apps/otp/router/ docs/ADR/ CHANGELOG.md

# Commit changes
git commit -m "feat(observability): enhance router_jetstream_redelivery_total with label support

- Add full label support (assignment_id, request_id, reason, source)
- Implement ETS/Prometheus labeled metrics storage
- Add structured logging for redelivery events
- Update all consumers to pass context
- Add comprehensive test suites
- Update documentation and ADR references
- Add validation scripts and staging guide

See: apps/otp/router/docs/dev/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md"

# Push branch
git push origin observability/redelivery-metrics-enhancement

# Create PR using GitHub CLI
gh pr create \
  --title "feat(observability): enhance router_jetstream_redelivery_total with label support" \
  --body-file apps/otp/router/docs/dev/PR_DESCRIPTION_TEMPLATE.md \
  --label "observability,metrics,cp2" \
  --reviewer "observability-team,router-team"
```

### Option 2: Using GitLab CLI (glab)

```bash
cd /home/rustkas/aigroup

# Create a new branch (if not already created)
git checkout -b observability/redelivery-metrics-enhancement

# Stage and commit (same as above)
git add apps/otp/router/ docs/ADR/ CHANGELOG.md
git commit -m "feat(observability): enhance router_jetstream_redelivery_total with label support"

# Push branch
git push origin observability/redelivery-metrics-enhancement

# Create MR using GitLab CLI
glab mr create \
  --title "feat(observability): enhance router_jetstream_redelivery_total with label support" \
  --description "$(cat apps/otp/router/docs/dev/PR_DESCRIPTION_TEMPLATE.md)" \
  --label "observability,metrics,cp2"
```

### Option 3: Manual PR Creation

1. **Create branch and commit**:
   ```bash
   cd /home/rustkas/aigroup
   git checkout -b observability/redelivery-metrics-enhancement
   git add apps/otp/router/ docs/ADR/ CHANGELOG.md
   git commit -m "feat(observability): enhance router_jetstream_redelivery_total with label support"
   git push origin observability/redelivery-metrics-enhancement
   ```

2. **Open PR in web interface**:
   - Go to GitHub/GitLab repository
   - Click "New Pull Request" / "New Merge Request"
   - Select your branch
   - **Title**: `feat(observability): enhance router_jetstream_redelivery_total with label support`
   - **Description**: Copy content from `apps/otp/router/docs/dev/PR_DESCRIPTION_TEMPLATE.md`
   - **Labels**: `observability`, `metrics`, `cp2`
   - **Reviewers**: Add observability and router teams

## PR Description Content

**File**: `apps/otp/router/docs/dev/PR_DESCRIPTION_TEMPLATE.md`

This file contains the complete PR description ready to copy-paste.

## Review Checklist

**File**: `apps/otp/router/docs/dev/PR_RELEASE_CHECKLIST.md`

Use this checklist during PR review:

### Pre-PR ✅
- [x] Code implemented and tested
- [x] Tests pass
- [x] Documentation updated
- [x] ADR references updated
- [x] Validation scripts created
- [x] Staging validation guide provided

### PR Creation
- [ ] Copy `PR_DESCRIPTION_TEMPLATE.md` to PR description
- [ ] Add links to task completion and validation reports
- [ ] Reference ADR updates (ADR-014, ADR-011)
- [ ] Add reviewers (observability team, router team)
- [ ] Tag with labels: `observability`, `metrics`, `cp2`

## Files Changed Summary

**Source Files (6)**:
- `apps/otp/router/src/router_jetstream.erl`
- `apps/otp/router/src/router_metrics.erl`
- `apps/otp/router/src/router_prometheus.erl`
- `apps/otp/router/src/router_result_consumer.erl`
- `apps/otp/router/src/router_ack_consumer.erl`
- `apps/otp/router/src/router_decide_consumer.erl`

**Test Files (4)**:
- `apps/otp/router/test/router_jetstream_redelivery_metrics_SUITE.erl` (new)
- `apps/otp/router/test/router_jetstream_redelivery_runtime_SUITE.erl` (new)
- `apps/otp/router/test/router_metrics_dump_SUITE.erl` (updated)
- `apps/otp/router/test/router_jetstream_e2e_SUITE.erl` (updated)

**Documentation (10+)**:
- `docs/OBSERVABILITY_ROUTER_DASHBOARD.md` (updated)
- `docs/ADR/ADR-014-metrics-tracing.md` (updated)
- `docs/ADR/ADR-011-jetstream-e2e.md` (updated)
- `CHANGELOG.md` (updated)
- Multiple dev reports in `apps/otp/router/docs/dev/`

**Scripts (7)**:
- `apps/otp/router/scripts/validate_redelivery_metrics.sh`
- `apps/otp/router/scripts/check_metrics_endpoint.sh`
- `apps/otp/router/scripts/validate_prometheus_queries.sh`
- `apps/otp/router/scripts/check_alert_rules.sh`
- `apps/otp/router/scripts/simulate_fault_injection.sh`
- `apps/otp/router/scripts/run_complete_staging_validation.sh`
- `apps/otp/router/scripts/validate_staging_observability.sh`

## Validation Before PR

Run these commands to validate before creating PR:

```bash
cd apps/otp/router

# Run tests
rebar3 ct --suite router_jetstream_redelivery_metrics_SUITE
rebar3 ct --suite router_jetstream_redelivery_runtime_SUITE

# Run validation scripts
./scripts/check_alert_rules.sh
./scripts/run_complete_staging_validation.sh --skip-prometheus
```

## After PR Creation

1. **Monitor PR status**: Wait for CI/CD to pass
2. **Address review comments**: Use `PR_RELEASE_CHECKLIST.md` as reference
3. **After merge**: Proceed with staging validation
4. **After staging**: Use `RELEASE_NOTES_REDELIVERY_METRICS.md` for release

## References

- **PR Description**: `apps/otp/router/docs/dev/PR_DESCRIPTION_TEMPLATE.md`
- **PR Checklist**: `apps/otp/router/docs/dev/PR_RELEASE_CHECKLIST.md`
- **Task Completion**: `apps/otp/router/docs/dev/OBSERVABILITY_REDELIVERY_METRICS_TASK_COMPLETE.md`
- **Staging Validation**: `apps/otp/router/docs/dev/STAGING_VALIDATION_COMPLETE.md`
- **Final Report**: `apps/otp/router/docs/dev/OBSERVABILITY_REDELIVERY_FINAL_REPORT.md`

---

**Status**: ✅ **READY TO CREATE PR**

