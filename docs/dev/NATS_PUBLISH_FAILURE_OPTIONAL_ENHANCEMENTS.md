# NATS Publish Failure - Optional Enhancements (Second Wave)

## Status

**Date**: 2025-11-30  
**Priority**: ⏳ **OPTIONAL** (not part of core TZ)  
**Status**: 📋 **PLANNED** (ready for prioritization)

## Overview

This document lists optional enhancements that improve observability and operational processes around the existing publish failure behavior specification and tests. These are **improvements**, not requirements.

## Enhancement Tasks

### 1. SRE Review and Sign-off

**Priority**: Medium  
**Effort**: 1-2 hours  
**Status**: ⏳ **PENDING SRE REVIEW**

**Goal**: Formally close operational aspect with SRE team approval.

**Actions**:
1. SRE team reviews `NATS_PUBLISH_FAILURE_METRICS_ALERTS.md`
2. SRE team fills out `NATS_PUBLISH_FAILURE_SRE_REVIEW_TEMPLATE.md`
3. Sign-off section completed with:
   - Reviewer name and date
   - Approval status
   - Action items (if any)

**Deliverable**:
- Completed `NATS_PUBLISH_FAILURE_SRE_REVIEW_TEMPLATE.md` with sign-off

**Benefits**:
- Formal approval of metrics and alerts
- Clear ownership and responsibility
- Documented decisions for future reference

**Dependencies**: SRE team availability

---

### 2. Implement Metric Labels Enhancement

**Priority**: Low (can be deferred)  
**Effort**: 11-18 hours  
**Status**: 📋 **PLANNED** (ready for implementation)

**Goal**: Add labels to metrics for better observability and filtering.

**Plan**: `NATS_PUBLISH_FAILURE_METRICS_ENHANCEMENT_PLAN.md`

**Implementation Steps**:
1. Update `router_nats.erl` to use `emit_metric/3` with labels:
   ```erlang
   router_metrics:emit_metric(router_nats_publish_with_ack_failures_total, 
       #{count => 1}, 
       #{reason => Reason, error_type => ErrorType, mode => Mode}).
   ```

2. Add helper functions:
   - `classify_error_type/1` - Classify error into category
   - `sanitize_reason/1` - Sanitize error reason for labels

3. Update tests to verify labels

4. Update documentation

**Deliverables**:
- Updated `router_nats.erl` with labeled metrics
- Updated tests
- Updated documentation

**Benefits**:
- Better observability (filter by error type, reason, mode)
- Enhanced dashboards (breakdown by labels)
- Improved alerts (target specific error types)

**Dependencies**: 
- Decision to implement (priority from SRE)
- No code changes to `router_metrics` needed (API already supports labels)

**Note**: This is a **nice-to-have** enhancement, not a requirement. Current metrics are sufficient for basic monitoring.

---

### 3. Integrate Stability Scripts into Dev Practice

**Priority**: Low  
**Effort**: 1 hour  
**Status**: 📋 **PLANNED**

**Goal**: Make stability validation scripts part of development workflow.

**Actions**:
1. Add brief mention in developer guide:
   - When to run stability validation (before merging large changes to router_nats)
   - How to interpret results
   - Integration with PR process (optional)

2. Update `test/RUN_TESTS.md` or create `test/STABILITY_VALIDATION.md`:
   - Usage instructions
   - When to run (burn-in before merge)
   - Expected results

**Deliverables**:
- Updated developer guide or test documentation
- Brief instructions for stability validation

**Benefits**:
- Developers know when to run stability checks
- Prevents regressions in test stability
- Better quality assurance

**Dependencies**: None (scripts already exist)

**Note**: This is a **process improvement**, not a code change.

---

## Task Prioritization

### Recommended Order

1. **SRE Review** (Task 1) - Medium priority
   - **Why**: Formal approval needed for operational readiness
   - **When**: As soon as SRE team available
   - **Blocking**: None (can proceed without, but formal approval is better)

2. **Stability Scripts Integration** (Task 3) - Low priority
   - **Why**: Quick win, improves development process
   - **When**: Next documentation update cycle
   - **Blocking**: None

3. **Metric Labels** (Task 2) - Low priority (deferrable)
   - **Why**: Enhancement, not requirement
   - **When**: If SRE prioritizes it or observability needs increase
   - **Blocking**: SRE prioritization decision

### Decision Matrix

| Task | Priority | Effort | Value | Recommendation |
|------|----------|--------|-------|-----------------|
| SRE Review | Medium | 1-2h | High | Do when SRE available |
| Stability Scripts | Low | 1h | Medium | Do in next doc update |
| Metric Labels | Low | 11-18h | High | Defer unless prioritized |

## Current State

**Core TZ**: ✅ **COMPLETE**

All core tasks from original specification are completed:
- ✅ Behavior documented
- ✅ Tests comprehensive
- ✅ Metrics verified
- ✅ Implementation verified
- ✅ Documentation integrated

**Optional Enhancements**: 📋 **PLANNED**

Enhancements are optional improvements that can be done when:
- SRE team is available for review
- Observability needs increase
- Development process improvements are prioritized

## Recommendations

### Immediate Actions

**None required** - Core TZ is complete.

### Short-term (Next Sprint)

1. **SRE Review** (if SRE team available):
   - Schedule review session
   - Complete review template
   - Get sign-off

2. **Stability Scripts Integration**:
   - Add to developer guide
   - Document usage

### Long-term (Future Sprints)

1. **Metric Labels** (if prioritized):
   - Implement when SRE prioritizes
   - Or when observability needs increase

## No Further Core Work Needed

**Status**: ✅ **CORE WORK COMPLETE**

The topic "publish/publish_with_ack failures in router_nats" is:
- ✅ Fully documented
- ✅ Comprehensively tested
- ✅ Verified against implementation
- ✅ Ready for production use

**Further work is optional enhancements only**, not requirements.

## References

- `NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Behavior specification
- `NATS_PUBLISH_FAILURE_METRICS_ALERTS.md` - SRE recommendations
- `NATS_PUBLISH_FAILURE_METRICS_ENHANCEMENT_PLAN.md` - Enhancement plan
- `NATS_PUBLISH_FAILURE_SRE_REVIEW_TEMPLATE.md` - SRE review template
- `NATS_PUBLISH_FAILURE_TASKS_CLOSED.md` - Closed tasks registry

