# R10 Next Phase: Hardening, Reuse, Operations, Hygiene

## Overview

R10 infrastructure is complete. Focus shifts from "building" to "hardening, reuse, operations, and maintenance".

## Track 1: Hardening R10 as Safety Net

### 1.1. Invariants for Sliding Window and trigger_reason

**Goal**: Property tests or targeted tests for:
- Window monotonicity: events older than `window_seconds` never participate in calculation
- trigger_reason correctness:
  - If latency triggered → no failure/error_rate threshold scenarios in window
  - If failure/error_rate triggered → latency doesn't exceed threshold

**Implementation**:
- Use `router_r10_metrics` (no direct ETS access)
- Add to `router_circuit_breaker_SUITE` or new `router_circuit_breaker_invariants_SUITE`

**Status**: ⏳ Pending

### 1.2. Mini-Randomization for E2E

**Goal**: One test that:
- Takes random profile (`ci`/`heavy`) within reasonable bounds
- Randomizes `NumClients` and `RequestsPerClient` in narrow range (±20% from base)
- Runs `scenario_mass_failure_opens_breaker` and `scenario_recovery_after_failure`

**Purpose**: Catch hidden timing dependencies without creating new scenarios

**Status**: ⏳ Pending

## Track 2: Replicating R10 Patterns to Other Code

### 2.1. Template "X_rN_metrics"

**Goal**: For next risk theme (R11/R12 or other module):
- Create `<module>_metrics.erl` following `router_r10_metrics` pattern
- Enforce no direct ETS access in tests and code (like R10)

**Documentation**: Add section to `OBSERVABILITY_CONVENTIONS.md`:
- "New risk tests must go through their own *_metrics module"

**Status**: ⏳ Pending

### 2.2. Reuse reset/lifecycle Pattern

**Goal**: For other gen_servers with ETS that participate in tests:
- Implement same pattern:
  - `init/1` → `do_init/1`
  - Safe reset via `handle_call(reset_all, ...)`
  - Lifecycle helpers in `*_test_utils`

**Purpose**: Reduce chance of repeating old "ETS+CT+sup" problems

**Status**: ⏳ Pending

## Track 3: Operationalization: From Test R10 to Runtime

### 3.1. Dashboard Verification for R10

**Goal**: Ensure key R10 metrics:
- Are exported (Prometheus/equivalent)
- Have readable labels (`tenant_id`, `provider_id`, `reason`)
- Are easily filterable by R10 conventions

**If QA-dashboard/Grafana exists**: Add:
- Panel with `router_circuit_breaker_state`
- Panel with `router_circuit_breaker_trigger_reason` (pie/bar by reasons)

**Status**: ⏳ Pending

### 3.2. Runbook Based on Existing Docs

**Goal**: Based on `R10_P0_COMPLETE_FINAL.md` + `QA_TEST_PLAN.md`:
- Create short **Runbook for provider incidents**:
  - How to identify circuit breaker trigger from metrics
  - How to quickly reproduce locally via R10 E2E scenario
  - Which settings (thresholds, timeouts) to adjust first

**Status**: ⏳ Pending

## Track 4: Hygiene and Long-Term Support

### 4.1. Cleanup Temporary Diagnostics

**Goal**: Review and clean:
- `router_circuit_breaker.erl`
- `router_test_utils.erl`
- `router_publish_failure_e2e_SUITE.erl`

**Keep**:
- Structured logs (`router_logger` + metrics)

**Remove**:
- `io:format`, `ct:pal`/`ct:log` that are no longer needed in green state

**Status**: ⏳ Pending

### 4.2. Mini-Checklist for Future Changes

**Goal**: Add to `R10_P0_COMPLETE_FINAL.md` or separate `R10_MAINTENANCE_CHECKLIST.md`:
- "If changing breaker config → update R10 E2E + ct.config"
- "If adding new trigger_reason → add constant and test in `router_r10_metrics` + update OBSERVABILITY_CONVENTIONS"
- "If touching ETS table → check CT environment and reset_all"

**Status**: ⏳ Pending

## Priority Order

1. **Track 4.2** (Maintenance Checklist) - Immediate value, low effort
2. **Track 4.1** (Cleanup Diagnostics) - Clean up codebase
3. **Track 3.2** (Runbook) - Operational value
4. **Track 1.1** (Invariants) - Hardening
5. **Track 2.1** (Template Documentation) - Reuse preparation
6. **Track 1.2** (Randomization) - Advanced hardening
7. **Track 3.1** (Dashboard) - Requires infrastructure
8. **Track 2.2** (Pattern Reuse) - Depends on other modules

## Next Actions

Starting with Track 4 (Hygiene) as it provides immediate value with low effort.

