# Metrics Contract Maintenance Process

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Define clear process for maintaining metric contracts when TZ (Technical Specification) changes.

## Overview

This document defines the process for maintaining metric contracts when TZ requirements change. The goal is to prevent drift between TZ specifications and the actual implementation in `router_metrics_contract_helpers.erl`.

## Key Principle

**Single Source of Truth**: `router_metrics_contract_helpers.erl` is the authoritative source of truth for all metric contracts. All changes must flow through this module first.

## Process: Adding or Modifying Metrics

### Step 1: Update Contract in Code (FIRST)

**Location**: `apps/otp/router/test/router_metrics_contract_helpers.erl`

**Actions**:
1. Update `get_metric_contract/1` function:
   - Add/modify required labels list
   - Add/modify optional labels list
   - Update format rules map (binary/integer types)

2. Update validation functions if needed:
   - `validate_label_format/2` - if new label formats are introduced
   - `validate_label_type/2` - if new label types are introduced

**Example**:
```erlang
get_metric_contract(router_new_metric_total) ->
    RequiredLabels = [label1, label2],
    OptionalLabels = [label3],
    FormatRules = #{
        label1 => {binary, undefined},
        label2 => {binary, undefined},
        label3 => {integer, optional}
    },
    {RequiredLabels, OptionalLabels, FormatRules};
```

**Why First**: Code changes are testable and verifiable immediately. Tests will fail if contract is incorrect.

### Step 2: Update Tests (SECOND)

**Locations**:
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`
- `apps/otp/router/test/router_result_consumer_SUITE.erl`
- `apps/otp/router/test/router_decide_consumer_SUITE.erl`

**Actions**:
1. Update existing tests to match new contract:
   - `test_metrics_contract_compliance` - add new metric validation
   - `test_metrics_label_formats_and_types` - add format validation for new labels

2. Add new tests if new scenarios are introduced:
   - Scenario-specific tests (e.g., `test_redelivery_all_scenarios`)
   - End-to-end tests (e.g., `test_metrics_e2e_integration`)

**Why Second**: Tests validate that contract changes work correctly and catch regressions.

### Step 3: Update Documentation (THIRD)

**Locations**:
- `apps/otp/router/docs/dev/METRICS_CONTRACT_SPECIFICATION.md` - Contract specification
- `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert definitions (if alerts change)

**Actions**:
1. Update Metrics Catalog section:
   - Add/modify metric description
   - Update required/optional labels
   - Update validation rules

2. Update Test Coverage section:
   - Add new tests to coverage list
   - Update test descriptions if needed

**Why Third**: Documentation reflects the final state after code and tests are complete and verified.

## Process Checklist

When TZ changes require metric contract modifications:

- [ ] **Step 1**: Update `router_metrics_contract_helpers.erl`
  - [ ] Update `get_metric_contract/1` for affected metrics
  - [ ] Update format rules if label types change
  - [ ] Verify code compiles without errors

- [ ] **Step 2**: Update tests
  - [ ] Update `test_metrics_contract_compliance` to include new/modified metrics
  - [ ] Update `test_metrics_label_formats_and_types` for new label formats
  - [ ] Add scenario-specific tests if new use cases are introduced
  - [ ] Run tests locally to verify they pass

- [ ] **Step 3**: Update documentation
  - [ ] Update `METRICS_CONTRACT_SPECIFICATION.md` with new/modified metric specs
  - [ ] Update `PROMETHEUS_ALERTS.md` if alert definitions change
  - [ ] Update test coverage section

- [ ] **Step 4**: Verify in CI
  - [ ] Ensure all tests pass in CI/CD pipeline
  - [ ] Check for any flaky tests (timeout-related failures)
  - [ ] Verify metric contract validation works in all environments

## Common Scenarios

### Scenario 1: Adding a New Metric

1. **Update Contract**: Add new metric to `get_metric_contract/1`
2. **Add Tests**: Add metric to `test_metrics_contract_compliance` and format validation tests
3. **Update Docs**: Add metric to Metrics Catalog in `METRICS_CONTRACT_SPECIFICATION.md`

### Scenario 2: Adding a New Required Label

1. **Update Contract**: Add label to `RequiredLabels` list in `get_metric_contract/1`
2. **Update Tests**: Ensure existing tests include new required label
3. **Update Docs**: Update metric specification with new required label

### Scenario 3: Making a Label Optional

1. **Update Contract**: Move label from `RequiredLabels` to `OptionalLabels` in `get_metric_contract/1`
2. **Update Tests**: Update tests to handle optional label (may be present or absent)
3. **Update Docs**: Update metric specification to mark label as optional

### Scenario 4: Changing Label Type

1. **Update Contract**: Update format rule in `FormatRules` map (e.g., binary → integer)
2. **Update Validation**: Update `validate_label_format/2` if new type needs special handling
3. **Update Tests**: Update format validation tests for new type
4. **Update Docs**: Update label type specification

## Best Practices

### 1. Always Update Contract First

**Why**: Code is the source of truth. Tests will fail if contract is wrong, providing immediate feedback.

**Anti-pattern**: Updating documentation first without updating code leads to drift.

### 2. Test-Driven Changes

**Why**: Tests validate that contract changes work correctly and prevent regressions.

**Practice**: Write or update tests alongside contract changes, not after.

### 3. Documentation Last

**Why**: Documentation should reflect the final, verified state of the system.

**Practice**: Update documentation after code and tests are complete and verified.

### 4. Backward Compatibility

**Why**: Existing consumers may depend on current contract structure.

**Practice**: 
- Prefer adding optional labels over making required labels optional
- Consider deprecation period for breaking changes
- Document breaking changes clearly

### 5. CI/CD Validation

**Why**: Ensures contract changes work in all environments, not just local.

**Practice**: 
- Run full test suite in CI before merging
- Monitor for flaky tests (may indicate timeout issues)
- Verify metric contract validation in CI environment

## Stability Considerations

### Timeout Values

**Current Default**: 2000ms (2 seconds)

**Guidelines**:
- Fast operations (local processing): 1000ms
- Normal operations (telemetry events): 2000ms
- Slow operations (network retries): 5000ms
- Minimum recommended: 500ms

**CI/CD Considerations**:
- CI environments may be slower than local
- Consider increasing timeouts if tests are flaky in CI
- Document timeout rationale in test comments

### Polling Intervals

**Current Default**: 50ms

**Guidelines**:
- Balance between responsiveness and CPU usage
- 50ms is sufficient for telemetry events (typically fast)
- Adjust if telemetry events are known to be slower

## Verification

### Local Verification

```bash
# Run metric contract tests
rebar3 ct --suite apps/otp/router/test/router_jetstream_fault_injection_SUITE \
  --case test_metrics_contract_compliance

# Run all metric-related tests
rebar3 ct --suite apps/otp/router/test/router_jetstream_fault_injection_SUITE \
  --case test_metrics
```

### CI Verification

- All tests in `router_jetstream_fault_injection_SUITE` should pass
- No flaky test failures (timeout-related)
- Metric contract validation should work in CI environment

## References

- **Contract Code**: `apps/otp/router/test/router_metrics_contract_helpers.erl`
- **Contract Spec**: `apps/otp/router/docs/dev/METRICS_CONTRACT_SPECIFICATION.md`
- **Test Suite**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`
- **Prometheus Alerts**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md`

## Change History

**v1.0 (2025-11-30)**:
- Initial process definition
- Three-step process: Code → Tests → Documentation
- Common scenarios and best practices documented

