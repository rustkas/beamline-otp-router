# Metrics Contract Specification

**Version**: CP2+  
**Date**: 2025-11-30  
**Status**: ✅ Complete  
**Purpose**: Formal specification of Router metrics contracts, including required/optional labels, types, and validation rules.

## Overview

This document provides a declarative specification of Router metrics contracts. The authoritative source of truth for metric contracts in code is `router_metrics_contract_helpers.erl` module.

**Key Principle**: All metric contracts are defined in a single location (`router_metrics_contract_helpers.erl`) to ensure consistency and avoid drift between documentation and implementation.

## Metrics Catalog

### JetStream Metrics

#### `router_jetstream_redelivery_total`

**Purpose**: Tracks message redeliveries (NAK calls) in JetStream consumers.

**Type**: Counter

**Required Labels**:
- `assignment_id` (binary, non-empty): Assignment identifier
- `request_id` (binary, non-empty): Request identifier
- `reason` (binary, non-empty): Failure reason (e.g., `tenant_validation_failed`, `backoff`)
- `source` (binary, non-empty): Source of redelivery (e.g., `tenant_validation`, `ack_failure`)

**Optional Labels**:
- `msg_id` (binary, non-empty): Message identifier
- `tenant_id` (binary, non-empty): Tenant identifier

**Validation Rules**:
- All binary labels must be non-empty
- `reason` must match known failure reasons
- `source` must match known redelivery sources

**Test Coverage**:
- `test_redelivery_all_scenarios` - validates all redelivery scenarios
- `test_metrics_contract_compliance` - validates contract compliance
- `test_metrics_label_formats_and_types` - validates label formats
- `test_metrics_e2e_integration` - validates end-to-end metric emission

#### `router_jetstream_maxdeliver_exhausted_total`

**Purpose**: Tracks messages that exceeded MaxDeliver limit in JetStream consumers.

**Type**: Counter

**Required Labels**:
- `assignment_id` (binary, non-empty): Assignment identifier
- `request_id` (binary, non-empty): Request identifier
- `reason` (binary, non-empty): Exhaustion reason (typically `maxdeliver_exhausted`)

**Optional Labels**:
- `msg_id` (binary, non-empty): Message identifier
- `delivery_count` (integer, non-negative): Number of delivery attempts
- `max_deliver` (integer, non-negative): MaxDeliver limit configured
- `tenant_id` (binary, non-empty): Tenant identifier

**Validation Rules**:
- All binary labels must be non-empty
- Integer labels (`delivery_count`, `max_deliver`) must be non-negative
- `delivery_count` should equal `max_deliver` when exhaustion occurs

**Test Coverage**:
- `test_maxdeliver_exhausted_multiple_values` - validates different MaxDeliver values (1, 3)
- `test_metrics_contract_compliance` - validates contract compliance
- `test_metrics_label_formats_and_types` - validates label formats
- `test_metrics_e2e_integration` - validates end-to-end metric emission

### Result Processing Metrics

#### `router_results_tenant_rejected_total`

**Purpose**: Tracks result messages rejected due to tenant validation failures.

**Type**: Counter

**Required Labels**:
- `assignment_id` (binary, non-empty): Assignment identifier
- `request_id` (binary, non-empty): Request identifier
- `reason` (binary, non-empty): Rejection reason (e.g., `tenant_validation_failed`)
- `tenant_id` (binary, non-empty): Tenant identifier that failed validation

**Optional Labels**:
- `msg_id` (binary, non-empty): Message identifier

**Validation Rules**:
- All binary labels must be non-empty
- `reason` must indicate tenant validation failure

**Test Coverage**:
- `test_ack_error_with_tenant_validation_fail_concurrent` - validates tenant rejection metrics
- `test_metrics_contract_compliance` - validates contract compliance

### Usage Metrics

#### `router_usage_emit_failed_total`

**Purpose**: Tracks failed usage event publications.

**Type**: Counter

**Required Labels**:
- `subject` (binary, non-empty): NATS subject where publication failed

**Optional Labels**:
- `error` (binary, non-empty): Error message or code
- `tenant_id` (binary, non-empty): Tenant identifier
- `assignment_id` (binary, non-empty): Assignment identifier
- `request_id` (binary, non-empty): Request identifier

**Validation Rules**:
- All binary labels must be non-empty
- `subject` must be a valid NATS subject format

**Test Coverage**:
- `test_nak_with_publish_failure_recovery` - validates usage emit failure metrics
- `test_metrics_contract_compliance` - validates contract compliance

#### `router_usage_emit_total`

**Purpose**: Tracks successful usage event publications.

**Type**: Counter

**Required Labels**:
- `status` (binary, non-empty): Publication status (e.g., `ok`, `failed`)

**Optional Labels**:
- `provider_id` (binary, non-empty): Provider identifier
- `tenant_id` (binary, non-empty): Tenant identifier
- `assignment_id` (binary, non-empty): Assignment identifier
- `request_id` (binary, non-empty): Request identifier

**Validation Rules**:
- All binary labels must be non-empty
- `status` must be a known status value

**Test Coverage**:
- `test_metrics_contract_compliance` - validates contract compliance

## Label Type Specifications

### Binary Labels

**Format**: Non-empty binary strings

**Validation**:
- Must be `is_binary/1` → `true`
- Must have `byte_size/1` > 0

**Examples**:
- `assignment_id`: `~"assign-123"`
- `request_id`: `~"req-456"`
- `reason`: `~"tenant_validation_failed"`

### Integer Labels

**Format**: Non-negative integers

**Validation**:
- Must be `is_integer/1` → `true`
- Must be `>= 0`

**Examples**:
- `delivery_count`: `3`
- `max_deliver`: `3`

## Contract Validation

### Using `router_metrics_contract_helpers`

All metric contracts are validated using the `router_metrics_contract_helpers` module:

```erlang
%% Get contract for a metric
{RequiredLabels, OptionalLabels, FormatRules} = 
    router_metrics_contract_helpers:get_metric_contract(router_jetstream_redelivery_total).

%% Validate metric labels against contract
case router_metrics_contract_helpers:validate_metric_labels(
    router_jetstream_redelivery_total, Metadata, ExpectedLabels) of
    {ok, Details} ->
        %% Contract validation passed
        ok;
    {fail, Reason} ->
        %% Contract validation failed
        ct:fail("Metric contract validation failed: ~p", [Reason])
end.

%% Assert contract compliance (convenience function)
router_metrics_contract_helpers:assert_metric_contract(
    router_jetstream_redelivery_total, Metadata, ExpectedLabels).
```

### Validation Rules

1. **Required Labels**: All required labels must be present in metadata
2. **Optional Labels**: Optional labels may be present, but are not required
3. **Unexpected Labels**: Labels not in required or optional lists are considered errors
4. **Format Validation**: All labels must match their type specifications (binary/integer)
5. **Value Validation**: Expected label values (if provided) must match actual values

## Test Coverage

### Unit/Integration Tests

- `test_metrics_contract_compliance` - Validates all metrics comply with documented contracts
- `test_metrics_label_formats_and_types` - Validates label formats and types
- `test_redelivery_all_scenarios` - Validates redelivery metric contracts for all scenarios
- `test_maxdeliver_exhausted_multiple_values` - Validates MaxDeliver exhaustion metric contracts

### End-to-End Tests

- `test_metrics_e2e_integration` - Validates metrics reach telemetry layer with complete label sets

### Test Location

All metric contract tests are located in:
- `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl`
- `apps/otp/router/test/router_result_consumer_SUITE.erl`
- `apps/otp/router/test/router_decide_consumer_SUITE.erl`

## Maintenance Process

### Adding New Metrics

1. **Update Contract**: Add metric contract to `router_metrics_contract_helpers.erl`
   - Define required and optional labels
   - Define format rules for each label
   - Add to `get_metric_contract/1` function

2. **Update Tests**: Add test coverage for new metric
   - Add to `test_metrics_contract_compliance`
   - Add format/type validation in `test_metrics_label_formats_and_types`
   - Add scenario-specific tests if needed

3. **Update Documentation**: Update this document
   - Add metric to Metrics Catalog section
   - Document required/optional labels
   - Document validation rules
   - Update test coverage section

### Modifying Existing Metrics

1. **Update Contract**: Modify metric contract in `router_metrics_contract_helpers.erl`
   - Update required/optional labels as needed
   - Update format rules if label types change

2. **Update Tests**: Ensure tests reflect contract changes
   - Update existing tests to match new contract
   - Add new tests if new scenarios are needed

3. **Update Documentation**: Update this document
   - Update metric specification
   - Update validation rules if changed
   - Update test coverage if changed

### Best Practices

- **Single Source of Truth**: Always update `router_metrics_contract_helpers.erl` first
- **Test-Driven**: Write tests before or alongside contract changes
- **Documentation Last**: Update documentation after code and tests are complete
- **Backward Compatibility**: Consider impact on existing consumers when modifying contracts

## References

- **Code**: `apps/otp/router/test/router_metrics_contract_helpers.erl` - Authoritative source of truth
- **Tests**: `apps/otp/router/test/router_jetstream_fault_injection_SUITE.erl` - Metric contract tests
- **Prometheus Alerts**: `apps/otp/router/docs/PROMETHEUS_ALERTS.md` - Alert definitions
- **JetStream Tests**: `apps/otp/router/docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md` - Test documentation

## Change History

**v1.0 (2025-11-30)**:
- Initial specification
- Contracts for JetStream, Result Processing, and Usage metrics
- Validation rules and test coverage documented

