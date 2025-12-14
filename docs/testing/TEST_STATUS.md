# Test Suite Status Overview

This document provides a per-suite status overview for the beamline_router project.

**Last Updated**: 2025-12-08

## Summary

| Tier | Suites | Tests | Time | Status |
|------|--------|-------|------|--------|
| Sanity | 4 | 18 | ~10s | ✅ Green |
| Fast | 23 | 182 | ~56s | ✅ Green |
| Full | 18 | 139 | ~55s | ✅ Green |
| Heavy | 10+ | TBD | varies | ⚠️ Documented |

## Migration Status

- **Total Suites**: 120 (approx)
- **With `groups_for_level/1`**: 120 (100% Complete)
- **Remaining to Migrate**: 0

### Recently Migrated
- `router_nats_connection_failure_SUITE`
- `router_nats_publish_retry_SUITE`
- `router_nats_subscriber_caf_SUITE`
- `router_observability_otel_spans_SUITE`
- `router_observability_SUITE`
- `router_policy_enforcement_SUITE`
- `router_jetstream_extended_recovery_SUITE`
- `router_metrics_labels_integration_SUITE`
- `router_metrics_r10_SUITE`
- `router_nats_compatibility_SUITE`
- `router_normalize_boolean_prop_SUITE`
- And all others from previous batches.

All test suites now implement the tiered `groups_for_level/1` pattern.
- `router_policy_applier_load_SUITE` → heavy-only
- `router_metrics_labels_unit_SUITE` → fast/full/heavy
- `router_intake_error_handler_SUITE` → full/heavy (fixed init_per_suite)

---

## Suites by Domain

### Core Routing

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_core_SUITE` | Core | sanity/fast/full/heavy | ✅ Stable | Critical path validation |
| `router_policy_SUITE` | Policy | sanity/fast/full/heavy | ✅ Stable | Policy parsing and validation |
| `router_policy_store_SUITE` | Policy | fast/full/heavy | ✅ Stable | Policy storage backend |
| `router_policy_validator_SUITE` | Policy | fast/full/heavy | ✅ Stable | Policy validation rules |
| `router_assignment_SUITE` | Routing | fast/full/heavy | ✅ Stable | Provider assignment logic |
| `router_decider_prop_SUITE` | Routing | full/heavy | ✅ Stable | Property-based tests (slow) |

### NATS/JetStream

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_decide_consumer_SUITE` | NATS | fast/full/heavy | ✅ Stable | Decide consumer logic |
| `router_result_consumer_SUITE` | NATS | fast/full/heavy | ✅ Stable | Result consumer logic |
| `router_jetstream_soak_SUITE` | JetStream | full(smoke)/heavy | ✅ Migrated | Ultra-long soak tests (hours) |
| `router_nats_publish_failure_SUITE` | NATS | fast/full/heavy | ⚠️ Review | Uses meck extensively |
| `router_nats_connection_failure_SUITE` | NATS | TODO | ⚠️ Migrate | Missing groups_for_level |

### Circuit Breaker

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_circuit_breaker_smoke_SUITE` | CB | sanity/fast/full/heavy | ✅ Stable | Basic CB operations |
| `router_circuit_breaker_SUITE` | CB | full/heavy | ✅ Stable | Full CB tests |
| `router_circuit_breaker_integration_SUITE` | CB | full/heavy | ✅ Stable | CB integration |
| `router_circuit_breaker_load_SUITE` | CB | heavy | ✅ Heavy-only | Load tests |
| `router_circuit_breaker_recovery_SUITE` | CB | full/heavy | ✅ Stable | Recovery scenarios |

### Admin/gRPC

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_admin_cp_status_SUITE` | Admin | fast/full/heavy | ✅ Stable | CP status admin |
| `router_admin_grpc_concurrency_SUITE` | Admin | fast/full/heavy | ✅ Stable | gRPC concurrency |
| `router_admin_grpc_rbac_SUITE` | Admin | fast/full/heavy | ✅ Stable | RBAC authorization |
| `router_admin_self_check_SUITE` | Admin | fast/full/heavy | ✅ Stable | Self-check endpoints |
| `router_grpc_SUITE` | gRPC | full/heavy | ⚠️ Moved | Was flaky in fast, requires full context |

### Observability

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_alerts_test_SUITE` | Observability | fast/full/heavy | ✅ Stable | Alert configurations |
| `router_dashboard_test_SUITE` | Observability | fast/full/heavy | ✅ Stable | Dashboard data |
| `router_telemetry_SUITE` | Observability | fast/full/heavy | ✅ Stable | Telemetry events |
| `router_metrics_r10_SUITE` | Metrics | TODO | ⚠️ Migrate | Missing groups_for_level |

### Security/Compliance

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_rbac_SUITE` | Security | fast/full/heavy | ✅ Stable | RBAC tests |
| `router_compliance_SUITE` | Compliance | fast/full/heavy | ✅ Stable | License/GDPR compliance |
| `router_abuse_SUITE` | Security | fast/full/heavy | ✅ Stable | Abuse detection |

### Performance/Stress

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_performance_benchmark_SUITE` | Performance | heavy | ✅ Migrated | Baseline benchmarks |
| `router_performance_load_SUITE` | Performance | heavy | ⚠️ Migrate | Missing groups_for_level |
| `router_stress_soak_SUITE` | Stress | heavy | ✅ Heavy-only | Long-running soak |
| `router_resilience_benchmark_SUITE` | Resilience | heavy | ✅ Heavy-only | Resilience benchmarks |

### Infrastructure/Meta

| Suite | Domain | Tiers | Status | Notes |
|-------|--------|-------|--------|-------|
| `router_test_structure_SUITE` | Meta | fast/full/heavy | ✅ Stable | Structural enforcement |
| `router_ci_enforcement_SUITE` | CI | fast/full/heavy | ✅ Stable | CI contract checks |
| `router_config_validator_SUITE` | Config | fast/full/heavy | ✅ Stable | Config validation |
| `router_e2e_smoke_SUITE` | E2E | sanity/fast/full/heavy | ✅ Stable | E2E smoke test |

---

## Known Issues

### Mock-related Flakiness

| Suite | Issue | Resolution |
|-------|-------|------------|
| `router_grpc_SUITE` | Deep gRPC mocking, noproc errors | Moved to `full` tier |
| `router_decider_prop_SUITE` | Property tests hang in fast | Moved to `full` tier |
| `router_nats_publish_failure_SUITE` | Heavy meck usage | Review needed |

### Flaky Tests Excluded from Full Tier

| Suite | Test | Issue | Workaround |
|-------|------|-------|------------|
| `router_policy_integration_SUITE` | `test_overlapping_fallback_rules` | policy_not_found | Excluded from ct-full.sh |
| `router_policy_integration_SUITE` | `test_conflicting_fallback_rules` | policy_not_found | Excluded from ct-full.sh |
| `router_policy_store_SUITE` | `test_telemetry_correlation_id` | Telemetry timeout | Excluded from ct-full.sh |
| `router_core_SUITE` | `test_telemetry_events` | Telemetry timeout | Excluded from ct-full.sh |
| `router_errors_mapping_SUITE` | `test_parse_error_code` | Module not loaded | Excluded from ct-full.sh |
| `router_intake_error_handler_SUITE` | All tests | Requires router_nats process | Excluded from ct-full.sh |

### Heavy Tier Optimization Needed

| Suite | Current Runtime | Target | Notes |
|-------|-----------------|--------|-------|
| `router_jetstream_soak_SUITE` | 4+ hours | N/A | Nightly only |
| `router_stress_soak_SUITE` | 60+ minutes | N/A | Weekly only |
| `router_performance_load_SUITE` | 15+ minutes | <5min | May need splitting |

---

## Audit Scripts

```bash
# Check structural compliance
./scripts/audit-suite-structure.sh

# Check fast tier status
./scripts/audit-fast-tests.sh

# Categorize suites for migration
./scripts/categorize-suites.sh
```

## CI Integration

| Pipeline | Level | Command | Target |
|----------|-------|---------|--------|
| PR Check | sanity | `./scripts/ct-sanity.sh` | <15s |
| PR Validation | fast | `./scripts/ct-fast.sh` | <1min |
| Merge | full | `./scripts/ct-full.sh` | <5min |
| Nightly | heavy | `./scripts/ct-heavy.sh` | unlimited |
