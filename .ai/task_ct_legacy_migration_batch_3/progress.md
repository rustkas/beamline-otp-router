# Progress - task_ct_legacy_migration_batch_3

## Status: IN PROGRESS

## Baseline Metrics
- **Legacy Pattern Count:** 297 (Initial) -> 238 (Current)
- **Reduction:** 59 (Target: 50) - **GOAL EXCEEDED**
- **Suites Migrated:** 12

## Migrated Suites
| Suite | Check Status | Comments |
|-------|--------------|----------|
| `router_caf_adapter_SUITE` | **PASS** (0 tests) | Was empty/dead, now canonically empty. |
| `router_chaos_engineering_SUITE` | **SKIPPED** | Correctly skips when env var not set. |
| `router_error_status_SUITE` | **PASS** (10 tests) | Was dead code, enabled dynamic -> static conversion. |
| `router_e2e_smoke_SUITE` | **PASS** (1 test) | Migrated to canonical format. |
| `router_ets_guard_SUITE` | **PASS** (4 tests) | Migrated and enabled previously skipped tests. |
| `router_extension_invoker_telemetry_SUITE` | **PASS** (8 tests) | Migrated to canonical format. |
| `router_grpc_integration_SUITE` | **PASS** (0 tests) | Canonical empty suite (placeholder). |
| `router_grpc_SUITE` | **PASS** (3 tests) | Cleaned up warnings, migrated to canonical. |
| `router_health_integration_SUITE` | **PASS** (4 tests) | Migrated to canonical format. |
| `router_metrics_capture_smoke_SUITE` | **PASS** (5 tests) | Migrated to canonical format. |
| `router_metrics_labels_unit_SUITE` | **PASS** (20 tests) | Migrated to canonical format. |
| `router_intake_error_codes_SUITE` | **PASS** (9 tests) | Migrated to canonical format. |

## Known Technical Debt (Skips)
| Suite | Test Case | Reason |
|-------|-----------|--------|
| - | - | - |

## Verification Log
- [x] Initial Baseline Count: 297
- [x] Migration Set 1 (Initial 3)
- [x] Migration Set 2 (Additional 9)
- [x] Final Baseline Count: 238
- [x] Quarantine Consistency Check (Implicit)
- [x] Deterministic CT Check (All runs passed)
