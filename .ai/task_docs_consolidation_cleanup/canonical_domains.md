# Canonical Domain Structure

Generated: 2025-12-20

## Purpose

Define the SINGLE canonical document per topic, and classify all other documents.

---

## Domain 1: Architecture

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `ARCHITECTURE_DOCUMENTATION.md` | Core system architecture |
| `DESIGN_PATTERNS.md` | Design patterns used |
| `ETS_SEMANTICS.md` | ETS usage patterns |

### Merge Into Canonical
| Document | Merge Into | Reason |
|----------|------------|--------|
| `FULL_SYSTEM_INTEGRATION.md` | ARCHITECTURE | Overlaps |
| `RELIABILITY_FAULT_TOLERANCE.md` | ARCHITECTURE | Subsection |
| `RESILIENCE_REQUIREMENTS.md` | ARCHITECTURE | Subsection |

### Archive (Status Reports)
| Document | Reason |
|----------|--------|
| (none at top level) | |

---

## Domain 2: API & Contracts

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `API_CONTRACTS.md` | API contracts specification |
| `GRPC_API.md` | gRPC API reference |
| `GRPC_ERROR_CODES.md` | Error codes reference |
| `PROTO_SYNC.md` | Proto synchronization guide |

### Archive
| Document | Reason |
|----------|--------|
| `API_DOCUMENTATION_COMPLETE.md` | Status marker, merge into API_CONTRACTS |
| `RELEASE_NOTES_GRPC_API.md` | Historical, archive |

---

## Domain 3: NATS / Messaging

### Canonical Documents (KEEP - MERGE INTO ONE)
| Document | Purpose |
|----------|---------|
| `NATS_INTEGRATION_GUIDE.md` | **PRIMARY** - merge all NATS docs here |
| `NATS_SUBJECTS.md` | Subject naming (keep separate) |

### Merge into NATS_INTEGRATION_GUIDE.md
| Document | Section |
|----------|---------|
| `NATS_BEST_PRACTICES.md` | Best Practices section |
| `NATS_CONNECTION_RESILIENCE.md` | Resilience section |
| `NATS_PUBLISH_FAILURE_BEHAVIOR.md` | Failure handling section |

### Archive (Status/Summary docs)
| Document | Reason |
|----------|--------|
| `NATS_RESILIENCE_COMPLETE.md` | Status marker |
| `NATS_RESILIENCE_NEXT_STEPS.md` | Obsolete planning |
| `NATS_RESILIENCE_STATUS.md` | Status marker |
| `NATS_RESILIENCE_SUMMARY.md` | Redundant summary |
| `CHANGELOG_NATS_PUBLISH_FAILURE.md` | Historical changelog |
| `MR_DESCRIPTION_NATS_PUBLISH_FAILURE.md` | MR description (archive) |

### Merge into OBSERVABILITY (Metrics section)
| Document | Reason |
|----------|--------|
| `NATS_METRICS_ALERTS.md` | → PROMETHEUS_ALERTS |
| `NATS_METRICS_COMPLIANCE.md` | → METRICS_DOCUMENTATION |
| `NATS_PRODUCTION_MONITORING.md` | → OPERATIONAL_RUNBOOK |
| `NATS_PUBLISH_FAILURE_METRICS_ALERTS.md` | → PROMETHEUS_ALERTS |
| `NATS_PUBLISH_FAILURE_MONITORING.md` | → OPERATIONAL_RUNBOOK |
| `ROUTER_NATS_METRICS.md` | → METRICS_DOCUMENTATION |

---

## Domain 4: Observability

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `OBSERVABILITY.md` | **PRIMARY** observability guide |
| `METRICS_DOCUMENTATION.md` | Metrics reference |
| `TELEMETRY_EVENTS.md` | Telemetry events catalog |
| `PROMETHEUS_ALERTS.md` | Alerting rules |
| `PRODUCTION_LOGGING.md` | Logging standards |

### Merge/Archive
| Document | Action | Reason |
|----------|--------|--------|
| `OBSERVABILITY_DASHBOARD.md` | MERGE → OBSERVABILITY | Subsection |
| `OBSERVABILITY_CP2_PLANNING.md` | ARCHIVE | Planning doc |
| `TELEMETRY_CAF_ADAPTER.md` | MERGE → TELEMETRY_EVENTS | Subset |

---

## Domain 5: Configuration

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `CONFIG.md` | **PRIMARY** configuration reference |

### Merge/Archive
| Document | Action | Reason |
|----------|--------|--------|
| `CONFIGURATION_REFERENCE_COMPLETE.md` | MERGE → CONFIG | Duplicate |
| `ROUTER_CAF_CONFIG.md` | MERGE → CONFIG | Subsection |

---

## Domain 6: Security

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `SECURITY_GUIDE.md` | **PRIMARY** security documentation |
| `ACL_MODEL.md` | ACL specification |

### Merge
| Document | Action |
|----------|--------|
| `EXTENSIONS_SECURITY_GUIDE.md` | MERGE → SECURITY_GUIDE |

---

## Domain 7: Testing

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `TESTING_GUIDE.md` | **PRIMARY** testing documentation |
| `TEST_CLASSIFICATION.md` | Test level classification |
| `PROPERTY_TESTING.md` | Property testing guide |

### Merge/Archive
| Document | Action | Reason |
|----------|--------|--------|
| `CP1_TESTING.md` | ARCHIVE | Historical |
| `TEST_ENVIRONMENT.md` | MERGE → TESTING_GUIDE | Subset |
| `TESTING_RECOMMENDATIONS.md` | MERGE → TESTING_GUIDE | Subset |
| `QA_TEST_PLAN.md` | ARCHIVE | Planning doc |
| `CONCURRENCY_TESTS_ETS_AND_MECK.md` | MERGE → TESTING_GUIDE | Subset |

---

## Domain 8: Operations

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `OPERATIONAL_RUNBOOK.md` | **PRIMARY** runbook |
| `TROUBLESHOOTING_GUIDE.md` | Troubleshooting |
| `INCIDENT_RESPONSE_PROCEDURES.md` | Incident response |

### Merge/Archive
| Document | Action | Reason |
|----------|--------|--------|
| `OPERATIONAL_GUIDE.md` | MERGE → OPERATIONAL_RUNBOOK | Duplicate |
| `OPERATIONAL_RUNBOOK_COMPLETE.md` | DELETE | Status marker |

---

## Domain 9: Developer Guide

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `DEVELOPER_GUIDE.md` | **PRIMARY** developer guide |
| `ELP_SETUP.md` | ELP setup instructions |

### Merge/Archive
| Document | Action | Reason |
|----------|--------|--------|
| `ELP_QUICK_FIX.md` | MERGE → ELP_SETUP | Subset |
| `GENERATION.md` | Review (unclear purpose) |

---

## Domain 10: Performance

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `PERFORMANCE_GUIDE.md` | **PRIMARY** - merge PERFORMANCE.md here |

### Merge
| Document | Action |
|----------|--------|
| `PERFORMANCE.md` | MERGE → PERFORMANCE_GUIDE |

---

## Domain 11: CAF Integration

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `ROUTER_CAF_INTEGRATION.md` | CAF integration guide |

---

## Domain 12: Extensions

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `EXTENSIONS_E2E_GUIDE.md` | Extensions E2E guide |
| `EXTENSIONS_RUNBOOK.md` | Extensions operations |

---

## Domain 13: Extended Recovery

### Canonical Documents (MERGE INTO ONE)
| Document | Action |
|----------|--------|
| `EXTENDED_RECOVERY_IMPLEMENTATION_SUMMARY.md` | **KEEP as PRIMARY** |
| `EXTENDED_RECOVERY_PRODUCTION_SCALE.md` | MERGE → above |
| `EXTENDED_RECOVERY_RESOURCE_LIMITS.md` | MERGE → above |

---

## Domain 14: R10 (Risk/Rate Limiting)

### Canonical Documents (KEEP)
| Document | Purpose |
|----------|---------|
| `R10_RUNBOOK.md` | **PRIMARY** R10 operations |

### Merge
| Document | Action |
|----------|--------|
| `R10_CLI_EXAMPLES.md` | MERGE → R10_RUNBOOK |
| `R10_CLI_INTEGRATION.md` | MERGE → R10_RUNBOOK |
| `R10_HOW_TO_OPERATE.md` | MERGE → R10_RUNBOOK |
| `R10_SRE_OPERATIONAL_CHECKLIST.md` | MERGE → R10_RUNBOOK |

---

## Summary: Top-Level docs/ Actions

| Action | Count |
|--------|-------|
| KEEP (canonical) | ~25 |
| MERGE | ~25 |
| ARCHIVE | ~15 |
| DELETE | ~10 |
| **Total** | 75 |
