# Router Documentation

> **Last Updated**: 2025-12-20  
> **Status**: Consolidated and structured

## Quick Links

| Document | Purpose |
|----------|---------|
| [Architecture](ARCHITECTURE_DOCUMENTATION.md) | System architecture overview |
| [API Contracts](API_CONTRACTS.md) | API specifications |
| [Configuration](CONFIG.md) | Configuration reference |
| [Testing Guide](TESTING_GUIDE.md) | Testing standards |
| [Operational Runbook](OPERATIONAL_RUNBOOK.md) | Operations guide |
| [Troubleshooting](TROUBLESHOOTING_GUIDE.md) | Problem resolution |

---

## Documentation Structure

### Architecture & Design
- [ARCHITECTURE_DOCUMENTATION.md](ARCHITECTURE_DOCUMENTATION.md) - Core system architecture
- [DESIGN_PATTERNS.md](DESIGN_PATTERNS.md) - Design patterns used
- [ETS_SEMANTICS.md](ETS_SEMANTICS.md) - ETS usage patterns
- [RELIABILITY_FAULT_TOLERANCE.md](RELIABILITY_FAULT_TOLERANCE.md) - Fault tolerance design
- [RESILIENCE_REQUIREMENTS.md](RESILIENCE_REQUIREMENTS.md) - Resilience requirements

### API & Contracts
- [API_CONTRACTS.md](API_CONTRACTS.md) - API specifications
- [GRPC_API.md](GRPC_API.md) - gRPC API reference
- [GRPC_ERROR_CODES.md](GRPC_ERROR_CODES.md) - Error codes
- [PROTO_SYNC.md](PROTO_SYNC.md) - Proto synchronization

### NATS & Messaging
- [NATS_INTEGRATION_GUIDE.md](NATS_INTEGRATION_GUIDE.md) - **Primary** NATS guide
- [NATS_SUBJECTS.md](NATS_SUBJECTS.md) - Subject naming conventions
- [ROUTER_NATS_METRICS.md](ROUTER_NATS_METRICS.md) - NATS metrics

### Observability
- [OBSERVABILITY.md](OBSERVABILITY.md) - Observability overview
- [METRICS_DOCUMENTATION.md](METRICS_DOCUMENTATION.md) - Metrics reference
- [TELEMETRY_EVENTS.md](TELEMETRY_EVENTS.md) - Telemetry events
- [PROMETHEUS_ALERTS.md](PROMETHEUS_ALERTS.md) - Alert rules
- [PRODUCTION_LOGGING.md](PRODUCTION_LOGGING.md) - Logging standards

### Configuration
- [CONFIG.md](CONFIG.md) - Configuration reference

### Security
- [SECURITY_GUIDE.md](SECURITY_GUIDE.md) - Security documentation
- [ACL_MODEL.md](ACL_MODEL.md) - ACL specification

### Testing
- [TESTING_GUIDE.md](TESTING_GUIDE.md) - Testing standards
- [TEST_CLASSIFICATION.md](TEST_CLASSIFICATION.md) - Test level classification
- [PROPERTY_TESTING.md](PROPERTY_TESTING.md) - Property testing

### Operations
- [OPERATIONAL_RUNBOOK.md](OPERATIONAL_RUNBOOK.md) - Runbook
- [TROUBLESHOOTING_GUIDE.md](TROUBLESHOOTING_GUIDE.md) - Troubleshooting
- [INCIDENT_RESPONSE_PROCEDURES.md](INCIDENT_RESPONSE_PROCEDURES.md) - Incident response
- [R10_RUNBOOK.md](R10_RUNBOOK.md) - R10 operations
- [PERFORMANCE_GUIDE.md](PERFORMANCE_GUIDE.md) - Performance tuning

### Developer Guide
- [DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md) - Developer guide
- [ELP_SETUP.md](ELP_SETUP.md) - ELP/editor setup

### Integration
- [ROUTER_CAF_INTEGRATION.md](ROUTER_CAF_INTEGRATION.md) - CAF integration
- [EXTENSIONS_E2E_GUIDE.md](EXTENSIONS_E2E_GUIDE.md) - Extensions guide
- [EXTENSIONS_RUNBOOK.md](EXTENSIONS_RUNBOOK.md) - Extensions operations
- [INTAKE_ERROR_HANDLING.md](INTAKE_ERROR_HANDLING.md) - Error handling

---

## Subdirectories

| Directory | Purpose |
|-----------|---------|
| `dev/` | Development-specific guides |
| `testing/` | Test documentation |
| `observability/` | Observability details |
| `runbooks/` | Operational runbooks |
| `guides/` | User guides |
| `schemas/` | Schema definitions |
| `workflows/` | Workflow guides |
| `system-prompts/` | AI prompts |
| `archive/` | Historical/archived docs |

---

## Archive

Historical documents have been moved to `archive/`:
- Status reports
- Execution logs
- Planning documents
- Superseded documentation

See `archive/` for historical reference only.
## Router control subjects

- Control mode listens on `beamline.router.control.v1.*`. Set `ROUTER_CONTROL_SUBJECT_ALIAS=true` to keep responding on `beamline.router.ide.v1.*` for compatibility.
- Control API startup (core-only vs `dev_control` profile) is documented in `docs/CONTROL_API.md`.
