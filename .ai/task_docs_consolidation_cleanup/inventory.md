# Documentation Inventory

Generated: 2025-12-20

## Summary Statistics

| Category | Count |
|----------|-------|
| **Total docs** | 481 |
| Top-level docs/ | 75 |
| docs/dev/ | 258 |
| docs/testing/ | ? |
| docs/runbooks/ | ? |
| docs/guides/ | ? |
| docs/examples/ | ? |
| docs/observability/ | ? |
| docs/schemas/ | ? |
| docs/workflows/ | ? |
| docs/system-prompts/ | ? |

## Identified Obsolete Categories

| Pattern | Count | Action |
|---------|-------|--------|
| `TODO_EXECUTION_*` | 92 | ARCHIVE/DELETE |
| `*_REPORT*.md` | 50 | REVIEW/ARCHIVE |
| `*_SUMMARY*.md` | 29 | REVIEW/MERGE |
| `*_COMPLETE*.md` | 50 | ARCHIVE |
| `*_STATUS*.md` | 16 | ARCHIVE |

## Domain Classification (Proposed)

### 1. Architecture
- Core system architecture
- Design patterns
- Component relationships

### 2. API & Contracts
- gRPC API documentation
- Proto contracts
- API error codes

### 3. NATS / Messaging
- NATS integration
- JetStream configuration
- Resilience patterns

### 4. Observability
- Metrics documentation
- Telemetry events
- Prometheus alerts
- Tracing

### 5. Security
- ACL model
- RBAC
- Tenant isolation

### 6. Testing
- Test strategy
- Test classification
- Property testing

### 7. Operations
- Operational guides
- Runbooks
- Incident response
- Troubleshooting

### 8. Configuration
- Configuration reference
- Environment variables

## Next Steps

1. Generate full file list by subdirectory
2. Identify canonical docs per domain
3. Mark duplicates and execution logs for removal
