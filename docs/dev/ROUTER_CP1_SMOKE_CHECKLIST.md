# Router CP1 Smoke Checklist

## Scope

This document is an **operational checklist** for verifying Router readiness for CP1. Use this checklist locally, before PRs, and before CP1 release tags.

**Component**: `apps/otp/router` and related CP1 contracts  
**CP Status**: CP1-LC (baseline functionality, NATS contracts, minimal observability)

## Quick Status Summary

| Block                    | Status | Command/Script                          | Comment       |
|--------------------------|--------|-----------------------------------------|---------------|
| Build + Dialyzer         | ☐/☑    | `make test` / `rebar3 ct`                |               |
| NATS / JetStream basic   | ☐/☑    | `scripts/gateway_router_cp1_smoke.sh`    |               |
| CP1 routing               | ☐/☑    | `scripts/gateway_router_cp1_smoke.sh`    |               |
| Contracts/schemas        | ☐/☑    | `scripts/check_proto*.sh`                |               |
| Observability CP1         | ☐/☑    | `scripts/observability/validate_observability.sh` |               |

**Status**: ☐ = Not checked / ☑ = Passed  
**Update**: Manual or CI bot

## Static Checks (Router)

### 3.1. Build and Tests

**Checklist**:
- ☐ Router compiles without errors (`rebar3 compile` / `make`)
- ☐ Unit/CT tests pass (`rebar3 ct`, `make test`)
- ☐ Dialyzer clean (no new warnings)

**Commands**:
```bash
# Build
cd apps/otp/router && rebar3 compile

# Run tests
rebar3 ct

# Dialyzer
rebar3 dialyzer
```

**Implementation**: `apps/otp/router/Makefile` (targets: `test`, `dialyzer`), `apps/otp/router/rebar.config`  
**Tests**: `apps/otp/router/test/*_SUITE.erl`  
**Verification**: `rebar3 ct --suite router_core_SUITE`

### 3.2. CP1 Contracts and Schemas

**Checklist**:
- ☐ Passed `scripts/check_proto.sh`
- ☐ Passed `scripts/check_proto_sync.sh` / `check_proto_sync_fast.sh`
- ☐ Passed `scripts/check_proto_nats_compatibility.sh`
- ☐ Passed `scripts/check_schema_changes.sh`

**Commands**:
```bash
# Proto validation
bash scripts/check_proto.sh
bash scripts/check_proto_sync.sh
bash scripts/check_proto_nats_compatibility.sh

# Schema validation
bash scripts/check_schema_changes.sh
```

**References**:
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`
- `docs/ARCHITECTURE/api-registry.md`
- `docs/STATE.schema.json`, `docs/HISTORY.schema.json`

## Functional Smoke (Router CP1)

**Focus**: Router accepts NATS messages, executes routing decisions, maintains CP1 invariants.

### 4.1. Health & Basic Startup

**Checklist**:
- ☐ Router starts locally (via `docker-compose.yml` or `make run`)
- ☐ NATS/JetStream available (health check)
- ☐ Router responds to `/_health` (via gRPC health probe or direct check)

**Commands**:
```bash
# Start Router (via docker-compose)
docker-compose up -d router

# Check NATS health
curl -f http://localhost:4222/healthz || echo "NATS health check failed"

# Check Router health (gRPC)
grpc_health_probe -addr=localhost:9000
```

**Implementation**: `apps/otp/router/src/router_grpc_sup.erl`  
**Tests**: `apps/otp/router/test/router_health_SUITE.erl:test_health_endpoint/1`  
**Verification**: `rebar3 ct --suite router_health_SUITE`

### 4.2. Basic CP1 Routing Scenarios

**Scenarios** (test references in `apps/otp/router/test` and e2e tests in `tests/`):

#### Scenario 1: Successful Routing

**Description**: Basic message routing through CP1 pipeline  
**Checklist**: ☐

**How to Run**:
```bash
# Via smoke script
bash scripts/gateway_router_cp1_smoke.sh --router-only

# Via direct NATS publish
nats pub beamline.router.v1.decide '{"version":"1","request_id":"test-123","tenant_id":"test-tenant","task":{"type":"text.generate","payload":{"prompt":"test"}}}'
```

**Expected Result**:
- `DecideResponse` with `ok: true`
- `decision.provider_id` non-empty
- `decision.reason` one of: `"weighted"`, `"sticky"`, `"fallback"`, `"best_score"`
- Log entry with `level: "INFO"`, `component: "router"`, `message: "Request processed"`

**Tests**: 
- `apps/otp/router/test/router_nats_subscriber_caf_SUITE.erl:test_decide_request_success/1`
- `apps/otp/router/test/router_core_SUITE.erl:test_basic_decision/1`  
**Verification**: `rebar3 ct --suite router_nats_subscriber_caf_SUITE --suite router_core_SUITE`

#### Scenario 2: Validation Error Handling

**Description**: Invalid payload → correct error DTO  
**Checklist**: ☐

**How to Run**:
```bash
# Via smoke script
bash scripts/gateway_router_cp1_smoke.sh --router-only --error-scenarios

# Via direct NATS publish (missing required field)
nats pub beamline.router.v1.decide '{"version":"1","request_id":"test-123"}'
```

**Expected Result**:
- `ErrorResponse` with `ok: false`
- `error.code: "invalid_request"`
- `error.message` non-empty
- Log entry with `level: "WARN"`, `component: "router"`, `message: "Request validation failed"`

**Tests**: 
- `apps/otp/router/test/router_nats_contract_validation_SUITE.erl` (validation tests)
- `apps/otp/router/test/router_error_SUITE.erl:test_missing_tenant_id/1`  
**Verification**: `rebar3 ct --suite router_nats_contract_validation_SUITE --suite router_error_SUITE`

#### Scenario 3: Idempotency (CP1 Minimal)

**Description**: Repeated request with `idempotency_key` does not create duplicates  
**Checklist**: ☐

**Note**: Idempotency: CP2+ feature. CP1: no duplicate assignment on re-delivery (if stated in CP1 requirements). See `docs/dev/ROUTER_CP1_IDEMPOTENCY_SCOPE.md`.

**How to Run**:
```bash
# Via smoke script (if idempotency tests available)
bash scripts/gateway_router_cp1_smoke.sh --router-only --idempotency

# Via direct NATS publish (same idempotency_key)
nats pub beamline.router.v1.decide '{"version":"1","request_id":"test-123","tenant_id":"test-tenant","task":{"type":"text.generate","payload":{"prompt":"test"}},"idempotency_key":"test-key-123"}'
# Repeat with same idempotency_key
```

**Expected Result**:
- First request: `DecideResponse` with `ok: true`
- Second request (same `idempotency_key`): Either same response (idempotent) or error (if CP1 does not guarantee idempotency)
- Log entry indicates idempotency handling (if applicable)

**Tests**: `apps/otp/router/test/router_idempotency_SUITE.erl` (if exists)  
**Reference**: `docs/dev/ROUTER_CP1_IDEMPOTENCY_SCOPE.md` - CP1 idempotency scope  
**Verification**: `grep -r "router_idempotency_SUITE" apps/otp/router/test/ || echo "Suite not found"`

## Observability CP1

**Checklist**:
- ☐ JSON logs conform to convention (`docs/OBSERVABILITY_CONVENTIONS.md`)
- ☐ `/_health` returns 200 + expected JSON
- ☐ Key routing events logged (no secret/PII leakage)

**Commands**:
```bash
# Validate observability
bash scripts/observability/validate_observability.sh

# Check health endpoint
grpc_health_probe -addr=localhost:9000

# Check logs (JSON format)
tail -n 100 logs/router_$(date -u +"%Y-%m-%d").jsonl | jq .
```

**Expected Log Format**:
```json
{
  "timestamp": "2025-11-30T12:00:00Z",
  "level": "INFO",
  "component": "router",
  "message": "Request processed",
  "context": {
    "request_id": "uuid",
    "tenant_id": "test-tenant",
    "provider_id": "openai"
  }
}
```

**References**:
- `docs/OBSERVABILITY.md`
- `docs/OBSERVABILITY_CONVENTIONS.md`
- `apps/otp/router/docs/OPERATIONAL_GUIDE.md`

## CP1 Invariants and Regressions

**Architectural Invariants** (Router CP1):

1. **HTTP Isolation**: Router **never** exposes HTTP endpoints (NATS/gRPC only)
2. **Versioned NATS Subjects**: All subjects **must** be versioned (`beamline.*.v1.*`)
3. **DTO Compliance**: All DTOs **must** match `api-registry.md`
4. **Proto Contract**: All messages **must** match Proto definitions (see `PROTO_NATS_MAPPING.md`)

**Checklist**:
- ☐ All related ADRs (numbers/links) are up-to-date and do not contradict implementation

**ADR References**:
- `docs/ADR/ADR-004-erlang-otp-router.md` - Router architecture
- `docs/ADR/ADR-012-idempotency-layer.md` - Idempotency (CP2+)
- JetStream ADR: Check `ls docs/ADR/ADR-*-jetstream.md` (if exists)

**Implementation Verification**:
- HTTP isolation: `apps/otp/router/src/router_grpc_sup.erl` (gRPC only, no HTTP)
- Versioned subjects: `apps/otp/router/src/router_nats_subscriber.erl` (subject: `beamline.router.v1.decide`)
- DTO compliance: `docs/ARCHITECTURE/api-registry.md#rest-api-c-gateway`
- Proto contract: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md#router-service-beamlineflowv1`

## Change History and .trae/state Link

**How to Use Checklist When Changing `current_cp` in `.trae/state.json`**:

1. **Before CP Transition**:
   - Run all checklist items
   - Verify all ☑ marks
   - Document any failures in `docs/dev/ROUTER_CP1_GAP_ANALYSIS.md`

2. **After CP Transition**:
   - Update `.trae/state.json` with new `current_cp`
   - Update `.trae/history.json` with HMAC entry
   - Verify checklist still passes

**References**:
- `.trae/state.json` - Current project state
- `docs/CP1_ACCEPTANCE_REPORT.md` - CP1 acceptance criteria
- `docs/dev/ROUTER_CP1_COMPLETE_IMPLEMENTATION_REPORT.md` - CP1 implementation report
- `.windsurf/reports/*` - Related reports

## Acceptance Criteria

**Router is CP1-ready if**:
- ✅ All static checks pass (build, tests, Dialyzer, schemas)
- ✅ All functional smoke scenarios pass (health, routing, error handling)
- ✅ Observability baseline meets CP1 requirements (JSON logs, health endpoint)
- ✅ All CP1 invariants verified (HTTP isolation, versioned subjects, DTO compliance)
- ✅ All related ADRs up-to-date and consistent with implementation

**Exit Code**: 0 = All checks passed, 1 = One or more checks failed

