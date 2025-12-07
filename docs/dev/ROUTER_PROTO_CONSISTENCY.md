# Router Proto Consistency

## Scope

**Contract invariants**: Proto ↔ Router code. Router **must** conform to protobuf definitions and NATS mapping.

**Component**: `apps/otp/router` + protobuf/NATS contracts (excluding c-gateway/CAF)  
**CP Status**: CP1-LC (baseline) → CP2-LC+ (enhanced features)

## Sources of Truth and Dependencies

### Proto

**Location**: `proto/beamline/*/v1/*.proto`
- `proto/beamline/flow/v1/flow.proto` - Router service definitions
- `proto/beamline/provider/v1/provider.proto` - Provider definitions
- `proto/beamline/usage/v1/usage.proto` - Usage/metering definitions

**Status**: Proto source files: **missing** (CP2-LC restoration). Source of truth: generated code (`flow_pb.erl`, `flow_pb.hrl`). See `docs/dev/ROUTER_PROTO_NATS_CP2_DETAILED_PLAN.md#proto-file-restoration`.

**References**:
- `proto/README.md` - Proto overview
- `apps/otp/router/docs/GENERATION.md` - Code generation guide

### Router

**Generated Code**:
- `apps/otp/router/src/flow_pb.erl` - Generated Erlang module
- `apps/otp/router/include/flow_pb.hrl` - Generated header file

**Mapping Modules**:
- `apps/otp/router/src/router_nats_subscriber.erl` - NATS message handling
- `apps/otp/router/src/router_core.erl` - Core routing logic
- `apps/otp/router/src/router_caf_adapter.erl` - CAF assignment publishing

### Documentation

**References**:
- `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Proto ↔ NATS subject mapping
- `docs/ARCHITECTURE/api-registry.md` - REST DTO definitions
- `docs/NATS_SUBJECTS.md` - NATS subject specifications

## Consistency Invariants

**Invariants** (always satisfied):

| Invariant                                   | Description                          | Verification/Instrument                     |
|---------------------------------------------|--------------------------------------|---------------------------------------------|
| Versioned subjects                          | `beamline.router.v1.*` format       | `docs/NATS_SUBJECTS.md`, `buf breaking`     |
| Full proto ↔ Router include sync           | `flow_pb.hrl` matches proto          | `scripts/check_proto_sync*.sh`             |
| JSON/NATS compatibility                     | Types compatible with NATS JSON      | `scripts/check_proto_nats_compatibility.sh` |
| Proto field optionality vs runtime          | Proto optional, Router enforces required | `docs/API_CONTRACTS.md`                |
| Two-level contract architecture             | Proto wire vs NATS JSON logical      | `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md`   |

### Invariant 1: Versioned Subjects

**Rule**: Every NATS subject Router uses **must** be versioned (`beamline.*.v1.*`)

**Verification**:
- Check `docs/NATS_SUBJECTS.md` for all Router subjects
- Verify subject format: `beamline.{component}.v1.{action}`
- Run `buf breaking` to detect breaking changes

**Implementation**: `apps/otp/router/src/router_nats_subscriber.erl:11` (subject: `beamline.router.v1.decide`)  
**Verification**: `grep -n "beamline.router.v1.decide" apps/otp/router/src/router_nats_subscriber.erl`

### Invariant 2: Full Proto ↔ Router Include Sync

**Rule**: `flow_pb.hrl` **must** match proto definitions (if proto files exist)

**Verification**:
```bash
bash scripts/check_proto_sync.sh
bash scripts/check_proto_sync_fast.sh
```

**Implementation**: Regenerate after proto changes: `rebar3 gpb_compile` or `buf generate`  
**Verification**: `bash scripts/check_proto_sync.sh`

### Invariant 3: JSON/NATS Compatibility

**Rule**: Proto types **must** be compatible with NATS JSON payload format

**Verification**:
```bash
bash scripts/check_proto_nats_compatibility.sh
```

**Implementation**: NATS adapter layer handles JSON ↔ Proto conversion

### Invariant 4: Proto Field Optionality vs Runtime

**Rule**: Proto fields are optional (protobuf v3), but Router **enforces** required fields at runtime

**Verification**: `docs/API_CONTRACTS.md` - Field Requirements section

**Implementation**: `apps/otp/router/src/router_nats_subscriber.erl` - Validation logic (`validate_decide_request/2` or `handle_decide_request/2`)  
**Verification**: `grep -n "validate\|required" apps/otp/router/src/router_nats_subscriber.erl`

### Invariant 5: Two-Level Contract Architecture

**Rule**: Proto wire protocol (ABI) is separate from NATS JSON logical payload

**Verification**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Section "Proto Contract vs NATS JSON Format"

**Implementation**: NATS adapter layer (`router_nats_subscriber.erl` or `router_caf_adapter.erl`) adds fields (`version`, `request_id`, `task`, `constraints`, `push_assignment`) not in Proto  
**Verification**: `grep -n "version\|request_id\|task" apps/otp/router/src/router_nats_subscriber.erl`

## Generation and Synchronization Mechanics

### 4.1. Stub Generation

**How Erlang modules/headers are generated from proto**:

1. **Proto Source** (if exists): `proto/beamline/flow/v1/flow.proto`
2. **Code Generation**: `buf generate` or `rebar3 gpb_compile`
3. **Generated Artifacts**:
   - `apps/otp/router/src/flow_pb.erl` - Generated Erlang module
   - `apps/otp/router/include/flow_pb.hrl` - Generated header file

**What is committed**:
- ✅ Generated code (`flow_pb.erl`, `flow_pb.hrl`) - **committed**
- ❌ Proto source files - **not committed** (currently missing, to be restored in CP2-LC)

**References**:
- `apps/otp/router/docs/GENERATION.md` - Generation guide
- `apps/otp/router/rebar.config` - `rebar3_gpb_plugin` configuration

### 4.2. Consistency Checks

**Scripts and what they verify**:

| Script                                    | What It Checks                                    | Context for Router                         |
|-------------------------------------------|---------------------------------------------------|--------------------------------------------|
| `scripts/check_proto.sh`                  | Proto syntax, linting, breaking changes           | Proto files must be valid                  |
| `scripts/check_proto_sync.sh`             | Generated code matches proto definitions          | `flow_pb.erl` must match proto             |
| `scripts/check_proto_sync_fast.sh`        | Fast check (checksums) for proto sync             | Quick validation during development         |
| `scripts/check_proto_nats_compatibility.sh`| Proto types compatible with NATS JSON             | JSON payload must be valid for Proto types |

**Commands**:
```bash
# Full proto validation
bash scripts/check_proto.sh

# Sync check (full)
bash scripts/check_proto_sync.sh

# Sync check (fast)
bash scripts/check_proto_sync_fast.sh

# NATS compatibility
bash scripts/check_proto_nats_compatibility.sh
```

## Subject → Protobuf → Router Handler Map

**Main section for auditor/developer**:

| NATS Subject                         | Protobuf Message                         | Router Module/Function                     | Notes |
|--------------------------------------|------------------------------------------|-------------------------------------------|-------|
| `beamline.router.v1.decide`          | `beamline.flow.v1.RouteRequest`          | `router_nats_subscriber.erl:handle_decide_request/2` (lines 116-161) | Request-reply pattern. Test: `router_nats_subscriber_caf_SUITE.erl:test_decide_request_success/1` |
| `beamline.router.v1.decide.reply`    | `beamline.flow.v1.RouteDecision`          | `router_nats_subscriber.erl:send_response/3` | Reply-inbox pattern |
| `caf.exec.assign.v1`                 | `beamline.flow.v1.ExecAssignment`        | `router_caf_adapter.erl:publish_assignment/2` | Push assignment (if `push_assignment: true`). Test: `router_caf_adapter_SUITE.erl` |
| `caf.exec.assign.v1.ack`             | `beamline.flow.v1.ExecAssignmentAck`      | `router_ack_consumer.erl:handle_ack/2`     | Optional acknowledgment |
| `caf.exec.result.v1`                 | `beamline.flow.v1.ExecResult`            | `router_result_consumer.erl:handle_result/2` | Execution result. Test: `router_result_consumer_SUITE.erl` |
| `beamline.usage.v1.metered`          | `beamline.usage.v1.UsageEvent`           | `router_result_consumer.erl:publish_usage_event/2` | Usage metering (published by Router). Test: `router_result_consumer_SUITE.erl`. Verification: `grep -n "publish_usage_event\|beamline.usage.v1.metered" apps/otp/router/src/router_result_consumer.erl` |

**For each route**:

- **Proto Definition**: `proto/beamline/flow/v1/flow.proto` (if exists) or generated code
- **Router Usage**: Module/function references above
- **PROTO_NATS_MAPPING Reference**: `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Router Service section

## Process for Changing Proto (Router)

**Steps for correctly introducing changes**:

1. **Update Proto** (if proto files exist):
   - Modify `proto/beamline/flow/v1/flow.proto`
   - Follow SemVer + `buf breaking` rules
   - Run `buf lint` and `buf build`

2. **Run buf Checks**:
   ```bash
   buf lint
   buf breaking --against '.git#branch=main'
   ```

3. **Generate Stubs**:
   ```bash
   buf generate
   # or
   rebar3 gpb_compile
   ```

4. **Update Router Code**:
   - Update `router_nats_subscriber.erl` if message structure changed
   - Update `router_core.erl` if routing logic affected
   - Update `router_caf_adapter.erl` if assignment structure changed

5. **Update Documentation**:
   - `docs/ARCHITECTURE/PROTO_NATS_MAPPING.md` - Update message definitions
   - `docs/ARCHITECTURE/api-registry.md` - Update REST DTO if affected
   - `docs/NATS_SUBJECTS.md` - Update subject specifications if needed

6. **Run All Checks**:
   ```bash
   bash scripts/check_proto.sh
   bash scripts/check_proto_sync.sh
   bash scripts/check_proto_nats_compatibility.sh
   bash scripts/gateway_router_cp1_smoke.sh --router-only
   ```

**References**:
- `docs/SCHEMA_VERSIONING.md` - Versioning policy
- `docs/ARCHITECTURE/compatibility-rules.md` - Compatibility rules
- ADR on protobuf/compatibility: Check `ls docs/ADR/ADR-*-proto*.md docs/ADR/ADR-*-compatibility*.md` (if exists)

## CP1/CP2 Link and Future Versions

### CP1 Scope (Required)

**Minimal protocol set**:
- `beamline.router.v1.decide` - Basic routing decision
- `beamline.flow.v1.RouteRequest` / `RouteDecision` - Core messages
- `beamline.usage.v1.metered` - Usage metering (published by Router)

**CP1 Invariants**:
- All subjects versioned (`v1`)
- Proto fields optional (protobuf v3), Router enforces required at runtime
- NATS JSON format includes adapter-layer fields (not in Proto)

### CP2-LC+ Scope (Planned)

**Enhanced features**:
- JetStream durable subscriptions (CP2-LC)
- Idempotency layer (CP2-LC)
- Extended observability (tracing, metrics) (CP2+)
- New fields in Proto (backward compatible) (CP2+)

**CP2 Evolution**:
- New Proto packages (e.g., `beamline.flow.v2`) for breaking changes
- New NATS subjects (e.g., `beamline.router.v2.decide`) for new versions
- Backward compatibility maintained where possible

**References**:
- `docs/ARCHITECTURE/compatibility-rules.md` - Compatibility policy
- `docs/RELEASE_PROCESS.md` - Release process and versioning
- `docs/dev/CP2_ROUTER_PLAN.md` - CP2 Router plan
- ADR on DTO/ABI gates (if exists)

## Verification Checklist

**Router Proto consistency is verified if**:
- ✅ All consistency invariants satisfied (versioned subjects, proto sync, JSON compatibility)
- ✅ All generation/synchronization checks pass (`check_proto*.sh`)
- ✅ Subject → Proto → Router handler map is accurate
- ✅ Proto change process documented and followed
- ✅ CP1/CP2 scope clearly defined

**Exit Code**: 0 = All checks passed, 1 = One or more checks failed

