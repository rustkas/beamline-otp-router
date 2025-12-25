# CP1 to CP2 Migration Guide

**Status**: CP2 Phase 2 Deliverable  
**Audience**: Engineers implementing v2 adoption  
**Version**: 1.0

---

## Overview

This guide describes how to migrate from CP1 (v1 subjects) to CP2 (v2 subjects) while maintaining backward compatibility and operational safety.

**Key Principle**: v1 and v2 operate **in parallel**. Migration is **opt-in**, not forced.

---

## When to Use v2

### Choose v2 When:
- ✅ New deployments (greenfield projects)
- ✅ Need enhanced observability (tracing, strict headers)
- ✅ Require DLQ infrastructure
- ✅ Want stricter contract enforcement
- ✅ Planning for future streaming support

### Stay on v1 When:
- ⏸️ Production stability is critical (no bandwidth for migration)
- ⏸️ Happy with current v1 semantics
- ⏸️ No immediate need for v2 features
- ⏸️ Migrating would delay other priorities

**Recommendation**: New features on v2, existing stable systems on v1.

---

## v1 vs v2 Comparison

| Feature | v1 (CP1 Frozen) | v2 (CP2 Parallel) |
|---------|-----------------|-------------------|
| **Headers** | Optional (backward compat) | **Required** (strict) |
| **Tracing** | Best-effort | End-to-end propagation |
| **Idempotency** | Optional (in-memory) | **Required** (persistent) |
| **DLQ** | Not available | Full DLQ infrastructure |
| **Correlation** | Loose (fallback to request_id) | Strict (assignment_id primary) |
| **Error Codes** | Generic | Structured taxonomy |
| **Contract Enforcement** | Tolerant | **Strict** (fails on violation) |

---

## Header Requirements

### v1 Headers (Optional)
```
trace_id: optional
tenant_id: optional
version: optional
nats-msg-id: optional (but critical for ack/nak)
```

### v2 Headers (REQUIRED)
```
x_trace_id: REQUIRED (non-empty string, max 128 bytes)
x_tenant_id: REQUIRED (non-empty string, max 128 bytes)
x_contract_version: REQUIRED (must be "v2")
x_msg_id: REQUIRED for JetStream events
```

**Migration Path**:
1. Add v2 headers to your publisher
2. Test with v2 validator
3. Switch to v2 subjects when ready

---

## Dual-Publish Pattern (v1 + v2 Parallel)

During migration, publish to BOTH v1 and v2 subjects:

```erlang
%% Erlang example: Dual-publish during migration
publish_dual(Message) ->
    % Convert to v1 format (tolerant headers)
    V1Payload = prepare_v1_payload(Message),
    V1Headers = #{
        <<"trace_id">> => maps:get(trace_id, Message, undefined),
        <<"tenant_id">> => maps:get(tenant_id, Message, undefined)
    },
    
    % Enhance to v2 format (strict headers)
    V2Payload = prepare_v2_payload(Message),
    V2Headers = #{
        <<"x_trace_id">> => maps:get(trace_id, Message),  % Required!
        <<"x_tenant_id">> => maps:get(tenant_id, Message),  % Required!
        <<"x_contract_version">> => <<"v2">>,
        <<"x_msg_id">> => generate_msg_id()  % Required for v2!
    },
    
    % Publish to both (order matters: v1 first for safety)
    ok = nats:publish("subject.v1", V1Headers, V1Payload),
    ok = nats:publish("subject.v2", V2Headers, V2Payload),
    
    ok.
```

**Canary Approach**: Start with 1% traffic to v2, monitor, then expand.

---

## Rollback Strategy (v2 → v1)

If v2 causes issues, rollback is **safe and quick**:

### Step 1: Stop Publishing to v2
```bash
# Feature flag or config change
{cp2_enabled, false}
```

### Step 2: Wait for v2 Queue Drain
```bash
# Monitor pending messages
curl -s http://localhost:8222/jsz | jq '.streams[] | {name, messages}'

# Wait until v2 subjects have messages=0
```

### Step 3: Switch Consumers Back to v1
```bash
# Update consumer configuration
{result_subject, "caf.exec.result.v1"}  # Not v2
```

### Step 4: Verify v1 Traffic Resuming
```bash
# Check v1 subject activity
curl -s http://localhost:8222/varz | jq '.routes'
```

### Step 5: Keep v2 Infrastructure Dormant
**Do NOT delete v2 configs**. Keep them inactive for future retry.

**Rollback Time**: < 5 minutes (if prepared)

---

## Subject Migration Mapping

| v1 Subject (Frozen) | v2 Subject (Parallel) | Migration Notes |
|---------------------|----------------------|-----------------|
| `beamline.router.v1.decide` | `beamline.router.v2.decide` | Same semantics, stricter headers |
| `caf.exec.assign.v1` | `caf.exec.assign.v2` | Planned (not implemented yet) |
| `caf.exec.result.v1` | `caf.exec.result.v2` | Planned (not implemented yet) |
| `beamline.usage.v1.metered` | `beamline.usage.v2.metered` | Planned (not implemented yet) |
| N/A | **`beamline.router.dlq.v2`** | NEW in v2 (DLQ only) |
| N/A | **`beamline.router.v2.status.backpressure`** | NEW in v2 (status query) |

**Strategy**: Migrate one subject at a time, not all at once.

---

## Observability Deltas

### v2 Adds (vs v1):
- ✅ **End-to-end tracing**: Trace ID propagates intake → CAF → result
- ✅ **DLQ metrics**: `router_dlq_messages_total`, `router_dlq_replay_total`
- ✅ **Strict validation metrics**: `router_contract_violations_total`
- ✅ **Idempotency metrics**: `router_idempotency_hits_total`, `router_idempotency_misses_total`

### v2 Changes (vs v1):
- ⚠️ **Error codes**: Stricter taxonomy (e.g., `E001_INVALID_HEADER` instead of generic errors)
- ⚠️ **Correlation failures**: v2 FAILS on missing correlation keys; v1 warns

### Monitoring Recommendations:
```promql
# v2 adoption rate
sum(rate(router_messages_total{version="v2"}[5m])) /
sum(rate(router_messages_total[5m]))

# v2 contract violations (should be 0)
sum(rate(router_contract_violations_total{version="v2"}[5m]))
```

---

## Known Incompatibilities

### 1. Headers Are Required in v2
- **v1**: Tolerates missing headers
- **v2**: **FAILS** if required headers missing
- **Fix**: Add headers before switching to v2

### 2. Idempotency Enforcement
- **v1**: Best-effort (in-memory, can be lost on restart)
- **v2**: **REQUIRED** (persistent store)
- **Impact**: v2 may reject duplicates that v1 accepted

### 3. DLQ Subjects Only in v2
- **v1**: No DLQ (messages discarded on MaxDeliver exhaustion)
- **v2**: **DLQ enabled** (messages routed to `beamline.router.dlq.v2`)
- **Impact**: v2 produces more events (DLQ traffic)

### 4. Correlation Key Strictness
- **v1**: Accepts `request_id` OR `assignment_id` (fallback chain)
- **v2**: **Requires primary key** (`assignment_id` for CAF results)
- **Fix**: Ensure primary key always present

---

## Rollout Strategy

### Phase 1: Canary (1% traffic, 24-48h)
```erlang
% Feature flag
{cp2_tenant_allowlist, [<<"tenant_alpha">>]}.
{cp2_subjects_allowlist, [<<"beamline.router.v2.decide">>]}.
```

**Monitor**:
- v2 message rate
- Contract violations (should be 0)
- Latency delta (v1 vs v2)

### Phase 2: Expand (10% → 25% → 50%, weekly increments)
```erlang
% Gradually add tenants
{cp2_tenant_allowlist, [
  <<"tenant_alpha">>,
  <<"tenant_beta">>,
  <<"tenant_gamma">>
]}.
```

### Phase 3: Full Adoption (100%, after 90+ days stability)
```erlang
% Default to v2 for all new traffic
{cp2_enabled, true}.
{cp2_default_version, "v2"}.
```

### Phase 4: Deprecate v1 (only after proven v2 stability)
**Earliest**: 6 months after v2 100% adoption  
**Condition**: Zero v1 traffic for 30+ days consecutive

**Do NOT delete v1 code** until deprecation complete.

---

## Feature Flags (Config-Driven)

```erlang
% Application config (sys.config or runtime)
[
  {beamline_router, [
    % Phase 2 control
    {cp2_enabled, true},  % Master switch
    {cp2_subjects_allowlist, [
      <<"beamline.router.v2.decide">>,
      <<"beamline.router.v2.status.backpressure">>
    ]},
    {cp2_tenant_allowlist, [
      <<"tenant_alpha">>,
      <<"tenant_beta">>
    ]},
    
    % DLQ config (v2 only)
    {dlq_enabled, true},
    {dlq_subject, <<"beamline.router.dlq.v2">>},
    {dlq_max_retries, 3},
    
    % Idempotency (v2 stricter)
    {idempotency_store, persistent},  % v1 uses in-memory
    {idempotency_ttl_seconds, 86400}
  ]}
].
```

---

## Testing Before Migration

### 1. Contract Validation
```bash
# Ensure v2 headers present
python3 scripts/contract_check.py

# Should output:
# INFO: 7 subjects have ownership mapping
# ✅ Validation PASSED
```

### 2. Dual-Publish Test (Local)
```bash
# Publish to both v1 and v2
# Monitor both queues
watch -n 1 'curl -s :8222/jsz | jq ".streams[] | {name, messages}"'
```

### 3. Load Test v2
```bash
# Run perf harness against v2 endpoint
./scripts/bench_router.sh --subject beamline.router.v2.decide
```

---

## Success Criteria

v2 migration is **successful** when:

- ✅ Zero contract violations for 7+ days
- ✅ v2 latency within 10% of v1 baseline
- ✅ DLQ messages < 0.1% of total traffic
- ✅ Idempotency hit rate > 95%
- ✅ No rollbacks required for 30+ days

---

## Support & Escalation

**Documentation**:
- Contracts: `contracts/cp2_contracts.json`
- Validator: `scripts/contract_check.py`
- Phase 2 Roadmap: `.ai/CP2_PHASE2_ROADMAP.md`

**Troubleshooting**:
- Check: `docs/TROUBLESHOOTING.md`
- Runbook: `docs/RUNBOOK.md`

---

**Migration Principle**: **Incremental, reversible, observable** 🎯

**Last Updated**: 2025-12-22  
**Status**: Phase 2 Deliverable (Complete)
