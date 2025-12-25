# CP1 FREEZE MANIFEST

**Official CP1 Freeze Declaration**

---

## Freeze Metadata

**Status**: `FROZEN` ✅  
**Version**: `1.0.0-rc1`  
**Date**: `2025-12-22`  
**Git Tag**: `cp1-freeze-1.0.0-rc1`

---

## CP1 Invariants (Cannot Change Without CP2)

The following are **frozen** as of CP1:

### Architecture
- ✅ Single-node NATS topology
- ✅ Single-node Router (no clustering)
- ✅ Ephemeral JetStream storage (`/tmp/nats-store`)
- ✅ Request-reply protocol (CP1)

### Performance Baseline
- ✅ Frozen metrics: `perf/baseline_cp1.json`
- ✅ Gate policy: `perf/policy_cp1.json`
- ✅ Baseline: 62 rps, p95=99ms, p99=200ms

### Security Model
- ✅ Self-signed CA for local/staging
- ✅ Client certificate verification required
- ✅ TLS on NATS port 4222

### Operational Scope
- ✅ No HA (single node)
- ✅ No zero-downtime deploys
- ✅ Ephemeral state (restart = fresh)
- ✅ Controlled load (baseline validated)

### Gateway Backpressure Contract
- ✅ Errors must be 0
- ✅ Backpressure must be inactive during baseline

---

## Official Freeze Artifacts

### Infrastructure (T-INFRA-01)
- `scripts/nats_start.sh` - Idempotent NATS startup
- `scripts/nats_status.sh` - Health diagnostics
- `scripts/nats_stop.sh` - Graceful shutdown
- `scripts/heavy_with_nats.sh` - Orchestrated testing

### Performance (T-PERF-01)
- `perf/baseline_cp1.json` - Frozen baseline (6 metrics)
- `perf/policy_cp1.json` - Regression policy
- `perf/README.md` - CP1 freeze documentation
- `scripts/bench_router.sh` - Benchmark harness
- `scripts/perf_gate.sh` - CI gate enforcement
- `.gitlab-ci.perf.example.yml` - CI integration

### Security (T-SEC-01)
- `scripts/generate_certs.sh` - Certificate generation
- `scripts/validate_nats_tls.sh` - TLS validation
- `config/nats_tls.conf` - NATS TLS config
- `config/test_real_nats_tls.config` - Router TLS config

### Operations (T-OPS-01)
- `docs/OPERATIONS.md` - Normal operations & boundaries
- `docs/RUNBOOK.md` - Incident response procedures
- `docs/TROUBLESHOOTING.md` - Symptom → action guide

---

## Official CP1 Commands

### Infrastructure + Heavy Testing
```bash
./scripts/heavy_with_nats.sh
```

### Performance Baseline Measurement
```bash
./scripts/bench_router.sh
```

### Performance Gate (CI/Local)
```bash
./scripts/perf_gate.sh
```

### TLS Validation
```bash
./scripts/validate_nats_tls.sh
```

---

## CP1 Guarantees (IS)

**CP1 IS**:
- ✅ Validated foundation (infra, perf, TLS, ops)
- ✅ Reproducible (idempotent scripts, frozen baseline)
- ✅ CI-safe (deterministic, bounded timeouts)
- ✅ Operationally documented (3 runbook docs)
- ✅ Single-node production-ready (controlled load)

---

## CP1 Non-Guarantees (IS NOT)

**CP1 IS NOT**:
- ❌ Production HA (multi-node clustering)
- ❌ Zero-downtime deploys (restarts expected)
- ❌ Uncontrolled traffic (baseline: 62 rps max validated)
- ❌ Persistent state (ephemeral JetStream)
- ❌ Multi-region deployment

---

## Change Control Policy

### Allowed Without Breaking Freeze

**Bug fixes**:
- Crash fixes (if preserve contract)
- Memory leaks
- Logic errors (if don't change API)

**Documentation**:
- Clarifications in ops docs
- Additional troubleshooting entries
- Performance tips (not baseline changes)

### Requires CP2 Branch

**Architecture changes**:
- Multi-node support
- Streaming protocol
- Policy DSL
- HA considerations

**Baseline changes**:
- Performance baseline update (requires separate MR + justification)
- New metrics added to gate
- Policy threshold changes

**Protocol changes**:
- Request-reply → streaming
- CP1 → CP2

---

## CI Enforcement

### Required Jobs

**Perf Gate** (must be enabled):
```yaml
# In .gitlab-ci.yml
include:
  - local: .gitlab-ci.perf.example.yml
```

**Rules**:
- Must run on: MR, main branch
- Blocks: Performance regressions beyond policy
- Allows: Manual override with justification

### Guard Against Accidental Changes

**Perf gate job** must not be:
- Removed from CI
- Set to `allow_failure: true` on main
- Run without baseline comparison

---

## Baseline Update Process

**To update `perf/baseline_cp1.json`**:

1. Create separate MR (not feature work)
2. Run benchmark 5 times: `./scripts/bench_router.sh`
3. Take median values across runs
4. Provide justification:
   - What changed (optimization, architecture)
   - Why change is acceptable
   - Impact analysis
5. Get approval from maintainers
6. Update baseline + policy if needed

**Frequency**: Rare (expect 1-2 times during CP1 lifetime)

---

## CP1 Lifecycle

### Current Phase: FROZEN ✅

**Started**: 2025-12-22  
**Duration**: Until CP2 kickoff  
**Activities**: Bug fixes, production hardening, documentation

### Next Phase: CP2 Design

**Prerequisites**:
- CP1 deployed to staging
- Real-world performance data collected
- Customer feedback incorporated

**CP2 Must Start With**:
1. Multi-node NATS design
2. Streaming protocol (SSE/gRPC)
3. Policy DSL specification
4. HA assumptions documented

---

## Verification Commands

### Verify Freeze Integrity

```bash
# Check tag exists
git tag -l "cp1-*"

# Verify freeze artifacts
ls -lh scripts/nats*.sh scripts/heavy_with_nats.sh
ls -lh perf/baseline_cp1.json perf/policy_cp1.json
ls -lh scripts/generate_certs.sh scripts/validate_nats_tls.sh
ls -lh docs/OPERATIONS.md docs/RUNBOOK.md docs/TROUBLESHOOTING.md

# Run freeze validation
./scripts/heavy_with_nats.sh
./scripts/perf_gate.sh
```

### Expected Output
- All files present ✅
- All scripts executable ✅
- Heavy CT passes ✅
- Perf gate passes ✅

---

## Document Authority

This document is the **single source of truth** for CP1 freeze.

**Conflicts Resolution**:
- This document > any other documentation
- Git tag commit > any verbal agreements
- Frozen artifacts > any proposals

**Updates**:
- Minor clarifications: PR to main
- Baseline updates: Follow process above
- Scope changes: Requires CP2

---

## Stakeholder Sign-Off

**Engineering**: ✅ Complete (2025-12-22)  
**Operations**: ✅ Documented (3 runbook docs)  
**Performance**: ✅ Frozen (baseline + gate)  
**Security**: ✅ Validated (TLS baseline)

---

**CP1 FREEZE: Officially Closed** 🔒

**Next Milestone**: CP2 Design Kickoff

**Freeze Owner**: Technical Team  
**Review Cycle**: Quarterly (or on major production incidents)

---

**Last Updated**: 2025-12-22  
**Document Version**: 1.0  
**Status**: OFFICIAL ✅
