# Operations — CP1

**Beamline Router Operational Baseline**

This document defines the **normal operating mode** for CP1 (Pre-Production Foundation).

CP1 is **NOT production HA**. It is a **validated baseline** for controlled deployments.

---

## Supported Deployment Modes

**In Scope (CP1)**:
- ✅ Local development
- ✅ CI/CD pipelines
- ✅ Single-node staging
- ✅ Controlled load testing

**Explicitly NOT Supported** (future phases):
- ❌ High Availability (multi-node)
- ❌ Multi-region deployment
- ❌ Zero-downtime deploys
- ❌ Horizontal autoscaling
- ❌ Production traffic (uncontrolled)

---

## Runtime Components

### Required Processes

**1. NATS Server**
- Binary: `nats-server` (v2.10.7+)
- Ports: 4222 (client), 8222 (monitor)
- JetStream: enabled
- Store: `/tmp/nats-store` (ephemeral OK for CP1)

**2. Beamline Router**
- Runtime: Erlang/OTP 26-27
- Mode: single node
- NATS client: gnat library

### External Dependencies

**3. c-gateway** (optional for Router-only testing)
- HTTP/gRPC frontend
- Not required for heavy CT suites

**4. CAF Worker** (optional for Router-only testing)
- Custom block execution
- Not required for basic routing validation

---

## Health Signals

### NATS

**Endpoint**: `http://localhost:8222/healthz`

**Expected Response**:
```json
{"status": "ok"}
```

**Verification**:
```bash
./scripts/nats_status.sh
```

### Router

**Erlang Health Check**:
```erlang
% In rebar3 shell
application:which_applications().
% Should include: beamline_router
```

**NATS Connectivity**:
```bash
# Router should subscribe to beamline.router.v1.*
# Check NATS connections:
curl -s http://localhost:8222/connz | jq '.connections[].subscriptions'
```

### System-Level

**Backpressure** (expected: inactive):
```erlang
router_backpressure:status().
% Expected: {ok, inactive} or {ok, {active, _}}
```

**Message Queues** (expected: low):
```erlang
erlang:process_info(whereis(router_nats_client), message_queue_len).
% Expected: < 100 under normal load
```

---

## Resource Envelope (CP1 Baseline)

### Memory

**Router**:
- RSS: ~100 MB (single node, idle)
- Under load: ~200-300 MB (depends on policy complexity)

**NATS**:
- RSS: ~50 MB (JetStream enabled, low load)
- JetStream store: ~10-50 MB (ephemeral OK)

**Verification**:
```bash
ps aux | grep -E "(beam.smp|nats-server)" | awk '{print $2, $4, $11}'
```

### CPU

**Pattern**: Bursty, event-driven

- Idle: < 1% CPU
- Under routing load: 5-20% CPU per core
- CT heavy suites: 30-50% CPU (expected)

### Disk

**Used For**:
- JetStream store: `/tmp/nats-store`
- Router logs: `_artifacts/`
- CT logs: `_test/`

**Expected Usage**: < 500 MB total

**Cleanup**:
```bash
rm -rf /tmp/nats-store/*  # Safe after NATS stop
rm -rf _artifacts/*.log    # Old artifacts
```

---

## Normal Operations

### Startup Sequence

**1. Start NATS**:
```bash
./scripts/nats_start.sh
```

**2. Verify NATS**:
```bash
./scripts/nats_status.sh
```

**3. Start Router** (development):
```bash
rebar3 shell
```

**4. Verify Router** (NATS connectivity):
```erlang
% In rebar3 shell
application:ensure_all_started(beamline_router).
```

### Shutdown Sequence

**1. Stop Router**:
```erlang
% In rebar3 shell: Ctrl+C twice
% Or: application:stop(beamline_router).
```

**2. Stop NATS**:
```bash
./scripts/nats_stop.sh
```

### Running Tests

**Heavy CT Suite**:
```bash
./scripts/heavy_with_nats.sh
```

**Specific Suite**:
```bash
ROUTER_TEST_LEVEL=heavy \
  rebar3 ct --suite test/router_gateway_integration_SUITE.erl
```

---

## What CP1 Does NOT Guarantee

### Durability

- ❌ **No persistent state guarantee**
  - JetStream store is `/tmp` (ephemeral)
  - Router state is in-memory
  - Restart = fresh state

### Availability

- ❌ **No HA / failover**
  - Single NATS node
  - Single Router node
  - Restart required on crash

### Performance

- ❌ **No production SLA**
  - Baseline: 62 rps, p95 < 99ms
  - Not validated under sustained production load
  - No capacity planning for multi-tenant production

### Operations

- ❌ **No zero-downtime deploys**
  - Restart = brief unavailability
  - No rolling updates
  - No canary deployments

### Monitoring

- ❌ **No production observability stack**
  - Basic Prometheus metrics only
  - No alerting (except CI gates)
  - No distributed tracing in production

---

## CP1 Boundaries (What This IS)

**CP1 is**:
- ✅ A **validated foundation**
- ✅ **Repeatable baseline** for testing
- ✅ **CI/CD-safe** (deterministic, bounded)
- ✅ **Operationally documented** (this doc + runbook)

**CP1 is NOT**:
- ❌ Production-ready for uncontrolled traffic
- ❌ A promise of specific SLAs
- ❌ A complete product (requires c-gateway, CAF, etc.)

---

## Next Phase (Post-CP1)

**CP2 will add**:
- Streaming support (SSE, WebSocket)
- Enhanced policy language (DSL)
- Advanced multi-tenancy
- HA considerations

**Production hardening will add**:
- Multi-node clustering
- Persistent state management
- Full observability stack
- Capacity planning
- Incident automation

---

## Operational Contacts (CP1)

**For Issues**:
- Check: `docs/TROUBLESHOOTING.md`
- Runbook: `docs/RUNBOOK.md`
- Task tracking: `.ai/task_ops_baseline/progress.md`

**For Changes**:
- Baseline updates require PR + justification
- Performance regressions blocked by `perf_gate.sh`

---

**CP1 Operations: Bounded, Validated, Reproducible** ✅

**Last Updated**: 2025-12-22  
**Status**: CP1 Freeze Candidate
