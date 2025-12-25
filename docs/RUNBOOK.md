# Runbook — CP1

**Incident Response Procedures**

This runbook provides **copy-paste commands** for common operational scenarios.

All commands assume you are in the repo root: `/home/rustkas/aigroup/apps/otp/router`

---

## Incident Classification

### Critical (Immediate Action Required)

- 🔴 **Router Down**: Cannot start or crashes immediately
- 🔴 **NATS Down**: Port 4222/8222 unreachable
- 🔴 **Persistent Backpressure**: Stuck active for > 5 minutes

### High (Action Within 15 Minutes)

- 🟠 **Message Backlog**: JetStream lag growing
- 🟠 **CT Suite Failures**: Heavy suites failing consistently
- 🟠 **TLS Handshake Errors**: Client cannot connect

### Medium (Action Within 1 Hour)

- 🟡 **Performance Degradation**: Latency above baseline
- 🟡 **Memory Growth**: RSS > 500 MB
- 🟡 **Flaky Tests**: Intermittent CT failures

---

## Router Restart (Safe)

**When**: Router crashed, hanging, or misconfigured

**Steps**:

```bash
# 1. Stop current Router (if running)
# In rebar3 shell: Ctrl+C twice
# Or kill process:
pkill -f beam.smp

# 2. Verify NATS is healthy
./scripts/nats_status.sh

# 3. If NATS unhealthy, restart it first (see below)

# 4. Clear old artifacts (optional)
rm -rf _artifacts/*.log

# 5. Start Router
rebar3 shell
```

**Verification**:
```erlang
% In rebar3 shell
application:ensure_all_started(beamline_router).
% Expected: {ok, [beamline_router, ...]}
```

**Expected Time**: < 30 seconds

---

## NATS Restart

**When**: NATS crashed, ports unavailable, or healthz failing

**Steps**:

```bash
# 1. Stop NATS (graceful)
./scripts/nats_stop.sh

# 2. Verify ports are free
ss -tlnp | grep -E ':(4222|8222)'
# Expected: no output

# 3. Start NATS
./scripts/nats_start.sh

# 4. Verify health
./scripts/nats_status.sh
curl -s http://localhost:8222/healthz
# Expected: {"status": "ok"}
```

**Expected Time**: < 10 seconds

**If ports still in use**:
```bash
# Force kill (last resort)
sudo fuser -k 4222/tcp 8222/tcp
./scripts/nats_start.sh
```

---

## Full System Reset (Safe in CP1)

**When**: Unknown state, persistent errors, or after major changes

**WARNING**: This will **delete all JetStream state**. Safe for CP1 (ephemeral).

**Steps**:

```bash
# 1. Stop everything
pkill -f beam.smp || true
./scripts/nats_stop.sh

# 2. Clean state
rm -rf /tmp/nats-store/*
rm -rf _artifacts/*.log
rm -rf _test/

# 3. Restart NATS
./scripts/nats_start.sh

# 4. Verify NATS
./scripts/nats_status.sh

# 5. Restart Router
rebar3 shell
```

**Expected Time**: < 1 minute

---

## Backpressure Stuck Active

**When**: Backpressure does not deactivate after load decreases

**Diagnosis**:
```erlang
% In rebar3 shell
router_backpressure:status().
% If {ok, {active, _Reason}} for > 5 minutes → stuck
```

**Recovery**:

```bash
# Option 1: Restart Router (preferred)
# Ctrl+C twice in rebar3 shell, then:
rebar3 shell

# Option 2: Force reset backpressure (experimental)
```

```erlang
% In rebar3 shell
router_backpressure:force_reset().
```

**Verification**:
```erlang
router_backpressure:status().
% Expected: {ok, inactive}
```

---

## Message Backlog (JetStream Lag)

**When**: JetStream pending messages growing

**Diagnosis**:
```bash
curl -s http://localhost:8222/jsz | jq '.streams[] | {name, messages}'
# Check if messages > 1000
```

**Recovery**:

```bash
# Option 1: Wait for consumers to catch up (if Router running)
# Monitor:
watch -n 5 'curl -s http://localhost:8222/jsz | jq ".streams[] | {name, messages}"'

# Option 2: Restart consumers (Router restart)
pkill -f beam.smp
rebar3 shell

# Option 3: Purge streams (DESTRUCTIVE, only if safe)
# This requires nats CLI tool:
nats stream purge <stream_name> --force
```

**Prevention**: Ensure Router is running and processing messages

---

## CT Suite Failures

**When**: Heavy CT suites failing consistently

**Common Causes**:
1. NATS not running
2. Port conflicts (4222/8222)
3. Stale artifacts
4. Race conditions

**Recovery**:

```bash
# 1. Full reset
./scripts/nats_stop.sh
rm -rf /tmp/nats-store _artifacts/*.log _test/
./scripts/nats_start.sh

# 2. Run orchestrated heavy CT
./scripts/heavy_with_nats.sh

# 3. If still failing, run specific suite with verbose output
ROUTER_TEST_LEVEL=heavy \
  rebar3 ct --suite test/router_gateway_integration_SUITE.erl \
  --readable=true --verbose \
  2>&1 | tee _artifacts/ct_debug_$(date +%Y%m%d_%H%M%S).log
```

**Check Artifacts**:
```bash
tail -n 200 _artifacts/ct_*.log
```

---

## TLS Handshake Errors

**When**: Client cannot connect to NATS with TLS

**Symptoms**:
- `ssl_error` or `tls_alert` in logs
- Connection refused on port 4222

**Recovery**:

```bash
# 1. Regenerate certificates (idempotent)
./scripts/generate_certs.sh

# 2. Verify certs exist
ls -lh _artifacts/certs/
# Expected: ca.crt, server.crt, server.key, client.crt, client.key

# 3. Restart NATS with TLS
./scripts/nats_stop.sh
./scripts/validate_nats_tls.sh

# 4. Check NATS TLS config
cat config/nats_tls.conf
# Verify cert paths are correct
```

**Verification**:
```bash
# Test TLS connection manually
openssl s_client -connect localhost:4222 \
  -CAfile _artifacts/certs/ca.crt \
  -cert _artifacts/certs/client.crt \
  -key _artifacts/certs/client.key
# Expected: "Verify return code: 0 (ok)"
```

---

## Performance Degradation

**When**: Latency significantly above baseline (p95 > 115ms)

**Diagnosis**:

```bash
# 1. Check system resources
top -p $(pgrep beam.smp)
top -p $(pgrep nats-server)

# 2. Check NATS stats
curl -s http://localhost:8222/varz | jq '{cpu, mem, connections, slow_consumers}'

# 3. Run benchmark
./scripts/bench_router.sh

# 4. Compare against baseline
./scripts/perf_gate.sh
```

**Common Causes**:
- Too many concurrent connections
- JetStream lag
- Memory pressure
- CPU throttling

**Recovery**:

```bash
# 1. Restart to clear state
./scripts/nats_stop.sh
pkill -f beam.smp
rm -rf /tmp/nats-store

# 2. Restart clean
./scripts/nats_start.sh
rebar3 shell

# 3. Re-measure
./scripts/bench_router.sh
```

---

## Memory Growth

**When**: Router RSS > 500 MB

**Diagnosis**:

```erlang
% In rebar3 shell
erlang:memory().
% Check 'total', 'processes', 'binary', 'ets'

% List top memory processes
recon:proc_count(memory, 10).

% List large ETS tables
recon:ets_by_memory(10).
```

**Recovery**:

```bash
# Restart (releases memory)
pkill -f beam.smp
rebar3 shell
```

**Prevention**: Check for memory leaks in extensions or policies

---

## Flaky Tests (Race Conditions)

**When**: CT suites pass sometimes, fail sometimes

**Mitigation**:

```bash
# 1. Run with increased timeouts
CT_TIMEOUT_SECONDS=1800 ./scripts/heavy_with_nats.sh

# 2. Disable warmup (if warmup causes issues)
WARMUP_ENABLED=0 ./scripts/bench_router.sh

# 3. Run suites sequentially (slower but more stable)
rebar3 ct --suite test/*.erl --readable=false

# 4. Check for port conflicts
ss -tlnp | grep -E ':(4222|8222|50051)'
```

---

## Verification After Recovery

**Always run these checks after any recovery procedure**:

```bash
# 1. NATS health
./scripts/nats_status.sh
curl -s http://localhost:8222/healthz
# Expected: {"status": "ok"}

# 2. Router connectivity (if running)
# In rebar3 shell:
application:which_applications().
# Should include: beamline_router

# 3. No backpressure
# In rebar3 shell:
router_backpressure:status().
# Expected: {ok, inactive}

# 4. Run smoke test
ROUTER_TEST_LEVEL=heavy \
  rebar3 ct --suite test/router_decide_SUITE.erl
```

**Expected**: All checks pass

---

## Escalation Path

If recovery procedures do not resolve the issue:

1. **Collect Diagnostics**:
```bash
# Save full state
tar -czf /tmp/router_incident_$(date +%Y%m%d_%H%M%S).tar.gz \
  _artifacts/ \
  _test/ \
  /tmp/nats-store/
```

2. **Check Recent Changes**:
```bash
git log --oneline -10
git diff HEAD~1
```

3. **Review Task Progress**:
```bash
cat .ai/CP1_READINESS_STATUS.md
```

4. **Consult Troubleshooting Guide**:
```bash
cat docs/TROUBLESHOOTING.md
```

---

## Emergency Contacts (CP1)

- **Documentation**: `docs/OPERATIONS.md`
- **Troubleshooting**: `docs/TROUBLESHOOTING.md`
- **Task Tracking**: `.ai/task_ops_baseline/progress.md`

---

**CP1 Runbook: When in doubt, restart everything** 🔄

**Last Updated**: 2025-12-22  
**Status**: CP1 Freeze Candidate
