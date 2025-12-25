# CP1 Deployment Checklist

**Status**: Pre-Deployment Validation  
**Version**: cp1-freeze-1.0.0-rc1  
**Date**: 2025-12-22

---

## Pre-Deployment Verification

### ✅ Step 1: Frozen State Confirmed

```bash
cd /home/rustkas/aigroup/apps/otp/router

# Verify git tag exists
git tag -l "cp1-*"
# Expected: cp1-freeze-1.0.0-rc1

# Verify freeze manifest
ls -lh docs/CP1_FREEZE.md
# Expected: File exists

# Verify artifacts
ls -lh scripts/nats*.sh scripts/heavy_with_nats.sh
ls -lh perf/baseline_cp1.json perf/policy_cp1.json
ls -lh config/nats_tls.conf
ls -lh docs/OPERATIONS.md docs/RUNBOOK.md docs/TROUBLESHOOTING.md
```

**Status**: ✅ All frozen artifacts present

---

### ✅ Step 2: Local Validation

```bash
# Test NATS lifecycle
./scripts/nats_start.sh
./scripts/nats_status.sh
./scripts/nats_stop.sh

# Expected: All scripts run without errors
```

```bash
# Test heavy CT suite
./scripts/heavy_with_nats.sh

# Expected: Tests pass (or known failures documented)
```

```bash
# Test TLS (optional but recommended)
./scripts/generate_certs.sh
./scripts/validate_nats_tls.sh

# Expected: TLS validation passes
```

---

### ✅ Step 3: Performance Baseline Capture

```bash
# Capture real baseline (replace placeholder)
./scripts/bench_router.sh

# Record output
ls -lh _artifacts/perf_baseline_*.json

# Run performance gate
./scripts/perf_gate.sh

# Expected: PASS (or establish new baseline)
```

**Action**: If baseline needs update, follow process in `perf/README.md`

---

## Deployment Steps (Staging/Production)

### Environment: [STAGING | PRODUCTION]

**Deployment Date**: _________  
**Deployed By**: _________  
**Version**: cp1-freeze-1.0.0-rc1

---

### Step 1: Environment Preparation

```bash
# Verify environment
echo $ENVIRONMENT  # staging or production

# Check NATS availability
nats-server --version
# Expected: v2.10.7+

# Check Erlang/OTP
erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().' -noshell
# Expected: 26 or 27

# Check disk space
df -h /tmp
# Expected: > 10GB free for JetStream
```

---

### Step 2: NATS Deployment

```bash
# Option A: Use frozen scripts (development/staging)
./scripts/nats_start.sh

# Option B: Use systemd (production)
# Create /etc/systemd/system/nats.service
# See: docs/OPERATIONS.md for template
```

**Verify**:
```bash
./scripts/nats_status.sh
curl -s http://localhost:8222/healthz
# Expected: {"status": "ok"}
```

---

### Step 3: Router Deployment

```bash
# Build release
rebar3 as prod release

# Deploy to target
rsync -av _build/prod/rel/beamline_router/ $TARGET:/opt/beamline_router/

# Start Router
$TARGET:/opt/beamline_router/bin/beamline_router start

# Verify
$TARGET:/opt/beamline_router/bin/beamline_router ping
# Expected: pong
```

---

### Step 4: Health Check & Monitoring

```bash
# Check NATS connectivity from Router
# (Depends on your health endpoint implementation)

# Monitor logs
tail -f /var/log/beamline_router/console.log

# Check metrics endpoint
curl -s http://localhost:9090/metrics | grep beamline_router
```

---

## Post-Deployment Data Collection

### Metrics to Track (First 24-48h)

**Performance**:
- Actual RPS (vs baseline 62 rps)
- p95 latency (vs baseline 99ms)
- p99 latency (vs baseline 200ms)
- Memory usage (vs baseline 94MB)

**Errors**:
- Total errors (baseline: 0)
- Error types breakdown
- NATS connection failures
- Reconnect frequency

**Backpressure**:
- Backpressure active duration
- Backpressure triggers
- Recovery time

**NATS/JetStream**:
- Message throughput
- Consumer lag
- Redelivery count
- MaxDeliver exhaustions

**Collection Commands**:
```bash
# Export Prometheus metrics (if available)
curl -s http://localhost:9090/metrics > metrics_$(date +%s).txt

# Or manual observation
watch -n 5 'curl -s :8222/varz | jq "{in_msgs, out_msgs, slow_consumers}"'
watch -n 5 'curl -s :8222/jsz | jq ".streams[] | {name, messages}"'
```

---

## Baseline Update Process (If Needed)

If real-world differs significantly from frozen baseline:

### Step 1: Collect Evidence
```bash
# Run benchmark 5 times
for i in {1..5}; do
  echo "Run $i/5"
  ./scripts/bench_router.sh
  sleep 60
done

# Calculate median
ls -t _artifacts/perf_baseline_*.json | head -5
```

### Step 2: Create Baseline Update MR
```bash
# Create branch
git checkout -b perf-baseline-update-$(date +%Y%m%d)

# Update perf/baseline_cp1.json with median values
# Include justification in commit message
git commit -am "perf: Update CP1 baseline with production data

Justification:
- Production environment differs from test (CPU/network)
- Median values from 5 runs over 48h
- All values within 20% of frozen baseline

Evidence: _artifacts/perf_baseline_20251222_*.json"

# Push and request approval
git push origin perf-baseline-update-$(date +%Y%m%d)
```

**Approval Required**: Maintainers must review before merge

---

## Rollback Plan

If CP1 deployment fails:

### Emergency Rollback
```bash
# Stop Router
$TARGET:/opt/beamline_router/bin/beamline_router stop

# Stop NATS (if safe)
./scripts/nats_stop.sh

# Restore previous version
# (Depends on your deployment strategy)
```

### Partial Rollback (Keep NATS, restart Router)
```bash
# Just restart Router with previous config
$TARGET:/opt/beamline_router/bin/beamline_router restart
```

---

## Success Criteria (First Week)

CP1 deployment is **successful** if:

- ✅ Uptime > 99% (max 1.44h downtime/week)
- ✅ Error rate < 0.1%
- ✅ Performance within ±20% of baseline
- ✅ Zero critical incidents
- ✅ NATS reconnects < 5/day
- ✅ Backpressure active < 5% of time

---

## Data to Collect for CP2 Phase 2

**What to measure** (informs CP2 decisions):

1. **Actual header usage**:
   - How many messages have trace_id?
   - How many have tenant_id?
   - → Informs v2 required headers

2. **Idempotency patterns**:
   - Duplicate rate
   - Idempotency store hit/miss
   - → Informs v2 persistence needs

3. **Correlation key usage**:
   - assignment_id vs request_id prevalence
   - → Informs v2 strictness

4. **Error patterns**:
   - Top 5 error types
   - Retry vs permanent failures
   - → Informs v2 error taxonomy + DLQ

5. **Performance under load**:
   - Peak RPS observed
   - Latency distribution
   - → Informs v2 capacity planning

**Collection Period**: 7-30 days before CP2 Phase 2 continuation

---

## Next Steps After Deployment

### Immediate (First 48h)
- [ ] Monitor health dashboards
- [ ] Collect baseline metrics
- [ ] Document any deviations

### Short-term (First Week)
- [ ] Run perf gate daily
- [ ] Review error logs
- [ ] Collect usage patterns

### Medium-term (First Month)
- [ ] Compile deployment report
- [ ] Update baseline (if needed)
- [ ] Feed data into CP2 Phase 2

---

## Contacts & Escalation

**Documentation**:
- Operations: `docs/OPERATIONS.md`
- Runbook: `docs/RUNBOOK.md`
- Troubleshooting: `docs/TROUBLESHOOTING.md`

**Freeze Authority**: `docs/CP1_FREEZE.md`

---

**CP1 Deployment: Data-Driven, Reversible, Observable** 📊

**Last Updated**: 2025-12-22  
**Status**: Pre-Deployment Ready
