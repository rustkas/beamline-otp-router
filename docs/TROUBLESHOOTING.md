# Troubleshooting — CP1

**Quick Reference: Symptom → Cause → Action**

This is the **project memory** of known issues and their resolutions.

---

## How to Use This Guide

1. Find your **symptom** in the table below
2. Check the **likely cause**
3. Follow the **action** (or see `docs/RUNBOOK.md` for detailed steps)

---

## Common Issues

| Symptom | Likely Cause | Action |
|---------|--------------|--------|
| **Router won't start** | NATS unreachable | Check `:8222`, run `./scripts/nats_start.sh` |
| **NATS won't start** | Ports 4222/8222 in use | Run `./scripts/nats_stop.sh`, check `ss -tlnp` |
| **Backpressure always active** | JetStream lag or stuck state | Restart Router: `pkill beam.smp && rebar3 shell` |
| **CT heavy flakes** | Race conditions or timing | Run sequentially or increase `CT_TIMEOUT_SECONDS` |
| **TLS handshake error** | Cert mismatch or expired | Run `./scripts/generate_certs.sh` |
| **Connection refused :4222** | NATS not running | Run `./scripts/nats_start.sh` |
| **Connection refused :8222** | NATS monitor disabled | Check `nats_tls.conf` has `http: 8222` |
| **enoent in ct_logs** | Crash during test run | Ignore if CT reports PASS |
| **Memory > 500 MB** | Memory leak or large load | Restart Router to release memory |
| **JetStream messages stuck** | Consumer stopped | Restart Router or purge stream |
| **p95 latency > 115ms** | System under load or degraded | Run `./scripts/bench_router.sh`, compare baseline |
| **rps < 56** | Performance regression | Check `./scripts/perf_gate.sh` for details |
| **SSL alert** | TLS config mismatch | Verify cert paths in `config/test_real_nats_tls.config` |
| **beam.smp 100% CPU** | Infinite loop or hot code path | Restart Router, check for runaway processes |
| **NATS slow_consumers > 0** | Router not consuming fast enough | Check backpressure status, increase consumer count |
| **Process message queue > 1000** | Backlog building up | Check `erlang:process_info(Pid, message_queue_len)` |
| **ETS table too large** | Memory leak in cache | Check `recon:ets_by_memory(10)` |
| **Tests pass locally, fail in CI** | Environment differences | Pin CI runner, check OTP version |
| **Perf gate fails** | Real regression or noise | Run 3 times, check median (see `perf_gate.sh --runs 3`) |
| **NATS disconnects** | Network issue or timeout | Check NATS logs: `_artifacts/nats_*.log` |
| **Router reconnects loop** | NATS unstable or crashing | Check NATS health: `./scripts/nats_status.sh` |
| **CT timeout** | Suite hangs or infinite wait | Increase `CT_TIMEOUT_SECONDS` or fix test |
| **Artifacts missing** | Script failed before writing | Check exit codes, re-run with `tee` |
| **Port already in use (4222)** | Previous NATS not stopped | Run `./scripts/nats_stop.sh` or `fuser -k 4222/tcp` |
| **Port already in use (8222)** | Previous NATS monitor not stopped | Run `./scripts/nats_stop.sh` or `fuser -k 8222/tcp` |
| **warmup timeout** | Warmup phase taking too long | Increase `WARMUP_TIMEOUT_SECONDS` or disable warmup |
| **bench_router.sh fails** | NATS not started or suite missing | Ensure NATS running, check suite exists |
| **perf_gate.sh no JSON** | Benchmark didn't produce output | Check `_artifacts/bench_router_*.log` for errors |
| **generate_certs.sh fails** | openssl not found | Install openssl: `sudo apt install openssl` |
| **validate_nats_tls.sh fails** | TLS config error | Check `config/nats_tls.conf` cert paths |
| **Router logs "unknown_app"** | Application not compiled | Run `rebar3 compile` |
| **Router logs "undef"** | Function doesn't exist | Check module exports, recompile |
| **Router logs "badmatch"** | Pattern match failure | Check Erlang logs for details |
| **CT logs "connection_refused"** | NATS down during test | Restart NATS, re-run test |
| **CT logs "timeout"** | Test waiting too long | Increase test timeout or fix logic |

---

## Diagnostic Commands

### Check System State

```bash
# NATS health
./scripts/nats_status.sh

# NATS detailed stats
curl -s http://localhost:8222/varz | jq .

# NATS connections
curl -s http://localhost:8222/connz | jq '.connections[] | {cid, subscriptions}'

# JetStream status
curl -s http://localhost:8222/jsz | jq '.streams[] | {name, messages, bytes}'

# Check ports
ss -tlnp | grep -E ':(4222|8222|50051)'

# Check processes
ps aux | grep -E "(beam.smp|nats-server)"

# Check disk usage
du -sh /tmp/nats-store _artifacts _test
```

### Check Router State (Erlang Shell)

```erlang
% Applications
application:which_applications().

% Memory usage
erlang:memory().

% Process count
erlang:system_info(process_count).

% Top memory consumers
recon:proc_count(memory, 10).

% Top ETS tables
recon:ets_by_memory(10).

% Backpressure status
router_backpressure:status().

% NATS client state
gen_server:call(router_nats_client, status).  % If this module exists
```

---

## Known Limitations (CP1)

These are **expected** in CP1 and not bugs:

| Limitation | Reason | Workaround |
|-----------|--------|-----------|
| No zero-downtime deploys | Single node, no HA | Restart = brief downtime (expected) |
| JetStream state lost on NATS restart | `/tmp` storage ephemeral | Accept in CP1, persistent storage in CP2+ |
| Flaky tests on slow CI | Timing-sensitive tests | Increase timeouts or run sequentially |
| Performance varies by runner | Different CPU/memory | Pin CI runner type |
| Memory grows under load | No GC tuning yet | Restart if > 500 MB |
| CT artifacts sometimes missing | Crash before write | Re-run test |
| NATS reconnects visible in logs | Network blips or restarts | Ignore if < 10/hour |

---

## Debug Mode

**Enable Verbose Logging**:

```bash
# CT with readable output
rebar3 ct --suite test/*.erl --readable=true --verbose

# NATS with debug logging
# Edit nats_tls.conf or nats.conf:
# debug: true
# trace: true
```

**Capture Full State**:

```bash
# Save everything for investigation
tar -czf /tmp/router_debug_$(date +%Y%m%d_%H%M%S).tar.gz \
  _artifacts/ \
  _test/ \
  /tmp/nats-store/ \
  config/

# Send to colleague or save for later analysis
```

---

## Environment-Specific Issues

### Local Development

- **Issue**: NATS won't start (permission denied)
- **Cause**: User lacks permission for `/tmp/nats-store`
- **Fix**: `mkdir -p /tmp/nats-store && chmod 755 /tmp/nats-store`

### CI/CD

- **Issue**: Tests timeout frequently
- **Cause**: Resource contention on shared runner
- **Fix**: Use dedicated runner or increase `CT_TIMEOUT_SECONDS`

### Docker/Containers

- **Issue**: NATS binds fail
- **Cause**: Container network isolation
- **Fix**: Use `host` network mode or expose ports correctly

---

## Performance Troubleshooting

### Latency Above Baseline

**Baseline**: p95 = 99ms, p99 = 200ms

**If p95 > 115ms**:

1. Check system load: `top`, `vmstat 1`
2. Check NATS stats: `curl -s :8222/varz | jq '{cpu, mem}'`
3. Run benchmark: `./scripts/bench_router.sh`
4. Compare: `./scripts/perf_gate.sh`

**Common Causes**:
- CPU throttling (thermal)
- Memory pressure (swapping)
- JetStream lag (check `jsz`)
- Too many concurrent connections

### Throughput Below Baseline

**Baseline**: rps = 62

**If rps < 56** (10% below):

1. Check backpressure: `router_backpressure:status()`
2. Check message queue: `erlang:process_info(Pid, message_queue_len)`
3. Check NATS slow consumers: `curl -s :8222/varz | jq .slow_consumers`

**Common Causes**:
- Backpressure active
- NATS cannot keep up (slow consumers)
- Router process bottleneck

---

## TLS-Specific Issues

| Symptom | Cause | Fix |
|---------|-------|-----|
| `ssl_error:40` | Certificate expired | Regenerate: `./scripts/generate_certs.sh` (certs last 825 days) |
| `ssl_error:20` | CA cert unknown | Check `nats_tls.conf` has correct `ca_file` path |
| `tls_alert:bad_certificate` | Client cert invalid | Regenerate client cert: `./scripts/generate_certs.sh` |
| `tls_alert:handshake_failure` | Protocol mismatch | Check NATS and Router use same TLS version |
| Certificate verification failed | `verify: true` but wrong CA | Ensure Router uses same CA as NATS |

---

## Test-Specific Issues

### CT Suite Hangs

**Symptoms**: Suite runs forever, no output

**Actions**:
1. Kill: `pkill -f ct_run`
2. Check NATS: `./scripts/nats_status.sh`
3. Re-run with timeout: `timeout 900 rebar3 ct ...`

### CT Suite Crashes

**Symptoms**: `{error, {shutdown, ...}}` or beam crash

**Actions**:
1. Check CT logs: `_test/ct_run.*/test.*/run.*/logs/`
2. Look for `CRASH REPORT` or `ERROR REPORT`
3. Full reset: `./scripts/nats_stop.sh && rm -rf _test && ./scripts/nats_start.sh`

### Mocking Issues

**Symptoms**: `{error, mock_active}` or conflicting mocks

**Actions**:
1. Ensure meck is properly unloaded: `meck:unload()`
2. Check for leftover mocks: `meck:validate(ModuleName)`
3. Restart shell if mocks stuck

---

## Recovery Checklist

After applying any fix, always verify:

```bash
# 1. NATS healthy
curl -s http://localhost:8222/healthz
# Expected: {"status": "ok"}

# 2. Ports free and bound correctly
ss -tlnp | grep -E ':(4222|8222)'
# Expected: nats-server listening

# 3. Router can start (if applicable)
# rebar3 shell, then:
application:ensure_all_started(beamline_router).
# Expected: {ok, [...]}

# 4. Smoke test passes
rebar3 ct --suite test/router_decide_SUITE.erl
# Expected: All tests passed
```

---

## When All Else Fails

**Nuclear Option** (safe in CP1):

```bash
./scripts/nats_stop.sh
pkill -9 -f beam.smp
pkill -9 -f nats-server
rm -rf /tmp/nats-store _artifacts _test
./scripts/nats_start.sh
rebar3 clean
rebar3 compile
rebar3 shell
```

**Then**: Re-run your test or operation

**If still broken**: Check `docs/RUNBOOK.md` or collect diagnostics for escalation

---

**CP1 Troubleshooting: Known issues, known fixes** 🔧

**Last Updated**: 2025-12-22  
**Status**: CP1 Freeze Candidate
