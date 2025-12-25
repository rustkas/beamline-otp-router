# T-INFRA-01 — Staging NATS/JetStream Baseline: Summary

**Status**: COMPLETE ✅

## Created Files

### Task Definition (.ai/task_staging_nats_setup/)
- ✅ task.md - Goal definition  
- ✅ scope.md - In/out of scope
- ✅ acceptance.md - Must pass criteria
- ✅ plan.md - Implementation plan
- ✅ prompts.md - Operating rules
- ✅ progress.md - Execution checklist

### Scripts (scripts/)
- ✅ nats_start.sh - Idempotent start with healthz check
- ✅ nats_status.sh - Status: pid, ports, healthz, varz, logs
- ✅ nats_stop.sh - Graceful stop + port verification
- ✅ heavy_with_nats.sh - Full orchestration (start → ct → stop)

## Key Features

**Idempotent Start**:
```bash
./scripts/nats_start.sh
# If already running + healthy → exit 0
# Writes: _artifacts/nats_YYYYmmdd_HHMMSS.log
# Writes: _artifacts/nats.pid
```

**Comprehensive Status**:
```bash
./scripts/nats_status.sh
# Shows: pid, ports (4222/8222), healthz, varz, log tail
```

**Graceful Stop**:
```bash
./scripts/nats_stop.sh
# SIGTERM → wait → SIGKILL if needed
# Verifies ports 4222/8222 are free
```

**Heavy CT Orchestration**:
```bash
./scripts/heavy_with_nats.sh [suite]
# start → status → ct → stop (via trap)
# Always writes: _artifacts/ct_heavy_with_nats_*.log
```

## Ports

- **4222**: NATS client protocol
- **8222**: HTTP monitor (healthz, varz)

## Artifacts Location

All in `_artifacts/`:
- `nats_YYYYmmdd_HHMMSS.log` - NATS server logs
- `nats.pid` - Process ID
- `ct_heavy_with_nats_*.log` - CT suite logs

## Acceptance Criteria

✅ **Idempotency**: Start checks :8222/healthz before spawning  
✅ **Status**: Reports pid, ports, healthz, logs  
✅ **Stop**: SIGTERM, verifies ports free  
✅ **Artifacts**: All writes to _artifacts/  
✅ **Documentation**: Manual verification in progress.md

## Usage

```bash
# Start NATS
./scripts/nats_start.sh

# Check status
./scripts/nats_status.sh

# Run heavy CT with orchestration
./scripts/heavy_with_nats.sh

# Or manual CT
ROUTER_TEST_LEVEL=heavy NATS_URL=nats://127.0.0.1:4222 \
  rebar3 as test ct --suite test/router_gateway_integration_SUITE.erl \
  2>&1 | tee _artifacts/ct_heavy_manual_$(date +%Y%m%d_%H%M%S).log

# Stop NATS
./scripts/nats_stop.sh
```

## Notes

- **Monitor port required**: Scripts check :8222/healthz (not :4222)
- **All commands from router root**: `apps/otp/router/`
- **Bounded waits**: Healthz check waits max 2s (10×200ms)
- **Trap on exit**: heavy_with_nats.sh stops NATS even if CT fails

---

**T-INFRA-01: Baseline NATS infrastructure complete and tested!** 🚀
