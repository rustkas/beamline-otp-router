# Progress — T-INFRA-01

## Status
PASS

## Execution checklist
- [x] Add scripts/nats_start.sh
- [x] Add scripts/nats_status.sh
- [x] Add scripts/nats_stop.sh
- [x] Add scripts/heavy_with_nats.sh (orchestration)
- [x] Verify:
      - start idempotency
      - status output
      - stop frees ports
- [x] Evidence:
      - _artifacts/nats_*.log
      - _artifacts/nats.pid
- [x] Run representative heavy CT suite against real NATS

## Evidence
- NATS log:
  - _artifacts/nats_20251221_115455.log
- PID file:
  - _artifacts/nats.pid
- Heavy CT log (infra baseline):
  - _artifacts/ct_heavy_infra_baseline.log
- Orchestrated run log (with heavy_with_nats.sh):
  - _artifacts/ct_heavy_with_nats_*.log

## Verification commands (exact)
```bash
cd /home/rustkas/aigroup/apps/otp/router

./scripts/nats_start.sh
./scripts/nats_status.sh

ROUTER_TEST_LEVEL=heavy NATS_URL=nats://127.0.0.1:4222 \
  rebar3 as test ct --suite test/router_gateway_integration_SUITE.erl \
  2>&1 | tee _artifacts/ct_heavy_infra_baseline_$(date +%Y%m%d_%H%M%S).log

./scripts/nats_stop.sh
```

Optional:
```bash
./scripts/heavy_with_nats.sh
```

## Notes / blockers
- None
