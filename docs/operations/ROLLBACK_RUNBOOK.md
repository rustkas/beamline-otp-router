# Rollback Runbook (T-OPS-01)

This runbook describes the procedures for rolling back the Beamline Router to a previous version in case of failure or instability.

## Overview

The primary tool for rollback is the `scripts/rollback.sh` script. It supports various deployment modes (Docker Compose, Systemd, Kubernetes, and Erlang Release) and handles version resolution and post-rollback health checks.

## Quick Start (Emergency)

If the system is unstable and immediate action is required:

```bash
# Dry-run first to verify what will happen
./scripts/rollback.sh previous --dry-run

# Execute rollback to previous version
./scripts/rollback.sh previous
```

## Rollback Scenarios

### 1. Rolling back to the immediately previous version
Use this when a deployment has just completed and immediate issues are detected.
```bash
./scripts/rollback.sh previous
```
This will restore the last known-good state from `.deployment_backup_` folders (for Systemd/Release) or use `kubectl rollout undo` (for Kubernetes).

### 2. Rolling back to a specific Git Tag
Use this to return to a known stable release.
```bash
./scripts/rollback.sh v1.2.3
```

### 3. Rolling back to a specific Git Commit
Use this if a specific commit needs to be targeted.
```bash
./scripts/rollback.sh abc1234
```

## Deployment Modes

The script auto-detects the deployment mode, but it can be overridden using the `ROLLBACK_MODE` environment variable.

| Mode | Detection Logic | Action |
|------|-----------------|--------|
| `docker` | Presence of `docker-compose.yml` | `docker-compose down` followed by `up -d` with previous image or rebuild. |
| `k8s` | `kubectl` available and deployment found | `kubectl rollout undo` or image update. |
| `systemd` | `beamline-router` service active | Stop service, restore `_build` from backup, start service. |
| `release` | `_build/default/rel/` exists | Restore `_build` from backup or checkout and rebuild. |

## Verification (Post-Rollback)

After every rollback, the script automatically executes a post-check:
1.  **Smoke Test**: Runs `scripts/smoke.sh` (if available) to verify basic connectivity and API health.
2.  **Health Check**: Verifies that the gRPC and HTTP metrics ports are responsive.
3.  **Logging**: Records the rollback event in `_artifacts/rollback_YYYYMMDD_HHMMSS.log`.

### Manual Verification
If automatic checks are skipped or fail, manually verify:
- gRPC endpoint: `grpcurl -plaintext localhost:9000 list`
- Metrics endpoint: `curl http://localhost:9001/metrics`
- NATS connectivity: Check logs for `NATS connection established`.

## Troubleshooting

### "No previous backup found"
This occurs in `systemd` or `release` mode if the deployment script did not create a backup.
**Resolution**: You must roll back by specifying a specific Git tag or commit.

### "Smoke test FAILED"
The rollback completed, but the system is still not healthy.
**Resolution**:
1. Check Router logs: `tail -f logs/console.log`
2. Check NATS status: `nats sub ">"` to see if messages are flowing.
3. Consider a **Clean State Restart** (see `RECOVERY_RUNBOOK.md`).

### "Permission Denied"
Rollback requires permissions to restart services or modify files.
**Resolution**: Run with `sudo` if using Systemd or if the user lacks Docker/Kubernetes permissions.

## Related Runbooks
- [RECOVERY_RUNBOOK.md](RECOVERY_RUNBOOK.md) - For cluster-wide failures and NATS recovery.
- [INCIDENT_RESPONSE.md](../INCIDENT_RESPONSE.md) - For general incident handling procedures.
