# Plan — T-INFRA-01

1. Inventory current NATS binary in repo (`nats-server-v2.10.7-linux-amd64/nats-server`) and define baseline paths:
   - store_dir: `/tmp/nats-store`
   - pidfile: `_artifacts/nats.pid`
   - logs: `_artifacts/nats_<timestamp>.log`

2. Implement scripts:
   - start: create dirs, ensure ports free, spawn NATS in background, write pidfile, verify healthz
   - status: show pid, ports, healthz/varz, tail of log
   - stop: kill pid, wait, verify ports are free, remove pidfile

3. Run verification locally:
   - start -> status -> stop
   - capture `_artifacts/nats_<ts>.log`

4. Document the heavy-tier execution against real NATS:
   - command + required envs
   - capture `_artifacts/ct_heavy_<ts>.log`

5. Update progress.md with:
   - executed commands
   - artifact filenames
   - final status: PASS
