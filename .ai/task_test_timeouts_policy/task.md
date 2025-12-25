## Task: Introduce test timeouts policy by tiers

- Create a shared `router_test_timeouts` helper that centralizes tier-specific timeout values: sanity / fast / full / heavy.
- Replace at least ten literal timeouts (`receive ... after`, `gen_server:call(..., Timeout)`, `ct:timetrap` configs, etc.) with helper calls to enforce consistent waits across tiers.
- Verify that the full-tier run (`./scripts/ct-full.sh`) remains within ±10% of `reports/full_baseline.json` after the change.
