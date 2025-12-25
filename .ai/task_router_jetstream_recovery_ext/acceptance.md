1. `bash scripts/lint/check_ct_quarantine_consistency.sh` -> PASS
2. `rebar3 as deterministic ct` -> PASS
3. `ROUTER_TEST_LEVEL=heavy rebar3 as test ct --suite router_jetstream_recovery_ext_SUITE --readable true --retry` -> PASS
4. Repeat step 3 back-to-back -> PASS (Deterministic)
