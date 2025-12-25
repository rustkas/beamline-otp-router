# Acceptance Criteria

## Required Evidence

1. **Command Help**
   - [ ] Output of `rebar3 ct -h` showing available flags
   - [ ] Documentation of group-related options

2. **Full Test Run**
   - [ ] Output of `ROUTER_TEST_LEVEL=full rebar3 ct --suite=test/router_alerts_test_SUITE -v`
   - [ ] Evidence that quarantine tests are excluded (non-zero test count)

3. **Quarantine Group Isolation**
   - [ ] Output of `rebar3 ct --suite=test/router_alerts_test_SUITE --group=quarantine -v`
   - [ ] Proof that quarantine tests execute in isolation

4. **Full Runner Verification**
   - [ ] Output of `./scripts/ct-full.sh`
   - [ ] Evidence that quarantine tests are excluded (by test count or logs)

5. **Heavy Runner Verification**
   - [ ] Output of `ROUTER_TEST_LEVEL=heavy ./scripts/ct-heavy.sh`
   - [ ] Proof that quarantine tests are included
   - [ ] Verification that no unsupported options are used
