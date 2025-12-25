# Prompts

## 3-Strikes Protocol
- Max 3 fix attempts per failing test/suite
- On 3rd failure:
  1. Rollback to last compiling state
  2. Quarantine the failing test (skip with `ct:comment/1`)
  3. Log in `progress.md` with justification
  4. Move to next item

## Rollback Commands
```bash
git checkout -- <file>
rebar3 compile
```

## Mock Cleanup Pattern
```erlang
end_per_testcase(_TC, _Config) ->
    catch meck:unload(router_nats),
    ok.
```

## Non-Degradation Rule
After EVERY code change:
1. Run `rebar3 compile`
2. Verify exit code 0
3. If compilation fails → immediate rollback

## Evidence Requirements
Every ledger entry must include:
- Exact command run
- Exit code
- Log path (if CT run)
- Error excerpt (if failure)
