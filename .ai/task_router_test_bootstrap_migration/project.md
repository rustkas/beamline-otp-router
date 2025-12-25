## Project context

**Repo area**
- OTP router app: `/home/rustkas/aigroup/apps/otp/router`

**Main command**
- Full tier: `./scripts/ct-full.sh`

**Quality gate**
- Suite linter: `erl -noshell -pa test_support -s router_suite_linter run -s init stop`

**Important env toggles**
- `ROUTER_TEST_LEVEL`: влияет на то, какие группы/кейсы реально запускаются.
- `ROUTER_ENABLE_META`: включает мета-наборы в части suites.
- `CHAOS_MODE`: влияет на infra-mode (mock/docker) через `router_testops_helper:get_chaos_mode/0`.

**Helper entrypoint**
- `test/router_test_bootstrap.erl`

