# Test Governance & Standards

> Router Project Test Infrastructure Governance Document
> Version: 1.1 | Last Updated: 2025-12-13

---

## 1. Test Policy

### 1.1 Mandatory Requirements

| Requirement | Enforcement | CI Target |
|-------------|-------------|-----------|
| All PRs must pass `make test-fast` | **HARD BLOCK** | `ci-full-pipeline` |
| No new compiler warnings | **HARD BLOCK** | `quality-gates` |
| **Full Tier Quality Gates** (see below) | **HARD BLOCK** | `test-full` |
| CB changes require `ci-circuit-breaker-pipeline` | **HARD BLOCK** | `ci-circuit-breaker-pipeline` |
| Chaos tests: Docker mode in staging | **SOFT BLOCK** | `test-chaos-ci` |
| Mock discipline check | **WARN** (HARD in strict mode) | `test-discipline` |

### 1.1.1 Full Tier Quality Gates

| Gate | Condition | Description | Enforcement |
|------|-----------|-------------|-------------|
| Gate 1 | `suite_linter == ok` | All test suites pass structural linting | **HARD** |
| Gate 2 | `failed_tests == 0` | Zero test failures allowed in full tier | **HARD** |
| Gate 3 | `unexpected_skips == 0` | No undocumented skips | **HARD** |
| Gate 4 | `targeted_coverage >= 12%` | Coverage above threshold | **SOFT** (warning) |

See [QUALITY_GATES.md](./QUALITY_GATES.md) for detailed documentation.

### 1.2 Test Categories

| Category | Tag | When Required | CI Stage |
|----------|-----|---------------|----------|
| Unit | `@unit` | Every PR | `test-fast` |
| Integration | `@integration` | API/DB changes | `test-slow` |
| Circuit Breaker | `@circuit_breaker` | CB logic changes | `ci-circuit-breaker-pipeline` |
| Chaos | `@chaos` | Resilience changes | `test-chaos-ci` |
| Property-based | `@property` | Data structure changes | `test-slow` |

### 1.3 Coverage Requirements

| Component | Minimum Coverage | Target Coverage |
|-----------|------------------|-----------------|
| `router_circuit_breaker.erl` | 80% | 95% |
| `router_nats.erl` | 70% | 85% |
| `router_caf_adapter.erl` | 75% | 90% |
| Other production modules | 60% | 80% |

### 1.4 Quarantine Policy

- A quarantined suite is a documented exception: add it to `config/quarantine/quarantined_suites.txt` via the format `suite_name | owner | short_reason`. Skip empty lines and lines starting with `#` to keep the file human-readable.
- `scripts/ct-full.sh` loads that metadata, omits each quarantined suite from the full-tier execution plan, and prints a dedicated “Quarantined suites” block when `--list` is used so reviewers see the owner/reason without digging into code.
- The nightly/heavy runner (`scripts/ct-heavy.sh`) does not filter these suites but does log `Heavy tier includes quarantined suites: ...`, so teams still get signal on resolved flakes.
- Add a suite to quarantine only after repeated nondeterministic failures in `ROUTER_TEST_LEVEL=full` runs and follow up with the heavy tier when fixing. Remove the entry once the suite has passed several heavy runs and the owner confirms stability.

### 1.5 Quarantine lifecycle

- **Review cadence & ownership**: Every quarantine entry must be reviewed at least once per release cycle (or every calendar week, whichever is shorter). The `owner` field tags the team/controller responsible for that suite; they collect flakiness data and confirm the suite still requires quarantine before each review.
- **Exit criteria**: A quarantined suite can be removed only after the owning team documents at least three consecutive heavy-tier executions (`ROUTER_TEST_LEVEL=heavy`) that complete without the previously observed nondeterministic failure. The heavy-tier logs (CT reports, `_build/test/logs/.../ctlog.html`) serve as the evidence trail for each pass.
- **Visibility guardrails**: Track the total number of quarantine entries and the age of the oldest entry in the policy artifact or a lightweight dashboard so the team can spot growth. Escalate the list when it grows beyond five suites or when any entry is older than four weeks without a review update.
- **Tooling hooks (future-ready)**: Prepare for optional automation that warns when the quarantine list exceeds a configurable threshold and consider extending the metadata format with a timestamp column so reviewers know when each entry was last validated. These touches remain policy-level guidance until automation is implemented.

---

## 2. Library Standards

### 2.1 Meck (Mocking)

```erlang
%% ✅ REQUIRED: Always use router_nats_test_helper for NATS mocking
router_nats_test_helper:setup_mock(),
router_nats_test_helper:expect_publish_success(),
%% ... test code ...
router_nats_test_helper:teardown_mock().

%% ✅ REQUIRED: Cleanup in end_per_testcase
end_per_testcase(_TestCase, Config) ->
    router_nats_test_helper:teardown_mock(),
    Config.

%% ✅ REQUIRED: Check meck availability before use
case code:which(meck) of
    non_existing -> {skip, "meck not available"};
    _ -> ok
end.

%% ❌ FORBIDDEN: Direct meck:new without passthrough
meck:new(router_nats).  %% BAD - breaks other functions

%% ✅ CORRECT: Always use passthrough
meck:new(router_nats, [passthrough, no_link]).
```

### 2.2 Common Test

```erlang
%% ✅ REQUIRED: Export all CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% ✅ REQUIRED: Use groups for test organization
groups() ->
    [{unit_tests, [parallel], [test_a, test_b]},
     {integration_tests, [sequence], [test_c, test_d]}].

%% ✅ REQUIRED: Proper cleanup in end_per_* callbacks
end_per_testcase(_TestCase, Config) ->
    %% Cleanup mocks
    catch meck:unload(router_nats),
    %% Cleanup state
    catch router_circuit_breaker:reset_all(),
    Config.

%% ❌ FORBIDDEN: Skip without reason
{skip, undefined}.  %% BAD

%% ✅ CORRECT: Always provide skip reason
{skip, "NATS not available (set RUN_INTEGRATION_TESTS=true)"}.
```

### 2.3 Assertions

```erlang
%% ✅ REQUIRED: Use ?assert macros with descriptive messages
?assertEqual(open, State, "Circuit breaker MUST open after 5 failures"),
?assert(Latency < 1000, io_lib:format("Latency ~p must be < 1000ms", [Latency])).

%% ❌ FORBIDDEN: Bare assertions without context
?assert(X).  %% BAD - no context on failure

%% ✅ REQUIRED: Use ct:fail for business logic violations
case Result of
    {error, Reason} ->
        ct:fail({business_logic_violation, expected_success, Reason});
    _ -> ok
end.
```

### 2.4 TestOps Helpers

```erlang
%% ✅ REQUIRED: Use router_testops_helper for chaos tests
init_per_suite(Config) ->
    Config1 = router_testops_helper:init_chaos_mode(Config),
    router_testops_helper:ci_fail_on_mock_mode(),
    Config1.

%% ✅ REQUIRED: Use deterministic seeds for reproducibility
init_per_testcase(TestCase, Config) ->
    router_testops_helper:setup_deterministic_seed(TestCase, Config).

%% ✅ RECOMMENDED: Check mock discipline
end_per_testcase(TestCase, Config) ->
    router_testops_helper:check_mock_discipline(TestCase, Config),
    Config.
```

---

## 3. Test File Organization

```
test/
├── *_SUITE.erl           # Test suites (CT format)
├── *_test_helper.erl     # Reusable test utilities
├── TEST_GOVERNANCE.md    # This document
├── TEST_MATURITY.md      # Maturity model
├── MOCK_DISCIPLINE.md    # Mock usage guidelines
├── BUSINESS_PROBLEMS_MAP.md  # Known issues tracker
└── TEST_NOTES.md         # General test notes
```

### 3.1 Suite Naming Convention

| Pattern | Purpose | Example |
|---------|---------|---------|
| `router_*_SUITE.erl` | Unit tests | `router_circuit_breaker_SUITE.erl` |
| `router_*_integration_SUITE.erl` | Integration tests | `router_nats_integration_SUITE.erl` |
| `router_*_chaos_SUITE.erl` | Chaos tests | `router_intake_chaos_SUITE.erl` |
| `router_*_prop_SUITE.erl` | Property tests | `router_policy_store_prop_SUITE.erl` |

### 3.2 Helper Naming Convention

| Pattern | Purpose | Example |
|---------|---------|---------|
| `router_*_test_helper.erl` | Domain-specific helpers | `router_nats_test_helper.erl` |
| `router_testops_helper.erl` | Infrastructure helpers | `router_testops_helper.erl` |

---

## 4. CI/CD Integration

### 4.1 Required CI Targets

```makefile
# MUST pass for every PR
make quality-gates    # Compile + dialyzer + no warnings
make test-fast        # Unit tests

# MUST pass for specific changes
make ci-circuit-breaker-pipeline  # CB changes
make ci-testops-pipeline          # Test infra changes
make test-chaos-ci                # Resilience changes
```

### 4.2 CI Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `STRICT_MOCK_DISCIPLINE` | Fail on basic-only mocks | `false` |
| `CHAOS_REQUIRE_DOCKER` | Require Docker for chaos | `false` |
| `CHAOS_MOCK_ALLOWED` | Allow mock fallback | `true` |
| `TEST_SEED` | Deterministic seed | auto |
| `RUN_CHAOS_TESTS` | Enable chaos tests | `false` |

### 4.3 GitHub Actions Example

```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26.0'
          rebar3-version: '3.22.1'
      
      - name: Quality Gates
        run: make quality-gates
      
      - name: Fast Tests
        run: make test-fast
      
      - name: CB Pipeline (if changed)
        if: contains(github.event.commits.*.modified, 'router_circuit_breaker')
        run: make ci-circuit-breaker-pipeline
      
      - name: TestOps Pipeline
        run: make ci-testops-pipeline
```

---

## 5. Review Checklist

### 5.1 PR Review Checklist

- [ ] All tests pass locally (`make test-fast`)
- [ ] No new compiler warnings (`make quality-gates`)
- [ ] Mock cleanup in `end_per_testcase`
- [ ] Assertions have descriptive messages
- [ ] New tests follow naming conventions
- [ ] Coverage not decreased
- [ ] If CB changes: `make ci-circuit-breaker-pipeline` passes
- [ ] If chaos changes: `make test-chaos-ci` passes

### 5.2 Test Review Checklist

- [ ] Test name describes behavior, not implementation
- [ ] Test covers happy path AND error cases
- [ ] Mocks are properly scoped and cleaned
- [ ] No flaky patterns (unseeded rand, timing assumptions)
- [ ] Documentation for complex test scenarios

---

## 6. Ownership

| Area | Owner | Escalation |
|------|-------|------------|
| Test Governance | Platform Team Lead | CTO |
| Test Infrastructure | Platform Team | Platform Lead |
| CI Pipeline | DevOps Team | DevOps Lead |
| Test Standards | All Engineers | Platform Lead |

---

## 7. Exceptions Process

1. **Request**: Create issue with `test-exception` label
2. **Review**: Platform Team Lead reviews within 2 business days
3. **Approve/Deny**: Decision documented in issue
4. **Track**: Approved exceptions tracked in `TEST_EXCEPTIONS.md`

---

## 8. Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-12-07 | Platform Team | Initial governance document |
