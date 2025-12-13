# Quality Gates for Full Tier

> Router Project Quality Gates Documentation
> Version: 1.0 | Last Updated: 2025-12-13

---

## Overview

Quality Gates are mandatory checks that enforce code quality standards for the Full Tier test suite. These gates **block CI pipeline** if any check fails.

## Quality Gates Definition

| Gate | Condition | Description | Enforcement |
|------|-----------|-------------|-------------|
| **Gate 1** | `suite_linter == ok` | All test suites pass structural linting | **HARD BLOCK** |
| **Gate 2** | `failed_tests == 0` | No test failures in full tier | **HARD BLOCK** |
| **Gate 3** | `unexpected_skips == 0` | No undocumented skips | **HARD BLOCK** |

---

## Gate 1: Suite Linter

The suite linter checks test suites for structural issues:

### Checks Performed
- **Missing callbacks**: `init_per_suite`, `end_per_suite`, `init_per_testcase`, `end_per_testcase`
- **Meck usage**: Number of `meck:new` calls vs baseline
- **ETS usage**: Number of `ets:new` calls vs baseline
- **Sleep usage**: Number of `timer:sleep` calls vs baseline
- **Meck in init_per_testcase**: Special check for meck setup location

### Running Manually
```bash
# Run suite linter
erl -noshell -pa test_support -s router_suite_linter run -s init stop

# Run in strict mode (fail on any baseline violation)
ROUTER_SUITE_LINTER_STRICT=true erl -noshell -pa test_support -s router_suite_linter run -s init stop
```

### Updating Baseline
If you intentionally add new meck/ets/sleep usage:
1. Run linter to see current counts
2. Update `test_support/router_suite_linter_baseline.eterm`
3. Document the change in PR

---

## Gate 2: Failed Tests

All tests in full tier must pass. Zero failures are accepted.

### What Counts as Failure
- Test assertion failures
- Test crashes (badmatch, badarg, etc.)
- Timeouts
- init_per_testcase failures
- end_per_testcase failures

### Fixing Failures
1. Run the failing test locally
2. Check CT logs in `_build/test/logs/`
3. Fix root cause (not symptoms)
4. Verify fix with full tier run

---

## Gate 3: Unexpected Skips

Skips must be documented. Undocumented skips fail the gate.

### Allowed Skip Patterns
These skip reasons are pre-approved and documented:

```
"meck not available"
"nats not available"
"docker not available"
"skip_reason:documented"
"NATS connection required"
"JetStream not available"
"integration test disabled"
"heavy tier only"
"disabled in CI"
```

### Adding New Allowed Skips
To allow a new skip pattern:

1. **Environment variable** (temporary):
   ```bash
   ROUTER_ALLOWED_SKIPS="my pattern,another pattern" make test-full
   ```

2. **Script update** (permanent):
   Edit `scripts/ci_full_quality_gates.sh` and add to `ALLOWED_SKIP_PATTERNS`

3. **Documentation**:
   Update `docs/testing/TEST_STATUS.md` with skip justification

---

## Running Quality Gates

### Full Tier with Quality Gates
```bash
# Run full tier tests with all quality gates
make test-full

# Or directly
./scripts/ct-full.sh
```

### Quality Gates Only (no tests)
```bash
# Check quality gates against existing logs
make test-full-quality-gates

# Or directly
./scripts/ci_full_quality_gates.sh
```

### Linter Only
```bash
./scripts/ci_full_quality_gates.sh --linter-only
```

---

## CI Integration

### GitHub Actions Example
```yaml
name: Full Tier CI
on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  full-tier:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '27.0'
          rebar3-version: '3.23.0'
      
      - name: Full Tier with Quality Gates
        run: make test-full
```

### Exit Codes
| Code | Meaning |
|------|---------|
| 0 | All quality gates passed |
| 1 | One or more gates failed |
| 2 | Script usage error |

---

## Troubleshooting

### Gate 1 Failing: Suite Linter
```
✗ Suite linter FAILED
```
**Solution**: Run linter manually to see issues:
```bash
erl -noshell -pa test_support -s router_suite_linter run -s init stop
```

### Gate 2 Failing: Test Failures
```
✗ Found N test failure(s)
```
**Solution**: Check CT logs:
```bash
ls -la _build/test/logs/
```

### Gate 3 Failing: Unexpected Skips
```
✗ Found N unexpected skip(s)
```
**Solution**: Either:
1. Fix the test so it doesn't skip
2. Add skip reason to allowed patterns
3. Document skip in TEST_STATUS.md

---

## Related Documentation

- [TEST_GOVERNANCE.md](./TEST_GOVERNANCE.md) - Test standards and policies
- [TEST_STATUS.md](./TEST_STATUS.md) - Test suite status overview
- [TEST_MATURITY.md](./TEST_MATURITY.md) - Test maturity model

---

## Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-12-13 | Platform Team | Initial quality gates documentation |
