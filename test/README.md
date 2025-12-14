# Beamline Router Tests

This directory contains the Common Test suites for the Beamline Router.

## Test Levels

To balance speed and coverage, tests are categorized into three levels controlled by the `ROUTER_TEST_LEVEL` environment variable.

| Level | Env Variable | Description |
|---|---|---|
| **Fast** | `ROUTER_TEST_LEVEL=fast` | Unit tests, mock-based tests, and smoke tests. Fast to run (~1-2m). Suitable for local dev loop. |
| **Full** | `ROUTER_TEST_LEVEL=full` | Includes `fast` tests plus integration tests with dependencies (NATS, Postgres, etc.). Run in CI. |
| **Heavy** | `ROUTER_TEST_LEVEL=heavy` | Includes `full` tests plus stress tests, chaos engineering, soak tests, and benchmarks. Run on demand or nightly. |

## Running Tests

Helper scripts are provided in `../scripts/`:

```bash
# Run Fast tests
./scripts/ct-fast.sh

# Run Full tests
./scripts/ct-full.sh

# Run Heavy tests
./scripts/ct-heavy.sh
```

You can also run specific suites:
```bash
./scripts/ct-fast.sh router_result_consumer_SUITE
```

## Documentation

Detailed testing documentation, including strategies, contract definitions, and governance, can be found in `../docs/testing/`.

*   [Test Notes](../docs/testing/TEST_NOTES.md)
*   [Business Problems Map](../docs/testing/BUSINESS_PROBLEMS_MAP.md)
*   [Test Governance](../docs/testing/TEST_GOVERNANCE.md)
*   [Mock Discipline](../docs/testing/MOCK_DISCIPLINE.md)
*   [Test Maturity](../docs/testing/TEST_MATURITY.md)
