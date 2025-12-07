# Test Classification and Marking

**Version**: 1.1  
**Date**: 2025-11-30

## Purpose

This document defines test classification (fast vs slow) and marking strategy for Router test suites. Tests are categorized to enable:
- Fast CI runs (smoke/contract tests only)
- Comprehensive test runs (all tests including load/E2E/property)
- Selective test execution based on test type

## Test Category Tags

Test suites are marked with machine-readable tags using `@test_category` in module comments:

```erlang
%% @doc Common Test Suite for Router Core
%% Tests policy parsing, basic decision, errors (CP1)
%% @test_category cp1_smoke, fast
-module(router_core_SUITE).
```

### Tagging Format for Tooling

**Format**: `%% @test_category tag1, tag2, tag3`

**Rules**:
- Must be in a comment line starting with `%%`
- Must appear in the **upper part of the file** (within first 50 lines recommended)
- Tags are comma-separated
- Tags may have spaces around commas (normalized during parsing)
- Tag names: lowercase letters, numbers, underscores, hyphens only
- Multiple tags allowed per suite
- Case-insensitive matching (tags are normalized to lowercase)

**Example**:
```erlang
%% @doc Common Test Suite for Router Core
%% Tests policy parsing, basic decision, errors (CP1)
%% @test_category cp1_smoke, fast
-module(router_core_SUITE).
```

**Parsing**: The `list_tests_by_tag.sh` script parses this format and extracts suites by tag.

### Available Tags

- **`cp1_smoke`**: CP1 baseline smoke tests (7 suites)
- **`fast`**: Fast tests (smoke/contract, < 30s) - includes all CP1 smoke tests
- **`slow`**: Slow tests (load/JetStream E2E/property, > 5min)
- **`cp2_plus`**: CP2+ feature tests (idempotency, tenant validation, admin gRPC)
- **`jetstream`**: JetStream E2E tests (durable subscriptions, ACK/NAK, redelivery)
- **`fault_injection`**: Fault injection tests (simulating NATS/JetStream failures)
- **`property`**: Property-based tests (PropEr)
- **`load`**: Load tests (1k-5k operations)
- **`integration`**: Integration tests (may be fast or slow depending on scope)

### Tag Usage

- **Multiple tags**: A test suite can have multiple tags (e.g., `cp1_smoke, fast`)
- **Hierarchy**: `cp1_smoke` implies `fast` (all CP1 smoke tests are fast)
- **Documentation**: Tags serve as machine-readable documentation and can be used for future automation

## Test Categories

### Fast Tests (Smoke/Contract) - CP1 CI

**Duration**: < 30 seconds (excluding compilation)  
**Purpose**: Quick validation of core functionality, contracts, and basic integration

**Test Suites**:
- `router_core_SUITE.erl` - Core routing decisions, policy parsing
- `router_e2e_smoke_SUITE.erl` - End-to-end smoke test (RBAC → Quota → Rate Limiting)
- `router_rbac_SUITE.erl` - Role-Based Access Control
- `router_policy_enforcement_SUITE.erl` - RBAC + Quota + Rate Limiting integration
- `router_decider_SUITE.erl` - Decision engine (weighted routing, fallback)
- `router_policy_store_SUITE.erl` - Policy store operations (basic)
- `router_error_SUITE.erl` - Error mapping and gRPC error codes
- `router_grpc_SUITE.erl` - gRPC Router.Decide service (basic)
- `router_grpc_integration_SUITE.erl` - gRPC integration tests (basic)
- `router_caf_adapter_unit_SUITE.erl` - CAF adapter unit tests
- `router_core_telemetry_contract_SUITE.erl` - Telemetry contract validation
- `router_secrets_logging_SUITE.erl` - Secrets masking validation
- `router_nats_contract_validation_SUITE.erl` - NATS contract validation (headers)
- `router_tenant_multitenant_smoke_SUITE.erl` - Multi-tenant smoke tests (tenant isolation, validation, metrics/logs)

**Total**: 14 test suites

### Slow Tests (Load/JetStream E2E/Property) - Full CI

**Duration**: > 5 minutes (some suites)  
**Purpose**: Comprehensive validation including performance, JetStream features, property-based testing  
**Tag**: `@test_category slow` (with additional tags: `jetstream`, `property`, `load`, `cp2_plus`)

**Test Suites**:

#### JetStream E2E Tests
- `router_jetstream_e2e_SUITE.erl` - Heavy JetStream E2E tests (durable subscriptions, ACK/NAK, redelivery)
- `router_delivery_count_tracking_SUITE.erl` - JetStream delivery count tracking
- `router_result_consumer_SUITE.erl` - Result consumer (may use JetStream features)
- `router_caf_adapter_SUITE.erl` - CAF adapter (may use JetStream features)
- `router_caf_adapter_enhanced_SUITE.erl` - Enhanced CAF adapter features
- `router_nats_subscriber_caf_SUITE.erl` - NATS/JetStream integration
- `router_jetstream_fault_injection_SUITE.erl` - Fault injection tests for JetStream (intermittent errors, delays, MaxDeliver exhaustion)

#### Property-Based Tests
- `router_decider_prop_SUITE.erl` - Property-based tests for decision engine
- `router_policy_store_prop_SUITE.erl` - Property-based tests for policy store
- `router_normalize_boolean_prop_SUITE.erl` - Property-based tests for boolean normalization
- `router_options_merge_prop_SUITE.erl` - Property-based tests for options merging
- `router_ets_consistency_prop_SUITE.erl` - Property-based tests for ETS consistency (table integrity, no orphaned entries, cleanup)

#### Load Tests
- `router_policy_store_load_SUITE.erl` - Load tests (1k-5k operations)

#### CP2+ Feature Tests
- `router_idempotency_SUITE.erl` - Idempotency layer (CP2+)
- `router_tenant_allowlist_SUITE.erl` - Tenant validation (CP2+)

#### Advanced Integration Tests
- `router_policy_store_fault_tolerance_SUITE.erl` - Fault tolerance (heir mechanism, recovery)
- `router_admin_grpc_integration_SUITE.erl` - Admin gRPC integration (CP2+)
- `router_admin_grpc_concurrency_SUITE.erl` - Admin gRPC concurrency tests (CP2+)
- `router_assignment_SUITE.erl` - Assignment handling
- `router_sticky_store_SUITE.erl` - Sticky session store
- `router_policy_SUITE.erl` - Policy management
- `router_policy_validator_SUITE.erl` - Policy validation
- `router_ets_guard_SUITE.erl` - ETS guard tests
- `router_error_status_SUITE.erl` - Error status handling

**Total**: 20+ test suites

## Test Marking Strategy

### By Test Category Tags (Primary)

**Machine-readable tags** in module comments:

```erlang
%% @doc Common Test Suite for Router Core
%% Tests policy parsing, basic decision, errors (CP1)
%% @test_category cp1_smoke, fast
-module(router_core_SUITE).
```

**Tag values**:
- `cp1_smoke` - CP1 baseline smoke tests (7 suites)
- `fast` - Fast tests (smoke/contract, < 30s) - includes all CP1 smoke tests
- `slow` - Slow tests (load/JetStream E2E/property, > 5min)
- `cp2_plus` - CP2+ feature tests
- `jetstream` - JetStream E2E tests
- `property` - Property-based tests
- `load` - Load tests
- `integration` - Integration tests

**Usage**:
- Multiple tags per suite (comma-separated)
- Tags serve as documentation and can be used for future automation
- Scripts currently use explicit lists but can be enhanced to read tags

### By Naming Convention (Secondary)

**Fast Tests**: No special suffix (default)  
**Slow Tests**: Suffixes indicate test type:
- `*_prop_SUITE.erl` - Property-based tests (`@test_category property, slow`)
- `*_load_SUITE.erl` - Load tests (`@test_category load, slow`)
- `*_e2e_SUITE.erl` - End-to-end tests (if heavy) (`@test_category jetstream, slow`)
- `*_integration_SUITE.erl` - Integration tests (if heavy) (`@test_category integration, slow`)

### By Test Suite Metadata

Each test suite can define metadata in `suite/0` function:

```erlang
suite() ->
    [
        {timetrap, {seconds, 30}},  %% Fast tests: 30s timeout
        {ct_hooks, []}
    ].
```

For slow tests:
```erlang
suite() ->
    [
        {timetrap, {minutes, 5}},  %% Slow tests: 5min timeout
        {ct_hooks, []}
    ].
```

## Test Execution Scripts

### Fast Tests (CP1 CI)

**Script**: `scripts/test_fast.sh` / `scripts/test_fast.ps1`  
**Makefile target**: `test-fast`  
**Usage**: `make test-fast` or `./scripts/test_fast.sh`

**Runs**: All fast test suites (smoke/contract)

### Slow Tests (Full CI)

**Script**: `scripts/test_slow.sh` / `scripts/test_slow.ps1`  
**Makefile target**: `test-slow`  
**Usage**: `make test-slow` or `./scripts/test_slow.sh`

**Runs**: All slow test suites (load/JetStream E2E/property)

### All Tests

**Script**: `scripts/test_all.sh` / `scripts/test_all.ps1`  
**Makefile target**: `test-all`  
**Usage**: `make test-all` or `./scripts/test_all.sh`

**Runs**: All test suites (fast + slow)

## CI/CD Integration

### CP1 CI (Fast Tests Only)

CP1 validation should run only fast tests:

```yaml
- name: Run Fast Tests (CP1)
  working-directory: apps/otp/router
  run: make test-fast
```

### Full CI (All Tests)

Full CI runs all tests (can be scheduled or run on demand):

```yaml
- name: Run All Tests
  working-directory: apps/otp/router
  run: make test-all
```

### Nightly CI (Slow Tests Only)

Nightly CI can run slow tests separately:

```yaml
- name: Run Slow Tests (Nightly)
  working-directory: apps/otp/router
  run: make test-slow
```

## Test Discovery by Tags

### Automatic Test Discovery

The `scripts/list_tests_by_tag.sh` utility can automatically discover test suites by tags:

**Usage**:
```bash
# List all fast test suites
./scripts/list_tests_by_tag.sh fast

# List all CP1 smoke test suites
./scripts/list_tests_by_tag.sh cp1_smoke

# List with full paths
./scripts/list_tests_by_tag.sh slow --full-path

# List with .erl extension
./scripts/list_tests_by_tag.sh fast --with-ext
```

**Output format** (default):
- Suite names without path or extension (as `rebar3 ct` expects)
- Example: `router_core_SUITE`

**Options**:
- `--full-path`: Output relative paths (e.g., `test/router_core_SUITE.erl`)
- `--with-ext`: Include `.erl` extension
- `--help`: Show usage information

**Integration with test scripts**:
- Test scripts (`test_fast.sh`, `test_cp1_smoke.sh`) can optionally use autodiscovery
- Set `ROUTER_TEST_AUTODISCOVERY=1` to enable (see script documentation)

**Examples**:
```bash
# Get list of fast suites
FAST_SUITES=$(./scripts/list_tests_by_tag.sh fast)

# Use with rebar3
rebar3 ct --suite $(echo $FAST_SUITES | tr '\n' ' ')
```

## Test Discovery Tool

### list_tests_by_tag.sh

**Location**: `apps/otp/router/scripts/list_tests_by_tag.sh`

**Purpose**: Automatically discover test suites by tag

**Usage**:
```bash
cd apps/otp/router
./scripts/list_tests_by_tag.sh <tag_expr>
```

**Tag Expression Syntax**:
- **Single tag**: `fast` - All suites with the `fast` tag
- **Include + Exclude**: `fast,!cp1_smoke` - Suites with `fast` tag but NOT `cp1_smoke`
- **Multiple includes (intersection)**: `cp1_smoke,cp2_plus` - Suites with BOTH `cp1_smoke` AND `cp2_plus` tags
- **Multiple excludes**: `fast,!cp1_smoke,!cp2_plus` - Suites with `fast` but NOT `cp1_smoke` and NOT `cp2_plus`

**Examples**:
```bash
# List all fast test suites
./scripts/list_tests_by_tag.sh fast

# List all CP1 smoke test suites
./scripts/list_tests_by_tag.sh cp1_smoke

# List all slow test suites
./scripts/list_tests_by_tag.sh slow

# List fast tests but NOT CP1 smoke (6 suites: fast tests excluding cp1_smoke)
./scripts/list_tests_by_tag.sh fast,!cp1_smoke

# List suites with both cp1_smoke and cp2_plus tags (intersection)
./scripts/list_tests_by_tag.sh cp1_smoke,cp2_plus

# List fast tests excluding both cp1_smoke and cp2_plus
./scripts/list_tests_by_tag.sh fast,!cp1_smoke,!cp2_plus
```

**Use Cases**:
- **Run all fast tests except CP1 smoke**: `./scripts/list_tests_by_tag.sh fast,!cp1_smoke`
- **List only CP1 smoke tests**: `./scripts/list_tests_by_tag.sh cp1_smoke`
- **Find suites with multiple tags**: `./scripts/list_tests_by_tag.sh cp1_smoke,fast` (suites with both tags)

**Output**: List of suite names (without `.erl` extension), one per line, suitable for `rebar3 ct --suite`:
```
router_core_SUITE
router_e2e_smoke_SUITE
router_rbac_SUITE
...
```

**Integration with Test Scripts**:
- Can be used as alternative to explicit lists in `test_fast.sh`, `test_cp1_smoke.sh`
- Set `ROUTER_TEST_AUTODISCOVERY=1` to enable autodiscovery (see script documentation)

**Exit Codes**:
- `0` - Success (suites found)
- `1` - Invalid arguments
- `2` - No suites found with the specified tag

**Testing the Tool**:
```bash
# Run test script to verify autodiscovery works
./scripts/test_list_tests_by_tag.sh
```

**Implementation Details**:
- Parses first 50 lines of each `*_SUITE.erl` file
- Looks for `%% @test_category` comment lines
- Extracts comma-separated tags
- Case-insensitive tag matching
- Returns suite names without `.erl` extension (compatible with `rebar3 ct --suite`)

## Test Suite List Reference

### Fast Test Suites (12 suites)

All marked with `@test_category fast`:

1. `router_core_SUITE.erl` - `@test_category cp1_smoke, fast`
2. `router_e2e_smoke_SUITE.erl` - `@test_category cp1_smoke, fast`
3. `router_rbac_SUITE.erl` - `@test_category cp1_smoke, fast`
4. `router_policy_enforcement_SUITE.erl` - `@test_category cp1_smoke, fast`
5. `router_decider_SUITE.erl` - `@test_category cp1_smoke, fast`
6. `router_policy_store_SUITE.erl` - `@test_category cp1_smoke, fast`
7. `router_error_SUITE.erl` - `@test_category cp1_smoke, fast`
8. `router_grpc_SUITE.erl` - `@test_category fast`
9. `router_grpc_integration_SUITE.erl` - `@test_category fast`
10. `router_caf_adapter_unit_SUITE.erl` - `@test_category fast`
11. `router_core_telemetry_contract_SUITE.erl` - `@test_category fast`
12. `router_secrets_logging_SUITE.erl` - `@test_category fast`
13. `router_nats_contract_validation_SUITE.erl` - `@test_category fast`

### Slow Test Suites (20+ suites)

#### JetStream E2E (7 suites)
1. `router_jetstream_e2e_SUITE.erl`
2. `router_delivery_count_tracking_SUITE.erl`
3. `router_result_consumer_SUITE.erl`
4. `router_caf_adapter_SUITE.erl`
5. `router_caf_adapter_enhanced_SUITE.erl`
6. `router_nats_subscriber_caf_SUITE.erl`
7. `router_jetstream_fault_injection_SUITE.erl` - `@test_category slow, jetstream, fault_injection`

#### Property-Based (4 suites)
7. `router_decider_prop_SUITE.erl`
8. `router_policy_store_prop_SUITE.erl`
9. `router_normalize_boolean_prop_SUITE.erl`
10. `router_options_merge_prop_SUITE.erl`

#### Load (1 suite)
11. `router_policy_store_load_SUITE.erl`

#### CP2+ Features (2 suites)
12. `router_idempotency_SUITE.erl`
13. `router_tenant_allowlist_SUITE.erl`

#### Advanced Integration (7+ suites)
14. `router_policy_store_fault_tolerance_SUITE.erl`
15. `router_admin_grpc_integration_SUITE.erl`
16. `router_admin_grpc_concurrency_SUITE.erl`
17. `router_assignment_SUITE.erl`
18. `router_sticky_store_SUITE.erl`
19. `router_policy_SUITE.erl`
20. `router_policy_validator_SUITE.erl`
21. `router_ets_guard_SUITE.erl`
22. `router_error_status_SUITE.erl`

## Migration Notes

**From CP1 Smoke to Fast Tests**:
- CP1 smoke tests are a subset of fast tests
- Fast tests include additional contract/unit tests
- CP1 smoke tests remain available via `make test-cp1-smoke`

## References

- `docs/CP1_TESTING.md`: CP1 baseline testing guide
- `docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`: JetStream fault injection tests documentation
- `scripts/test_cp1_smoke.sh`: CP1 smoke test runner
- `scripts/test_fast.sh`: Fast test runner (new)
- `scripts/test_slow.sh`: Slow test runner (new)
- `.github/workflows/ci.yml`: CI workflow configuration

