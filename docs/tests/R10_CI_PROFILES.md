# R10 CI Profiles and Test Strategy

## Overview

R10 E2E tests support two profiles for different testing scenarios:
- **`ci`** - Fast, lightweight tests for CI/CD pipelines (default)
- **`heavy`** - Extended load tests for nightly/extended validation

## Configuration

### ct.config

Profiles are configured in `test/ct.config`:

```erlang
[
  {ct,
    [
      %% R10 Load Parameters (defaults for CI)
      {r10_load_clients, 10},              % Number of parallel clients
      {r10_requests_per_client, 20},      % Requests per client
      {r10_failure_type, error},          % error | timeout | delay
      {r10_profile, ci}                   % ci | heavy
    ]
  }
].
```

### Profile-Specific Overrides

Profile-specific parameters are handled in `router_r10_client_utils:get_r10_config/0`:

- **`ci` profile** (default):
  - `r10_load_clients`: 10
  - `r10_requests_per_client`: 20
  - Total requests: 200 per scenario
  - Timeout: ~30 seconds per scenario

- **`heavy` profile**:
  - `r10_load_clients`: 50
  - `r10_requests_per_client`: 100
  - Total requests: 5000 per scenario
  - Timeout: ~60-120 seconds per scenario

## Usage

### Canonical Test Commands

**CRITICAL**: These are the officially supported commands for running R10 tests.

#### Unit Tests (Circuit Breaker)

```bash
# Run unit tests only
rebar3 ct --suite test/router_circuit_breaker_SUITE
```

#### E2E Tests (R10 Scenarios)

```bash
# Default (ci profile)
rebar3 ct --suite test/router_publish_failure_e2e_SUITE

# Explicit ci profile
R10_PROFILE=ci rebar3 ct --suite test/router_publish_failure_e2e_SUITE

# Heavy profile (nightly/extended)
R10_PROFILE=heavy rebar3 ct --suite test/router_publish_failure_e2e_SUITE
```

#### Running All Router Tests

```bash
# Run all tests in router test directory
rebar3 ct --dir apps/otp/router/test
```

**Note**: Combined execution of both suites via `rebar3 ct --suite test/router_circuit_breaker_SUITE test/router_publish_failure_e2e_SUITE` is not currently standardized. Use separate commands or `--dir` approach.

### Overriding Individual Parameters

```bash
# Custom load parameters
R10_LOAD_CLIENTS=50 R10_REQUESTS_PER_CLIENT=100 \
  rebar3 ct --suite test/router_publish_failure_e2e_SUITE
```

## CI/CD Integration

### Main CI Pipeline

**Location**: PR/merge pipelines

**Configuration**:
- Profile: `ci` (default)
- Suites: `router_circuit_breaker_SUITE` + `router_publish_failure_e2e_SUITE`
- Timeout: ~5-10 minutes total

**Purpose**: Fast validation that R10 functionality works correctly

### Nightly/Extended Pipeline

**Location**: Separate workflow/job (not blocking PRs)

**Configuration**:
- Profile: `heavy`
- Suites: `router_circuit_breaker_SUITE` + `router_publish_failure_e2e_SUITE`
- Timeout: ~20-30 minutes total

**Purpose**: Extended load testing and stress validation

## Test Strategy

### Unit Tests (`router_circuit_breaker_SUITE`)

- **Profile**: Not applicable (unit tests don't use profiles)
- **Purpose**: Fast validation of circuit breaker logic
- **CI**: Always run in main pipeline

### E2E Tests (`router_publish_failure_e2e_SUITE`)

- **Profile**: `ci` (main pipeline) or `heavy` (nightly)
- **Purpose**: End-to-end validation with realistic load
- **CI**: 
  - Main pipeline: `ci` profile
  - Nightly pipeline: `heavy` profile

## Property-Based Tests

### Overview

R10 includes a lightweight property-based test suite (`router_circuit_breaker_prop_SUITE.erl`) that validates state machine invariants through random sequences.

**Purpose**: Catch logic errors in circuit breaker state transitions that might not be caught by deterministic tests.

### Configuration

- **Profile**: Not applicable (property tests don't use load profiles)
- **Sequences**: 20 random sequences per run
- **Execution Time**: < 0.5 seconds (light mode)
- **CI Integration**: Nightly pipeline (`router-r10-property` job)

### Running Property Tests

```bash
# Run property tests
rebar3 ct --suite test/router_circuit_breaker_prop_SUITE
```

### Test Coverage

Property tests verify:
- No transition to `open` from pure successes
- Transition to `open` after failure threshold
- `should_allow` consistency with state
- State machine invariants hold across random sequences

### CI Integration

**Job**: `router-r10-property`
- **Trigger**: Nightly/scheduled runs
- **Status**: Non-blocking (`allow_failure: true`)
- **Timeout**: 120 seconds
- **Purpose**: Regression detection for state machine logic

## Notes

- Profile selection is done via environment variable `R10_PROFILE` or `ct.config`
- Individual parameters can be overridden via environment variables
- Profile-specific overrides are handled in `router_r10_client_utils:get_r10_config/0`
- All profiles use the same test scenarios, only load parameters differ
- Property tests are separate from E2E tests and don't use profiles

