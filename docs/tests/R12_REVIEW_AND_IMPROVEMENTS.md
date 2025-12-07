# R12: Network Partition Scenarios - Review and Improvements

**Date**: 2025-11-30  
**Status**: Review Report  
**Purpose**: Comprehensive review of R12 implementation with improvement recommendations

## Executive Summary

**Overall Status**: ✅ **GOOD** - Core implementation complete, but needs alignment with R8/R10/R13 patterns

**Key Findings**:
- ✅ All required files exist and are properly structured
- ✅ Test coverage is comprehensive (21 test cases)
- ⚠️ **Missing**: Metrics verification (aligned with R13)
- ⚠️ **Missing**: Contract invariant checks (aligned with R8)
- ⚠️ **Missing**: Helper function usage (aligned with R8/R10/R13)
- ⚠️ **Inconsistency**: Dual fault injection mechanisms (router_network_partition + router_nats_fault_injection)

## File Existence Verification

### ✅ All Required Files Exist

| File | Status | Notes |
|------|--------|-------|
| `R12_NETWORK_PARTITION_SCENARIOS.md` | ✅ Exists | Comprehensive specification |
| `R12_README.md` | ✅ Exists | Quick start guide |
| `R12_LOGS_AND_METRICS.md` | ✅ Exists | Log/metric specifications |
| `R12_RESULTS_REPORT_TEMPLATE.md` | ✅ Exists | Report template |
| `R12_RESULTS_REPORT_EXAMPLE.md` | ✅ Exists | Example report |
| `router_network_partition_SUITE.erl` | ✅ Exists | Test suite (21 tests) |
| `scripts/r12_network_partition_fault_injection.sh` | ✅ Exists | Bash script |
| `scripts/r12_network_partition_fault_injection.ps1` | ✅ Exists | PowerShell script |

**Total**: 8 files, all present ✅

## Structure and Quality Review

### 1. Specification (R12_NETWORK_PARTITION_SCENARIOS.md)

#### ✅ Strengths

1. **Comprehensive Coverage**: All scenarios from requirements are documented
2. **Clear Structure**: Well-organized with single-instance and multi-instance sections
3. **Detailed Expected Behavior**: Each scenario has clear expected behavior points
4. **Reproduction Steps**: Both mock and real mode instructions provided
5. **Metrics Documentation**: Expected metrics listed for each scenario

#### ⚠️ Areas for Improvement

1. **Missing Consistency Check**: No document like `R10_CONSISTENCY_CHECK.md` to verify alignment with R8/R10/R13
2. **No Traceability Matrix**: Missing traceability to requirements (like `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`)
3. **No Pattern Catalog**: Missing formal pattern catalog (like `TRIPLE_FAULT_PATTERNS_CATALOG.md`)

**Recommendation**: Create `R12_CONSISTENCY_CHECK.md` to verify alignment with R8/R10/R13 patterns.

### 2. Test Suite (router_network_partition_SUITE.erl)

#### ✅ Strengths

1. **Comprehensive Coverage**: 21 test cases covering all scenarios
2. **Proper Structure**: Well-organized test groups (single-instance, multi-instance, service-broker, flapping)
3. **Fault Injection Integration**: Uses `router_nats_fault_injection` (consistent with R8/R10)
4. **Resource Leak Checks**: Tests verify memory and process growth
5. **Fail-Open Verification**: Tests verify process liveness

#### ⚠️ Critical Issues

##### Issue 1: Missing Metrics Verification

**Problem**: Tests don't verify metrics explicitly (unlike R8/R10/R13)

**Current State**:
```erlang
%% Verify: Connection errors detected (via fault injection)
%% Metrics would reflect partition (in real scenario)
```

**Expected State** (aligned with R8/R10/R13):
```erlang
%% Get initial metrics
InitialMetrics = get_metrics_snapshot(),

%% Enable fault
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
timer:sleep(5000),

%% Get final metrics
FinalMetrics = get_metrics_snapshot(),

%% Verify metrics reflect partition
InitialConnectionFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
FinalConnectionFailures = maps:get(router_nats_connection_failures_total, FinalMetrics, 0),
true = FinalConnectionFailures > InitialConnectionFailures,
```

**Impact**: Tests don't verify that metrics actually reflect partition state.

**Recommendation**: Add metrics verification to all tests (similar to R8/R10/R13).

##### Issue 2: Missing Contract Invariant Checks

**Problem**: Tests don't verify contract invariants (MaxDeliver, redelivery limits, delivery count)

**Current State**: Tests only check `is_process_alive/1`

**Expected State** (aligned with R8):
```erlang
%% Verify contract invariants
verify_contract_invariants(InitialMetrics, FinalMetrics, #{
    expected_maxdeliver_exhaustion => 0,
    max_redelivery => 50,
    faults_injected => true
}),
```

**Impact**: Tests don't verify MaxDeliver semantics, redelivery limits, or delivery count tracking.

**Recommendation**: Add contract invariant verification (reuse from `router_triple_fault_contract_SUITE.erl`).

##### Issue 3: Missing Helper Function Usage

**Problem**: Tests don't use `router_fault_injection_helpers` (unlike R8/R10/R13)

**Current State**: Manual verification in each test

**Expected State** (aligned with R8/R10/R13):
```erlang
%% Use helper functions
{ok, ResilienceDetails} = router_fault_injection_helpers:verify_resilience([]),
{ok, MetricsDetails} = router_fault_injection_helpers:verify_observability_metrics(
    InitialMetrics, FinalMetrics, false
),
```

**Impact**: Code duplication, inconsistent verification patterns.

**Recommendation**: Refactor tests to use `router_fault_injection_helpers`.

##### Issue 4: Dual Fault Injection Mechanisms

**Problem**: Tests use both `router_network_partition:create_partition/2` and `router_nats_fault_injection:enable_fault/2`

**Current State**:
```erlang
%% Some tests use router_network_partition
{ok, PartitionId} = router_network_partition:create_partition(single_instance, PartitionConfig),

%% Other tests use router_nats_fault_injection directly
router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
```

**Impact**: Inconsistent approach, unclear which mechanism to use when.

**Recommendation**: 
- **Option A**: Use only `router_nats_fault_injection` (consistent with R8/R10/R13)
- **Option B**: Use `router_network_partition` as high-level wrapper, but ensure it uses `router_nats_fault_injection` internally
- **Option C**: Document when to use each mechanism

**Preferred**: Option A (use only `router_nats_fault_injection` for consistency).

##### Issue 5: Missing Log Verification

**Problem**: Tests don't verify that expected logs are emitted

**Current State**: Comments like "Logs would be emitted (in real scenario)"

**Expected State** (aligned with R8/R10/R13):
```erlang
%% Verify logs
{ok, Logs} = router_fault_injection_helpers:verify_observability_logs(
    ExpectedLogs, AllLogs
),
```

**Impact**: Tests don't verify logging behavior.

**Recommendation**: Add log verification (if log capture infrastructure exists).

##### Issue 6: Missing Test Helper Functions

**Problem**: No helper functions for common patterns (metrics snapshots, contract verification)

**Current State**: Each test manually implements verification

**Expected State** (aligned with R8/R10/R13):
```erlang
%% Helper functions
get_metrics_snapshot() -> ...
verify_contract_invariants(InitialMetrics, FinalMetrics, ExpectedBehavior) -> ...
run_fault_injection_lifecycle(Faults, Action, RecoveryWaitMs) -> ...
```

**Impact**: Code duplication, inconsistent verification.

**Recommendation**: Add helper functions to test suite (or reuse from `router_triple_fault_contract_SUITE.erl`).

#### 📊 Comparison with R8/R10/R13 Patterns

| Feature | R8 (Triple Fault) | R10 (Publish Failure) | R13 (Metrics) | R12 (Network Partition) | Status |
|---------|-------------------|----------------------|---------------|-------------------------|--------|
| Metrics verification | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ⚠️ Missing |
| Contract invariants | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ⚠️ Missing |
| Helper functions | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ⚠️ Missing |
| Log verification | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ⚠️ Missing |
| Fault injection mechanism | ✅ router_nats_fault_injection | ✅ router_nats_fault_injection | ✅ router_nats_fault_injection | ⚠️ Mixed | ⚠️ Inconsistent |
| Test structure | ✅ Groups + helpers | ✅ Groups + helpers | ✅ Groups + helpers | ✅ Groups, ❌ No helpers | ⚠️ Partial |

### 3. Scripts (r12_network_partition_fault_injection.sh / .ps1)

#### ✅ Strengths

1. **Cross-Platform**: Both Bash and PowerShell versions exist
2. **Complete API**: Create, remove, list, status, heal, flapping commands
3. **Mock and Real Modes**: Support for both modes
4. **Good Documentation**: Help text and examples provided

#### ⚠️ Areas for Improvement

1. **No Integration with router_nats_fault_injection**: Scripts don't integrate with Erlang fault injection mechanism
2. **Real Mode Limitations**: Real mode (iptables/tc) requires root privileges, may not work in CI
3. **No Metrics Collection**: Scripts don't collect or verify metrics

**Recommendation**: 
- Add integration with `router_nats_fault_injection` via Erlang shell commands
- Document real mode limitations clearly
- Consider adding metrics collection/verification scripts

### 4. Documentation

#### ✅ Strengths

1. **Comprehensive**: All required documentation exists
2. **Clear Structure**: Well-organized with clear sections
3. **Examples**: Good examples in reports and README

#### ⚠️ Areas for Improvement

1. **Missing Consistency Document**: No `R12_CONSISTENCY_CHECK.md` (like R10)
2. **Missing Traceability**: No traceability matrix to requirements
3. **Missing Pattern Catalog**: No formal pattern catalog (like R8's `TRIPLE_FAULT_PATTERNS_CATALOG.md`)

**Recommendation**: Create consistency check document and traceability matrix.

## Improvement Recommendations

### Priority 1: Critical (Must Fix)

#### 1.1. Add Metrics Verification

**Action**: Add metrics verification to all tests

**Example**:
```erlang
test_single_instance_jetstream_partition_short(_Config) ->
    %% Get initial metrics
    InitialMetrics = get_metrics_snapshot(),
    
    %% Enable fault
    router_nats_fault_injection:enable_fault(connect, {error, connection_refused}),
    timer:sleep(5000),
    
    %% Get metrics during partition
    PartitionMetrics = get_metrics_snapshot(),
    
    %% Verify metrics reflect partition
    InitialFailures = maps:get(router_nats_connection_failures_total, InitialMetrics, 0),
    PartitionFailures = maps:get(router_nats_connection_failures_total, PartitionMetrics, 0),
    true = PartitionFailures > InitialFailures,
    
    %% Recovery
    router_nats_fault_injection:disable_fault(connect),
    timer:sleep(3000),
    
    %% Get final metrics
    FinalMetrics = get_metrics_snapshot(),
    
    %% Verify recovery metrics
    FinalRestored = maps:get(router_nats_connection_restored_total, FinalMetrics, 0),
    InitialRestored = maps:get(router_nats_connection_restored_total, InitialMetrics, 0),
    true = FinalRestored > InitialRestored,
    
    ok.
```

**Helper Function Needed**:
```erlang
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    router_metrics:ensure(),
    %% Collect metrics from ETS table
    ...
```

#### 1.2. Add Contract Invariant Verification

**Action**: Add contract invariant checks (MaxDeliver, redelivery, delivery count)

**Example**:
```erlang
%% Verify contract invariants
verify_network_partition_contracts(InitialMetrics, FinalMetrics) ->
    %% Check MaxDeliver semantics
    verify_maxdeliver_semantics(InitialMetrics, FinalMetrics),
    
    %% Check redelivery limits
    verify_redelivery_limits(InitialMetrics, FinalMetrics),
    
    %% Check delivery count tracking
    verify_delivery_count_tracking(InitialMetrics, FinalMetrics),
    
    ok.
```

**Reuse**: Copy helper functions from `router_triple_fault_contract_SUITE.erl`.

#### 1.3. Standardize Fault Injection Mechanism

**Action**: Use only `router_nats_fault_injection` (remove `router_network_partition:create_partition/2` usage)

**Rationale**: Consistent with R8/R10/R13, simpler, more maintainable

**Migration**:
- Replace `router_network_partition:create_partition/2` with `router_nats_fault_injection:enable_fault/2`
- Keep `router_network_partition` for real network tools mode only (if needed)

### Priority 2: High (Should Fix)

#### 2.1. Add Helper Functions

**Action**: Add helper functions to test suite (or reuse from existing suites)

**Functions Needed**:
```erlang
%% Metrics helpers
get_metrics_snapshot() -> map().
calculate_metric_deltas(InitialMetrics, FinalMetrics) -> map().

%% Contract verification helpers
verify_network_partition_contracts(InitialMetrics, FinalMetrics) -> ok.
verify_fail_open_behavior() -> ok.
verify_recovery_behavior(InitialMetrics, FinalMetrics) -> ok.

%% Test lifecycle helpers
run_partition_lifecycle(Faults, PartitionDuration, RecoveryWait) -> 
    {InitialMetrics, PartitionMetrics, FinalMetrics}.
```

**Reuse**: Consider extracting common helpers to `router_fault_injection_helpers.erl`.

#### 2.2. Add Log Verification

**Action**: Add log verification (if log capture infrastructure exists)

**Example**:
```erlang
%% Verify expected logs are emitted
ExpectedLogs = [
    {warn, <<"Network partition detected">>, #{from => <<"router">>, to => <<"nats">>}},
    {error, <<"Connection lost to NATS JetStream broker">>, #{service => <<"nats-jetstream">>}},
    {info, <<"Network partition resolved">>, #{from => <<"router">>, to => <<"nats">>}}
],
verify_logs_emitted(ExpectedLogs),
```

**Note**: Requires log capture infrastructure (may not exist yet).

#### 2.3. Create Consistency Check Document

**Action**: Create `R12_CONSISTENCY_CHECK.md` (similar to `R10_CONSISTENCY_CHECK.md`)

**Content**:
- Verification that R12 uses same fault injection mechanism as R8/R10/R13
- Comparison of test patterns
- Verification of metrics/logs alignment
- Identification of gaps

### Priority 3: Medium (Nice to Have)

#### 3.1. Add Pattern Catalog

**Action**: Create `R12_NETWORK_PARTITION_PATTERNS_CATALOG.md` (similar to `TRIPLE_FAULT_PATTERNS_CATALOG.md`)

**Content**:
- Formal catalog of network partition patterns
- Expected behavior for each pattern
- Contract rules and invariants
- Test coverage matrix

#### 3.2. Add Traceability Matrix

**Action**: Create traceability matrix to requirements (similar to `FAULT_INJECTION_REQUIREMENTS_TRACEABILITY.md`)

**Content**:
- Map R12 scenarios to requirements
- Verify all requirements have test coverage
- Identify any gaps

#### 3.3. Enhance Scripts

**Action**: Add Erlang shell integration to scripts

**Example**:
```bash
# Script could call Erlang shell to enable fault injection
erl -noshell -eval "router_nats_fault_injection:enable_fault(connect, {error, connection_refused})" -s init stop
```

**Note**: Requires Erlang shell access, may be complex.

## Alignment with R8/R10/R13

### Current Alignment Status

| Aspect | R8 | R10 | R13 | R12 | Alignment |
|--------|----|----|----|----|-----------|
| Fault injection mechanism | `router_nats_fault_injection` | `router_nats_fault_injection` | `router_nats_fault_injection` | ⚠️ Mixed | ⚠️ Partial |
| Metrics verification | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ❌ Missing |
| Contract invariants | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ❌ Missing |
| Helper functions | ✅ Yes | ✅ Yes | ✅ Yes | ❌ No | ❌ Missing |
| Test structure | ✅ Groups + helpers | ✅ Groups + helpers | ✅ Groups + helpers | ✅ Groups only | ⚠️ Partial |
| Documentation | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Good |

### Recommended Alignment Actions

1. **Standardize fault injection**: Use only `router_nats_fault_injection` (remove `router_network_partition` usage in tests)
2. **Add metrics verification**: Implement metrics snapshot and verification (reuse from R8/R10/R13)
3. **Add contract invariants**: Implement contract verification (reuse from R8)
4. **Add helper functions**: Extract common helpers or reuse from existing suites
5. **Create consistency check**: Document alignment with R8/R10/R13

## Test Coverage Analysis

### Current Coverage

| Scenario Category | Test Cases | Status |
|------------------|------------|--------|
| Single-instance JetStream partition | 3 | ✅ Covered |
| Single-instance external service partition | 3 | ✅ Covered |
| Multi-instance split-brain | 3 | ✅ Covered |
| Multi-instance JetStream partition | 3 | ✅ Covered |
| Multi-instance distributed locks | 2 | ✅ Covered |
| Service-broker partition | 3 | ✅ Covered |
| Flapping network | 3 | ✅ Covered |
| **Total** | **21** | ✅ **Complete** |

### Coverage Gaps

1. **Metrics verification**: Tests don't verify metrics (coverage gap)
2. **Contract invariants**: Tests don't verify MaxDeliver/redelivery (coverage gap)
3. **Log verification**: Tests don't verify logs (coverage gap)
4. **Multi-tenant isolation**: No explicit multi-tenant tests (may be covered implicitly)

**Recommendation**: Add explicit verification for metrics, contracts, and logs.

## Readability and Maintainability

### ✅ Strengths

1. **Clear Test Names**: Test names clearly describe scenarios
2. **Good Comments**: Tests have descriptive comments
3. **Organized Structure**: Tests grouped logically

### ⚠️ Areas for Improvement

1. **Code Duplication**: Each test manually implements verification (should use helpers)
2. **Inconsistent Patterns**: Some tests use `router_network_partition`, others use `router_nats_fault_injection`
3. **Missing Assertions**: Tests rely on comments instead of explicit assertions

**Recommendation**: Refactor to use helper functions, standardize patterns, add explicit assertions.

## Summary

### ✅ What's Good

1. **Comprehensive Coverage**: All scenarios from requirements are covered
2. **Complete Documentation**: All required documentation exists
3. **Good Structure**: Tests are well-organized
4. **Cross-Platform Scripts**: Both Bash and PowerShell versions exist

### ⚠️ What Needs Improvement

1. **Metrics Verification**: Missing in all tests (critical)
2. **Contract Invariants**: Missing in all tests (critical)
3. **Helper Functions**: Not used (should reuse from R8/R10/R13)
4. **Fault Injection Consistency**: Dual mechanisms (should standardize)
5. **Consistency Documentation**: Missing consistency check document

### 📋 Action Items

**Priority 1 (Critical)**:
- [ ] Add metrics verification to all tests
- [ ] Add contract invariant verification
- [ ] Standardize fault injection mechanism (use only `router_nats_fault_injection`)

**Priority 2 (High)**:
- [ ] Add helper functions (reuse from R8/R10/R13)
- [ ] Add log verification (if infrastructure exists)
- [ ] Create `R12_CONSISTENCY_CHECK.md`

**Priority 3 (Medium)**:
- [ ] Create `R12_NETWORK_PARTITION_PATTERNS_CATALOG.md`
- [ ] Create traceability matrix
- [ ] Enhance scripts with Erlang integration

## References

- **R8**: `TRIPLE_FAULT_PATTERNS_CATALOG.md`, `router_triple_fault_contract_SUITE.erl`
- **R10**: `R10_CONSISTENCY_CHECK.md`, `R10_PUBLISH_FAILURE_E2E_SPEC.md`
- **R13**: `R13_METRICS_UNDER_FAULTS_SPEC.md`, `router_metrics_under_faults_SUITE.erl`
- **Helpers**: `router_fault_injection_helpers.erl`, `router_triple_fault_contract_SUITE.erl` (helper functions)

