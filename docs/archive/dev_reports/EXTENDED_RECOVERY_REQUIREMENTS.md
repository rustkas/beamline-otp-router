# Extended Recovery System - Formal Requirements Specification

## 1. Goals

### 1.1 Functional Goals

Ensure verification of correct recovery behavior for Router and JetStream after complex and extended failures, including:

- **MaxDeliver exhaustion**: Gradual accumulation, mass exhaustion, periodic consumer hangs
- **Network partitions**: Short and long-term partitions between router and JetStream, cross-region partitions
- **Restarts and failures**: JetStream node restarts, router process restarts, rolling restarts
- **Combined scenarios**: Sequential fault chains, repeating fault cycles with multiple fault types

**Expected Behavior**:
- No unexplained message loss (except expected MaxDeliver exhaustion)
- Predictable MaxDeliver behavior: messages either deliver successfully or go to DLQ
- Correct recovery: new messages process normally after recovery
- No stuck messages: backlog processes correctly or exhausts predictably

### 1.2 Non-Functional Goals

Confirm that after multiple "fault → recovery" cycles:

- **Performance preservation**: Throughput and latency remain within acceptable bounds
- **Resource stability**: No degradation in CPU, memory, connections, ETS tables, disk usage
- **Production readiness**: System remains suitable for production-scale deployment

**Performance Criteria**:
- Recovery time: < 5 minutes to return to baseline throughput/latency
- Throughput degradation: < 10% (warning), < 20% (failure) from baseline
- Latency increase: < 20% (warning), < 50% (failure) from baseline

**Resource Criteria**:
- Process count growth: < 50 (warning), < 100 (failure) from baseline
- Memory usage growth: < 100 MB (warning), < 200 MB (failure) from baseline
- Connection count growth: < 5 (warning), < 10 (failure) from baseline
- ETS table growth: < 1000 entries/table (warning), < 5000 entries/table (failure)

### 1.3 Engineering Goals

Provide a unified, automatable framework:

- **CI/CD integration**: Nightly job for extended test runs with extended timeouts
- **Test suite**: Comprehensive suite with production-scale scenarios
- **Formalized resource limits**: Documented thresholds with warning/failure levels
- **Documentation**: Complete coverage of implementation, limits, and scenario coverage

## 2. Scope

### 2.1 In Scope

#### 2.1.1 CI/CD Integration

**Deliverable**: `.github/workflows/router-extended-recovery-nightly.yml`

**Requirements**:
- Scheduled nightly runs (daily, 3:00 AM UTC)
- Manual trigger via GitHub Actions UI
- OTP version matrix support (e.g., 25.3, 26.2)
- NATS/JetStream environment setup
- Extended timeout (up to 8 hours)
- Log, metrics, and report collection
- Artifact upload and retention
- Test summary generation

#### 2.1.2 Performance Baseline

**Deliverable**: `apps/otp/router/scripts/establish_performance_baseline.sh`

**Requirements**:
- Baseline metrics collection under normal load
- Statistical analysis (mean, std dev, percentiles)
- Threshold calculation (95% confidence interval)
- Output formats:
  - `baseline_metrics.json` (machine-readable)
  - `baseline_report.md` (human-readable)
  - `baseline_thresholds.json` (performance thresholds)

**Parameters**:
- Number of iterations (default: 10)
- Duration per iteration (default: 5 minutes)
- Message rate (default: 100 msg/s)
- Output directory

#### 2.1.3 Resource Limits

**Deliverable**: `apps/otp/router/docs/EXTENDED_RECOVERY_RESOURCE_LIMITS.md`

**Requirements**:
- Defined thresholds (Warning/Failure) for:
  - Process count
  - Memory usage
  - Connection count
  - ETS table sizes
  - Throughput (percentage of baseline)
  - Latency (percentage increase from baseline)
- Scenario-specific limits
- Implementation details
- CI/CD integration guidelines

#### 2.1.4 Extended Recovery Test Suite

**Deliverable**: `apps/otp/router/test/router_jetstream_extended_recovery_SUITE.erl`

**Requirements**:
- ≥13 test scenarios, grouped by category:
  - MaxDeliver scenarios (3 tests)
  - Restart scenarios (3 tests)
  - Combined scenarios (2 tests)
  - Performance scenarios (2 tests)
  - Production-scale scenarios (3 tests)
- Production-scale scenarios:
  - Multi-node JetStream cluster failures
  - Cross-region network partitions
  - Rolling restart zero downtime
- Helper functions for metrics, resource tracking, recovery measurement
- Integration with baseline and resource limits

#### 2.1.5 Documentation

**Deliverables**:
- `apps/otp/router/test/EXTENDED_RECOVERY_SCENARIOS_SPEC.md` - Scenario specifications
- `apps/otp/router/test/EXTENDED_RECOVERY_COVERAGE.md` - Coverage analysis
- `apps/otp/router/test/README_EXTENDED_RECOVERY.md` - Quick start guide
- `apps/otp/router/docs/EXTENDED_RECOVERY_RESOURCE_LIMITS.md` - Resource limits
- `apps/otp/router/docs/EXTENDED_RECOVERY_PRODUCTION_SCALE.md` - Production-scale scenarios
- `apps/otp/router/docs/EXTENDED_RECOVERY_IMPLEMENTATION_SUMMARY.md` - Implementation summary

**Requirements**:
- Complete scenario descriptions
- Phase-by-phase breakdown
- Success criteria
- Metrics collection points
- Test configuration
- Gap analysis
- Coverage matrix

### 2.2 Out of Scope

- **Business domain logic** outside JetStream integration
- **Fine-tuning NATS/JetStream cluster** for production infrastructure (Terraform, Helm, etc.)
- **Full-stack end-to-end scenarios** with external services outside Router
- **Deep performance analysis** of NATS platform itself (outside Router scope)
- **Real multi-node cluster setup** in test environment (simulated via mocks)
- **Real multi-region setup** in test environment (simulated via mocks)
- **Real multi-instance router deployment** in test environment (simulated via mocks)

## 3. Boundaries

### 3.1 Test Environment

- **Simulation-based**: Uses mocks for multi-node, multi-region, multi-instance scenarios
- **Local development**: Tests run in local environment with mock NATS
- **CI/CD**: Tests run in GitHub Actions with NATS service container
- **No production data**: Tests use synthetic workload, not production data replay

### 3.2 Time Constraints

- **Test duration**: 17 minutes to 4 hours per test scenario
- **Full suite**: Up to 8 hours total execution time
- **CI/CD timeout**: 8 hours (480 minutes) for full suite
- **Baseline establishment**: 50+ minutes (10 iterations × 5 minutes)

### 3.3 Resource Constraints

- **CI/CD resources**: Limited by GitHub Actions runner resources
- **Local resources**: Limited by developer machine resources
- **NATS resources**: Limited by NATS service container resources

## 4. Implementation Requirements

### 4.1 Automatability

- All scenarios must run without manual intervention
- Support for local execution and CI/CD execution
- Configurable via environment variables
- Graceful termination with results collection

### 4.2 Observability

- **Clear phase boundaries**: Before fault / During fault / After recovery
- **Key event logging**: Fault injection, recovery, threshold crossings
- **Metrics export**: JSON/Markdown format for post-test analysis
- **Periodic collection**: Metrics collected every 5 minutes during long phases

### 4.3 Integration with Baseline and Limits

- Extended tests use thresholds from `baseline_thresholds.json`
- Extended tests use limits from `EXTENDED_RECOVERY_RESOURCE_LIMITS.md`
- Threshold violations treated as test failure (Failure) or degradation (Warning)
- Baseline comparison performed automatically

### 4.4 Diagnosability

On test failure:
- Router and NATS logs available
- Metrics snapshots before/after fault preserved
- Reports indicate which scenario and phase failed
- Resource usage trends available for analysis

## 5. Success Criteria

### 5.1 Functional Success Criteria

- ✅ **No unexplained message loss** (except expected MaxDeliver exhaustion)
- ✅ **Predictable MaxDeliver behavior**: Messages either deliver or go to DLQ
- ✅ **Correct recovery**: New messages process normally after recovery
- ✅ **No stuck messages**: Backlog processes or exhausts correctly

### 5.2 Non-Functional Success Criteria

- ✅ **Recovery time**: < 5 minutes to return to baseline
- ✅ **Resource stability**: No unbounded growth (memory, processes, connections)
- ✅ **Performance stability**: Cycle N performance >= Cycle 1 performance
- ✅ **Extended run stability**: 4+ hour runs complete without resource exhaustion

### 5.3 Process Success Criteria

- ✅ **Nightly workflow stability**: Runs complete successfully or provide clear failure artifacts
- ✅ **Baseline establishment**: Script runs successfully and generates all required outputs
- ✅ **Resource limit enforcement**: Limits are checked and violations are reported
- ✅ **Documentation completeness**: All scenarios documented with clear success criteria

## 6. Dependencies

### 6.1 Existing Infrastructure

- **Fault injection infrastructure**: `router_nats_fault_injection` module
- **Existing test suites**: `router_jetstream_fault_injection_SUITE`, `router_concurrent_faults_stress_SUITE`
- **Telemetry system**: Telemetry handlers for metrics collection
- **Test helpers**: Common test helpers and utilities

### 6.2 External Dependencies

- **NATS/JetStream**: NATS server with JetStream enabled
- **Erlang/OTP**: OTP 25.3+ for test execution
- **CI/CD platform**: GitHub Actions for nightly runs
- **Analysis tools**: jq, python3 for metrics analysis (optional)

## 7. Acceptance Criteria

### 7.1 Test Suite Acceptance

- All 13+ test scenarios implemented and passing
- Test execution time within expected bounds
- No false positives (tests fail only on actual issues)
- Clear failure messages with diagnostic information

### 7.2 CI/CD Acceptance

- Nightly workflow runs successfully
- Artifacts uploaded correctly
- Reports generated and accessible
- Test summary provides actionable information

### 7.3 Baseline Acceptance

- Baseline script runs successfully
- All output files generated correctly
- Thresholds calculated using statistical methods
- Baseline values are reproducible

### 7.4 Documentation Acceptance

- All scenarios documented with clear descriptions
- Coverage analysis complete
- Resource limits clearly defined
- Quick start guide enables developers to run tests

## 8. Future Enhancements (Out of Scope for Initial Implementation)

- Real multi-node JetStream cluster setup in tests
- Real multi-region NATS setup in tests
- Real multi-instance router deployment in tests
- Production data replay scenarios
- Extended duration tests (24+ hours)
- Edge case JetStream configurations
- Version upgrade/downgrade scenarios
- Business-critical message flow coverage

