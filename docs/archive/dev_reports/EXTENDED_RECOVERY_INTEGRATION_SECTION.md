# Extended Recovery Scenarios - Integration Section for JETSTREAM_FAULT_INJECTION_TESTS.md

> **Note**: This section is designed to be integrated into `JETSTREAM_FAULT_INJECTION_TESTS.md` as a new section. It follows the structure and style of the existing document.

## Extended Recovery Scenarios

### Overview

The **Extended Recovery Scenarios** test suite extends the basic fault injection test coverage with long-running, production-scale recovery tests. These scenarios verify Router and JetStream behavior under sustained fault conditions and repeated recovery cycles over extended periods (minutes to hours).

**Purpose**: Bridge the gap between short fault injection tests (seconds to minutes) and real-world production scenarios where systems experience repeated failures and recoveries over hours or days.

**Status**: ✅ **Implemented** - Suite is active and integrated in CI

**Tags**: `@test_category extended_recovery, slow, long_running, performance`

**File**: `apps/otp/router/test/router_jetstream_extended_recovery_SUITE.erl`

### Test Suite Structure

The Extended Recovery suite is organized into 5 test groups:

1. **MaxDeliver Scenarios** (3 tests)
2. **Restart Scenarios** (3 tests)
3. **Combined Scenarios** (2 tests)
4. **Performance Scenarios** (2 tests)
5. **Production-Scale Scenarios** (3 tests)

**Total**: 13+ extended recovery test scenarios

### Key Tests

#### MaxDeliver Scenarios

- `test_maxdeliver_gradual_accumulation/1` - Gradual delivery count accumulation over 30 minutes
- `test_maxdeliver_mass_exhaustion/1` - Mass MaxDeliver exhaustion (18 minutes)
- `test_maxdeliver_periodic_consumer_hang/1` - Periodic consumer hang cycles (45 minutes)

#### Restart Scenarios

- `test_repeated_jetstream_restarts/1` - Repeated JetStream node restarts (75 minutes)
- `test_repeated_router_restarts/1` - Repeated router process restarts (75 minutes)
- `test_network_partition_recovery/1` - Network partition recovery (75 minutes)

#### Combined Scenarios

- `test_sequential_fault_chain/1` - Sequential fault chain (110 minutes)
- `test_repeating_fault_cycles/1` - Repeating fault cycles (135 minutes)

#### Performance Scenarios

- `test_long_running_stability/1` - Long-running stability (240 minutes / 4 hours)
- `test_recovery_time_measurement/1` - Recovery time measurement (17 minutes per fault type)

#### Production-Scale Scenarios

- `test_multi_node_jetstream_cluster_failures/1` - Multi-node JetStream cluster failures (90 minutes)
- `test_cross_region_network_partitions/1` - Cross-region network partitions (90 minutes)
- `test_rolling_restart_zero_downtime/1` - Rolling restart zero downtime (60 minutes)

### Relationship to Existing Scenarios

#### Extended Recovery vs S1 (Single Fault Injection)

**S1 Coverage**: Single fault events with immediate recovery verification (seconds to minutes)

**Extended Recovery Extension**:
- Scales S1 scenarios to extended duration (hours instead of minutes)
- Adds performance measurement (throughput/latency tracking)
- Adds resource tracking (memory, processes, connections)
- Example: `test_repeated_jetstream_restarts` extends single restart to 12+ cycles over 75 minutes

**Status**: ✅ **Extended** - S1 scenarios scaled to long-running contexts

#### Extended Recovery vs S2 (Concurrent Faults)

**S2 Coverage**: Multiple simultaneous faults under stress (30 sec - 5 min, 5 cycles)

**Extended Recovery Extension**:
- Extends cycle count from 5 to 12+ cycles
- Adds stability measurement (performance consistency across cycles)
- Adds resource leak detection (resource usage tracking)
- Example: `test_repeating_fault_cycles` extends concurrent faults to 12 cycles over 135 minutes

**Status**: ✅ **Extended** - S2 scenarios scaled to extended cycles with stability verification

#### Extended Recovery vs S3 (MaxDeliver)

**S3 Coverage**: Single MaxDeliver exhaustion events, delivery count tracking, DLQ handling

**Extended Recovery Extension**:
- Adds accumulation scenarios (gradual and mass exhaustion)
- Adds periodic patterns (consumer hang cycles)
- Adds performance impact measurement
- Example: `test_maxdeliver_gradual_accumulation` extends single exhaustion to 30-minute accumulation

**Status**: ✅ **Extended** - S3 scenarios extended to accumulation patterns with performance tracking

### CI/CD Integration

**Workflow**: `.github/workflows/router-extended-recovery-nightly.yml`

**Schedule**: Daily at 3:00 AM UTC

**Features**:
- Automatic nightly execution
- Manual trigger via GitHub Actions UI
- OTP version matrix (25.3, 26.2)
- Extended timeout (8 hours)
- NATS service setup
- Metrics collection and reporting
- Artifact upload (logs, metrics, reports)

**Status**: ✅ **Integrated** - Nightly workflow operational

### Performance Baseline

**Script**: `apps/otp/router/scripts/establish_performance_baseline.sh`

**Purpose**: Establish baseline performance metrics under normal load

**Output**:
- `baseline_metrics.json` - Machine-readable baseline metrics
- `baseline_report.md` - Human-readable baseline report
- `baseline_thresholds.json` - Performance thresholds (95% confidence interval)

**Status**: ✅ **Implemented** - Baseline establishment script functional

### Resource Limits

**Documentation**: `apps/otp/router/docs/EXTENDED_RECOVERY_RESOURCE_LIMITS.md`

**Defined Limits**:

| Resource | Warning Threshold | Failure Threshold |
|----------|-------------------|-------------------|
| Process Count | Baseline + 50 | Baseline + 100 |
| Memory Usage | Baseline + 100 MB | Baseline + 200 MB |
| Connection Count | Baseline + 5 | Baseline + 10 |
| ETS Table Size | Baseline + 1000 entries | Baseline + 5000 entries |
| Throughput | 90% of baseline | 80% of baseline |
| Latency | +20% from baseline | +50% from baseline |

**Status**: ✅ **Documented** - Resource limits defined and enforced

### Execution

**Run all extended recovery tests**:
```bash
cd apps/otp/router
rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

**Run specific test group**:
```bash
# MaxDeliver scenarios only
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --group maxdeliver_scenarios

# Production-scale scenarios only
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --group production_scale_scenarios
```

**Run single scenario**:
```bash
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --case test_maxdeliver_gradual_accumulation
```

**CI/CD (Nightly)**:
- Automatically runs daily at 3:00 AM UTC
- Can be manually triggered via GitHub Actions UI
- Results available in artifacts (logs, metrics, reports)

### Coverage Matrix

| Scenario Type | Existing Tests | Extended Recovery | Total Coverage |
|--------------|---------------|-------------------|----------------|
| Short faults | ✅ S1, S2, S3 | ❌ Not needed | ✅ Complete |
| Long-running faults | ❌ Missing | ✅ Complete | ✅ Complete |
| Performance tracking | ❌ Missing | ✅ Complete | ✅ Complete |
| Resource leak detection | ❌ Missing | ✅ Complete | ✅ Complete |
| Recovery time measurement | ❌ Missing | ✅ Complete | ✅ Complete |
| Production-scale | ❌ Missing | ✅ Complete | ✅ Complete |
| Multi-node cluster | ❌ Missing | ✅ Complete | ✅ Complete |
| Cross-region partitions | ❌ Missing | ✅ Complete | ✅ Complete |
| Rolling restart | ❌ Missing | ✅ Complete | ✅ Complete |

### Metrics and Observability

**Functional Metrics**:
- Message processing count
- MaxDeliver exhausted count
- DLQ message count
- Redelivery count
- Connection state changes
- Consumer reconnection count

**Performance Metrics**:
- Throughput (messages/second)
- Latency (p50, p95, p99)
- Recovery time (time to return to baseline)

**Resource Metrics**:
- Memory usage (MB)
- Process count
- ETS table sizes
- Connection count

**Collection Points**:
- Phase boundaries (baseline, fault, recovery)
- Periodic collection (every 5 minutes during long phases)
- Event-driven (fault injection, recovery, threshold crossings)

### Success Criteria

**Functional Criteria**:
- ✅ No unexplained message loss (except expected MaxDeliver exhaustion)
- ✅ Predictable MaxDeliver behavior: messages either deliver or go to DLQ
- ✅ Correct recovery: new messages process normally after recovery
- ✅ No stuck messages: backlog processes or exhausts correctly

**Performance Criteria**:
- ✅ Recovery time: < 5 minutes to return to baseline
- ✅ Resource stability: No unbounded growth (memory, processes, connections)
- ✅ Performance stability: Cycle N performance >= Cycle 1 performance

**Stability Criteria**:
- ✅ No resource leaks: Memory/process count stable over time
- ✅ No error accumulation: Error rate doesn't grow unbounded
- ✅ Consistent behavior: System behaves consistently across cycles

### Future Enhancements

**Planned** (not yet implemented):
- Business-flow-specific scenarios
- Edge case JetStream configurations
- Version upgrade/downgrade scenarios
- Real multi-node/multi-region infrastructure setup
- 24+ hour extended duration tests
- Operational change scenarios (configuration, schema migration)

### References

- `apps/otp/router/test/EXTENDED_RECOVERY_SCENARIOS_SPEC.md` - Detailed scenario specifications
- `apps/otp/router/test/EXTENDED_RECOVERY_COVERAGE.md` - Coverage analysis and gap analysis
- `apps/otp/router/test/README_EXTENDED_RECOVERY.md` - Quick start guide
- `apps/otp/router/docs/EXTENDED_RECOVERY_RESOURCE_LIMITS.md` - Resource limits documentation
- `apps/otp/router/docs/EXTENDED_RECOVERY_PRODUCTION_SCALE.md` - Production-scale scenarios
- `apps/otp/router/docs/EXTENDED_RECOVERY_IMPLEMENTATION_SUMMARY.md` - Implementation summary
- `.github/workflows/router-extended-recovery-nightly.yml` - CI/CD workflow

