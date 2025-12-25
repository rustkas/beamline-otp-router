# Extended Recovery Scenarios - Documentation Section

> **Note**: This section can be integrated into `JETSTREAM_FAULT_INJECTION_TESTS.md` or used as standalone documentation.

## Extended Recovery Scenarios

### Overview

The **Extended Recovery Scenarios** system extends the basic fault injection test suite with long-running, production-scale recovery tests. These scenarios verify Router and JetStream behavior under sustained fault conditions and repeated recovery cycles, ensuring system stability and performance preservation over extended periods.

**Purpose**: Bridge the gap between short fault injection tests (seconds to minutes) and real-world production scenarios where systems experience repeated failures and recoveries over hours or days.

### Key Components

The Extended Recovery system consists of four main components:

1. **CI/CD Integration**: Nightly workflow for automated extended test execution
2. **Performance Baseline**: Scripts and processes for establishing performance baselines
3. **Resource Limits**: Formalized thresholds for resource usage and performance degradation
4. **Extended Test Suite**: Comprehensive test scenarios covering various failure and recovery patterns

### Test Categories

#### Category 1: MaxDeliver Exhaustion Scenarios

Tests focus on message delivery count accumulation and MaxDeliver limit exhaustion:

- **Gradual Accumulation** (`test_maxdeliver_gradual_accumulation`): Messages gradually accumulate delivery count over 30 minutes until MaxDeliver is exhausted
- **Mass Exhaustion** (`test_maxdeliver_mass_exhaustion`): Many messages exhaust MaxDeliver simultaneously (18 minutes)
- **Periodic Consumer Hang** (`test_maxdeliver_periodic_consumer_hang`): Consumer periodically "hangs" causing delivery count accumulation (45 minutes)

**Key Metrics**:
- Delivery count distribution
- MaxDeliver exhausted count
- DLQ message count
- Throughput during accumulation
- Recovery time after exhaustion

#### Category 2: Restart Scenarios

Tests verify system behavior during repeated infrastructure restarts:

- **Repeated JetStream Restarts** (`test_repeated_jetstream_restarts`): JetStream node restarts repeatedly over 75 minutes
- **Repeated Router Restarts** (`test_repeated_router_restarts`): Router process restarts repeatedly over 75 minutes
- **Network Partition Recovery** (`test_network_partition_recovery`): Network partitions between router and JetStream over 75 minutes

**Key Metrics**:
- Reconnection time
- Messages processed per cycle
- Throughput during/after restarts
- Memory usage over time
- State preservation verification

#### Category 3: Combined Scenarios

Tests combine multiple fault types in complex sequences:

- **Sequential Fault Chain** (`test_sequential_fault_chain`): Multiple different faults occur sequentially in one 110-minute test
- **Repeating Fault Cycles** (`test_repeating_fault_cycles`): Same fault pattern repeats 12+ times over 135 minutes

**Key Metrics**:
- Cumulative resource growth
- Performance degradation across phases
- Error accumulation
- Recovery consistency across cycles

#### Category 4: Performance Scenarios

Tests focus on long-term stability and recovery time measurement:

- **Long-Running Stability** (`test_long_running_stability`): System runs for 4 hours with normal load to verify stability
- **Recovery Time Measurement** (`test_recovery_time_measurement`): Measures time to recover throughput/latency after different fault types

**Key Metrics**:
- Throughput trends over time
- Latency trends over time
- Resource usage trends
- Recovery time per fault type

#### Category 5: Production-Scale Scenarios

Tests simulate production-scale infrastructure scenarios:

- **Multi-Node JetStream Cluster Failures** (`test_multi_node_jetstream_cluster_failures`): Failures in a multi-node JetStream cluster (90 minutes)
- **Cross-Region Network Partitions** (`test_cross_region_network_partitions`): Network partitions between regions (90 minutes)
- **Rolling Restart Zero Downtime** (`test_rolling_restart_zero_downtime`): Rolling restart of router processes without downtime (60 minutes)

**Key Metrics**:
- Cluster node availability
- Quorum status
- Region connectivity
- Active router count
- Zero downtime verification

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

**Configuration**:
- Test group selection (all, maxdeliver_scenarios, restart_scenarios, etc.)
- Duration override (optional)
- Message rate configuration

### Performance Baseline

**Script**: `apps/otp/router/scripts/establish_performance_baseline.sh`

**Purpose**: Establish baseline performance metrics under normal load conditions

**Output**:
- `baseline_metrics.json`: Machine-readable baseline metrics
- `baseline_report.md`: Human-readable baseline report
- `baseline_thresholds.json`: Performance thresholds (95% confidence interval)

**Usage**:
```bash
./scripts/establish_performance_baseline.sh \
  --iterations 10 \
  --duration-min 5 \
  --message-rate 100 \
  --output-dir reports/baseline
```

**Integration**: Extended recovery tests use baseline thresholds to detect performance degradation

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

**Enforcement**: Tests fail if failure thresholds are exceeded, warn if warning thresholds are exceeded

### Test Execution

#### Local Development

**Run Single Scenario**:
```bash
cd apps/otp/router
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --case test_maxdeliver_gradual_accumulation
```

**Run Test Group**:
```bash
rebar3 ct --suite router_jetstream_extended_recovery_SUITE \
          --group production_scale_scenarios
```

**Run All Scenarios**:
```bash
# Warning: May take several hours
rebar3 ct --suite router_jetstream_extended_recovery_SUITE
```

#### CI/CD Execution

- **Automatic**: Runs daily at 3:00 AM UTC
- **Manual**: Trigger via GitHub Actions UI
- **Artifacts**: Logs, metrics, and reports available for 30-90 days

### Metrics and Observability

#### Collected Metrics

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

#### Collection Points

- **Phase boundaries**: Start/end of baseline, fault, and recovery phases
- **Periodic collection**: Every 5 minutes during long phases
- **Event-driven**: On fault injection, recovery, threshold crossings

### Success Criteria

#### Functional Criteria

- ✅ No unexplained message loss (except expected MaxDeliver exhaustion)
- ✅ Predictable MaxDeliver behavior: messages either deliver or go to DLQ
- ✅ Correct recovery: new messages process normally after recovery
- ✅ No stuck messages: backlog processes or exhausts correctly

#### Performance Criteria

- ✅ Recovery time: < 5 minutes to return to baseline
- ✅ Resource stability: No unbounded growth (memory, processes, connections)
- ✅ Performance stability: Cycle N performance >= Cycle 1 performance

#### Stability Criteria

- ✅ No resource leaks: Memory/process count stable over time
- ✅ No error accumulation: Error rate doesn't grow unbounded
- ✅ Consistent behavior: System behaves consistently across cycles

### Relationship to Existing Tests

#### Existing Test Suites

- **`router_jetstream_fault_injection_SUITE`**: Short fault injection tests (< 5 minutes)
  - Focus: Correctness under single fault events
  - Extended Recovery: Extends to long-running scenarios with performance tracking

- **`router_concurrent_faults_stress_SUITE`**: Concurrent fault stress tests (30 sec - 5 min)
  - Focus: Multiple simultaneous faults
  - Extended Recovery: Extends to extended cycles with resource leak detection

#### Coverage Matrix

| Scenario Type | Existing Tests | Extended Recovery | Gap Status |
|--------------|---------------|-------------------|------------|
| Short faults | ✅ Complete | ❌ Not needed | ✅ Complete |
| Long-running faults | ❌ Missing | ✅ Complete | ✅ Complete |
| Performance tracking | ❌ Missing | ✅ Complete | ✅ Complete |
| Resource leak detection | ❌ Missing | ✅ Complete | ✅ Complete |
| Recovery time measurement | ❌ Missing | ✅ Complete | ✅ Complete |
| Production-scale | ❌ Missing | ✅ Complete | ✅ Complete |

### References

- **Specification**: `apps/otp/router/test/EXTENDED_RECOVERY_SCENARIOS_SPEC.md`
- **Coverage Analysis**: `apps/otp/router/test/EXTENDED_RECOVERY_COVERAGE.md`
- **Quick Start**: `apps/otp/router/test/README_EXTENDED_RECOVERY.md`
- **Resource Limits**: `apps/otp/router/docs/EXTENDED_RECOVERY_RESOURCE_LIMITS.md`
- **Production Scale**: `apps/otp/router/docs/EXTENDED_RECOVERY_PRODUCTION_SCALE.md`
- **Implementation Summary**: `apps/otp/router/docs/EXTENDED_RECOVERY_IMPLEMENTATION_SUMMARY.md`

