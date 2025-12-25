# Scope: T-SOAK-01

## In Scope

### 1. Memory Stability Testing
- **Duration**: 6-24 hours continuous operation
- **Metrics**: Process memory (RSS, heap size, binary memory)
- **Target**: < 10% growth over 24 hours
- **Monitoring**: `erlang:memory()`, OS memory stats

### 2. ETS Table Growth Monitoring
- **Tables to Monitor**:
  - `router_metrics` - Prometheus metrics storage
  - `router_policy_cache` - Policy cache
  - `router_provider_circuit_breaker` - Circuit breaker states
  - `router_jetstream_pending_cache` - Backpressure metrics
  - All other ETS tables
- **Target**: No unbounded growth, bounded by configured limits
- **Alerts**: Table size > 10,000 entries (configurable)

### 3. JetStream Consumer Stability
- **Metrics**:
  - Consumer lag (pending messages)
  - Redelivery count
  - ACK rate vs delivery rate
- **Target**: Lag < 100 messages (steady state)
- **Duration**: 24-hour window

### 4. Process Stability
- **Metrics**:
  - Total process count
  - Process crashes/restarts (supervisor stats)
  - Message queue lengths
- **Target**: No runaway process creation, no crashes

### 5. Metric Cardinality
- **Metrics**:
  - Total unique metric series
  - Label cardinality per metric
- **Target**: < 10,000 unique series (prevent cardinality explosion)

### 6. Load Profile
- **Request Rate**: Moderate sustained load (10-100 req/s)
- **Message Mix**: Varied routing decisions, circuit breaker triggers
- **Failure Injection**: Periodic provider failures (20% error rate)
- **Backpressure**: Periodic lag injection to test recovery

## Out of Scope

- **High-load stress testing** - defer to T-PERF-02 (Stress Testing)
- **Multi-node soak testing** - single node only for now
- **Database soak testing** - Router is stateless, no DB
- **Full 30-day burn-in** - 24 hours is sufficient for initial validation
- **Automated regression runs** - manual execution initially

## Test Configuration

### Short Soak (6 hours)
- **Purpose**: Quick validation, CI/CD integration
- **Request Rate**: 10 req/s
- **Memory Limit**: 1GB
- **Pass Criteria**: Memory growth < 5%, no crashes

### Long Soak (24 hours)
- **Purpose**: Production readiness validation
- **Request Rate**: 50 req/s
- **Memory Limit**: 2GB
- **Pass Criteria**: Memory growth < 10%, no crashes, ETS bounded

### Extended Soak (72 hours) - Future
- **Purpose**: Extreme stability validation
- **Request Rate**: Variable (10-100 req/s)
- **Memory Limit**: 4GB

## Dependencies

- Real NATS/JetStream (not mock mode)
- Prometheus for metric collection
- Sufficient disk space for logs (~10GB)
- Monitoring dashboard for real-time observation

## References

- Existing soak tests: `test/router_soak_single_fault_SUITE.erl`
- Memory profiling: `scripts/memory_profile.sh` (to be created)
