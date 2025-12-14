# Test Topology

This document maps the test suites to functionality zones, load levels, and coverage types.

## Legend

*   **Zone**: The logical subsystem (e.g., Policy, Intake, NATS).
*   **Levels**: `fast` (Unit/Smoke), `full` (Integration), `heavy` (Stress/Soak/Chaos).
*   **Tier**: `sanity` (Critical path, <10s), `commit` (PR checks, <5m), `nightly` (Deep validation).

## Test Map

| Suite Name | Zone | Levels | Tier | Description |
| :--- | :--- | :--- | :--- | :--- |
| `router_core_SUITE` | Core | `fast` | `sanity` | Basic application startup and config validation. |
| `router_policy_SUITE` | Policy | `fast`, `full` | `commit` | Policy logic, matching, and rules (mocked). |
| `router_nats_integration_SUITE` | NATS | `full` | `commit` | Integration with NATS (requires local NATS). |
| `router_e2e_smoke_SUITE` | E2E | `fast` | `sanity` | Full request flow (happy path). |
| `router_concurrent_faults_SUITE` | Resilience | `heavy` | `nightly` | Concurrent fault injection scenarios. |
| `router_stress_soak_SUITE` | Stress | `heavy` | `nightly` | Long-running soak tests. |
| ... | ... | ... | ... | ... |

## Coverage Matrix

### 1. Policy & Decisions
*   `router_policy_SUITE`: Core logic.
*   `router_decider_SUITE`: Decision engine integration.
*   `router_policy_store_SUITE`: Storage Backend interactions.

### 2. Intake & Protocol
*   `router_intake_error_codes_SUITE`: Error handling contract.
*   `router_grpc_SUITE`: gRPC interface.

### 3. Resilience & Fault Tolerance
*   `router_circuit_breaker_*_SUITE`: Circuit breaker behavior.
*   `router_*_fault_*_SUITE`: Fault injection scenarios.

### 4. Observability
*   `router_metrics_*_SUITE`: Metrics collection and accuracy.
*   `router_trace_SUITE`: Tracing propagation.

## Execution Tiers

### Sanity Tier (`ROUTER_TEST_LEVEL=sanity`)
**Goal**: Verify the build handles basic requests (~10 seconds).  
**Script**: `./scripts/ct-sanity.sh`  
**Suites** (18 tests):
*   `router_core_SUITE` - Core application startup and routing logic
*   `router_e2e_smoke_SUITE` - Full request flow (happy path)
*   `router_policy_SUITE` - Policy parsing and validation
*   `router_circuit_breaker_smoke_SUITE` - Circuit breaker basic operations

### Fast Tier (`ROUTER_TEST_LEVEL=fast`)
**Goal**: Developer feedback loop (< 1 min).  
**Script**: `./scripts/ct-fast.sh`  
**Suites** (178 tests, ~55s):
*   Core: `router_core_SUITE`, `router_policy_SUITE`, `router_policy_store_SUITE`, `router_policy_validator_SUITE`
*   Consumers: `router_decide_consumer_SUITE`, `router_result_consumer_SUITE`
*   Admin: `router_admin_*_SUITE`, `router_config_validator_SUITE`
*   Security: `router_rbac_SUITE`, `router_compliance_SUITE`, `router_abuse_SUITE`
*   Observability: `router_alerts_test_SUITE`, `router_dashboard_test_SUITE`
*   Infrastructure: `router_ci_enforcement_SUITE`, `router_caf_adapter_enhanced_SUITE`, others

### Full Tier (`ROUTER_TEST_LEVEL=full`)
**Goal**: PR Validation (< 5 mins).  
**Script**: `./scripts/ct-full.sh`  
**Suites** (139 tests, ~55s):
*   Core: `router_config_validator_SUITE`, `router_alerts_test_SUITE`, `router_dashboard_test_SUITE`
*   CI: `router_ci_enforcement_SUITE`, `router_test_structure_SUITE`, `router_compliance_SUITE`
*   Consumers: `router_decide_consumer_SUITE`, `router_result_consumer_SUITE`
*   CAF: `router_caf_adapter_enhanced_SUITE`
*   Errors: `router_error_SUITE`, `router_tenant_allowlist_SUITE`
*   Policy: `router_policy_SUITE`, `router_policy_validator_SUITE`
*   Persistence: `router_sticky_store_SUITE`
*   NATS: `router_nats_contract_validation_SUITE`
*   Admin: `router_admin_grpc_integration_SUITE`
*   Decision: `router_decider_SUITE`, `router_assignment_SUITE`
*   Resilience: `router_circuit_breaker_SUITE`

**Excluded (flaky)**:
*   `router_policy_store_SUITE` - telemetry timeout
*   `router_errors_mapping_SUITE` - parse_error_code
*   `router_core_SUITE` - telemetry timeout

### Integration Gate (`ROUTER_TEST_LEVEL=full`)
**Goal**: Subsystem Connectivity & Contracts.  
**Script**: `./scripts/ct-integration.sh`  
**Suites**:
*   `router_nats_integration_SUITE`
*   `router_policy_integration_SUITE`
*   `router_grpc_integration_SUITE`
*   Result of `ls test/router_*_integration_SUITE.erl`

### Heavy Tier (`ROUTER_TEST_LEVEL=heavy`)
**Goal**: Release Validation.  
**Script**: `./scripts/ct-heavy.sh`  
**Suites**: Stress, Soak, Chaos, Performance Benchmarks.
*   `router_performance_benchmark_SUITE` - benchmarks
*   `router_performance_load_SUITE` - load testing
*   `router_jetstream_soak_SUITE` - soak/endurance
*   `router_extensions_pipeline_load_SUITE` - extensions load
*   `router_observability_performance_SUITE` - metrics performance
*   `router_metrics_labels_performance_SUITE` - labels overhead
