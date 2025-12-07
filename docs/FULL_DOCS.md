# FULL_DOCS.md — Unified Router Documentation (Erlang/OTP)

Purpose: collect key materials about the Router Core (CP1) in a single file for convenient transfer, training, and navigation.

This file aggregates:
- Router architecture and core modules
- CP1 operational readiness and staging rollout plan
- gRPC API and invocation examples
- Configuration and contract rules
- Observability and alerts (Prometheus rules when/if needed)
- Build, test, and Dialyzer commands
- Training video topics
- Windows/WSL usage notes

Note: links and sections refer to `apps/otp/router/docs/`. If any referenced file is not yet present, the section remains as a placeholder for future content.

## Table of Contents
1. Overview and CP1 goals
2. Architecture and modules
3. Operational Readiness (OPERATIONAL_GUIDE.md)
4. gRPC API (GRPC_API.md)
5. Configuration (CONFIG.md)
6. Contracts and validation (API_CONTRACTS.md)
7. Observability and alerts (PROMETHEUS_ALERTS.md)
8. Verification commands and tests
9. Smoke tests and staging rollout
10. Training videos plan
11. Windows/WSL notes

---

## 1) Overview and CP1 Goals
- Router Core (Erlang/OTP) delivers the minimal CP1 scope:
  - gRPC service `Router.Decide` (ABI compatible)
  - Routing policy application (weights, sticky, fallback)
  - Basic integrations (NATS — mock/stub in CP1)
  - Tests (Common Test: core and gRPC smoke)
  - Documentation for operational readiness and configuration
- CP1 limitations:
  - Telemetry/Prometheus/OTel excluded from dependencies (per current requirements)
  - Minimal algorithm (weights/sticky/fallback)

## 2) Architecture and Modules
- OTP process tree:
  - `beamline_router_sup` (supervisor)
    - `router_policy_store` (gen_server) — ETS policy cache, loaded from fixtures
    - `router_grpc_sup` (supervisor) — gRPC server (grpcbox)
    - `router_nats` (gen_server) — NATS connection management with resilience
- Key modules:
  - `router_core.erl` — decision interface
  - `router_decider.erl` — provider selection algorithm
  - `router_policy_store.erl` — ETS cache, JSON fixtures loader
  - `router_grpc.erl` — gRPC Router.Decide implementation
  - `router_nats.erl` — NATS client with connection resilience (reconnection, queueing, fail-open)
- **NATS Connection Resilience**: Router handles NATS/JetStream failures gracefully:
  - Automatic reconnection with exponential backoff
  - Message queueing during disconnections (up to configurable limit)
  - Fail-open mode for degraded operation
  - Comprehensive metrics and logging for monitoring
  - **See**: `docs/NATS_CONNECTION_RESILIENCE.md` for complete documentation
- **Concurrent Fault Tolerance**: Router handles simultaneous failures gracefully:
  - ACK errors + tenant validation failures (concurrent messages)
  - NAK + publish failures with recovery
  - Consumer restart during faults
  - Tenant isolation under concurrent faults
  - **See**: `docs/RELIABILITY_FAULT_TOLERANCE.md` for reliability guarantees and `test/CONCURRENT_FAULTS_TEST_DOCUMENTATION.md` for test coverage

## 3) Operational Readiness (OPERATIONAL_GUIDE.md)
- Includes:
  - Pre‑production checklist
  - Configuration templates (TLS, timeouts, reconnects for NATS)
  - Smoke tests (step‑by‑step health checks)
  - Staged rollout guidance and emergency procedures
  - Monitoring and operational counters (without mandatory Prometheus integration)
- Practical checks:
  - NATS payload size limit before JSON parsing
  - Schema version validation (missing/unsupported)
  - Retry tuning (base and max) per SLA/latency guidelines
  - Publication controls (kill‑switch, allowlist)

## 4) gRPC API (GRPC_API.md)
- Includes:
  - Endpoints description: `Router.Decide`, optionally `RouterAdmin`
  - `grpcurl` examples with headers and request bodies
  - Retryability rules (UNAVAILABLE vs RESOURCE_EXHAUSTED)
  - Canonical reasons and error codes
  - Playbook for diagnostics and correlation
- Example `grpcurl` (Decide, illustrative):
  - `grpcurl -plaintext -d '{"message":{"type":"chat","payload":"..."}}' localhost:9000 beamline.flow.v1.Router/Decide`

## 5) Configuration (CONFIG.md)
- Includes:
  - NATS parameters (TLS, limits, timeouts, reconnects)
  - Publication controls (kill‑switch, allowlist)
  - Basic gRPC server settings

## 6) Contracts and Validation (API_CONTRACTS.md)
- Includes:
  - Schema version validation rules (required, supported versions)
  - Request/response formats (ABI compatibility)
  - Payload size constraints

## 7) Observability and Alerts (PROMETHEUS_ALERTS.md)
- Rule structure (practical, simplified):
- **Extensions Operations**: `EXTENSIONS_RUNBOOK.md` - Operational runbook for Extension Registry and Extensions (health checks, alerts, rollout/rollback)
  - Critical: service unavailability, high error rate, deadline violations
  - Warning: latency growth, retry degradation, increasing backlog
  - Informational: restarts, switches, control counters
- **NATS Publish Failure Monitoring**:
  - `NATS_PUBLISH_FAILURE_BEHAVIOR.md` - Complete behavior specification for publish/publish_with_ack failures
  - `NATS_PUBLISH_FAILURE_MONITORING.md` - Operational monitoring and alerting guide (SRE recommendations)
  - `NATS_PUBLISH_FAILURE_METRICS_ALERTS.md` - SRE recommendations for metrics and alerts (legacy, see NATS_PUBLISH_FAILURE_MONITORING.md)
  - `NATS_CONNECTION_RESILIENCE.md` - Connection resilience documentation
- Operational notes:
  - Thresholds tuned on staging
  - Alerts paired with dashboards
  - Prometheus integration not mandatory in CP1; rules act as a blueprint

## 8) Verification Commands and Tests
- Generate and build:
  - `rebar3 clean`
  - `rebar3 protobuf compile`
  - `rebar3 compile`
- Tests:
  - `rebar3 ct --suite test/router_core_SUITE`
  - `rebar3 ct --suite test/router_grpc_SUITE`
  - `rebar3 ct --suite test/router_admin_grpc_concurrency_SUITE`
  - `rebar3 ct -j 4` - Run all tests in parallel with 4 workers (recommended for faster execution)
- **NATS Connection Resilience Tests** (77 tests total):
  - `rebar3 ct --suite test/router_nats_connection_failure_SUITE` (22 tests, ~30-60s, stable)
  - `rebar3 ct --suite test/router_jetstream_fault_injection_SUITE` (15 tests, ~20-40s, stable)
  - `rebar3 ct --suite test/router_nats_integration_SUITE` (10 tests, ~15-30s, stable)
  - `rebar3 ct --suite test/router_nats_publish_failure_SUITE` (29 tests, ~30-50s, stable) - **Publish/Publish_with_ack failure scenarios with upper-level behavior tests**
  - `rebar3 ct --suite test/router_nats_performance_SUITE` (7 tests, ~10-20s, slow - nightly only)
  - **See**: `test/RUN_TESTS.md` for test execution guide and CI/CD integration
  - **See**: `docs/NATS_PUBLISH_FAILURE_BEHAVIOR.md` for complete publish failure behavior specification
  - Property-based tests (PropEr):
    - `rebar3 as test ct --suite test/router_policy_store_prop_SUITE`
    - `rebar3 as test ct --suite test/router_options_merge_prop_SUITE`
    - `rebar3 as test ct --suite test/router_normalize_boolean_prop_SUITE`
    - `rebar3 as test ct --suite test/router_decider_prop_SUITE`
- Dialyzer and full check:
  - `rebar3 dialyzer`
  - `rebar3 check`
- Note: tests use ephemeral ports (0) to allow parallel runs without conflicts. Parallel execution with `-j 4` significantly reduces test execution time.
- **Property-based tests**: All property-based tests use PropEr and include `proper.hrl` in test profile. PropEr is available in test profile (`rebar3 as test`). Runtime check for PropEr availability is done in `prop_*` functions, with skip fallbacks when PropEr is not available. All property tests follow a unified style:
  - `-include_lib("proper/include/proper.hrl")` is always included at the top of the file (in test profile)
  - Runtime check: `case code:which(proper) of non_existing -> {skip, ...}; _ -> ... end`
  - Skip fallback functions: `prop_*_skip/1` for when PropEr is not available

## 9) Smoke Tests and Staging Rollout
- Before staging:
  - Follow the OPERATIONAL_GUIDE checklist
  - Confirm payload limit and schema version validation
  - Tune retry parameters per SLA
  - Run 5 smoke tests and monitor counters/spans
  - Align kill‑switch/allowlist with access policies

## 10) Training Videos Plan
- Recommended topics:
  - Router ↔ CAF architecture overview
  - Quickstart with NATS TLS
  - DecideRequest validation (versions and tests)
  - ExecAssignment publication (flags and subjects)
  - Deadlines (`deadline_ms`) and overrides
  - Retry logic: exponential backoff + jitter
  - Publication controls: kill‑switch, allowlist
  - Observability: counters and spans
  - Error handling: NATS limits, reasons, versions
  - Smoke tests and staging dashboards

## 11) Windows/WSL Notes
- Use local WSL paths, avoid UNC `\\wsl.localhost\...` for `rebar3`
- Correct (inside WSL):
  - `cd /home/rustkas/aigroup/apps/otp/router`
  - `rebar3 compile`
- Incorrect (UNC path):
  - `cd \\wsl.localhost\Ubuntu\home\rustkas\aigroup\apps\otp\router`
  - `rebar3 compile`

---

## How to Use This File
- Use as a single entry point for analysis and training preparation
- For transfer, replace brief annotations with full text from corresponding files in `apps/otp/router/docs/` when needed
- Keep sections updated as CP1 artifacts evolve

Хотите, чтобы я сразу применил эту английскую версию к `apps/otp/router/docs/FULL_DOCS.md`? Если да — подтверждите, и я обновлю файл.
        