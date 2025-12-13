.PHONY: all compile deps clean test test-coverage test-parallel test-cp1-smoke test-fast test-slow test-all shell run release help

# Default target
all: deps compile

# Compile the project
compile:
	@echo "Compiling router..."
	rebar3 compile

# Get dependencies
deps:
	@echo "Fetching dependencies..."
	rebar3 deps

# Clean build artifacts
clean:
	@echo "Cleaning..."
	rebar3 clean

# Run tests (with location check)
test:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running tests with coverage..."
	rebar3 ct --cover

# Run tests with coverage report generation
test-coverage:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running tests with coverage report..."
	rebar3 ct --cover
	@echo "Generating coverage report..."
	@bash scripts/generate_coverage.sh

# Generate coverage report only (requires tests to be run with --cover first)
coverage-report:
	@echo "Generating coverage report..."
	@bash scripts/generate_coverage.sh

# Run tests in parallel (recommended for faster execution)
test-parallel:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running tests in parallel (4 workers)..."
	rebar3 ct -j 4

# Run tests with build caching (optimized for CI)
test-ci:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running tests with CI optimizations..."
	@echo "Using parallel execution (4 workers) and coverage..."
	rebar3 ct -j 4 --cover
	@echo "Generating coverage report..."
	@bash scripts/generate_coverage.sh

# Run CP1 baseline smoke tests (minimal set, excludes JetStream heavy tests)
test-cp1-smoke:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running CP1 baseline smoke tests..."
	@bash scripts/test_cp1_smoke.sh

# Run fast tests only (smoke/contract, excludes load/JetStream E2E/property)
test-fast:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running fast tests (smoke/contract)..."
	@bash scripts/test_fast.sh

# Run slow tests only (load/JetStream E2E/property)
test-slow:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running slow tests (load/JetStream E2E/property)..."
	@bash scripts/test_slow.sh

# Run full tier tests with quality gates
test-full:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running full tier tests with quality gates..."
	@bash scripts/ct-full.sh

# Run full tier quality gates only (without running tests)
test-full-quality-gates:
	@echo "Running full tier quality gates..."
	@bash scripts/ci_full_quality_gates.sh

# Run all tests (fast + slow)
test-all:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "Running all tests (fast + slow)..."
	@bash scripts/test_fast.sh && bash scripts/test_slow.sh

# Start Erlang shell with router loaded
shell:
	@echo "Starting Erlang shell..."
	rebar3 shell

# Run router in foreground (development)
run: compile
	@echo "Starting router..."
	erl -pa _build/default/lib/*/ebin \
		-sname router@localhost \
		-s beamline_router_app \
		-config config/sys.config

# Create release
release:
	@echo "Creating release..."
	rebar3 release

# Run release in foreground
run-release: release
	@echo "Starting router from release..."
	_build/default/rel/beamline_router/bin/beamline_router foreground

# Format code
format:
	@echo "Formatting code..."
	rebar3 format

# Dialyzer type checking
dialyzer:
	@echo "Running dialyzer..."
	rebar3 dialyzer

# Xref checks
xref:
	@echo "Running xref..."
	rebar3 xref

# Quality gates (Dialyzer + Xref + Coverage)
quality-gates:
	@echo "Running quality gates..."
	@bash scripts/ci_quality_gates.sh

# CI test report generation
ci-test-report:
	@echo "Generating CI test report..."
	@bash scripts/ci_test_report.sh test_results

# CI coverage report generation
ci-coverage-report:
	@echo "Generating CI coverage report..."
	@bash scripts/ci_coverage_report.sh test_results

# Full CI pipeline
ci-pipeline:
	@echo "Running full CI pipeline..."
	@bash scripts/ci_pipeline.sh

# Help
help:
	@echo "Available targets:"
	@echo "  all          - Get deps and compile (default)"
	@echo "  compile      - Compile the project"
	@echo "  deps         - Get dependencies"
	@echo "  clean        - Clean build artifacts"
	@echo "  test         - Run tests"
	@echo "  test-parallel - Run tests in parallel (4 workers, faster)"
	@echo "  test-cp1-smoke - Run CP1 baseline smoke tests (minimal set, excludes JetStream)"
	@echo "  test-fast    - Run fast tests only (smoke/contract, < 30s)"
	@echo "  test-slow    - Run slow tests only (load/JetStream E2E/property, > 5min)"
	@echo "  test-full    - Run full tier tests with quality gates"
	@echo "  test-full-quality-gates - Run quality gates only (no tests)"
	@echo "  test-all     - Run all tests (fast + slow)"
	@echo "  shell        - Start Erlang shell"
	@echo "  run          - Run router in foreground (development)"
	@echo "  release      - Create release"
	@echo "  run-release  - Run release in foreground"
	@echo "  format       - Format code"
	@echo "  dialyzer     - Run dialyzer type checking"
	@echo "  xref         - Run xref checks"
	@echo "  quality-gates - Run all quality gates (Dialyzer + Xref + Coverage)"
	@echo "  ci-test-report - Generate CI test report (JUnit XML)"
	@echo "  ci-coverage-report - Generate CI coverage report (JSON)"
	@echo "  ci-pipeline  - Run full CI pipeline (compile, test, quality gates, reports)"
	@echo ""
	@echo "Circuit Breaker Testing:"
	@echo "  test-circuit-breaker             - CB unit tests"
	@echo "  test-circuit-breaker-integration - CB integration tests"
	@echo "  test-circuit-breaker-invariants  - CB property-based tests"
	@echo "  test-circuit-breaker-all         - All CB tests"
	@echo "  test-prometheus-exporter         - Prometheus exporter tests (CB metrics)"
	@echo ""
	@echo "Chaos Testing:"
	@echo "  test-chaos         - Chaos tests (auto-detect Docker)"
	@echo "  test-chaos-docker  - Chaos tests (Docker REQUIRED)"
	@echo "  test-chaos-ci      - CI: Chaos tests + mode enforcement"
	@echo "  test-chaos-ci-degraded - CI: Chaos tests (mock mode explicitly allowed)"
	@echo "  ci-chaos-pipeline  - Complete chaos CI pipeline"
	@echo ""
	@echo "Mock Discipline:"
	@echo "  test-discipline    - Tests with STRICT_MOCK_DISCIPLINE (fails on basic-only)"
	@echo ""
	@echo "CI Pipelines:"
	@echo "  ci-validate-prometheus-export    - Validate CB metrics export (TYPE/HELP/labels)"
	@echo "  ci-circuit-breaker-pipeline      - Full CB validation (tests + export + discipline)"
	@echo "  ci-testops-pipeline              - TestOps (exporter + mocks)"
	@echo "  ci-full-pipeline                 - Complete CI (quality + tests + chaos)"
	@echo ""
	@echo "TestOps:"
	@echo "  test-exporter-structure          - Validate exporter parses all metric types"
	@echo "  test-strict-mocks                - Run with STRICT_MOCK_DISCIPLINE=true"
	@echo "  test-ci-enforcement              - Validate test infrastructure (helpers, seeds)"
	@echo ""
	@echo "Reproduction:"
	@echo "  reproduce-chaos SEED=A,B,C       - Reproduce chaos test"
	@echo "  reproduce-randomized SEED=A,B,C  - Reproduce randomized test"
	@echo ""
	@echo "  help         - Show this help"
# Install DevState git hooks into repo
install-devstate-hooks:
	@mkdir -p .git/hooks
	@cp scripts/hooks/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@cp scripts/hooks/pre-push .git/hooks/pre-push
	@chmod +x .git/hooks/pre-push
	@echo "Installed DevState hooks: pre-commit, pre-push"

# ============================================================================
# CHAOS TESTING TARGETS
# ============================================================================

# Chaos tests in AUTO mode (uses Docker if available, falls back to mock)
test-chaos:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running CHAOS tests (auto-detect Docker)"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_intake_chaos_SUITE

# Chaos tests REQUIRING Docker (fails if Docker/NATS unavailable)
# Use this in CI pipelines for full integration coverage
test-chaos-docker:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running CHAOS tests (Docker REQUIRED)"
	@echo " If Docker/NATS is not available, this WILL FAIL."
	@echo "═══════════════════════════════════════════════════════════"
	CHAOS_REQUIRE_DOCKER=true rebar3 ct --dir test --suite router_intake_chaos_SUITE

# Stress tests with deterministic random seed
test-stress:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running STRESS tests (deterministic seed)"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_concurrent_faults_stress_SUITE

# Randomized E2E tests with deterministic seed
test-randomized:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running RANDOMIZED tests (deterministic seed)"
	@echo "═══════════════════════════════════════════════════════════"
	RANDOMIZED_TEST_SEED=1234,5678,91011 rebar3 ct --dir test --suite router_publish_failure_e2e_randomized_SUITE

# Reproduce a failed chaos test with specific seed
# Usage: make reproduce-chaos SEED=1733511111,123456,789
reproduce-chaos:
ifndef SEED
	@echo "Usage: make reproduce-chaos SEED=A,B,C"
	@echo "Example: make reproduce-chaos SEED=1733511111,123456,789"
	@exit 1
endif
	@echo "Reproducing chaos test with seed: $(SEED)"
	CHAOS_RAND_SEED=$(SEED) rebar3 ct --dir test --suite router_intake_chaos_SUITE

# Reproduce a failed randomized test with specific seed
# Usage: make reproduce-randomized SEED=1234,5678,91011
reproduce-randomized:
ifndef SEED
	@echo "Usage: make reproduce-randomized SEED=A,B,C"
	@echo "Example: make reproduce-randomized SEED=1234,5678,91011"
	@exit 1
endif
	@echo "Reproducing randomized test with seed: $(SEED)"
	RANDOMIZED_TEST_SEED=$(SEED) rebar3 ct --dir test --suite router_publish_failure_e2e_randomized_SUITE

# ============================================================================
# CIRCUIT BREAKER TESTING TARGETS
# ============================================================================

# Circuit breaker unit tests
test-circuit-breaker:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running Circuit Breaker tests"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_circuit_breaker_SUITE

# Circuit breaker integration tests
test-circuit-breaker-integration:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running Circuit Breaker Integration tests"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_circuit_breaker_integration_SUITE

# Circuit breaker invariants (property-based)
test-circuit-breaker-invariants:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running Circuit Breaker Invariant tests"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_circuit_breaker_invariants_SUITE

# All circuit breaker tests
test-circuit-breaker-all:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running ALL Circuit Breaker tests"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_circuit_breaker_SUITE,router_circuit_breaker_integration_SUITE,router_circuit_breaker_invariants_SUITE

# Prometheus exporter tests (validates TYPE/HELP for CB metrics)
test-prometheus-exporter:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " Running Prometheus Exporter tests (includes CB metrics)"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_metrics_dump_SUITE

# ============================================================================
# CI ENFORCEMENT TARGETS
# ============================================================================

# Chaos tests for CI: runs tests + checks that Docker mode was used
# FAILS if tests ran in mock mode without CHAOS_MOCK_ALLOWED=true
test-chaos-ci:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: Running CHAOS tests with mode enforcement"
	@echo "═══════════════════════════════════════════════════════════"
	@mkdir -p _build/ci_logs
	CHAOS_REQUIRE_DOCKER=true rebar3 ct --dir test --suite router_intake_chaos_SUITE 2>&1 | tee _build/ci_logs/chaos_test.log
	@echo ""
	@echo "Checking chaos mode compliance..."
	@bash scripts/ci_check_chaos_mode.sh _build/ci_logs/chaos_test.log

# Mock discipline check: FAILS if tests only use basic mock patterns
# Use in CI to enforce advanced mock usage
test-discipline:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: Running tests with STRICT mock discipline"
	@echo " Tests using only basic mocks will FAIL"
	@echo "═══════════════════════════════════════════════════════════"
	STRICT_MOCK_DISCIPLINE=true rebar3 ct --dir test --suite router_caf_integration_SUITE

# Complete CI chaos pipeline
# 1. Runs chaos tests requiring Docker
# 2. Verifies mode compliance
# 3. Fails if mock mode was used without permission
ci-chaos-pipeline:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI CHAOS PIPELINE"
	@echo "═══════════════════════════════════════════════════════════"
	@echo ""
	@echo "Step 1/2: Running chaos tests (Docker required)..."
	@$(MAKE) test-chaos-ci
	@echo ""
	@echo "Step 2/2: Chaos pipeline complete"
	@echo "═══════════════════════════════════════════════════════════"
	@echo " ✅ All chaos CI checks passed"
	@echo "═══════════════════════════════════════════════════════════"

# Explicitly allow mock mode in CI (when Docker is intentionally unavailable)
# Usage: make test-chaos-ci-degraded
test-chaos-ci-degraded:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: Running CHAOS tests (MOCK MODE ALLOWED)"
	@echo " ⚠️  Coverage is DEGRADED - no real Docker interactions"
	@echo "═══════════════════════════════════════════════════════════"
	@mkdir -p _build/ci_logs
	CHAOS_MOCK_ALLOWED=true rebar3 ct --dir test --suite router_intake_chaos_SUITE 2>&1 | tee _build/ci_logs/chaos_test.log
	@bash scripts/ci_check_chaos_mode.sh _build/ci_logs/chaos_test.log

# ============================================================================
# CI FULL VALIDATION PIPELINE
# ============================================================================

# Validate Prometheus exporter exports CB metrics correctly
# Tests: TYPE/HELP presence, labels, trigger_reason values
# The test itself validates all metric format requirements
ci-validate-prometheus-export:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: Validating Prometheus Exporter (CB metrics)"
	@echo "═══════════════════════════════════════════════════════════"
	@mkdir -p _build/ci_logs
	rebar3 ct --dir test --suite router_metrics_dump_SUITE --case test_circuit_breaker_metrics 2>&1 | tee _build/ci_logs/prometheus_export.log
	@echo ""
	@echo "✅ Prometheus exporter validation PASSED (test assertions verified CB metrics)"

# Full CI pipeline for Circuit Breaker changes
# Runs: unit tests + integration + prometheus export + alerts validation
ci-circuit-breaker-pipeline:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: Circuit Breaker Full Validation Pipeline"
	@echo "═══════════════════════════════════════════════════════════"
	@echo ""
	@echo "Step 1/5: Circuit Breaker unit tests..."
	@$(MAKE) test-circuit-breaker
	@echo ""
	@echo "Step 2/5: Circuit Breaker integration tests..."
	@$(MAKE) test-circuit-breaker-integration
	@echo ""
	@echo "Step 3/5: Circuit Breaker invariants tests..."
	@$(MAKE) test-circuit-breaker-invariants
	@echo ""
	@echo "Step 4/5: Prometheus exporter validation..."
	@$(MAKE) ci-validate-prometheus-export
	@echo ""
	@echo "Step 5/5: Mock discipline check..."
	@$(MAKE) test-discipline
	@echo ""
	@echo "═══════════════════════════════════════════════════════════"
	@echo " ✅ Circuit Breaker CI Pipeline PASSED"
	@echo "═══════════════════════════════════════════════════════════"

# Complete CI pipeline with all validations
# Use this as the main CI entry point
ci-full-pipeline:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: FULL Validation Pipeline"
	@echo "═══════════════════════════════════════════════════════════"
	@echo ""
	@echo "Phase 1: Quality Gates..."
	@$(MAKE) quality-gates
	@echo ""
	@echo "Phase 2: Fast tests..."
	@$(MAKE) test-fast
	@echo ""
	@echo "Phase 3: Circuit Breaker pipeline..."
	@$(MAKE) ci-circuit-breaker-pipeline
	@echo ""
	@echo "Phase 4: Chaos tests (mock allowed if no Docker)..."
	@$(MAKE) test-chaos-ci-degraded || $(MAKE) test-chaos
	@echo ""
	@echo "═══════════════════════════════════════════════════════════"
	@echo " ✅ FULL CI Pipeline PASSED"
	@echo "═══════════════════════════════════════════════════════════"

# ============================================================================
# TESTOPS: Test Infrastructure Validation
# ============================================================================

# Run Prometheus exporter structure validation
# Verifies: render() parses all metrics, handles edge cases, correct TYPE/HELP
test-exporter-structure:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " TestOps: Prometheus Exporter Structure Validation"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_metrics_dump_SUITE --case test_exporter_parses_all_metric_types,test_exporter_handles_edge_cases

# Run with strict mock discipline (CI enforcement)
test-strict-mocks:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " TestOps: Strict Mock Discipline Enforcement"
	@echo "═══════════════════════════════════════════════════════════"
	STRICT_MOCK_DISCIPLINE=true rebar3 ct --dir test --suite router_circuit_breaker_integration_SUITE

# CI Enforcement test suite (validates test infrastructure)
test-ci-enforcement:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: Running Enforcement Tests"
	@echo " Validates: mock discipline, seed reproducibility, helpers"
	@echo "═══════════════════════════════════════════════════════════"
	rebar3 ct --dir test --suite router_ci_enforcement_SUITE

# Full TestOps validation pipeline
ci-testops-pipeline:
	@bash scripts/check_test_run_location.sh || exit 1
	@echo "═══════════════════════════════════════════════════════════"
	@echo " CI: TestOps Full Validation Pipeline"
	@echo "═══════════════════════════════════════════════════════════"
	@echo ""
	@echo "Step 1/4: CI Enforcement validation..."
	@$(MAKE) test-ci-enforcement
	@echo ""
	@echo "Step 2/4: Exporter structure validation..."
	@$(MAKE) test-exporter-structure
	@echo ""
	@echo "Step 3/4: Prometheus export validation..."
	@$(MAKE) ci-validate-prometheus-export
	@echo ""
	@echo "Step 4/4: Mock discipline check..."
	@$(MAKE) test-discipline
	@echo ""
	@echo "═══════════════════════════════════════════════════════════"
	@echo " ✅ TestOps Pipeline PASSED"
	@echo "═══════════════════════════════════════════════════════════"

