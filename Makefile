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
	@echo "  help         - Show this help"
# Install DevState git hooks into repo
install-devstate-hooks:
	@mkdir -p .git/hooks
	@cp scripts/hooks/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@cp scripts/hooks/pre-push .git/hooks/pre-push
	@chmod +x .git/hooks/pre-push
	@echo "Installed DevState hooks: pre-commit, pre-push"
