# Fast Test Runner (PowerShell)
# Runs fast test suites only (excludes load/JetStream E2E/property tests)
#
# Usage:
#   .\scripts\test_fast.ps1
#   .\scripts\test_fast.ps1 -Verbose
#
# Exit codes:
#   0 - All fast tests passed
#   1 - Test execution failed
#   2 - Compilation failed

param(
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RouterDir = Split-Path -Parent $ScriptDir
Set-Location $RouterDir

# Fast test suites (smoke/contract)
# Note: These suites are marked with @test_category fast in their module comments
$FastTestSuites = @(
    "router_core_SUITE",              # @test_category cp1_smoke, fast
    "router_e2e_smoke_SUITE",         # @test_category cp1_smoke, fast
    "router_rbac_SUITE",              # @test_category cp1_smoke, fast
    "router_policy_enforcement_SUITE", # @test_category cp1_smoke, fast
    "router_decider_SUITE",           # @test_category cp1_smoke, fast
    "router_policy_store_SUITE",      # @test_category cp1_smoke, fast
    "router_error_SUITE",             # @test_category cp1_smoke, fast
    "router_grpc_SUITE",              # @test_category fast
    "router_grpc_integration_SUITE",   # @test_category fast
    "router_caf_adapter_unit_SUITE",   # @test_category fast
    "router_core_telemetry_contract_SUITE", # @test_category fast
    "router_secrets_logging_SUITE",    # @test_category fast
    "router_nats_contract_validation_SUITE" # @test_category fast
)

# Slow test suites (excluded from fast run)
$SlowTestSuites = @(
    "router_jetstream_e2e_SUITE",
    "router_delivery_count_tracking_SUITE",
    "router_result_consumer_SUITE",
    "router_caf_adapter_SUITE",
    "router_caf_adapter_enhanced_SUITE",
    "router_nats_subscriber_caf_SUITE",
    "router_decider_prop_SUITE",
    "router_policy_store_prop_SUITE",
    "router_normalize_boolean_prop_SUITE",
    "router_options_merge_prop_SUITE",
    "router_policy_store_load_SUITE",
    "router_idempotency_SUITE",
    "router_tenant_allowlist_SUITE",
    "router_policy_store_fault_tolerance_SUITE",
    "router_admin_grpc_integration_SUITE",
    "router_admin_grpc_concurrency_SUITE",
    "router_assignment_SUITE",
    "router_sticky_store_SUITE",
    "router_policy_SUITE",
    "router_policy_validator_SUITE",
    "router_ets_guard_SUITE",
    "router_error_status_SUITE"
)

Write-Host "=== Fast Test Suite (Smoke/Contract) ===" -ForegroundColor Green
Write-Host ""
Write-Host "Running fast test suites (excludes load/JetStream E2E/property tests)"
Write-Host ""

Write-Host "Fast Test Suites ($($FastTestSuites.Count) suites):" -ForegroundColor Yellow
foreach ($suite in $FastTestSuites) {
    Write-Host "  ✓ $suite"
}
Write-Host ""

Write-Host "Excluded Slow Test Suites ($($SlowTestSuites.Count) suites):" -ForegroundColor Yellow
foreach ($suite in $SlowTestSuites) {
    Write-Host "  ✗ $suite"
}
Write-Host ""

# Check if rebar3 is available
if (-not (Get-Command rebar3 -ErrorAction SilentlyContinue)) {
    Write-Host "Error: rebar3 not found. Please install rebar3 first." -ForegroundColor Red
    exit 1
}

# Compile test suites
Write-Host "Compiling test suites..." -ForegroundColor Green
$compileResult = & rebar3 compile
if ($LASTEXITCODE -ne 0) {
    Write-Host "Compilation failed" -ForegroundColor Red
    exit 2
}

# Build test suites list for rebar3 ct
$SuitesArg = ""
foreach ($suite in $FastTestSuites) {
    $SuitesArg += " --suite test/$suite"
}

# Run Common Test with fast test suites
Write-Host "Running fast tests..." -ForegroundColor Green
Write-Host ""

if ($Verbose) {
    & rebar3 ct --dir test $SuitesArg --verbose
} else {
    & rebar3 ct --dir test $SuitesArg
}

$ExitCode = $LASTEXITCODE

if ($ExitCode -eq 0) {
    Write-Host ""
    Write-Host "✓ All fast tests passed" -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "✗ Some fast tests failed (exit code: $ExitCode)" -ForegroundColor Red
}

exit $ExitCode

