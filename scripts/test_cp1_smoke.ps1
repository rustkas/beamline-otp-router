# CP1 Baseline Smoke Test Runner (PowerShell)
# Runs minimal CP1 test suite without heavy JetStream tests
#
# Usage:
#   .\scripts\test_cp1_smoke.ps1
#   .\scripts\test_cp1_smoke.ps1 -Verbose
#
# Exit codes:
#   0 - All CP1 smoke tests passed
#   1 - Test execution failed
#   2 - Compilation failed

param(
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RouterDir = Split-Path -Parent $ScriptDir
Set-Location $RouterDir

# CP1 Baseline test suites (minimal smoke set)
# Note: These suites are marked with @test_category cp1_smoke, fast in their module comments
$CP1SmokeSuites = @(
    "router_core_SUITE",              # @test_category cp1_smoke, fast
    "router_e2e_smoke_SUITE",         # @test_category cp1_smoke, fast
    "router_rbac_SUITE",              # @test_category cp1_smoke, fast
    "router_policy_enforcement_SUITE", # @test_category cp1_smoke, fast
    "router_decider_SUITE",           # @test_category cp1_smoke, fast
    "router_policy_store_SUITE",      # @test_category cp1_smoke, fast
    "router_error_SUITE"             # @test_category cp1_smoke, fast
)

# CP2+ test suites (excluded from CP1 smoke)
$CP2ExcludedSuites = @(
    "router_jetstream_e2e_SUITE",           # Heavy JetStream E2E tests
    "router_delivery_count_tracking_SUITE", # JetStream delivery count
    "router_idempotency_SUITE",             # CP2+ idempotency
    "router_tenant_allowlist_SUITE",        # CP2+ tenant validation
    "router_result_consumer_SUITE",         # May use JetStream features
    "router_caf_adapter_SUITE",             # May use JetStream features
    "router_caf_adapter_enhanced_SUITE",    # Enhanced features
    "router_nats_subscriber_caf_SUITE"      # NATS/JetStream integration
)

Write-Host "=== CP1 Baseline Smoke Test Suite ===" -ForegroundColor Green
Write-Host ""
Write-Host "Running minimal CP1 test suite (excluding JetStream heavy tests)"
Write-Host ""

Write-Host "CP1 Baseline Test Suites:" -ForegroundColor Yellow
foreach ($suite in $CP1SmokeSuites) {
    Write-Host "  ✓ $suite"
}
Write-Host ""

Write-Host "Excluded CP2+ Test Suites:" -ForegroundColor Yellow
foreach ($suite in $CP2ExcludedSuites) {
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
foreach ($suite in $CP1SmokeSuites) {
    $SuitesArg += " --suite test/$suite"
}

# Run Common Test with CP1 smoke suites
Write-Host "Running CP1 smoke tests..." -ForegroundColor Green
Write-Host ""

if ($Verbose) {
    & rebar3 ct --dir test $SuitesArg --verbose
} else {
    & rebar3 ct --dir test $SuitesArg
}

$ExitCode = $LASTEXITCODE

if ($ExitCode -eq 0) {
    Write-Host ""
    Write-Host "✓ All CP1 smoke tests passed" -ForegroundColor Green
} else {
    Write-Host ""
    Write-Host "✗ Some CP1 smoke tests failed (exit code: $ExitCode)" -ForegroundColor Red
}

exit $ExitCode

