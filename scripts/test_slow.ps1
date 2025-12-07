# Slow Test Runner (PowerShell)
# Runs slow test suites only (load, JetStream E2E, property-based tests)
#
# Usage:
#   .\scripts\test_slow.ps1
#   .\scripts\test_slow.ps1 -Verbose
#
# Exit codes:
#   0 - All slow tests passed
#   1 - Test execution failed
#   2 - Compilation failed

param(
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RouterDir = Split-Path -Parent $ScriptDir
Set-Location $RouterDir

# Slow test suites (load/JetStream E2E/property)
$SlowTestSuites = @(
    # JetStream E2E
    "router_jetstream_e2e_SUITE",
    "router_delivery_count_tracking_SUITE",
    "router_result_consumer_SUITE",
    "router_caf_adapter_SUITE",
    "router_caf_adapter_enhanced_SUITE",
    "router_nats_subscriber_caf_SUITE",
    # Property-based
    "router_decider_prop_SUITE",
    "router_policy_store_prop_SUITE",
    "router_normalize_boolean_prop_SUITE",
    "router_options_merge_prop_SUITE",
    # Load
    "router_policy_store_load_SUITE",
    # CP2+ Features
    "router_idempotency_SUITE",
    "router_tenant_allowlist_SUITE",
    # Advanced Integration
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

Write-Host "=== Slow Test Suite (Load/JetStream E2E/Property) ===" -ForegroundColor Green
Write-Host ""
Write-Host "Running slow test suites (load, JetStream E2E, property-based tests)"
Write-Host ""

Write-Host "Slow Test Suites ($($SlowTestSuites.Count) suites):" -ForegroundColor Yellow
foreach ($suite in $SlowTestSuites) {
    Write-Host "  ✓ $suite"
}
Write-Host ""

Write-Host "Note: These tests may take > 5 minutes to complete" -ForegroundColor Yellow
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

# Run each test suite individually with timing
Write-Host "Running slow tests (with timing)..." -ForegroundColor Green
Write-Host ""

# Arrays to store timing data
$SuiteTimes = @()
$SuiteNames = @()
$SuiteExitCodes = @()
$TotalStart = Get-Date

$FailedSuites = 0
$PassedSuites = 0

# Run each suite individually
foreach ($suite in $SlowTestSuites) {
    Write-Host "Running $suite..." -ForegroundColor Yellow
    
    $SuiteStart = Get-Date
    
    if ($Verbose) {
        & rebar3 ct --dir test --suite "test/$suite" --verbose
    } else {
        # Suppress output but keep errors visible
        & rebar3 ct --dir test --suite "test/$suite" *> $null
    }
    
    $SuiteExit = $LASTEXITCODE
    $SuiteEnd = Get-Date
    $SuiteDuration = ($SuiteEnd - $SuiteStart).TotalSeconds
    
    # Store timing data
    $SuiteNames += $suite
    $SuiteTimes += [int]$SuiteDuration
    $SuiteExitCodes += $SuiteExit
    
    # Format duration
    if ($SuiteDuration -lt 60) {
        $DurationStr = "$([int]$SuiteDuration)s"
    } else {
        $Minutes = [math]::Floor($SuiteDuration / 60)
        $Seconds = [int]($SuiteDuration % 60)
        $DurationStr = "${Minutes}m ${Seconds}s"
    }
    
    if ($SuiteExit -eq 0) {
        Write-Host "  ✓ $suite`: $DurationStr" -ForegroundColor Green
        $PassedSuites++
    } else {
        Write-Host "  ✗ $suite`: $DurationStr (FAILED)" -ForegroundColor Red
        $FailedSuites++
    }
}

$TotalEnd = Get-Date
$TotalDuration = ($TotalEnd - $TotalStart).TotalSeconds

# Format total duration
if ($TotalDuration -lt 60) {
    $TotalDurationStr = "$([int]$TotalDuration)s"
} elseif ($TotalDuration -lt 3600) {
    $Minutes = [math]::Floor($TotalDuration / 60)
    $Seconds = [int]($TotalDuration % 60)
    $TotalDurationStr = "${Minutes}m ${Seconds}s"
} else {
    $Hours = [math]::Floor($TotalDuration / 3600)
    $Remaining = $TotalDuration % 3600
    $Minutes = [math]::Floor($Remaining / 60)
    $Seconds = [int]($Remaining % 60)
    $TotalDurationStr = "${Hours}h ${Minutes}m ${Seconds}s"
}

# Print summary
Write-Host ""
Write-Host "=== Slow Test Execution Summary ===" -ForegroundColor Green
Write-Host ""

# Individual suite timings
Write-Host "Suite Execution Times:" -ForegroundColor Yellow
for ($i = 0; $i -lt $SuiteNames.Count; $i++) {
    $SuiteName = $SuiteNames[$i]
    $SuiteTime = $SuiteTimes[$i]
    $SuiteExit = $SuiteExitCodes[$i]
    
    # Format duration
    if ($SuiteTime -lt 60) {
        $TimeStr = "${SuiteTime}s"
    } else {
        $Minutes = [math]::Floor($SuiteTime / 60)
        $Seconds = $SuiteTime % 60
        $TimeStr = "${Minutes}m ${Seconds}s"
    }
    
    $Status = if ($SuiteExit -eq 0) { "✓" } else { "✗" }
    $Color = if ($SuiteExit -eq 0) { "Green" } else { "Red" }
    
    Write-Host "  $Status $($SuiteName.PadRight(50)) $TimeStr" -ForegroundColor $Color
}

Write-Host ""

# Total duration
Write-Host "Total Duration: $TotalDurationStr" -ForegroundColor Yellow
Write-Host ""

# Top 5 slowest suites
Write-Host "Top 5 Slowest Suites:" -ForegroundColor Yellow

# Create sorted indices by time (descending)
$SortedIndices = 0..($SuiteNames.Count - 1) | Sort-Object { $SuiteTimes[$_] } -Descending

# Print top 5
$TopN = [Math]::Min(5, $SortedIndices.Count)
for ($i = 0; $i -lt $TopN; $i++) {
    $idx = $SortedIndices[$i]
    $SuiteName = $SuiteNames[$idx]
    $SuiteTime = $SuiteTimes[$idx]
    
    # Format duration
    if ($SuiteTime -lt 60) {
        $TimeStr = "${SuiteTime}s"
    } else {
        $Minutes = [math]::Floor($SuiteTime / 60)
        $Seconds = $SuiteTime % 60
        $TimeStr = "${Minutes}m ${Seconds}s"
    }
    
    Write-Host "  $($i + 1). $($SuiteName.PadRight(50)) $TimeStr"
}

Write-Host ""

# Overall result
if ($FailedSuites -eq 0) {
    Write-Host "✓ All slow tests passed ($PassedSuites suites, $TotalDurationStr)" -ForegroundColor Green
    $ExitCode = 0
} else {
    Write-Host "✗ Some slow tests failed ($FailedSuites failed, $PassedSuites passed, $TotalDurationStr)" -ForegroundColor Red
    $ExitCode = 1
}

exit $ExitCode

