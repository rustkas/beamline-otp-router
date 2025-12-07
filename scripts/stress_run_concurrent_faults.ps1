# Stress-run script for router_concurrent_faults_SUITE (PowerShell)
# Runs the suite multiple times to detect flaky tests
#
# Usage:
#   .\stress_run_concurrent_faults.ps1 [iterations] [suite_name]
#
# Examples:
#   .\stress_run_concurrent_faults.ps1 10                    # Run 10 iterations of all tests
#   .\stress_run_concurrent_faults.ps1 20 router_concurrent_faults_SUITE  # Run specific suite 20 times

param(
    [int]$Iterations = 10,
    [string]$SuiteName = "router_concurrent_faults_SUITE"
)

# Validate iterations
if ($Iterations -lt 1) {
    Write-Host "Error: Iterations must be a positive integer" -ForegroundColor Red
    exit 1
}

# Check if we're in the right directory
if (-not (Test-Path "rebar.config")) {
    Write-Host "Error: Must run from apps/otp/router directory" -ForegroundColor Red
    exit 1
}

# Check if rebar3 is available
if (-not (Get-Command rebar3 -ErrorAction SilentlyContinue)) {
    Write-Host "Error: rebar3 not found. Please install rebar3 first." -ForegroundColor Red
    exit 1
}

Write-Host "=== Stress Run: $SuiteName ===" -ForegroundColor Green
Write-Host "Iterations: $Iterations"
Write-Host "Suite: $SuiteName"
Write-Host ""

# Create results directory
$ResultsDir = "stress_run_results_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
New-Item -ItemType Directory -Path $ResultsDir -Force | Out-Null

# Track results
$Passed = 0
$Failed = 0
$TotalTime = 0

# Run iterations
for ($i = 1; $i -le $Iterations; $i++) {
    Write-Host "[$i/$Iterations] Running $SuiteName..." -ForegroundColor Yellow
    
    $IterationStart = Get-Date
    
    # Run test suite
    $LogFile = Join-Path $ResultsDir "run_$i.log"
    $LogDir = Join-Path $ResultsDir "run_$i"
    
    $Result = & rebar3 ct --suite "test/$SuiteName" --logdir $LogDir 2>&1 | Tee-Object -FilePath $LogFile
    
    $IterationEnd = Get-Date
    $IterationTime = ($IterationEnd - $IterationStart).TotalSeconds
    $TotalTime += $IterationTime
    
    if ($LASTEXITCODE -eq 0) {
        $Passed++
        Write-Host "✓ Passed ($([math]::Round($IterationTime, 2))s)" -ForegroundColor Green
    } else {
        $Failed++
        Write-Host "✗ Failed ($([math]::Round($IterationTime, 2))s)" -ForegroundColor Red
        Write-Host "  Log: $LogFile" -ForegroundColor Yellow
    }
}

# Calculate statistics
$AvgTime = [math]::Round($TotalTime / $Iterations, 2)
$SuccessRate = [math]::Round(($Passed * 100) / $Iterations, 2)

Write-Host ""
Write-Host "=== Stress Run Results ===" -ForegroundColor Green
Write-Host "Total iterations: $Iterations"
Write-Host "Passed: $Passed" -ForegroundColor Green
Write-Host "Failed: $Failed" -ForegroundColor Red
Write-Host "Success rate: $SuccessRate%"
Write-Host "Total time: $([math]::Round($TotalTime, 2))s"
Write-Host "Average time per run: ${AvgTime}s"
Write-Host "Results directory: $ResultsDir"

# Determine exit code
if ($Failed -eq 0) {
    Write-Host "✓ All iterations passed - no flaky tests detected" -ForegroundColor Green
    exit 0
} else {
    Write-Host "✗ $Failed iteration(s) failed - potential flaky tests detected" -ForegroundColor Red
    Write-Host "Review logs in $ResultsDir/ for details" -ForegroundColor Yellow
    exit 1
}

