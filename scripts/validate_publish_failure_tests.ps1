# PowerShell script to validate stability of router_nats_publish_failure_SUITE
# Runs the suite multiple times to verify no flaky tests

param(
    [int]$Iterations = 5  # Default: 5 iterations
)

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RouterDir = Join-Path $ScriptDir "..\apps\otp\router"

Set-Location $RouterDir

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Publish Failure Test Suite Stability Check" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Suite: router_nats_publish_failure_SUITE"
Write-Host "Iterations: $Iterations"
Write-Host ""

# Track results
$Passed = 0
$Failed = 0
$TotalTime = 0

# Run tests multiple times
for ($i = 1; $i -le $Iterations; $i++) {
    Write-Host "----------------------------------------" -ForegroundColor Yellow
    Write-Host "Iteration $i/$Iterations" -ForegroundColor Yellow
    Write-Host "----------------------------------------" -ForegroundColor Yellow
    
    $StartTime = Get-Date
    
    $LogFile = "stability_run_$i.log"
    $LogDir = "ct_logs_stability_$i"
    
    $Result = & rebar3 ct --suite test/router_nats_publish_failure_SUITE --logdir $LogDir *> $LogFile
    
    $EndTime = Get-Date
    $Duration = ($EndTime - $StartTime).TotalSeconds
    $TotalTime += $Duration
    
    if ($LASTEXITCODE -eq 0) {
        $Passed++
        Write-Host "✓ PASSED ($([math]::Round($Duration, 2))s)" -ForegroundColor Green
    } else {
        $Failed++
        Write-Host "✗ FAILED ($([math]::Round($Duration, 2))s)" -ForegroundColor Red
        Write-Host "  See: $LogFile" -ForegroundColor Red
    }
    
    Write-Host ""
}

# Summary
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Stability Check Summary" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Total iterations: $Iterations"
Write-Host "Passed: $Passed" -ForegroundColor Green
Write-Host "Failed: $Failed" -ForegroundColor $(if ($Failed -eq 0) { "Green" } else { "Red" })
Write-Host "Average duration: $([math]::Round($TotalTime / $Iterations, 2))s"
Write-Host ""

if ($Failed -eq 0) {
    Write-Host "✓ All iterations passed - suite is stable" -ForegroundColor Green
    exit 0
} else {
    Write-Host "✗ Some iterations failed - suite may be flaky" -ForegroundColor Red
    Write-Host ""
    Write-Host "Failed runs:" -ForegroundColor Red
    for ($i = 1; $i -le $Iterations; $i++) {
        $LogFile = "stability_run_$i.log"
        if (Test-Path $LogFile) {
            if (-not (Select-String -Path $LogFile -Pattern "passed" -Quiet)) {
                Write-Host "  - Iteration $i : $LogFile" -ForegroundColor Red
            }
        }
    }
    exit 1
}

