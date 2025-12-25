# PowerShell script to run R13 Metrics Under Faults tests
# Usage: .\run_r13_tests.ps1 [test-group|all]

param(
    [string]$TestGroup = "all"
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RouterDir = $ScriptDir
$OtpDir = Split-Path -Parent (Split-Path -Parent $ScriptDir)

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "R13 Metrics Under Faults Tests" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Test Group: $TestGroup"
Write-Host "Router Directory: $RouterDir"
Write-Host "OTP Directory: $OtpDir"
Write-Host ""

# Check if rebar3 is available
if (-not (Get-Command rebar3 -ErrorAction SilentlyContinue)) {
    Write-Host "Error: rebar3 is not installed or not in PATH" -ForegroundColor Red
    exit 1
}

# Compile first
Write-Host "Step 1: Compiling..." -ForegroundColor Yellow
Set-Location $OtpDir
try {
    rebar3 compile
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Error: Compilation failed" -ForegroundColor Red
        exit 1
    }
} catch {
    Write-Host "Error: Compilation failed: $_" -ForegroundColor Red
    exit 1
}

Write-Host ""
Write-Host "Step 2: Running R13 tests..." -ForegroundColor Yellow

# Run tests based on group
Set-Location $RouterDir

$LogDir = "ct_logs/r13"
New-Item -ItemType Directory -Force -Path $LogDir | Out-Null

switch ($TestGroup) {
    "all" {
        Write-Host "Running all R13 test groups..." -ForegroundColor Green
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --logdir $LogDir
    }
    "aggregation" {
        Write-Host "Running aggregation tests..." -ForegroundColor Green
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group aggregation_tests --logdir $LogDir
    }
    "rate" {
        Write-Host "Running rate tests..." -ForegroundColor Green
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group rate_tests --logdir $LogDir
    }
    "cardinality" {
        Write-Host "Running cardinality tests..." -ForegroundColor Green
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group cardinality_tests --logdir $LogDir
    }
    "combined" {
        Write-Host "Running combined tests..." -ForegroundColor Green
        rebar3 as test ct --suite router_metrics_under_faults_SUITE --group combined_tests --logdir $LogDir
    }
    default {
        Write-Host "Error: Unknown test group: $TestGroup" -ForegroundColor Red
        Write-Host "Available groups: all, aggregation, rate, cardinality, combined"
        exit 1
    }
}

$ExitCode = $LASTEXITCODE

Write-Host ""
if ($ExitCode -eq 0) {
    Write-Host "✅ All tests passed!" -ForegroundColor Green
} else {
    Write-Host "❌ Some tests failed. Check logs in $LogDir/" -ForegroundColor Red
}

Write-Host ""
Write-Host "Test logs location: $RouterDir/$LogDir/" -ForegroundColor Cyan
Write-Host ""

exit $ExitCode

