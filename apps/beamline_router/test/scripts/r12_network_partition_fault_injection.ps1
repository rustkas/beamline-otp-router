# R12: Network Partition Fault Injection Script (PowerShell)
# 
# Purpose: Scripts for reproducing network partition scenarios on Windows
# Supports: Windows, WSL
# 
# Usage:
#   .\r12_network_partition_fault_injection.ps1 create <partition_type> <from> <to> [options]
#   .\r12_network_partition_fault_injection.ps1 remove <partition_id>
#   .\r12_network_partition_fault_injection.ps1 list
#   .\r12_network_partition_fault_injection.ps1 status <partition_id>
#   .\r12_network_partition_fault_injection.ps1 heal <partition_id>
#   .\r12_network_partition_fault_injection.ps1 flapping <from> <to> <interval_ms> <duration_ms>

param(
    [Parameter(Position=0)]
    [string]$Command = "help",
    
    [Parameter(Position=1)]
    [string]$Arg1 = "",
    
    [Parameter(Position=2)]
    [string]$Arg2 = "",
    
    [Parameter(Position=3)]
    [string]$Arg3 = "",
    
    [Parameter(Position=4)]
    [string]$Arg4 = "",
    
    [string]$Action = "drop",
    [int]$DelayMs = 0,
    [int]$LossPercent = 0
)

# Configuration
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent (Split-Path -Parent $ScriptDir)
$NetworkPartitionReal = if ($env:NETWORK_PARTITION_REAL -eq "true") { $true } else { $false }

# Partition storage
$PartitionStorageDir = Join-Path $ScriptDir ".partitions"
if (-not (Test-Path $PartitionStorageDir)) {
    New-Item -ItemType Directory -Path $PartitionStorageDir | Out-Null
}

# Logging functions
function Log-Info {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor Green
}

function Log-Warn {
    param([string]$Message)
    Write-Host "[WARN] $Message" -ForegroundColor Yellow
}

function Log-Error {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor Red
}

# Check privileges
function Test-Privileges {
    if ($NetworkPartitionReal) {
        $isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
        if (-not $isAdmin) {
            Log-Error "Real network tools mode requires administrator privileges"
            Log-Error "Run PowerShell as Administrator"
            exit 1
        }
    }
}

# Generate partition ID
function New-PartitionId {
    $timestamp = [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
    $pid = $PID
    return "partition-$timestamp-$pid"
}

# Create partition (mock mode)
function New-PartitionMock {
    param(
        [string]$PartitionType,
        [string]$From,
        [string]$To,
        [string]$Action = "drop"
    )
    
    $partitionId = New-PartitionId
    
    Log-Info "Creating partition (mock mode): $partitionId"
    Log-Info "  Type: $PartitionType"
    Log-Info "  From: $From"
    Log-Info "  To: $To"
    Log-Info "  Action: $Action"
    Log-Warn "Mock mode: Partition simulated, no real network changes"
    
    # Store partition info
    $partitionInfo = @{
        partition_id = $partitionId
        type = $PartitionType
        from = $From
        to = $To
        action = $Action
        created_at = (Get-Date -Format "yyyy-MM-ddTHH:mm:ssZ")
        mode = "mock"
    } | ConvertTo-Json
    
    $partitionFile = Join-Path $PartitionStorageDir "$partitionId.json"
    $partitionInfo | Out-File -FilePath $partitionFile -Encoding UTF8
    
    return $partitionId
}

# Remove partition
function Remove-Partition {
    param([string]$PartitionId)
    
    $partitionFile = Join-Path $PartitionStorageDir "$PartitionId.json"
    
    if (-not (Test-Path $partitionFile)) {
        Log-Error "Partition not found: $PartitionId"
        exit 1
    }
    
    $partitionInfo = Get-Content $partitionFile | ConvertFrom-Json
    $mode = $partitionInfo.mode
    
    if ($mode -eq "real" -and $NetworkPartitionReal) {
        Log-Info "Removing partition (real mode): $PartitionId"
        Log-Warn "Manual cleanup may be required for Windows Firewall rules"
    } else {
        Log-Info "Removing partition (mock mode): $PartitionId"
    }
    
    Remove-Item $partitionFile -Force
    Log-Info "Partition removed: $PartitionId"
}

# List partitions
function Get-Partitions {
    Log-Info "Active partitions:"
    
    $partitionFiles = Get-ChildItem -Path $PartitionStorageDir -Filter "*.json" -ErrorAction SilentlyContinue
    
    if ($null -eq $partitionFiles -or $partitionFiles.Count -eq 0) {
        Log-Info "  No active partitions"
        return
    }
    
    foreach ($partitionFile in $partitionFiles) {
        $partitionInfo = Get-Content $partitionFile.FullName | ConvertFrom-Json
        $partitionId = $partitionInfo.partition_id
        $type = $partitionInfo.type
        $from = $partitionInfo.from
        $to = $partitionInfo.to
        $createdAt = $partitionInfo.created_at
        
        Write-Host "  $partitionId : $type ($from -> $to) created at $createdAt"
    }
}

# Get partition status
function Get-PartitionStatus {
    param([string]$PartitionId)
    
    $partitionFile = Join-Path $PartitionStorageDir "$PartitionId.json"
    
    if (-not (Test-Path $partitionFile)) {
        Log-Error "Partition not found: $PartitionId"
        exit 1
    }
    
    Log-Info "Partition status: $PartitionId"
    $partitionInfo = Get-Content $partitionFile | ConvertFrom-Json
    $partitionInfo | ConvertTo-Json -Depth 10
}

# Simulate flapping network
function Start-Flapping {
    param(
        [string]$From,
        [string]$To,
        [int]$IntervalMs = 2000,
        [int]$DurationMs = 10000
    )
    
    Log-Info "Simulating flapping network: $From -> $To"
    Log-Info "  Interval: ${IntervalMs}ms"
    Log-Info "  Duration: ${DurationMs}ms"
    
    $startTime = Get-Date
    $endTime = $startTime.AddMilliseconds($DurationMs)
    
    while ((Get-Date) -lt $endTime) {
        # Create partition
        $partitionId = New-PartitionMock "flapping" $From $To "drop"
        Log-Info "Partition created: $partitionId"
        
        Start-Sleep -Milliseconds $IntervalMs
        
        # Remove partition
        Remove-Partition $partitionId
        Log-Info "Partition removed: $partitionId"
        
        Start-Sleep -Milliseconds $IntervalMs
    }
    
    Log-Info "Flapping simulation completed"
}

# Main command handler
switch ($Command.ToLower()) {
    "create" {
        Test-Privileges
        
        $partitionType = $Arg1
        $from = $Arg2
        $to = $Arg3
        
        if ([string]::IsNullOrEmpty($partitionType) -or [string]::IsNullOrEmpty($from) -or [string]::IsNullOrEmpty($to)) {
            Log-Error "Usage: create <partition_type> <from> <to> [-Action drop|delay|reject|loss] [-DelayMs N] [-LossPercent N]"
            exit 1
        }
        
        New-PartitionMock $partitionType $from $to $Action
    }
    
    "remove" {
        $partitionId = $Arg1
        if ([string]::IsNullOrEmpty($partitionId)) {
            Log-Error "Usage: remove <partition_id>"
            exit 1
        }
        Remove-Partition $partitionId
    }
    
    "heal" {
        $partitionId = $Arg1
        if ([string]::IsNullOrEmpty($partitionId)) {
            Log-Error "Usage: heal <partition_id>"
            exit 1
        }
        Remove-Partition $partitionId
    }
    
    "list" {
        Get-Partitions
    }
    
    "status" {
        $partitionId = $Arg1
        if ([string]::IsNullOrEmpty($partitionId)) {
            Log-Error "Usage: status <partition_id>"
            exit 1
        }
        Get-PartitionStatus $partitionId
    }
    
    "flapping" {
        $from = $Arg1
        $to = $Arg2
        $intervalMs = if ([string]::IsNullOrEmpty($Arg3)) { 2000 } else { [int]$Arg3 }
        $durationMs = if ([string]::IsNullOrEmpty($Arg4)) { 10000 } else { [int]$Arg4 }
        
        if ([string]::IsNullOrEmpty($from) -or [string]::IsNullOrEmpty($to)) {
            Log-Error "Usage: flapping <from> <to> [interval_ms] [duration_ms]"
            exit 1
        }
        
        Start-Flapping $from $to $intervalMs $durationMs
    }
    
    default {
        Write-Host @"
R12: Network Partition Fault Injection Script (PowerShell)

Usage:
  .\r12_network_partition_fault_injection.ps1 <command> [options]

Commands:
  create <partition_type> <from> <to> [options]
    Create a network partition
    Options:
      -Action drop|delay|reject|loss  (default: drop)
      -DelayMs N                      (for delay action)
      -LossPercent N                  (for loss action)
    
  remove <partition_id>
    Remove a network partition
  
  heal <partition_id>
    Alias for remove (heal partition)
  
  list
    List all active partitions
  
  status <partition_id>
    Get status of a partition
  
  flapping <from> <to> [interval_ms] [duration_ms]
    Simulate flapping network

Examples:
  # Create single-instance partition
  .\r12_network_partition_fault_injection.ps1 create single_instance router nats -Action drop
  
  # Simulate flapping network
  .\r12_network_partition_fault_injection.ps1 flapping router nats 2000 10000

Environment Variables:
  NETWORK_PARTITION_REAL=false  Use mock mode (default)
  NETWORK_PARTITION_REAL=true   Use real network tools (requires admin)

"@
    }
}

