# R12: Network Partition Scenarios - Quick Start

**Date**: 2025-11-30  
**Status**: Documentation  
**Purpose**: Quick start guide for R12 network partition testing

## Overview

R12 tests verify system behavior under network partitions in two modes:

- **Single-instance** – one service/router instance losing connection to external dependencies
- **Multi-instance / split-brain** – multiple instances, some losing connection to others

## Quick Start

### Run All Network Partition Tests

```bash
cd apps/otp/router
rebar3 ct --suite router_network_partition_SUITE
```

### Run Specific Test Group

```bash
# Single-instance tests
rebar3 ct --suite router_network_partition_SUITE --group single_instance_tests

# Multi-instance tests
rebar3 ct --suite router_network_partition_SUITE --group multi_instance_tests

# Service-broker tests
rebar3 ct --suite router_network_partition_SUITE --group service_broker_tests

# Flapping network tests
rebar3 ct --suite router_network_partition_SUITE --group flapping_network_tests
```

### Run Single Test

```bash
rebar3 ct --suite router_network_partition_SUITE --case test_single_instance_jetstream_partition_short
```

## Test Modes

### Mock Mode (Default)

Tests run in mock mode by default, using `meck` to simulate network partitions. This works without root privileges and is suitable for CI/CD pipelines.

```bash
# Mock mode (default)
export NETWORK_PARTITION_REAL=false
rebar3 ct --suite router_network_partition_SUITE
```

### Real Network Tools Mode (Optional)

For real network manipulation using `iptables`, `tc`, and `netem`, enable real mode. This requires root privileges or container capabilities.

```bash
# Real network tools mode (requires root/privileges)
export NETWORK_PARTITION_REAL=true
sudo rebar3 ct --suite router_network_partition_SUITE
```

**Note**: Real mode is optional. Mock mode provides sufficient coverage for most scenarios.

## Fault Injection Scripts

### Bash Script

```bash
# Create single-instance partition (Router -> NATS)
./test/scripts/r12_network_partition_fault_injection.sh create single_instance router nats --action drop

# Create partition with delay
./test/scripts/r12_network_partition_fault_injection.sh create single_instance router nats --action delay --delay_ms 5000

# Simulate flapping network
./test/scripts/r12_network_partition_fault_injection.sh flapping router nats 2000 10000

# List active partitions
./test/scripts/r12_network_partition_fault_injection.sh list

# Remove partition
./test/scripts/r12_network_partition_fault_injection.sh remove <partition_id>
```

### PowerShell Script

```powershell
# Create single-instance partition
.\test\scripts\r12_network_partition_fault_injection.ps1 create single_instance router nats -Action drop

# Simulate flapping network
.\test\scripts\r12_network_partition_fault_injection.ps1 flapping router nats 2000 10000

# List active partitions
.\test\scripts\r12_network_partition_fault_injection.ps1 list
```

## Test Scenarios

### Single-Instance Tests

1. **Loss of Connection to JetStream / Message Broker**
   - Short partition (few seconds)
   - Long partition (minutes)
   - Recovery validation

2. **Loss of Connection to External Services (DB / API)**
   - Short partition
   - Long partition
   - Recovery validation

### Multi-Instance / Split-Brain Tests

1. **Service Cluster Partition (Split-Brain)**
   - Leader election during partition
   - No duplicate message processing
   - Recovery after partition

2. **Partition of Access to JetStream / Storage**
   - Instance A isolated
   - JetStream cluster split
   - Recovery validation

3. **Split-Brain with Locks / Distributed Transactions**
   - Distributed locks during partition
   - Lock recovery after partition

## Expected Behavior

### During Partition

- Router process remains alive (fail-open strategy)
- Metrics reflect partition (connection failures)
- No crashes or data loss
- Retry/backoff behavior (for service-broker partitions)
- Circuit breaker opens when threshold reached

### After Recovery

- Router reconnects automatically
- Normal operation restored
- New messages processed successfully
- State consistency maintained
- Circuit breaker closes after successful recovery

## Logs and Metrics

### Required Logs

- Connection loss detection
- Retry attempts with backoff
- Circuit breaker state changes
- Network partition resolution
- Connection restoration

### Required Metrics

- `router_nats_connection_failures_total` – Connection failures
- `router_nats_reconnect_attempts_total` – Reconnection attempts
- `router_nats_connection_restored_total` – Connection restorations
- `router_circuit_breaker_state` – Circuit breaker state
- `router_network_partition_duration_seconds` – Partition duration
- `router_network_partition_recovery_time_seconds` – Recovery time

See `R12_LOGS_AND_METRICS.md` for complete log and metric specifications.

## Troubleshooting

### Tests Fail with "Process Not Found"

**Issue**: Tests fail because router processes are not started.

**Solution**: Ensure `beamline_router` application is started in `init_per_suite/1`.

### Mock Mode Not Working

**Issue**: Tests fail because mocks are not set up correctly.

**Solution**: Check that `nats_mode = mock` is set in test configuration.

### Real Mode Requires Privileges

**Issue**: Real network tools mode fails with permission errors.

**Solution**: Run with `sudo` or use mock mode (default).

### Scripts Not Executable

**Issue**: Bash scripts fail with "Permission denied".

**Solution**: Make scripts executable:
```bash
chmod +x test/scripts/r12_network_partition_fault_injection.sh
```

## Documentation

- **Complete Specification**: `R12_NETWORK_PARTITION_SCENARIOS.md`
- **Logs and Metrics**: `R12_LOGS_AND_METRICS.md`
- **Network Partition Testing Guide**: `docs/dev/NETWORK_PARTITION_TESTING.md`
- **Fault Injection Tests**: `docs/dev/JETSTREAM_FAULT_INJECTION_TESTS.md`

## CI/CD Integration

Network partition tests are integrated into CI/CD pipelines:

- **Tags**: `@test_category network_partition, slow, integration, fault_injection`
- **Execution Time**: ~15-20 minutes (all tests)
- **Resource Requirements**: ~500 MB memory per test

## Status

✅ **Specification Complete** – All scenarios documented  
✅ **Scripts Created** – Bash and PowerShell fault injection scripts  
✅ **Documentation Complete** – Logs, metrics, and quick start guide  

**Test Coverage**:
- ✅ Single-instance partitions (3 tests)
- ✅ Multi-instance/split-brain (3 tests)
- ✅ Service-broker partitions (3 tests)
- ✅ Flapping network (3 tests)

**Total**: 12 test cases covering all network partition scenarios.

